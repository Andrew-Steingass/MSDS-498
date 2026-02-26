import os
from langchain_openai import ChatOpenAI
from langchain_core.messages import SystemMessage, RemoveMessage
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder
from pydantic import BaseModel, Field, ConfigDict
from langgraph.prebuilt import ToolNode

# Import our State and Tools
from src.state import AgentState
from src.tools import *
from dotenv import load_dotenv

load_dotenv()

# Initialize the LLMs
llm_fast = ChatOpenAI(model="gpt-4o-mini", temperature=0) #$0.15 / 1M tokens.
llm_smart = ChatOpenAI(model="gpt-4o", temperature=0) #$2.50 / 1M tokens

# ==============================================================================
# THE ROUTER AGENT
# ==============================================================================
# Define the Strict Output Structure
class RouterOutput(BaseModel):
    destination: str = Field(description="The agent to route to: 'injury_agent', 'pitching_agent', 'batting_agent', or 'general_agent'")
    new_player_detected: bool = Field(description="True ONLY if the user explicitly mentions a NEW player name different from the current context or says new player.")
    player_name: str = Field(description="The name of the NEW player detected. If no new player, leave empty.", default="")
    player_id: str = Field(description="The unique Player ID if the user explicitly types it (e.g., 'kershcl01'). Otherwise leave empty.", default="")

    model_config = ConfigDict(
        arbitrary_types_allowed=True,
        populate_by_name=True,
        )

def router_agent(state: AgentState):
    """
    This agent analyzes the user's latest message and decides which Specialist should handle it.
    """
    # --- DEBUG HISTORY BLOCK ---
    print("\n" + "="*50)
    print("[DEBUG] ROUTER RECEIVED FULL HISTORY:")
    for i, m in enumerate(state["messages"]):
        # Truncate the content to 75 characters so it does not flood your screen
        safe_content = str(m.content).replace('\n', ' ')[:150]
        print(f"   {i}. [{m.type.upper()}] {safe_content}...")
    print("="*50 + "\n")
    # --------------------------------

    # Get Current Context
    current_profile = state.get("profile", {})
    current_name = current_profile.get("name", "None") # Default to None if empty

    # The Persona
    system_prompt = f"""You are the Head of a baseball analytics team.
    CURRENT ACTIVE PLAYER: {current_name}

    YOUR JOB:
    1. Decide which agent should handle the user's request.
    2. Detect if the user is switching context to a NEW player.

    RULES FOR CONTEXT SWITCHING:
    - If user names a specific player different from "{current_name}": new_player_detected = True, player_name = [Extracted Name].
    - If user uses pronouns ("he", "him") AND "{current_name}" is "None": Look at the CHAT HISTORY. If a player was mentioned in the last AI response, set new_player_detected = True, player_name = [That Player].
    - If user uses pronouns referring to "{current_name}": new_player_detected = False.
    - If user says "Hi", "Thanks", or asks a general rule question: new_player_detected = False.

    SPECIALISTS:
    1. injury_agent: Model powered. Medical, injury risk, surgery probability.
    2. pitching_agent: Model powered. Future performance prediction, ERA outlook, 'What-If' scenarios for pitchers. Looking at improving or stagnation.
    3. batting_agent - Model powered. Use for specific calculations on matchup probabilities or expected batting average.
    4. general_agent - Use for greetings, history, rules, or broad baseball questions (e.g., "Who won the 1990 World Series?", "What is a slider?").
    5. model_explainer_agent: Use ONLY when the user asks HOW a model works, what features it uses, or requests mathematical explanations, or what models are avi.
    
    CRITICAL INSTRUCTION - CONTEXT CHECK:
    - Look at the LAST message from the AI.
    - If the AI just asked a question (e.g., "What is the age?"), you MUST route the user's answer back to the SAME agent.
    - Do NOT switch agents during data collection unless the user explicitly asks to switch.

    FIELD INSTRUCTIONS:
    - 'destination': The name of the agent (e.g., 'injury_agent').
    - 'new_player_detected': Set to True ONLY if the user explicitly names a DIFFERENT player than {current_name}.
    - 'player_name': If new_player_detected is True, put the new name here. If it is False, leave this empty string.
    """

    # --- FILTER SHIELD ---
    # Strip out RemoveMessage objects so the structured output parser does not crash
    allowed_types = ["human", "ai", "system", "tool"]
    clean_history = [m for m in state["messages"] if m.type in allowed_types]

    messages = [SystemMessage(content=system_prompt)] + clean_history
    
    # Use the smart model to ensure it adheres to the JSON schema
    structured_llm = llm_smart.with_structured_output(RouterOutput)
    decision = structured_llm.invoke(messages)
    print(f"Current Profile: {current_profile}")
    
    # Handle the Logic
    updates = {"next_agent": decision.destination}
    
    # --- LOGIC UPDATE: Handle Name AND ID ---
    if decision.new_player_detected and decision.player_name:
        # A NEW player was found! 
        print(f"\n--- SWITCHING CONTEXT: {current_name} -> {decision.player_name} ---")
        
        # Create the new profile dict
        # We check if 'decision.player_id' exists (it defaults to empty string "" in your model)
        new_id = decision.player_id if decision.player_id else None
        
        updates["profile"] = {
            "name": decision.player_name,
            "id": new_id  # Store the ID if provided!
        }
        
        # Clear old stats to avoid pollution
        updates["ml_inputs"] = {} 

    # Edge Case: User updates ONLY the ID for the CURRENT player
    # e.g. "Actually, use ID kershcl01" (without saying a new name)
    elif decision.player_id and not decision.new_player_detected:
        # Copy the existing profile so we don't lose the name
        updated_profile = current_profile.copy()
        updated_profile["id"] = decision.player_id
        
        updates["profile"] = updated_profile
        print(f"\n--- UPDATING ID: {decision.player_id} for {current_name} ---")
    else:
        # No new player. Keep the old profile and inputs.
        pass

    return updates
    

# ==============================================================================
# THE GENERAL ASSISTANT
# ==============================================================================
def general_agent(state: AgentState):
    """
    Handles general baseball chat, history, and rules.
    Explicitly instructed to RESOLVE PRONOUNS using history.
    """
    print(f"\n DEBUG: General Agent Memory Size: {len(state['messages'])}")
    for i, msg in enumerate(state['messages']):
        print(f"[{i}] {msg.type}: {msg.content[:50]}...")
    # Get the Profile Name (if available) to help ground the answer
    profile = state.get("profile", {})
    current_name = profile.get("name", "Unknown")

    system_prompt = f"""You are a helpful Baseball Assistant.
    
    CURRENT CONTEXT:
    - The user is currently discussing: {current_name}
    
    YOUR GOAL:
    Answer questions about baseball history, rules, teams, and players.
    
    CRITICAL INSTRUCTION - CONTEXT RESOLUTION:
    - The user may use pronouns like "he", "she", "they", "it", or "their".
    - You MUST look at the CHAT HISTORY (previous messages) to figure out who they are referring to.
    - If the user asks "Who was their pitcher?", look for the last mentioned team in the history.
    
    Example:
    - User: "Who won the 1999 World Series?"
    - AI: "The New York Yankees."
    - User: "Who was their manager?"
    - AI: "The Yankees' manager was Joe Torre." (Resolved 'their' -> Yankees & year is 1999)
    """
    
    # Pass the System Prompt + The Entire Chat History
    messages = [SystemMessage(content=system_prompt)] + state["messages"]
    
    response = llm_fast.invoke(messages)
    
    return {"messages": [response]}

# ==============================================================================
# THE INJURY SPECIALIST
# ==============================================================================
def injury_agent(state: AgentState):
    system_prompt = """You are an Injury Prevention Specialist.
    To use the model, you need: Age, Velocity, Surgery History.
    If missing any, ask the user. If all are present, call 'mock_injury_model'."""
    
    llm_with_tools = llm_smart.bind_tools([mock_injury_model])
    messages = [SystemMessage(content=system_prompt)] + state["messages"]
    return {"messages": [llm_with_tools.invoke(messages)]}

# Tool Node for Injury
def get_injury_tool_node():
    return ToolNode([mock_injury_model])

# ==============================================================================
# THE PITCHING SPECIALIST
# ==============================================================================
def pitching_agent(state: AgentState):
    """
    The specialized node for Pitching Analysis.
    Integrates State (Profile + ML Inputs) into the prompt.
    """
    pitching_tools = [predict_pitching_2016]
    llm_with_tools = llm_smart.bind_tools(pitching_tools)
    PITCHING_SYSTEM_PROMPT = """
        You are the Pitching Analytics Specialist for Spin Rate Consulting (2015-2016 Offseason).
        Your goal is to predict if a pitcher will improve their ERA in 2016 using the 'predict_pitching_2016' tool.

        ### CURRENT CONTEXT (READ CAREFULLY)
        - **Active Player Profile**: {active_player}
        - **Current Simulation Overrides**: {ml_inputs}

        ### INSTRUCTIONS
        1. **Identify the Player**: 
        - If the user says "he", "him", or "the pitcher", you MUST use the **Active Player Profile** defined above.
        - If the user specifies a new name (e.g., "Check Kershaw"), use that name.

        2. **Handle Simulation Data**:
        - The 'Current Simulation Overrides' dictionary contains stats the user has previously set (e.g., {{'era': 2.50}}).
        - When calling `predict_pitching_2016`, you MUST pass these values as arguments unless the user explicitly changes them in this turn.
        - Example: If inputs are {{'era': 2.50}} and user says "Run prediction", call `predict_pitching_2016(player_name="...", era=2.50)`.

        3. **Ambiguity Handling**:
        - If the tool returns a list of "Did you mean...?" suggestions, DO NOT guess. 
        - Stop and ask the user to clarify exactly which player they mean incorporating the tool output (e.g., "Did you mean the Will Smith born in 1989?").

        4. **Response Style**:
        - Be professional but concise. 
        - Present the "Stagnation Risk" clearly.
        - If running a simulation, explicitly state: "Based on a hypothetical ERA of [X]..."
        """
    
    """
    State-Aware Pitching Node.
    Injects profile & ml_inputs into the system prompt before invoking the LLM.
    """
    # Extract persistent state
    profile = state.get("profile", "Unknown")
    ml_inputs = state.get("ml_inputs", {})
    
    # Format the prompt with current context
    prompt = ChatPromptTemplate.from_messages([
        ("system", PITCHING_SYSTEM_PROMPT),
        MessagesPlaceholder(variable_name="messages"),
    ])
    
    # Generate the chain
    chain = prompt | llm_with_tools
    
    # Execute
    # We pass the full message history plus our injected variables
    response = chain.invoke({
        "messages": state["messages"],
        "active_player": profile,
        "ml_inputs": str(ml_inputs)
    })

    return {"messages": [response]}

# ==============================================================================
# THE BATTING SPECIALIST
# ==============================================================================
def batting_agent(state: AgentState):
    system_prompt = """You are a Batting Matchup Expert.
    Your goal is to project batting average.
    
    To use the model, you need:
    1. Batter Name
    2. Opposing Pitcher Type ('LHP' or 'RHP')
    
    If missing any, ask the user. If all are present, 'mock_batting_model'."""
    
    llm_with_tools = llm_smart.bind_tools([mock_batting_model])
    messages = [SystemMessage(content=system_prompt)] + state["messages"]
    return {"messages": [llm_with_tools.invoke(messages)]}

# Tool Node for Batting
def get_batting_tool_node():
    return ToolNode([mock_batting_model])


# ==============================================================================
# THE MODEL EXPLAINER
# ==============================================================================
def model_explainer_agent(state: AgentState):
    """
    Dedicated agent for explaining models.
    Uses tools for accuracy, but deletes the tool output from history to save tokens.
    """
# 1. FILTER SHIELD: Strip out any ghost RemoveMessages before processing
    allowed_types = ["human", "ai", "system", "tool"]
    clean_messages = [m for m in state["messages"] if m.type in allowed_types]
    
    last_msg = clean_messages[-1]

    system_prompt = """You are the Lead Data Scientist for a baseball analytics team.
    Your job is to explain HOW our models work to users.

    OUR AVAILABLE MODELS (The Directory):
    - Injury Risk Model: Predicts injury likelihood based on Age, Velocity, and Surgery History.
    - Pitching Analytics Model: Analyzes pitch mechanics based on Pitch Type and Spin Rate.
    - Batting Matchup Model: A forecasting model that estimates a batter's offensive production for the upcoming season to evaluate expected performance.
    
    RULES:
    1. If the user asks "What models do you have?", summarize the directory above.
    2. If the user asks for DEEP details about a specific model (like inputs), ALWAYS use the 'read_model_documentation' tool.
    3. Translate technical jargon into clear, digestible explanations using simple terms for a non-technical audience using metaphors where appropriate.
    4. If the user asks about a specific feature (like Age or Spin Rate), explain why the model cares about it.
    """
    # SCENARIO A: The tool just ran. 
    # The history looks like: [..., AI (Tool Call), Tool (Result)]
    if last_msg.type == "tool":
            # 1. Generate the final answer using the clean history
            final_response = llm_smart.invoke([SystemMessage(content=system_prompt)] + clean_messages)
            
            # 2. Identify the exact messages to delete
            tool_result_id = last_msg.id
            ai_call_id = clean_messages[-2].id # The AI thought that initiated the tool
            
            # 3. Return the final answer and delete BOTH the call and the result
            return {
                "messages": [
                    final_response,                  
                    RemoveMessage(id=tool_result_id), # Deletes the big doc
                    RemoveMessage(id=ai_call_id)      # Deletes the dangling tool call
                ]
            }

    # SCENARIO B: First pass (LLM decides to call tool)
    llm_with_tools = llm_smart.bind_tools([read_model_documentation])
    response = llm_with_tools.invoke([SystemMessage(content=system_prompt)] + clean_messages)
    
    return {"messages": [response]}


# ==========================================
# TOOL NODES (Execution Environments)
# ==========================================

def get_injury_tool_node(): 
    return ToolNode([mock_injury_model])

# def get_pitching_tool_node(): 
#     return ToolNode([mock_pitching_model])

def get_batting_tool_node(): 
    return ToolNode([mock_batting_model])

def get_explainer_tool_node(): 
    return ToolNode([read_model_documentation])