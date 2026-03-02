import os
from langchain_openai import ChatOpenAI
from langchain_core.messages import SystemMessage, RemoveMessage, AIMessage
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder
from pydantic import BaseModel, Field, ConfigDict
from langgraph.prebuilt import ToolNode
import json

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
    - If there is just a number look at the history to see who it is referring to. Use this to update the ID.

    SPECIALISTS:
    1. injury_agent: Model powered. Handles queries about player health, injury history, risk assessments, and durability. Use for checking medical records or asking "Is he safe to sign?"
    2. pitching_agent: Psudo Model powered. Future performance prediction, ERA outlook, 'What-If' scenarios for pitchers. Looking at improving or stagnation.
    3. batting_agent - Psudo Model powered. Handles offensive trend analysis and OPS forecasts. Use for evaluating batters, checking historical consistency, and making 'Sign' vs 'Fade' decisions based on past performance.
    4. general_agent - Use for greetings, history, rules, or broad baseball questions or general should we trade for X player (e.g., "Who won the 1990 World Series?", "What is a slider?").
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
    Answer questions about baseball history, rules, teams, and players. If a year is not specificed, but needed assume 2015 and say 2015 in the answer. 
    
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


def _fallback_extract_name(message_text: str, llm) -> str:
    """
    Quickly extracts a name if the Router missed it using JSON mode.
    ESCAPING FIX: We use {{ and }} for the JSON example so LangChain treats them as text.
    """
    # Notice the double curly braces {{ }} around the JSON example
    system_prompt = (
        "You are an expert entity extractor. "
        "Extract the baseball player's name from the user query. "
        "Return a JSON object with a single key 'name'. "
        "If no specific player is mentioned, return 'Unknown'. "
        "Example: {{\"name\": \"Clayton Kershaw\"}}" 
    )
    
    prompt = ChatPromptTemplate.from_messages([
        ("system", system_prompt),
        ("human", "{query}")
    ])
    
    try:
        # Force JSON mode
        chain = prompt | llm.bind(response_format={"type": "json_object"})
        
        # KEY FIX: Use tags=["hidden"] to hide this from the UI
        response = chain.invoke(
            {"query": message_text}, 
            config={"tags": ["hidden"]} 
        )
        
        # Parse the JSON content
        content = response.content
        if not content:
            return "Unknown"
            
        data = json.loads(content)
        return data.get("name", "Unknown")
        
    except Exception as e:
        # If anything goes wrong (JSON error, API error), fall back safely
        print(f"Fallback Extraction Error: {e}")
        return "Unknown"
    

# ==============================================================================
# THE INJURY SPECIALIST
# ==============================================================================
def injury_agent(state: AgentState):
    INJURY_SYSTEM_PROMPT = """
        You are the Head Athletic Trainer. Your goal is to protect the team from risky investments.

        You have access to the 'check_injury_history' tool. When asked about a player:
        1. Call the tool to get their risk timeline.
        2. Analyze the TREND of RISK and Actual Injury. (e.g., "Stable at Low Risk" vs "Escalating to High Risk").
        3. Provide a verdict: "Safe", "Caution", or "Hard Pass".

        CONTEXT:
        Active Player Name: {player_name}
        Active Player ID: {player_id}
        Current Date: Winter 2015.
    """
    
    # 1. GET CURRENT STATE
    profile = state.get("profile", {}).copy() # Copy to avoid mutation issues
    name = profile.get("name", "Unknown")
    p_id = profile.get("id") # Might be None
    
    # 2. RESOLUTION LOGIC (The New Logic)
    if name == "Unknown":
        last_msg = state["messages"][-1].content
        extracted_name = _fallback_extract_name(last_msg, llm_smart)
        
        if extracted_name != "Unknown":
            print(f"Pitching Agent: Fallback extraction found '{extracted_name}'")
            profile["name"] = extracted_name
            name = extracted_name

    # If we have a name but NO ID, we need to resolve it.
    if name and not p_id:
        print(f"Injury Agent: Resolving ID for '{name}'...")
        resolution = resolve_player_id(name, dataset=INJURY_DATA)
        
        # CHECK: Is it a clean ID or an Ambiguity Message?
        if "Did you mean" in resolution or "Could not find" in resolution:
            # STOP! Return the ambiguity message directly to the user.
            print(f"Injury Agent: Ambiguous Name. Asking user for clarification.")
            return {
                "messages": [AIMessage(content=resolution)]
            }
        else:
            # SUCCESS! We found the ID.
            print(f"Injury Agent: Resolved '{name}' to ID: {resolution}")
            p_id = resolution
            
            # Update the local profile variable so we can save it to state later
            profile["id"] = p_id

    # 3. STANDARD AGENT FLOW (Now that we have an ID)
    formatted_prompt = INJURY_SYSTEM_PROMPT.format(
        player_name=name,
        player_id=p_id
    )
    
    # Bind the tool
    tools = [check_injury_history]
    model_with_tools = llm_smart.bind_tools(tools)
    
    # Invoke the model
    messages = [{"role": "system", "content": formatted_prompt}] + state["messages"]
    response = model_with_tools.invoke(messages)
    
    # 4. RETURN UPDATE
    # We return the response AND the updated profile (with the new ID)
    return {
        "messages": [response],
        "profile": profile 
    }



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
    
    # 1. READ STATE
    profile = state.get("profile", {}).copy()
    ml_inputs = state.get("ml_inputs", {})
    
    name = profile.get("name", "Unknown")
    p_id = profile.get("id") # Might be None
    print(f"Name: {name}, ID: {p_id}")
    
    if name == "Unknown":
        last_msg = state["messages"][-1].content
        extracted_name = _fallback_extract_name(last_msg, llm_smart)
        
        if extracted_name != "Unknown":
            print(f"Pitching Agent: Fallback extraction found '{extracted_name}'")
            profile["name"] = extracted_name
            name = extracted_name
            
    p_id = profile.get("id")

    # If we have a name but NO ID, resolve it now.
    if name != "Unknown" and not p_id:
        print(f"Pitching Agent: Resolving ID for '{name}'...")
        resolution = resolve_player_id(name, dataset=INFERENCE_DATA)
        
        if "Did you mean" in resolution or "Could not find" in resolution:
            # IMPORTANT: We return the message AND the current profile state
            # This ensures 'name' stays 'Martinez' for the next turn.
            print("In did you mean-", resolution, profile)
            return {
                "messages": [AIMessage(content=resolution)],
                "profile": profile # Keep the current name in state
            }
        else:
            # SUCCESS! Update local variables.
            print(f"Pitching Agent: Resolved to ID: {resolution}")
            p_id = resolution
            profile["id"] = p_id # Save for return update

    # 3. CONSTRUCT CHAIN
    prompt = ChatPromptTemplate.from_messages([
        ("system", PITCHING_SYSTEM_PROMPT),
        MessagesPlaceholder(variable_name="messages"),
    ])
    
    # Bind the updated tool
    tools = [predict_pitching_2016]
    llm_with_tools = llm_smart.bind_tools(tools)
    chain = prompt | llm_with_tools
    
    # 4. EXECUTE
    response = chain.invoke({
        "messages": state["messages"],
        "active_player": name,
        "player_id": p_id, # <--- Pass the resolved ID to the prompt
        "ml_inputs": str(ml_inputs)
    })

    # 5. RETURN (Update profile with ID if we found it)
    return {
        "messages": [response],
        "profile": profile
    }
    


# ==============================================================================
# THE BATTING SPECIALIST
# ==============================================================================
def batting_agent(state: AgentState):
    BATTING_SYSTEM_PROMPT = """
        You are the Head Batting Coach. Your goal is to evaluate player offensive consistency and future value.

        You have access to the 'check_batting_stats' tool. When asked about a player:
        1. Call the tool to get their OPS history and Model Predictions.
        2. Analyze the accuracy of past predictions and the player's overall OPS Trend.
        3. Provide a verdict: "Sign", "Fade", or "Watch List".

        CONSTRAINTS:
        - Our data currently ends at 2014 (forecasting 2015). 
        - Since it is Winter 2015, use the 2014 prediction to discuss how they likely performed this past season.
        
        CONTEXT:
        Active Player Name: {player_name}
        Active Player ID: {player_id}
        Current Date: Winter 2015.
    """
    
    # 1. GET CURRENT STATE
    profile = state.get("profile", {}).copy()
    name = profile.get("name", "Unknown")
    p_id = profile.get("id")
    
    # 2. SELECTION & FALLBACK LOGIC
    # (Identical to Injury Agent to ensure robustness)
    
    # A. Check if user typed a number (Selection from previous list)
    last_msg_content = state["messages"][-1].content.strip()
    if last_msg_content.isdigit() and not p_id:
        selected_id = _fallback_extract_name(state["messages"], last_msg_content)
        if selected_id:
            print(f"Batting Agent: Selection resolved to ID: {selected_id}")
            p_id = selected_id
            profile["id"] = p_id

    # B. Fallback Extraction (If name is still Unknown)
    if name == "Unknown" and not p_id:
        extracted_name = _fallback_extract_name(last_msg_content, llm_smart)
        if extracted_name != "Unknown":
            print(f"Batting Agent: Fallback extraction found '{extracted_name}'")
            name = extracted_name
            profile["name"] = name

    # C. Resolution (Name -> ID)
    if name != "Unknown" and not p_id:
        print(f"Batting Agent: Resolving ID for '{name}'...")
        # CRITICAL: Use BATTING_DATA here so we don't suggest pitchers!
        resolution = resolve_player_id(name, dataset=BATTING_DATA)
        
        if "Did you mean" in resolution or "Could not find" in resolution:
            print(f"Batting Agent: Ambiguous Name. Asking user for clarification.")
            return {
                "messages": [AIMessage(content=resolution)],
                "profile": profile # Return profile to save the Name
            }
        else:
            print(f"Batting Agent: Resolved '{name}' to ID: {resolution}")
            p_id = resolution
            profile["id"] = p_id

    # 3. STANDARD AGENT FLOW
    # Only proceed if we have a valid ID
    if p_id:
        formatted_prompt = BATTING_SYSTEM_PROMPT.format(
            player_name=name,
            player_id=p_id
        )
        
        tools = [check_batting_stats]
        model_with_tools = llm_smart.bind_tools(tools)
        
        # Invoke
        messages = [{"role": "system", "content": formatted_prompt}] + state["messages"]
        response = model_with_tools.invoke(messages)
        
        return {
            "messages": [response],
            "profile": profile
        }
    
    # 4. FAIL SAFE (If we still don't have an ID or Name)
    return {
        "messages": [AIMessage(content="I couldn't identify the batter you are asking about. Please provide a full name.")],
        "profile": profile
    }


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
def get_explainer_tool_node(): 
    return ToolNode([read_model_documentation])