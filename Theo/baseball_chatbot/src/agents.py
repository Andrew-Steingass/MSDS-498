import os
from langchain_openai import ChatOpenAI
from langchain_core.messages import SystemMessage
# FIX: Use standard pydantic
from pydantic import BaseModel, Field
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

def router_agent(state: AgentState):
    """
    This agent analyzes the user's latest message and decides which Specialist
    should handle it.
    """
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
    - If user names a specific player different from "{current_name}": new_player_detected = True, player_name = "Ohtani".
    - If user uses pronouns ("he", "him", "his") referring to the current player: new_player_detected = False.
    - If user says "Hi", "Thanks", or asks a general rule question: new_player_detected = False.

    SPECIALISTS:
    1. 'injury_agent' - Model powered. Use for specific calculations on health, surgery risk, or "Assess [Player]".
    2. 'pitching_agent' - Model powered. Use for specific calculations on pitch mechanics, spin rate, velocity.
    3. 'batting_agent' - Model powered. Use for specific calculations on matchup probabilities or expected batting average.
    4. 'general_agent' - Use for greetings, history, rules, or broad baseball questions (e.g., "Who won the 1990 World Series?", "What is a slider?").

    CRITICAL INSTRUCTION - CONTEXT CHECK:
    - Look at the LAST message from the AI.
    - If the AI just asked a question (e.g., "What is the age?"), you MUST route the user's answer back to the SAME agent.
    - Do NOT switch agents during data collection unless the user explicitly asks to switch.

    FIELD INSTRUCTIONS:
    - 'destination': The name of the agent (e.g., 'injury_agent').
    - 'new_player_detected': Set to True ONLY if the user explicitly names a DIFFERENT player than {current_name}.
    - 'player_name': If new_player_detected is True, put the new name here. If it is False, leave this empty string.
    """

    messages = [SystemMessage(content=system_prompt)] + state["messages"]
    
    # We use the smart model to ensure it adheres to the JSON schema
    structured_llm = llm_smart.with_structured_output(RouterOutput)
    decision = structured_llm.invoke(messages)
    print(f"Current Profile: {current_profile}")
    
    # Handle the Logic
    updates = {"next_agent": decision.destination}
    
    if decision.new_player_detected and decision.player_name:
        # A NEW player was found! 
        # Action: Wipe the slate and set the new name.
        print(f"\n--- 🔄 SWITCHING CONTEXT: {current_name} -> {decision.player_name} ---")
        updates["profile"] = {"name": decision.player_name}
        updates["ml_inputs"] = {} # Clear old stats (velocity, age) to avoid pollution
    else:
        # No new player. Keep the old profile and inputs.
        pass

    return updates
    

# # ==============================================================================
# # THE CONTEXT REFINER - ONLY ADDING IF NEEDED
# # ==============================================================================
# def context_refiner_node(state: AgentState):
#     """
#     Pre-processing node.
#     Looks at the chat history and the latest user message.
#     If the latest message uses pronouns (he, she, it, they, their) or is ambiguous,
#     it rewrites the message to be standalone and explicit.
#     """
#     latest_msg = state["messages"][-1].content
    
#     # We only run this if the message is short or likely ambiguous to save tokens
#     # (Optional optimization)
    
#     system_prompt = """You are a Context Refiner.
#     Your job is to rewrite the user's latest query to be fully standalone based on the chat history.
    
#     RULES:
#     1. Replace pronouns (he, she, it, they) with specific names/entities from the chat history.
#     2. If the query is already clear, output it EXACTLY as is.
#     3. Do NOT answer the question. Just rewrite it.
    
#     Example:
#     Chat History: "Who won the 1999 World Series?" -> "The Yankees."
#     User: "Who was their manager?"
#     Output: "Who was the Yankees' manager in 1999?"
#     """
    
#     messages = [SystemMessage(content=system_prompt)] + state["messages"]
    
#     response = llm_fast.invoke(messages)
#     rewritten_query = response.content
    
#     if rewritten_query != latest_msg:
#         print(f"REWRITER: '{latest_msg}' -> '{rewritten_query}'")
        
#         # We replace the last message with the clearer version so the Agents see it too
#         # Note: In a real app, you might want to keep the original for the UI, 
#         # but for the logic, we want the clean version.
#         from langchain_core.messages import HumanMessage
#         return {"messages": [HumanMessage(content=rewritten_query)]}
    
#     return {} # No change


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
        print(f"   [{i}] {msg.type}: {msg.content}...")
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
    system_prompt = """You are a Pitching Performance Expert.
    Your goal is to grade pitch quality.
    
    To use the model, you need:
    1. Pitch Type (Fastball, Slider, etc.)
    2. Spin Rate (int)
    
    If missing any, ask the user. If all are present, 'mock_pitching_model'."""
    
    llm_with_tools = llm_smart.bind_tools([mock_pitching_model])
    messages = [SystemMessage(content=system_prompt)] + state["messages"]
    return {"messages": [llm_with_tools.invoke(messages)]}

# Tool Node for Pitching
def get_pitching_tool_node():
    return ToolNode([mock_pitching_model])

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
# THE TOOL NODE
# Needs all tools for all agents as this is actually running the tool code
# ==============================================================================

def get_tool_node():
    """
    This is a pre-built node from LangGraph that automatically:
    1. Reads the 'tool_calls' output from the Agent.
    2. Finds the matching function in your 'tools.py'.
    3. Runs the function.
    4. Returns the output as a 'ToolMessage'.
    """
    # We list ALL tools available to the bot here
    tools_list = [mock_injury_model]
    
    return ToolNode(tools_list)