import chainlit as cl
import uuid
import logging
from langchain_core.messages import HumanMessage, AIMessage
from src.graph import build_graph
import warnings
# 

# 1. Configuration
logging.getLogger("langchain_core.callbacks.manager").setLevel(logging.ERROR)
warnings.filterwarnings(
    "ignore", 
    message=".*PydanticSerializationUnexpectedValue.*"
)

# MAPPING: Graph Node Name -> UI Display Name
NODE_TO_NAME = {
    "general_agent": "General BaseBot",       
    "pitching_agent": "Pitching Model BaseBot", 
    "batting_agent": "Batting Model BaseBot",    
    "injury_agent": "Injury Model BaseBot",      
    "model_explainer_agent": "Explainer BaseBot", 
    "router": "Router"
}

graph_app = build_graph()

@cl.on_chat_start
async def start():
    thread_id = str(uuid.uuid4())
    cl.user_session.set("thread_id", thread_id)
    cl.user_session.set("graph", graph_app)

@cl.set_starters
async def set_starters():
    return [
        # 1. PITCHING AGENT
        cl.Starter(
            label="Pitching Simulation (Full ML Powered)",
            message="Run a simulation for Dallas Keuchel: What if he raises his K/9 to 10.5 in 2016? Will he improve?"
            ),

        # 2. INJURY AGENT (Medical/Risk Focus)
        cl.Starter(
            label="Injury Risk Assessment",
            message="Check the medical report for Yu Darvish. Is he too risky to sign for the 2016 season?"
        ),

        # 3. BATTING AGENT (Trend/Value Focus)
        cl.Starter(
            label="Batting OPS Forecast",
            message="Evaluate Mike Trout's offensive value. Based on his OPS trend and model predictions, should we sign him?"
        ),
        
        # 4. GENERAL AGENT
        cl.Starter(
            label="Who won the 2015 World Series?",
            message="Who won the 2015 World Series?"
            ),

        # 5. MODEL EXPLAINER
        cl.Starter(
            label="How does the pitching model work?",
            message="How does the pitching model determine if a player will improve? What features are most important?"
            )
    ]


@cl.on_message
async def main(message: cl.Message):
    graph = cl.user_session.get("graph")
    thread_id = cl.user_session.get("thread_id")
    
    config = {"configurable": {"thread_id": thread_id}}
    inputs = {"messages": [HumanMessage(content=message.content)]}
    
    msg = cl.Message(content="")
    active_steps = {}
    
    # TRACKING: Have we sent the prefix for this message yet?
    prefix_sent = False 

    async for event in graph.astream_events(inputs, config=config, version="v1"):
        kind = event["event"]
        run_id = event["run_id"]
        name = event["name"]
        tags = event.get("tags", []) 

        # --- NEW: FILTER HIDDEN EVENTS ---
        # If the event is tagged 'hidden', skip it immediately.
        if "hidden" in tags:
            continue
        
        # --- CASE 1: Steps (Expandable Boxes) ---
        if kind == "on_chain_start" and name in NODE_TO_NAME:
            friendly_name = NODE_TO_NAME.get(name, "BaseBot")
            step = cl.Step(name=friendly_name, type="process")
            step.language = "json"
            step.input = event["data"].get("input")
            await step.send()
            active_steps[run_id] = step

        # --- CASE 2: Tools ---
        elif kind == "on_tool_start":
            step = cl.Step(name=name, type="tool")
            step.language = "json"
            step.input = event["data"].get("input")
            await step.send()
            active_steps[run_id] = step

        # --- CASE 3: Finishing Steps ---
        elif kind in ["on_chain_end", "on_tool_end"]:
            if run_id in active_steps:
                step = active_steps[run_id]
                step.output = event["data"].get("output")
                await step.update()
                del active_steps[run_id]

        # --- CASE 4: Streaming the Final Answer ---
        elif kind in ["on_chat_model_stream", "on_chain_stream"]:
            node_name = event.get("metadata", {}).get("langgraph_node", "")
            if node_name == "router":
                continue

            content = None
            
            # Sub-Case A: It's a token stream from the LLM
            if kind == "on_chat_model_stream":
                content = event["data"]["chunk"].content
            
            # Sub-Case B: It's a static AIMessage from a node (The Ambiguity Catch)
            elif kind == "on_chain_stream":
                chunk = event.get("data", {}).get("chunk", {})
                # If nested (LangGraph level), dig one level deeper
                if event.get("name") == "LangGraph" and isinstance(chunk, dict):
                    for _, v in chunk.items():
                        chunk = v
                        break
                
                if isinstance(chunk, dict) and "messages" in chunk:
                    last_msg = chunk["messages"][-1]
                    # Only take it if we haven't started a message for this node yet
                    if isinstance(last_msg, AIMessage) and not prefix_sent:
                        content = last_msg.content

            # --- RENDER LOGIC ---
            if content:
                msg.author = "BaseBot"
                
                if not msg.id:
                    await msg.send()
                
                if not prefix_sent:
                    label = NODE_TO_NAME.get(node_name, "BaseBot")
                    await msg.stream_token(f"**{label}:** \n")
                    prefix_sent = True
                
                await msg.stream_token(content)
                
    if msg.id:
            if not prefix_sent:
                # If we created a box but never actually sent anything to it
                await msg.remove()
            else:
                await msg.update()