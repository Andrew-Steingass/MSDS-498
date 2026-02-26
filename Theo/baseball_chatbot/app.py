import chainlit as cl
import uuid
import logging
from langchain_core.messages import HumanMessage
from src.graph import build_graph
# 

# 1. Configuration
logging.getLogger("langchain_core.callbacks.manager").setLevel(logging.ERROR)

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
    
    # Welcome message (Always BaseBot)
    await cl.Message(
        content="Batter up! Lets talk some Baseball!",
        author="BaseBot" 
    ).send()

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
        elif kind == "on_chat_model_stream":
            node_name = event.get("metadata", {}).get("langgraph_node", "")
            
            if node_name == "router":
                continue
            
            # 1. ALWAYS use "BaseBot" for the avatar/header
            msg.author = "BaseBot"
            
            # 2. Get the specific name we want to inject
            specific_label = NODE_TO_NAME.get(node_name, "BaseBot")
            
            content = event["data"]["chunk"].content
            if content:
                # 3. IF this is the FIRST time we are sending text for this message:
                if not msg.id:
                    await msg.send() # Create the empty message box
                
                # 4. Check if we have sent the prefix YET
                if not prefix_sent:
                    # Stream the bold name: "**General BaseBot:** "
                    await msg.stream_token(f"**{specific_label}:** \n")
                    prefix_sent = True # Mark as done so we don't repeat it
                
                # 5. Stream the actual answer
                await msg.stream_token(content)
                
    await msg.update()