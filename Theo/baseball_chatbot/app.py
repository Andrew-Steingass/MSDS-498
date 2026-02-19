import chainlit as cl
import uuid
from langchain_core.messages import HumanMessage
from src.graph import build_graph
import warnings


# Filter out the specific Pydantic warnings that clutter the terminal
warnings.filterwarnings("ignore", message=".*Pydantic serializer warnings.*")

NODE_TO_NAME = {
    "general_agent": "BaseBot",       # Renamed from General Agent
    "pitching_agent": "Modeling Agent", # Shared Identity
    "batting_agent": "Modeling Agent",  # Shared Identity
    "injury_agent": "Modeling Agent",   # Shared Identity
    "model_explainer_agent": "The Explainer",
    "router": "Router"
}

# Initialize the graph ONCE
graph_app = build_graph()

@cl.on_chat_start
async def start():
    await cl.Avatar(name="BaseBot", path="images/basebot.png").send()
    await cl.Avatar(name="Modeling Agent", path="images/chart.png").send()
    await cl.Avatar(name="The Explainer", path="images/teacher.png").send()

    thread_id = str(uuid.uuid4())
    cl.user_session.set("thread_id", thread_id)
    cl.user_session.set("graph", graph_app)
    await cl.Message(
        content="Batter up- I'm ready to analyze and talk some baseball!",
        author="BaseBot" 
    ).send()

@cl.on_message
async def main(message: cl.Message):
    graph = cl.user_session.get("graph")
    thread_id = cl.user_session.get("thread_id")
    
    config = {"configurable": {"thread_id": thread_id}}
    inputs = {"messages": [HumanMessage(content=message.content)]}
    
    # Initialize the message placeholder
    msg = cl.Message(content="")
    
    # Track active steps (the expandable boxes) by their run_id
    active_steps = {}

    async for event in graph.astream_events(inputs, config=config, version="v1"):
        kind = event["event"]
        run_id = event["run_id"]
        name = event["name"]
        
        # --- CASE 1: The "Specialist Agent" Starts (Node Entry) ---
        # logic: Instead of a separate AGENT_NODES list, we check our mapping dict.
        if kind == "on_chain_start" and name in NODE_TO_NAME:
            # RETRIEVE FRIENDLY NAME: "pitching_agent" -> "Modeling Agent"
            friendly_name = NODE_TO_NAME.get(name, name)
            
            step = cl.Step(name=friendly_name, type="process")
            step.language = "json"
            step.input = event["data"].get("input")
            await step.send()
            active_steps[run_id] = step

        # --- CASE 2: The "Actual Tool" Starts (Leaf Function) ---
        elif kind == "on_tool_start":
            step = cl.Step(name=name, type="tool")
            step.language = "json"
            step.input = event["data"].get("input")
            await step.send()
            active_steps[run_id] = step

        # --- CASE 3: Finishing Steps (Both Agents and Tools) ---
        elif kind in ["on_chain_end", "on_tool_end"]:
            # logic: Close the step if we were tracking it
            if run_id in active_steps:
                step = active_steps[run_id]
                step.output = event["data"].get("output")
                await step.update()
                del active_steps[run_id]

        # --- CASE 4: Streaming the Final Answer ---
        elif kind == "on_chat_model_stream":
            # 1. Identify WHICH node is speaking
            node_name = event.get("metadata", {}).get("langgraph_node", "")
            
            # 2. FILTER: Gag the Router so it doesn't print JSON to the main chat
            if node_name == "router":
                continue
            
            # 3. DYNAMIC AVATAR: Switch the icon based on who is speaking
            # If the node isn't in our list, default to "BaseBot"
            friendly_author = NODE_TO_NAME.get(node_name, "BaseBot")
            msg.author = friendly_author
            
            # 4. Stream the text
            content = event["data"]["chunk"].content
            if content:
                if not msg.id:
                    await msg.send()
                await msg.stream_token(content)
                
    await msg.update()