import os
from langgraph.graph import StateGraph, END
from langgraph.checkpoint.memory import MemorySaver
from langgraph.prebuilt import tools_condition, ToolNode

# Import our components
from src.state import AgentState
from src.agents import *
from src.tools import *

# ==============================================================================
# THE GRAPH BUILDER
# ==============================================================================
def build_graph():
    builder = StateGraph(AgentState)

    # 1. Add Nodes
    builder.add_node("router", router_agent)
    builder.add_node("injury_agent", injury_agent)
    builder.add_node("pitching_agent", pitching_agent)
    builder.add_node("batting_agent", batting_agent)
    builder.add_node("general_agent", general_agent)
    builder.add_node("model_explainer_agent", model_explainer_agent)
    
    # Tool Nodes
    builder.add_node("injury_tools",ToolNode([check_injury_history]))
    builder.add_node("pitching_tools", ToolNode([predict_pitching_2016]))
    builder.add_node("batting_tools", ToolNode([check_batting_stats]))
    builder.add_node("explainer_tools", get_explainer_tool_node())

    # 2. Entry Point
    builder.set_entry_point("router")

    # 3. Router Logic 
    builder.add_conditional_edges(
            "router",
            lambda x: x["next_agent"],
            {
                "injury_agent": "injury_agent",
                "pitching_agent": "pitching_agent",
                "batting_agent": "batting_agent",
                "model_explainer_agent": "model_explainer_agent",
                "general_agent": "general_agent"
            }
        )

    # 4. THE AGENT LOOP
    # Injury Loop
    builder.add_conditional_edges(
        "injury_agent",
        tools_condition, 
        {"tools": "injury_tools", END: END}
    )
    builder.add_edge("injury_tools", "injury_agent") # Go BACK to agent after tool

    # Pitching Loop
    builder.add_conditional_edges(
        "pitching_agent",
        tools_condition,
        {"tools": "pitching_tools", END: END}
    )
    builder.add_edge("pitching_tools", "pitching_agent")

    # Batting Loop
    builder.add_conditional_edges(
        "batting_agent",
        tools_condition,
        {"tools": "batting_tools", END: END}
    )
    builder.add_edge("batting_tools", "batting_agent")

    # General Agent (No tools, just chat)
    builder.add_edge("general_agent", END)

    # Model Explainer agent
    builder.add_conditional_edges(
        "model_explainer_agent",
        tools_condition,
        {"tools": "explainer_tools", END: END}
    )
    builder.add_edge("explainer_tools", "model_explainer_agent")

    # 5. Compile with Memory
    memory = MemorySaver()
    graph = builder.compile(checkpointer=memory)
    
    return graph