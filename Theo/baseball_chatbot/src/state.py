import operator
from typing import Annotated, List, TypedDict, Union
from langchain_core.messages import AnyMessage
from langgraph.graph.message import add_messages
from langchain_core.messages import BaseMessage

class AgentState(TypedDict):
    """
    The state of our baseball agent. 
    This is the 'clipboard' that gets passed between every agent.
    """
    # Chat History (Appends)
    messages: Annotated[list[AnyMessage], add_messages]
    
    # The Decision (Overwrites)
    next_agent: str
    
    # Context (Overwrites)
    profile: dict
    
    # ML Inputs (Overwrites)
    ml_inputs: dict