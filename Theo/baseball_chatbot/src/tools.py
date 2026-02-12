import random

"""
THE KITCHEN (Tools)
Currently, they are MOCKS (fakes). 
When teammates finish the real models, I will replace the code inside these functions with their actual model calls.
"""

def mock_injury_model(age: int, velocity: float, prior_surgery: bool) -> str:
    """
    Simulates the Injury Prediction Model.
    Inputs:
        age: Player's age (int)
        velocity: Average pitch velocity (float)
        prior_surgery: Has the player had surgery? (bool)
    Returns:
        A string description of the risk.
    """
    # 1. Debug Print - so you can see this tool running in your terminal
    print(f"\n---[MOCK TOOL] Running Injury Model: Age={age}, Vel={velocity} ---")

    # 2. Fake Logic (The Mock)
    risk_score = random.randint(0, 100)
    
    # 3. Return the result
    if risk_score > 80:
        return f"High Risk ({risk_score}%). The model detects significant strain indicators."
    elif risk_score > 40:
        return f"Moderate Risk ({risk_score}%). Monitor workload closely."
    else:
        return f"Low Risk ({risk_score}%). Player appears healthy."


def mock_batting_model(batter_name: str, opposing_pitcher_type: str) -> str:
    """
    Simulates the Batting Performance Model.
    """
    print(f"\n--- [MOCK TOOL] Running Batting Model for {batter_name} vs {opposing_pitcher_type} ---")
    
    # Fake prediction
    projected_avg = random.uniform(0.200, 0.350)
    
    return f"Projected Batting Average: {projected_avg:.3f}. Favorable matchup detected."


def mock_pitching_model(pitch_type: str, spin_rate: int) -> str:
    """
    Simulates the Pitch Quality Model.
    """
    print(f"\n--- [MOCK TOOL] Running Pitching Model: {pitch_type} @ {spin_rate}rpm ---")
    
    effectiveness = random.choice(["Elite", "Above Average", "Average", "Below Average"])
    
    return f"Pitch Grade: {effectiveness}. Spin efficiency is consistent with major league averages."