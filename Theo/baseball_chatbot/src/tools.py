import random
import os
import pandas as pd
import joblib
from thefuzz import process, fuzz
from langchain_core.tools import tool


CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(CURRENT_DIR)
CSV_PATH = os.path.join(PROJECT_ROOT, "data", "inference_stats.csv")
try:
    INFERENCE_DATA = pd.read_csv(CSV_PATH)
except FileNotFoundError:
    print(f"CRITICAL ERROR: Could not find data file at: {CSV_PATH}")
    # Create an empty DataFrame so the import doesn't crash the whole app immediately
    INFERENCE_DATA = pd.DataFrame()

try:
    PITCHING_PIPELINE = joblib.load("models/pitching_pipeline.pkl")
except FileNotFoundError:
    print("WARNING: Model file not found. Inference will fail.")

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



# DEFINE THE EXACT FEATURE ORDER
# This MUST match X_train.columns from model perfectly.
MODEL_COLUMNS = [
                'ERA_last', 'ERA_mean3', 'ERA_trend', 'K9_last', 'K9_mean3', 'K9_trend',
                'BB9_last', 'BB9_mean3', 'BB9_trend', 'HR9_last', 'HR9_mean3',
                'HR9_trend', 'WHIP_last', 'WHIP_mean3', 'WHIP_trend', 'IP_last',
                'IP_mean3', 'IP_trend', 'age'
                ]

# --- HELPER FUNCTIONS (Math Logic) ---
def recalculate_features(base_row, overrides):
    """
    Takes the actual 2015 row and applies user overrides.
    Recalculates mean3 and trend using 2013/2014 anchors.
    """
    # Convert row to a mutable dictionary
    features = base_row.copy()
    
    # Map friendly argument names to CSV column names
    # Key = Argument Name, Value = Feature Prefix
    stat_map = {
        "era": "ERA", "k9": "K9", "bb9": "BB9", 
        "hr9": "HR9", "whip": "WHIP", "ip": "IP"
    }

    for arg, col_prefix in stat_map.items():
        if overrides.get(arg) is not None:
            new_val = float(overrides[arg])
            
            # 1. Update the "Last" (2015) value
            features[f"{col_prefix}_last"] = new_val
            
            # 2. Get Anchors (2013 and 2014)
            # These columns must exist in your inference_stats.csv
            v13 = features.get(f"{col_prefix}_2013_baseline", 0) 
            v14 = features.get(f"{col_prefix}_2014_baseline", 0)
            
            # 3. Recalculate Window Features
            features[f"{col_prefix}_mean3"] = (v13 + v14 + new_val) / 3.0
            features[f"{col_prefix}_trend"] = new_val - v13

    return features

@tool
def predict_pitching_2016(
    player_name: str, 
    era: float = None, 
    k9: float = None, 
    bb9: float = None, 
    hr9: float = None, 
    whip: float = None, 
    ip: float = None,
    age: int = None
):
    """
    Predicts if a pitcher will IMPROVE their ERA in 2016.
    
    Modes:
    1. Baseline: Provide only player_name to use actual 2015 stats.
    2. Simulation: Provide player_name AND specific stats (e.g., era=2.50) to 
       run a "What If" scenario.
       
    Returns probability of stagnation (failure to improve).
    """
    
    # --- STEP 1: FUZZY MATCHING ---
    all_names = INFERENCE_DATA['nameFull'].tolist()
    
    # Extract top 3 matches
    matches = process.extract(player_name, all_names, limit=3, scorer=fuzz.token_sort_ratio)
    top_match, top_score = matches[0]

    # Threshold Check: If score < 85, ask for clarification
    if top_score < 85:
            suggestions_str = "Ambiguous name. Did you mean:\n"
            
            # Iterate through the fuzzy matches that are "good enough" (> 60)
            count = 1
            for match_name, score in matches:
                if score < 60: continue
                    
                # Find all players with this name (Handles duplicate names like 'Jose Cruz')
                candidates = INFERENCE_DATA[INFERENCE_DATA['nameFull'] == match_name]
                
                for _, row in candidates.iterrows():
                    # Format DOB (Handle NaNs gracefully just in case)
                    b_mon = int(row.get('birthMonth', 0))
                    b_day = int(row.get('birthDay', 0))
                    b_year = int(row.get('birthYear', 0))
                    dob = f"{b_mon}/{b_day}/{b_year}"
                    
                    # Format: 1. First Last (ID: xxxx) - DOB: m/d/y
                    suggestions_str += (
                        f"{count}. {row['nameFirst']} {row['nameLast']} "
                        f"(ID: {row['playerID']}) - DOB: {dob}\n"
                    )
                    count += 1
                    
            return suggestions_str

    # --- STEP 2: LOAD BASELINE DATA ---
    # Get the row for the best match
    player_row = INFERENCE_DATA[INFERENCE_DATA['nameFull'] == top_match].iloc[0].to_dict()
    
    # --- STEP 3: APPLY OVERRIDES (SIMULATION LOGIC) ---
    overrides = {"era": era, "k9": k9, "bb9": bb9, "hr9": hr9, "whip": whip, "ip": ip}
    
    # Check if this is a simulation (are any values not None?)
    is_simulation = any(v is not None for v in overrides.values())
    
    if is_simulation:
        final_features = recalculate_features(player_row, overrides)

        # Age is just a single number, so we just overwrite it.
        if age is not None:
            final_features["age"] = age
    else:
        final_features = player_row

    # --- STEP 4: PREPARE MODEL INPUT ---
    # Create DataFrame with a single row
    input_df = pd.DataFrame([final_features])
    
    # FILTER: Keep ONLY the columns the model was trained on, in exact order
    try:
        input_df = input_df[MODEL_COLUMNS]
    except KeyError as e:
        return f"Data Error: Missing column {e} in inference CSV."
    
    # ==============================================================================
    # DEBUG LOGGING (PRINTS TO TERMINAL)
    # ==============================================================================
    print("\n" + "="*50)
    print(f"ML PITCHING MODEL INFERENCE DEBUG: {player_name}")
    print("="*50)
    
    # 1. Print the "What-If" Inputs (The raw 20-feature vector)
    print(f"\n[INPUTS] Feature Vector ({len(input_df.columns)} vars):")
    # Transpose for easier reading in terminal
    print(input_df.T.to_string(header=False)) 
    
    # 2. Highlight Key Drivers (The ones user might have changed)
    print("\n[KEY DRIVERS] Check these for Simulation:")
    print(f"  ERA_last:  {input_df['ERA_last'].values[0]:.2f}")
    print(f"  ERA_trend: {input_df['ERA_trend'].values[0]:.2f} (Should change if simulated)")
    print(f"  K9_last:   {input_df['K9_last'].values[0]:.2f}")
    
    # --- STEP 5: RUN INFERENCE ---
    raw_probs = PITCHING_PIPELINE.predict_proba(input_df)[0]
    
    print("\n[OUTPUTS] Raw Probabilities from XGBoost:")
    print(f"  Class 0 (Improve):  {raw_probs[0]:.4f}")
    print(f"  Class 1 (Stagnate): {raw_probs[1]:.4f}")
    print("="*50 + "\n")
    # ==============================================================================

    # --- STEP 5: RUN INFERENCE ---
    # Class 1 = "Not Improved" (Stagnation)
    # Class 0 = "Improved"
    prob_stagnation = raw_probs[1]
    prob_improvement = raw_probs[0]

    # --- STEP 6: RETURN RESULTS ---
    return {
        "analysis_type": "Simulation (What-If)" if is_simulation else "Baseline (Actual 2015)",
        "player_found": top_match,
        "input_summary": {k: v for k, v in overrides.items() if v is not None} if is_simulation else "Historical Data",
        "prediction": {
            "probability_of_improvement": f"{prob_improvement:.1%}",
            "risk_of_stagnation": f"{prob_stagnation:.1%}",
            "verdict": "Likely to Improve" if prob_improvement > 0.5 else "Likely to Stagnate/Regress"
        }
    }


@tool
def read_model_documentation(model_topic: str) -> str:
    """
    Reads the markdown documentation for a specific model.
    Input should be a single keyword: 'injury', 'pitching', or 'batting'.
    """
    file_map = {
        "injury": "injury_model_docs.md",
        "pitching": "pitching_model_docs.md",
        "batting": "batting_model_docs.md"
    }
    
    topic = model_topic.lower().strip()
    filename = file_map.get(topic)
    
    if not filename:
        return "Error: Documentation not found. Please ask about the injury, pitching, or batting models."
    
    # Get the directory where tools.py lives (baseball_chatbot/src)
    current_dir = os.path.dirname(__file__)
    
    # Step up one level to the project root (baseball_chatbot)
    project_root = os.path.dirname(current_dir)
    
    # Build the absolute path: baseball_chatbot/models/docs/filename
    filepath = os.path.join(project_root, "models", "docs", filename)
    
    try:
        with open(filepath, "r", encoding="utf-8") as file:
            return file.read()
    except FileNotFoundError:
        return f"System Note: The file {filepath} could not be found. Please verify the folder structure."
