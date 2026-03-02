import random
import os
import pandas as pd
import joblib
from thefuzz import process, fuzz
from langchain_core.tools import tool

# --- LOAD IN DATASETS --- #
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


injury_path = os.path.join(PROJECT_ROOT, "data", "injury_risk.csv")
try:
    # Load the CSV
    INJURY_DATA = pd.read_csv(injury_path)
    # Ensure we treat yearID as an integer for sorting
    if 'yearID' in INJURY_DATA.columns:
        INJURY_DATA['yearID'] = pd.to_numeric(INJURY_DATA['yearID'], errors='coerce')
except FileNotFoundError:
    print(f"ERROR: Could not find injury_risk.csv at {injury_path}")
    INJURY_DATA = pd.DataFrame()


batting_path = os.path.join(PROJECT_ROOT, "data", "MLB_Hitter_Predictions.csv")
try:
    # Load the CSV
    BATTING_DATA = pd.read_csv(batting_path)
    # Ensure we treat yearID as an integer for sorting
    if 'yearID' in BATTING_DATA.columns:
        BATTING_DATA['yearID'] = pd.to_numeric(BATTING_DATA['yearID'], errors='coerce')
except FileNotFoundError:
    print(f"ERROR: Could not find injury_risk.csv at {batting_path}")
    BATTING_DATA = pd.DataFrame()

primary_path = os.path.join(PROJECT_ROOT, "data", "Master2.csv")
PRIMARY_DATA = pd.read_csv(primary_path)

def _clean_int(val):
    """Helper to safely convert potential NaNs to 0."""
    if pd.isna(val):
        return 0
    return int(val)

# --- HELPER TOOL for Fuzzy Matching --- #
def resolve_player_id(name_query: str, dataset=None) -> str:
    """
    Merged helper to resolve a name (or ID) to a definitive playerID.
    Combines fuzzy matching logic with detailed DOB/ID suggestion strings.
    """
    if dataset is None:
        dataset = PRIMARY_DATA


    if name_query in dataset['playerID'].values:
        return name_query

    # 2. Fuzzy Matching
    all_names = dataset['nameFull'].unique().tolist()
    
    # Extract top 3 matches using the same slicing trick for library safety
    matches = process.extract(name_query, all_names, limit=3, scorer=fuzz.token_sort_ratio)
    
    if not matches:
        return f"Could not find any player matching '{name_query}'."

    top_match, top_score = matches[0][:3]
    print(f"Top match: {top_match}, Top score {top_score}\nMatches: {matches}")

    # Threshold Check: If score < 85, ask for clarification
    if top_score < 85:
        suggestions_str = f"Ambiguous name '{name_query}'. Did you mean:\n"
        
        count = 1
        for match_tuple in matches:
            match_name, score = match_tuple[:2]
            
            if score < 60: 
                continue
            
            # Find all players with this name (Handles duplicate names like 'Jose Cruz')
            candidates = dataset[dataset['nameFull'] == match_name]
            candidates = candidates.drop_duplicates(subset=['playerID'])
            
            for _, row in candidates.iterrows():
                # Format DOB (Handle NaNs gracefully)
                # Using .get() with a default to avoid crashes on missing columns
                b_mon = _clean_int(row.get('birthMonth'))
                b_day = _clean_int(row.get('birthDay'))
                b_year = _clean_int(row.get('birthYear'))
                dob = f"{b_mon}/{b_day}/{b_year}"

                # Format DOB - If all are 0, we show 'Unknown'
                if b_year == 0:
                    dob = "Unknown"
                else:
                    dob = f"{b_mon}/{b_day}/{b_year}"
                
                # Merged Format: 1. Full Name (ID: xxxx) - DOB: m/d/y
                suggestions_str += (
                    f"{count}. {row['nameFull']} (ID: {row['playerID']}) - DOB: {dob}\n"
                )
                count += 1
                
        return suggestions_str

    # 3. High Confidence Match (>85)
    matched_id = dataset[dataset['nameFull'] == top_match].iloc[0]['playerID']
    return matched_id

@tool
def check_injury_history(player_id: str):
    """
    Retrieves the injury risk history for a player.
    Accepts a Name (e.g. 'Clayton Kershaw') OR an ID.
    Returns a text timeline of risk.
    """
    if INJURY_DATA.empty:
        return "Error: Medical records system is offline (CSV not found)."

    # Filter for the player
    player_history = INJURY_DATA[INJURY_DATA['playerID'] == player_id].copy()
    
    if player_history.empty:
        return f"No medical records found for player ID: {player_id}"
    
    # Sort by year to create a proper timeline
    player_history = player_history.sort_values(by='yearID', ascending=True)
    
    # Format the output
    timeline = []
    for _, row in player_history.iterrows():
        year = int(row['yearID']) if pd.notna(row['yearID']) else "Unknown"
        risk = row.get('Injury_risk', 'Unknown')
        was_injured_prev_year = row.get('injured_last_year', 0)
        prior_status = "YES" if was_injured_prev_year == 1 else "NO"
        timeline.append(f"{year}: Risk {risk} [Prior Injury Last Year: {prior_status}]")
        
    return f"Medical History for {player_id}:\n" + " | ".join(timeline)



@tool
def check_batting_stats(player_id: str):
    """
    Retrieves OPS trends and model predictions for a player.
    Columns: Prev_Year_OPS, Next_Year_OPS (Actual), Predicted_Next_Year_OPS, OPS_Trend.
    """
    # 1. Safety Check
    if BATTING_DATA.empty:
        return "Error: Hitting analytics system is offline (CSV not found)."

    # 2. Filter
    player_history = BATTING_DATA[BATTING_DATA['playerID'] == player_id].copy()
    
    if player_history.empty:
        return f"No batting records found for player ID: {player_id}"
    
    # 3. Sort (Oldest to Newest)
    # Ensure yearID is treated as an integer
    if 'yearID' in player_history.columns:
        player_history['yearID'] = pd.to_numeric(player_history['yearID'], errors='coerce')
        player_history = player_history.sort_values(by='yearID', ascending=True)
    
    # 4. Format Timeline
    timeline = []
    for _, row in player_history.iterrows():
        year = int(row['yearID']) if pd.notna(row['yearID']) else "Unknown"
        
        # Extract Stats
        prev_ops = row.get('Prev_Year_OPS', 'N/A')
        trend = row.get('OPS_Trend', 'Unknown')
        predicted_next = row.get('Predicted_Next_Year_OPS', 'N/A')
        actual_next = row.get('Next_Year_OPS', 'TBD')

        # Formatting logic
        # e.g. "2014: Trend [Increasing] | Prev .850 -> Model Predicts .880 for Next Year (Actual: .875)"
        entry = (
            f"{year}: Trend [{trend}] | "
            f"Prev OPS: {prev_ops} -> "
            f"Model Forecast for Next Season: {predicted_next} "
            f"(Actual: {actual_next})"
        )
        timeline.append(entry)
        
    return f"Batting Performance History for {player_id}:\n" + "\n".join(timeline)



# DEFINE THE EXACT FEATURE ORDER
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
    player_id: str = None,
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
    
    player_row = None
    
    # --- PATH A: DIRECT ID LOOKUP (Fast & Accurate) ---
    if player_id:
        # Filter by ID
        match = INFERENCE_DATA[INFERENCE_DATA['playerID'] == player_id]
        if not match.empty:
            # We found them! Skip fuzzy matching.
            player_row = match.iloc[0].to_dict()
            top_match = player_row['nameFull'] # Update name for display
        else:
            return f"Error: Player ID '{player_id}' not found in database."

    # --- PATH B: FUZZY MATCHING (Fallback if no ID) ---
    if not player_row:
        # (This is your existing fuzzy logic, kept as a safety net)
        all_names = INFERENCE_DATA['nameFull'].tolist()
        matches = process.extract(player_name, all_names, limit=3, scorer=fuzz.token_sort_ratio)
        top_match, top_score = matches[0]

        if top_score < 85:
            # ... (Your existing ambiguity logic) ...
            return "Ambiguous name..." # (Shortened for brevity)

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
