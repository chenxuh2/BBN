import json
from pydoc import text
import pandas as pd
import ollama
import os
import re
import glob
from tqdm import tqdm  # Progress bar

# --- CONFIGURATION ---
INPUT_FOLDER = "processed_csvs"
OUTPUT_FOLDER = "annotated_data"
MODEL_NAME = "llama3"  # Or "llama3:instruct" depending on your pull

# Use the taxonomy to make the annotation include goals  
TAXONOMY = {
    "Goal 1: Establish a Supportive and Professional Environment": [
        "Addressed family member by name.",
        "Introduced him/herself by name and role.",
        "Clearly stated the name of the deceased family member.",
        "Sat down. (Body language/Eye contact)",
        "Displayed professional attire/presence.", 
        "Handled interruptions in non-disruptive manner.",
        "Conducted interaction in organized manner."
    ],
    "Goal 2: Assess and Align Expectations": [
        "Ensured all important survivors were present.",
        "Determined knowledge survivors possessed.",
        "Involved me when discussing reason for visit.",
        "Elicited patient perspective of health situation."
    ],
    "Goal 3: Deliver the News": [
        "Provided appropriate opening statement (warning shot).",
        "Accurately/succinctly chronicled events leading to death.",
        "Used phrase “dead” or “died” (avoided euphemisms).",
        "Avoided jargon or explained terms."
    ], 
    "Goal 4: Manage the Emotional Response": [
        "Paused to allow family to assimilate information.",
        "Responded to cues with appropriate touch.",
        "Emotional response did not interfere with communication.",
        "Legitimized my emotions.",
        "Reinforced positive behaviors."
    ], 
    "Goal 5: Ensure Understanding and Facilitate Closure": [
        "Offered viewing of the deceased.",
        "Established availability to answer questions.",
        "Encouraged questions/concerns.",
        "Summarized the interview.",
        "Checked for accuracy during interview.",
        "Reviewed next step(s).",
        "Verified patient’s understanding."
    ]
}

def clean_llm_response(response_text):
    """
    Parses messy LLM output to find the Item Name and the True/False flag.
    Handles: "Item Name: X", "X | True", "X \n Is Death: False", etc.
    """
    # 1. Normalize: Remove "Item Name:", "Is Death Announcement:", and extra whitespace
    clean_text = response_text.replace("Item Name:", "").replace("Is Death Announcement:", "").strip()
    
    # 2. Extract "True" or "False" for death announcement
    # We look for the words True/False at the end of the string or following a pipe/newline
    death_match = re.search(r'\b(True|False)\b', clean_text, re.IGNORECASE)
    is_death = "False"
    if death_match:
        is_death = death_match.group(1).title() # Force "True" or "False" capitalization
        
    # 3. Extract the Item Name
    # We assume the item name is everything BEFORE the True/False or the separator
    # This regex splits by pipe OR newline OR the words "True/False"
    split_pattern = r'\||\n|Is Death Announcement|\bTrue\b|\bFalse\b'
    parts = re.split(split_pattern, clean_text)
    
    item_name = parts[0].strip()
    
    # Clean up common garbage
    if item_name.startswith("MED:"): 
        item_name = item_name.replace("MED:", "").strip()
    if item_name in ["", "None", "N/A"]:
        item_name = "None"
        
    return item_name, is_death

def classify_context_row(prev2_role, prev2_text, prev1_role, prev1_text, current_role, current_text, next_role, next_text):
    allowed_roles = ["MEDICAL STUDENT", "SOCIAL WORKER"]
    
    # If the current speaker isn't a student/SW, skip it
    if not any(x in str(current_role).upper() for x in allowed_roles):
        return "None", "None", False

    # If the text is empty, skip
    if pd.isna(current_text) or str(current_text).strip() == "":
        return "None", "None", False

    prompt = f"""
    You are an expert medical simulation annotator.
    
    TAXONOMY OF GOALS AND ACTIONS:
    {json.dumps(TAXONOMY, indent=2)}

    CONVERSATION CONTEXT:
    [2 Utterances Ago] {prev2_role}: "{prev2_text}"
    [1 Utterance Ago] {prev1_role}: "{prev1_text}"
    ---
    [TARGET UTTERANCE TO ANALYZE] {current_role}: "{current_text}"
    ---
    [Next Utterance] {next_role}: "{next_text}"

    TASK:
    Analyze the TARGET UTTERANCE. Use the surrounding context to understand what the {current_role} is reacting to or prompting (e.g., if the target says "okay", look at what was just said).
    1. Determine which specific Goal(s) the TARGET UTTERANCE is trying to achieve.
    2. Determine which specific Action(s) the TARGET UTTERANCE is performing.
    3. Check if the TARGET UTTERANCE is the explicit "Death Announcement". 
       - Set to TRUE **ONLY IF** this specific utterance is the very first time the medical student explicitly breaks the news to the family that the patient has died.
       - Set to FALSE if they are merely discussing the death after the news has already been delivered, answering follow-up questions, or summarizing.
    
    OUTPUT FORMAT: You MUST return a valid JSON object matching this exact structure:
    {{
        "goals": ["List matched goals here", "or empty list"],
        "actions": ["List matched actions here", "or empty list"],
        "is_death_announcement": true/false
    }}
    """

    try:
        response = ollama.chat(
            model="llama3", 
            messages=[{'role': 'user', 'content': prompt}],
            format="json", 
            options={"temperature": 0.0}
        )
        
        result = json.loads(response['message']['content'])
        
        goals = ", ".join(result.get("goals", [])) if result.get("goals") else "None"
        actions = ", ".join(result.get("actions", [])) if result.get("actions") else "None"
        is_death = result.get("is_death_announcement", False)
        
        return goals, actions, is_death

    except Exception as e:
        print(f"Error parsing row: {e}")
        return "Error", "Error", False
    
def process_annotations():
    if not os.path.exists(OUTPUT_FOLDER):
        os.makedirs(OUTPUT_FOLDER)
        
    # Only grab SIMULATION files (ignore Debriefs for grading)
    files = glob.glob(os.path.join(INPUT_FOLDER, "*_SIMULATION.csv"))
    
    print(f"Found {len(files)} simulation files to annotate.")

    for file_path in files:
        base_name = os.path.basename(file_path)
        print(f"Annotating: {base_name}...")
        
        df = pd.read_csv(file_path)
        
        # --- NEW: CREATE CONTEXT WINDOW COLUMNS HERE ---
        # We do this immediately after reading the CSV so the whole conversation is linked
        df['prev_role_2'] = df['Role'].shift(2).fillna("None")
        df['prev_text_2'] = df['Utterance'].shift(2).fillna("None")
        
        df['prev_role_1'] = df['Role'].shift(1).fillna("None")
        df['prev_text_1'] = df['Utterance'].shift(1).fillna("None")
        
        df['next_role'] = df['Role'].shift(-1).fillna("None")
        df['next_text'] = df['Utterance'].shift(-1).fillna("None")
        # -----------------------------------------------
        
        # Create new lists for our updated outputs
        goals_list = []
        actions_list = []
        death_flags = []
        
        # Use tqdm to show a progress bar because LLMs are slow
        for index, row in tqdm(df.iterrows(), total=df.shape[0]):
            # Call the new context-aware function!
            goals, actions, is_death = classify_context_row(
                row['prev_role_2'], row['prev_text_2'],
                row['prev_role_1'], row['prev_text_1'],
                row['Role'], row['Utterance'],
                row['next_role'], row['next_text']
            )
            goals_list.append(goals)
            actions_list.append(actions)
            death_flags.append(is_death)
            
        df['Annotated_Goals'] = goals_list
        df['Annotated_Actions'] = actions_list
        df['Is_Death_Announcement'] = death_flags
        
        # --- NEW: CLEAN UP CONTEXT COLUMNS BEFORE SAVING ---
        # This deletes the shifted columns so the final CSV doesn't look messy
        df = df.drop(columns=[
            'prev_role_2', 'prev_text_2', 
            'prev_role_1', 'prev_text_1', 
            'next_role', 'next_text'
        ])
        
        # Save
        save_path = os.path.join(OUTPUT_FOLDER, f"ANNOTATED_{base_name}")
        df.to_csv(save_path, index=False)
        print(f"Saved to {save_path}")
if __name__ == "__main__":
    # Ensure Ollama is running in the background!
    process_annotations()