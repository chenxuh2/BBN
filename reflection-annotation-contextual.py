import pandas as pd
import ollama
import json
from tqdm import tqdm
import os
import glob
import re

# --- CONFIGURATION ---
INPUT_FOLDER = "processed_csvs" # Folder with debrief transcripts
OUTPUT_FOLDER = "annotated_reflections"

# Paste your taxonomy dictionary here
TAXONOMY = {
    # ... goals and items ...
    # Example Taxonomy (Replace with your actual goals and items)
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
        "Used phrase 'dead' or 'died' (avoided euphemisms).",
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
        "Verified patient's understanding."
    ],
    "Other": [
        "Other"
    ]
}


def classify_reflection_context(prev2_role, prev2_text, prev1_role, prev1_text, current_role, current_text, next_role, next_text):
    # Skip if the target utterance is empty
    if pd.isna(current_text) or str(current_text).strip() == "":
        return "None", "None"

    prompt = f"""
    You are an expert qualitative researcher analyzing a medical debriefing/reflection transcript.
    The speakers have just finished a simulation about breaking bad news and are now discussing how it went.
    
    TAXONOMY OF GOALS AND ACTIONS:
    {json.dumps(TAXONOMY, indent=2)}

    CONVERSATION CONTEXT:
    [2 Utterances Ago] {prev2_role}: "{prev2_text}"
    [1 Utterance Ago] {prev1_role}: "{prev1_text}"
    ---
    [TARGET UTTERANCE TO ANALYZE] {current_role}: "{current_text}"
    ---
    [Next Utterance] {next_role}: "{next_text}"
    
  
    CRITICAL RULES:
    1. ONLY REFLECTIONS: You must only annotate utterances where the speaker is actively evaluating, recalling, or reflecting on clinical actions from the simulation.
    2. IGNORE SMALL TALK (VERY IMPORTANT): If the target utterance is conversational filler ("yeah", "okay", "right", "um"), logistical room talk, or simple acknowledgement, you MUST return empty lists ([]). Do not force a mapping.
    3. INHERIT CONTEXT: If the target utterance is a generic agreement ("I agree", "exactly") AND the immediate previous utterance was a specific reflection, inherit the previous utterance's Goal and Action.
    
    *** 4. TOP-DOWN CLASSIFICATION (GOAL FIRST, THEN ACTION) ***
    You MUST classify the utterance by determining the broad GOAL first, and then selecting the specific ACTION.
    - Step 1: Which of the 5 Goals (or "Other") does this reflection fall under? Put this in "reflected_goals".
    - Step 2: Look ONLY at the Actions listed under that specific Goal in the TAXONOMY. Which Action matches best? Put this in "reflected_actions".
    
    *** 5. THE TWO WAYS TO USE "OTHER" ***
    If the speaker reflects on a clinical/emotional action that is not explicitly in the taxonomy, you must use "Other" and write a 3-7 word summary in "other_summary".
    - Scenario A (Goal matches, Action doesn't): If it fits a Goal but isn't on the checklist (e.g., handing the patient a tissue), output Goal: "Goal 4...", Action: "Other".
    - Scenario B (Nothing matches): If it is completely outside the 5 Goals (e.g., internal anxiety, running out of time), output Goal: "Other", Action: "Other".

    OUTPUT FORMAT (Strict JSON):
    {{ 
        "reflected_goals": ["List of Goal names here"],
        "reflected_actions": ["List of specific Action names here"]
        "other_summary": "Concise summary if 'Other' is used, otherwise empty string"
    }}
    """

    try:
        response = ollama.chat(
            # model="llama3", 
            model="qwen2.5:14b",   # trying a different model for better contextual understanding 
            messages=[{'role': 'user', 'content': prompt}],
            format="json",
            options={"temperature": 0.0}
        )
        
        result = json.loads(response['message']['content'])
        
        goals = result.get("reflected_goals", [])
        actions = result.get("reflected_actions", [])
        
        # --- NEW CODE ADDED HERE FOR OTHER SUMMARY ---
        other_summary = result.get("other_summary", "") 
        
        # Clean out punctuation and join into a comma-separated string for taxonomy items
        clean_goals = ", ".join([re.sub(r'[^\w\s]', '', g) for g in goals]) if goals else "None"
        clean_actions = ", ".join([re.sub(r'[^\w\s]', '', a) for a in actions]) if actions else "None"
        
        # Handle the Other summary
        clean_summary = str(other_summary).strip() if other_summary else "None"
        
        # Fallback safety: Wipe rogue summaries if the category isn't Other
        if ("Other" not in clean_goals and "Other" not in clean_actions) and clean_summary != "None":
            clean_summary = "None" 
            
        # Fallback safety: Provide a generic summary if it forgot to write one
        if ("Other" in clean_goals or "Other" in clean_actions) and clean_summary == "None":
            clean_summary = "Unspecified other reflection" 

        # Now returning THREE variables instead of two
        return clean_goals, clean_actions, clean_summary

    except Exception as e:
        print(f"Error parsing row: {e}")
        return "Error", "Error"

def process_reflections():
    if not os.path.exists(OUTPUT_FOLDER):
        os.makedirs(OUTPUT_FOLDER)
        
    # Look for anything ending in DEBRIEF (either .csv or .tsv)
    files = glob.glob(os.path.join(INPUT_FOLDER, "*_DEBRIEF*")) 
    print(f"Found {len(files)} files to annotate.")

    for file_path in files:
        print(f"Annotating Reflection: {os.path.basename(file_path)}...")
        
        try:
            df = pd.read_csv(file_path)
            if len(df.columns) == 1: 
                df = pd.read_csv(file_path, sep='\t')
        except Exception as e:
            print(f"Could not read {file_path}. Error: {e}")
            continue
            
        if not all(col in df.columns for col in ['Role', 'Timestamp', 'Utterance']):
            print(f"WARNING: File {file_path} is missing expected columns. Skipping.")
            continue
        
        # --- CREATE THE CONTEXT WINDOW ---
        df['prev_role_2'] = df['Role'].shift(2).fillna("None")
        df['prev_text_2'] = df['Utterance'].shift(2).fillna("None")
        
        df['prev_role_1'] = df['Role'].shift(1).fillna("None")
        df['prev_text_1'] = df['Utterance'].shift(1).fillna("None")
        
        df['next_role'] = df['Role'].shift(-1).fillna("None")
        df['next_text'] = df['Utterance'].shift(-1).fillna("None")
        # ---------------------------------

        # --- MODIFIED: Added summary_list ---
        goals_list, actions_list, summary_list = [], [], [] 
        
        for index, row in tqdm(df.iterrows(), total=df.shape[0]):
            
            # --- MODIFIED: Unpacking 3 variables ---
            goals, actions, summary = classify_reflection_context(
                row['prev_role_2'], row['prev_text_2'],
                row['prev_role_1'], row['prev_text_1'],
                row['Role'], row['Utterance'],
                row['next_role'], row['next_text']
            )
            goals_list.append(goals)
            actions_list.append(actions)
            
            # --- MODIFIED: Appending the summary ---
            summary_list.append(summary) 
            
        df['Reflected_Goals'] = goals_list
        df['Reflected_Actions'] = actions_list
        
        # --- MODIFIED: Creating the new dataframe column ---
        df['Other_Summary'] = summary_list
        # --- CLEAN UP CONTEXT COLUMNS BEFORE SAVING ---
        df = df.drop(columns=[
            'prev_role_2', 'prev_text_2', 
            'prev_role_1', 'prev_text_1', 
            'next_role', 'next_text'
        ])
        
        # Save the finalized CSV
        save_name = "ANNOTATED_" + os.path.basename(file_path).split('.')[0] + ".csv"
        df.to_csv(os.path.join(OUTPUT_FOLDER, save_name), index=False)

if __name__ == "__main__":
    process_reflections()

