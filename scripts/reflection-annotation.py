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
    # ... add other goals ...
}


def classify_reflection(role, utterance):
    # Skip if the utterance is empty or nan
    if pd.isna(utterance) or str(utterance).strip() == "":
        return "None", "None"

    prompt = f"""
    You are an expert qualitative researcher analyzing a medical debriefing transcript.
    
    TAXONOMY OF GOALS AND ACTIONS:
    {json.dumps(TAXONOMY, indent=2)}

    TASK:
    Analyze the speaker's utterance. Are they reflecting on, evaluating, or discussing any of the Goals or Actions listed in the taxonomy?
    - Match their discussion to the corresponding Goal(s) and specific Action(s).
    - If they are just making small talk or agreeing ("Yeah", "I agree"), return empty lists.

    OUTPUT FORMAT (Strict JSON):
    {{
        "reflected_goals": ["List of Goal names here"],
        "reflected_actions": ["List of specific Action names here"]
    }}
    
    UTTERANCE TO ANALYZE:
    "{utterance}"
    """

    try:
        response = ollama.chat(
            model="llama3", 
            messages=[{'role': 'user', 'content': prompt}],
            format="json",
            options={"temperature": 0.0}
        )
        
        result = json.loads(response['message']['content'])
        
        goals = result.get("reflected_goals", [])
        actions = result.get("reflected_actions", [])
        
        # Clean out punctuation and join 
        clean_goals = ", ".join([re.sub(r'[^\w\s]', '', g) for g in goals]) if goals else "None"
        clean_actions = ", ".join([re.sub(r'[^\w\s]', '', a) for a in actions]) if actions else "None"
        
        return clean_goals, clean_actions

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
            # Try reading as CSV first
            df = pd.read_csv(file_path)
            # If it only has 1 column, it's probably tab-separated
            if len(df.columns) == 1: 
                df = pd.read_csv(file_path, sep='\t')
        except Exception as e:
            print(f"Could not read {file_path}. Error: {e}")
            continue
            
        # Verify columns exist
        if not all(col in df.columns for col in ['Role', 'Timestamp', 'Utterance']):
            print(f"WARNING: File {file_path} is missing expected columns. Skipping.")
            continue
        
        goals_list, actions_list = [], []
        
        for index, row in tqdm(df.iterrows(), total=df.shape[0]):
            # Pass the role and utterance directly
            goals, actions = classify_reflection(row['Role'], row['Utterance'])
            goals_list.append(goals)
            actions_list.append(actions)
            
        df['Reflected_Goals'] = goals_list
        df['Reflected_Actions'] = actions_list
        
        save_name = "ANNOTATED_" + os.path.basename(file_path).split('.')[0] + ".csv"
        df.to_csv(os.path.join(OUTPUT_FOLDER, save_name), index=False)

if __name__ == "__main__":
    process_reflections()