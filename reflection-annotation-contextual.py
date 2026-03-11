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
    ] 
    # ... add other goals ...
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
    
  
    CRITICAL RULE - "DOING" VS "DISCUSSING" & INHERITING CONTEXT:
    1. The clinical simulation is OVER. DO NOT annotate what the speaker is *doing* in the current room (e.g., saying "Let's sit down" in the debrief is not the clinical action "Sat down").
    2. ONLY annotate which clinical actions they are *evaluating, recalling, or reflecting on* from the past simulation.
    3. INHERITED REFLECTION: If the TARGET UTTERANCE is a generic statement of agreement (e.g., "I agree", "Yeah", "Exactly"), look at the immediate previous utterance. If the target is validating or agreeing with a peer's reflection on a specific Goal/Action, you MUST annotate the target utterance with that exact same Goal/Action.
    4. STRICT VOCABULARY: 
        - You MUST use the EXACT strings from the TAXONOMY provided above. 
        - NEVER quote the transcript directly. E.g., if the speaker says "Super anxious", DO NOT put "Super anxious" in your JSON.
    5. GOAL INHERITANCE: Every Action belongs to a specific Goal in the TAXONOMY. If you identify a specific Action, you MUST also include its parent Goal in the "reflected_goals" list, even if the speaker didn't name the Goal explicitly.

    EXAMPLES:
    - DISCUSSING (Annotate): "I felt like I rushed the warning shot." -> Maps to "Provided appropriate opening statement (warning shot)."
    - INHERITED (Annotate): Target says "I agree completely." (Context: Peer just said "I forgot to introduce myself.") -> Maps to "Introduced him/herself by name and role."
    - DOING/LOGISTICS (Ignore): Target says "Okay, I agree." (Context: Peer just said "Let's move on to the next question.") -> Return empty lists.

    TASK:
    Analyze the TARGET UTTERANCE using the surrounding context and the CRITICAL RULE.
    1. Identify if the target speaker is **reflecting on, evaluating, or discussing** any of the Goals and Actions listed in the taxonomy.
    2. If a Facilitator asks about a specific action (e.g., "Did you introduce yourself?"), and the target replies ("No, I missed that"), map the target's reply to the corresponding Goal/Action being discussed.
    3. If they are just making small talk, managing the logistics of the room, or giving generic agreement, return empty lists.

    OUTPUT FORMAT (Strict JSON):
    You must strictly separate the overarching Goals from the specific Actions based on the TAXONOMY provided.
    - "reflected_goals" MUST ONLY contain the top-level categories that start with the word "Goal" (e.g., "Goal 3: Deliver the News").
    - "reflected_actions" MUST ONLY contain the specific bulleted items (e.g., "Used phrase 'dead' or 'died'"). DO NOT put items starting with "Goal" in this list.
    
    {{ 
        "reflected_goals": ["List of Goal names here"],
        "reflected_actions": ["List of specific Action names here"]
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
        
        # Clean out punctuation and join into a comma-separated string
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

        goals_list, actions_list = [], []
        
        for index, row in tqdm(df.iterrows(), total=df.shape[0]):
            # Pass the context window into the function
            goals, actions = classify_reflection_context(
                row['prev_role_2'], row['prev_text_2'],
                row['prev_role_1'], row['prev_text_1'],
                row['Role'], row['Utterance'],
                row['next_role'], row['next_text']
            )
            goals_list.append(goals)
            actions_list.append(actions)
            
        df['Reflected_Goals'] = goals_list
        df['Reflected_Actions'] = actions_list
        
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

