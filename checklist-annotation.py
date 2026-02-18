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

# PASTE YOUR FULL CHECKLIST HERE
# The LLM needs this to know what options to pick from.
CHECKLIST_TEXT = """
1. MED: Addressed family member by name.
2. MED: Introduced him/herself by name and role.
3. MED: Clearly stated the name of the deceased family member.
4. MED: Sat down. (Body language/Eye contact)
5. MED: Ensured all important survivors were present.
6. MED: Determined knowledge survivors possessed.
7. MED: Provided appropriate opening statement (“warning shot”).
8. MED: Accurately/succinctly chronicled events leading to death.
9. MED: Used phrase “dead” or “died” (avoided euphemisms).
10. MED: Avoided jargon or explained terms.
11. MED: Paused to allow family to assimilate information.
12. MED: Offered viewing of the deceased.
13. MED: Established availability to answer questions.
14. MED: Displayed professional attire/presence.
15. MED: Responded to cues with appropriate touch.
16. MED: Handled interruptions in non-disruptive manner.
17. MED: Emotional response did not interfere with communication.
18. MED: Involved me when discussing reason for visit.
19. MED: Legitimized my emotions.
20. MED: Reinforced positive behaviors.
21. MED: Encouraged questions/concerns.
22. MED: Elicited patient perspective of health situation.
23. MED: Conducted interaction in organized manner.
24. MED: Summarized the interview.
25. MED: Checked for accuracy during interview.
26. MED: Reviewed next step(s).
27. MED: Verified patient's understanding.
"""

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

def classify_utterance(role, text):
    # Skip non-students (Adjust based on your specific role names)
    # Checks for 'Student', 'Candidate', 'Participant' to be safe
    if not any(x in role.upper() for x in ["STUDENT", "CANDIDATE", "PARTICIPANT"]):
        return "N/A", "False"

    prompt = f"""
    You are a data extraction bot.
    
    CHECKLIST:
    {CHECKLIST_TEXT}
    
    INSTRUCTION:
    Map the utterance to the closest Checklist Item. 
    Then determine if it is the "Death Announcement".
    
    STRICT OUTPUT FORMAT:
    Item Name | True/False
    
    UTTERANCE: "{text}"
    """

    try:
        response = ollama.chat(model=MODEL_NAME, messages=[
            {'role': 'user', 'content': prompt},
        ])
        
        raw_answer = response['message']['content']
        return clean_llm_response(raw_answer)

    except Exception as e:
        print(f"Error: {e}")
        return "Error", "Error"
    
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
        
        # Create new columns
        predicted_items = []
        death_flags = []
        
        # Use tqdm to show a progress bar because LLMs are slow
        for index, row in tqdm(df.iterrows(), total=df.shape[0]):
            item, is_death = classify_utterance(row['Role'], row['Utterance'])
            predicted_items.append(item)
            death_flags.append(is_death)
            
        df['Predicted_Item'] = predicted_items
        df['Is_Death_Announcement'] = death_flags
        
        # Save
        save_path = os.path.join(OUTPUT_FOLDER, f"ANNOTATED_{base_name}")
        df.to_csv(save_path, index=False)
        print(f"Saved to {save_path}")

if __name__ == "__main__":
    # Ensure Ollama is running in the background!
    process_annotations()