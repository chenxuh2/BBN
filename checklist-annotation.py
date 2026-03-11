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
9. MED: Used phrase 'dead' or 'died' (avoided euphemisms).
10. MED: Avoided jargon or explained terms.
11. MED: Paused to allow family to assimilate information.
12. MED: Offered viewing of the deceased.
13. MED: Established availability to answer questions.
14. MED: Displayed professional attire/presence.
15. MED: Responded to cues with appropriate touch.
16. MED: Handled interruptions in non-disruptive manner.
17. MED: Emotional response did not interfere with communication.
18. MED: Involved me when discussing reason for visit.
19. MED: Legitimized patients emotions.
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

def classify_row(role, text):
    allowed_roles = ["MEDICAL STUDENT"]
    if not any(x in role.upper() for x in allowed_roles):
        return "N/A", False
    

    # THE NEW "FEW-SHOT MULTI-LABEL" PROMPT
    prompt = f"""
    You are an expert medical simulation annotator.
    
    VALID CHECKLIST ITEMS:
    {json.dumps(CHECKLIST_TEXT)}

    SPEAKER ROLE: {role}
    UTTERANCE: "{text}"

    TASK:
    1. Identify ALL checklist items present in this {role}'s utterance. 
    2. Determine if this specific sentence is the explicit "Death Announcement".

    RULES:
    - You MUST return a JSON object with two keys: "items" (a list of strings) and "is_death" (boolean).
    - If multiple actions happen, list them all.
    - If NO actions happen, return an empty list [].
    - ONLY use exact strings from the Valid Checklist Items.

    EXAMPLES:
    Utterance: "Hello, I am Student Doctor Smith. Are you Mrs. Jones? I'm afraid I have bad news."
    Result: {{ "items": ["Introduced him/herself by name and role", "Addressed family member by name", "Provided appropriate opening statement"], "is_death": false }}

    Utterance: "I am so sorry, but despite our best efforts, your husband has died."
    Result: {{ "items": ["Used phrase 'dead” or “died” (avoided euphemisms)", "Legitimized patients emotions"], "is_death": true }}

    Utterance: "Okay."
    Result: {{ "items": [], "is_death": false }}

    ---
    ACTUAL UTTERANCE TO ANALYZE:
    "{text}"
    """

    try:
        response = ollama.chat(
            model="llama3", 
            messages=[{'role': 'user', 'content': prompt}],
            format="json",
            options={"temperature": 0.0} # Keeps the model strictly logical
        )
        
        result = json.loads(response['message']['content'])
        
        # Extract the list of items
        item_list = result.get("items", [])
        
        if not item_list:
            final_items = "None"
        elif isinstance(item_list, list):
            # Use Regex to replace anything that is NOT a word (\w) or space (\s) with nothing
            clean_list = [re.sub(r'[^\w\s]', '', item) for item in item_list]
            
            # Join the cleaned items with a comma and a space
            final_items = ", ".join(clean_list)
        else:
            # Fallback
            final_items = re.sub(r'[^\w\s]', '', str(item_list))

        return final_items, result.get("is_death", False)
    
    except Exception as e:
        print(f"Error parsing row: {e}")
    return "Error", False
    
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
            item, is_death = classify_row(row['Role'], row['Utterance'])
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