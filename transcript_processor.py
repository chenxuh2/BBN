import os
import re
import glob
import pandas as pd

# --- CONFIGURATION ---
INPUT_FOLDER = "input_data"       # Folder where your raw .txt files are
OUTPUT_FOLDER = "processed_csvs"  # Folder where CSVs will be saved

def parse_utterances(text_block):
    """
    Parses a block of text into a list of dicts:
    Format expected:
    ROLE 12:30
    The utterance text here...
    """
    # REGEX EXPLANATION:
    # ^(?P<Role>.+?)       -> Capture Role at start of line (lazy match until time)
    # \s+                  -> Space(s) between Role and Time
    # (?P<Time>\d{1,2}:\d{2}) -> Capture Time (e.g., 5:00 or 12:30)
    # \s*\n                -> Newline immediately after time
    # (?P<Utterance>.*?)   -> Capture everything that follows (the speech)
    # (?=\n.+?\s+\d{1,2}:\d{2}|\Z) -> LOOKAHEAD: Stop capturing when you see the pattern of a NEW Role/Time OR End of File (\Z)
    
    pattern = r"^(?P<Role>.+?)\s+(?P<Time>\d{1,2}:\d{2})\s*\n(?P<Utterance>.*?)(?=\n.+?\s+\d{1,2}:\d{2}|\Z)"
    
    # flags=re.MULTILINE lets '^' match start of lines
    # flags=re.DOTALL lets '.' match newlines (for multi-line utterances)
    matches = re.finditer(pattern, text_block, flags=re.MULTILINE | re.DOTALL)
    
    data = []
    for m in matches:
        data.append({
            "Role": m.group("Role").strip(),
            "Timestamp": m.group("Time").strip(),
            "Utterance": m.group("Utterance").strip()  # Removes extra newlines
        })
        
    return pd.DataFrame(data)

def process_files():
    # 1. Setup Folders
    if not os.path.exists(OUTPUT_FOLDER):
        os.makedirs(OUTPUT_FOLDER)
    
    # Get all .txt files
    files = glob.glob(os.path.join(INPUT_FOLDER, "*.txt"))
    print(f"Found {len(files)} text files in '{INPUT_FOLDER}'...")

    for file_path in files:
        base_name = os.path.basename(file_path).replace(".txt", "")
        print(f"Processing: {base_name}...")
        
        with open(file_path, "r", encoding="utf-8") as f:
            full_text = f.read()

        # 2. Find the Split Points
        # We look for dashes + SIMULATION/DEBRIEF + dashes (case insensitive)
        sim_match = re.search(r"-{3,}\s*SIMULATION\s*-{3,}", full_text, re.IGNORECASE)
        debrief_match = re.search(r"-{3,}\s*DEBRIEF\s*-{3,}", full_text, re.IGNORECASE)

        sim_text = ""
        debrief_text = ""

        # Logic to slice the text correctly
        if sim_match:
            start_sim = sim_match.end() # Start reading AFTER the header
            # If there is a debrief, stop there. If not, go to end of file.
            end_sim = debrief_match.start() if debrief_match else len(full_text)
            sim_text = full_text[start_sim:end_sim].strip()
        
        if debrief_match:
            start_debrief = debrief_match.end()
            debrief_text = full_text[start_debrief:].strip()

        # 3. Process and Save Simulation
        if sim_text:
            df_sim = parse_utterances(sim_text)
            save_path = os.path.join(OUTPUT_FOLDER, f"{base_name}_SIMULATION.csv")
            df_sim.to_csv(save_path, index=False)
            print(f"   -> Saved SIMULATION: {len(df_sim)} rows")
        else:
            print("   [!] No SIMULATION section found.")

        # 4. Process and Save Debrief
        if debrief_text:
            df_debrief = parse_utterances(debrief_text)
            save_path = os.path.join(OUTPUT_FOLDER, f"{base_name}_DEBRIEF.csv")
            df_debrief.to_csv(save_path, index=False)
            print(f"   -> Saved DEBRIEF:    {len(df_debrief)} rows")
        else:
            print("   [!] No DEBRIEF section found.")

if __name__ == "__main__":
    # Create dummy folder if it doesn't exist so user knows where to put files
    if not os.path.exists(INPUT_FOLDER):
        os.makedirs(INPUT_FOLDER)
        print(f"Created folder '{INPUT_FOLDER}'. Please put your .txt files there and run again.")
    else:
        process_files()