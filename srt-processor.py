import re
import pandas as pd
from datetime import datetime
import os
import glob

def parse_srt_time(time_str):
    """Converts an SRT timestamp into a datetime object for math."""
    return datetime.strptime(time_str, '%H:%M:%S,%f')

def process_srt_to_df(file_path):
    """Parses a single SRT file into a Pandas DataFrame."""
    # 1. MUST be .read() so it is a single string, not .readlines()
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read()
        # print(f"Read {len(content)} characters from {file_path}")  # Debug print to check file reading
        
    blocks = content.strip().split('\n\n')
    # print(f"Found {len(blocks)} blocks in {file_path}")  # Debug print to check block splitting
    data = []
    
    for block in blocks:
        lines = block.split('\n')
        # print(f"Lines in block: {lines}")  # Debug print to check lines in each block
        # print(f"Processing block with {len(lines)} lines...")  # Debug print to check block structure
        if len(lines) >= 3:
            turn = lines[0].strip()
            # print(f"Processing turn: {turn}")  # Debug print to check the turn number
            time_line = lines[1].strip()
            
            # 2. MUST be joined before stripping
            raw_text = " ".join(lines[2:]).strip()
            # print(f"Raw text: '{raw_text}'")  # Debug print to check raw text extraction
            
            time_match = re.match(r'(\d{2}:\d{2}:\d{2},\d{3}) --> (\d{2}:\d{2}:\d{2},\d{3})', time_line)
            if not time_match:
                continue
                
            start_time_str, end_time_str = time_match.groups()
            
            start_time = parse_srt_time(start_time_str)
            end_time = parse_srt_time(end_time_str)
            duration = (end_time - start_time).total_seconds()
            
            speaker_match = re.match(r'\[(.*?)\]:\s*(.*)', raw_text)
            
            if speaker_match:
                speaker = speaker_match.group(1) 
                text = speaker_match.group(2)    
            else:
                speaker = "Unknown"
                text = raw_text
                
            data.append({
                'turn': turn,
                'speaker': speaker,
                'start time': start_time_str,
                'end time': end_time_str,
                'duration': round(duration, 3), 
                'text': text
            })
            
    return pd.DataFrame(data)

# --- BATCH EXECUTION LOGIC ---

INPUT_FOLDER = "srt_data"
OUTPUT_FOLDER = "processed_srt_to_csvs"

def batch_process():
    if not os.path.exists(OUTPUT_FOLDER):
        os.makedirs(OUTPUT_FOLDER)
        
    search_pattern = os.path.join(INPUT_FOLDER, "*.srt")
    srt_files = glob.glob(search_pattern)
    
    if len(srt_files) == 0:
        print(f"No .srt files found in '{INPUT_FOLDER}'.")
        return

    print(f"Found {len(srt_files)} files. Starting batch process...\n")
    
    for file_path in srt_files:
        base_name = os.path.basename(file_path).split('.')[0]
        print(f"Processing: {base_name}.srt...")
        output_path = os.path.join(OUTPUT_FOLDER, f"{base_name}.csv")
        
        try:
            df = process_srt_to_df(file_path)
            df.to_csv(output_path, index=False, encoding='utf-8')
            print(f"✅ Success: {base_name}.srt -> Saved to {OUTPUT_FOLDER}")
        except Exception as e:
            print(f"❌ Error processing {base_name}.srt: {e}")
            
    print("\nBatch processing complete!")

if __name__ == "__main__":
    batch_process()