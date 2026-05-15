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

def srt_time_to_seconds(time_str):
    """Converts an SRT timestamp string into seconds from the start."""
    time_value = parse_srt_time(time_str)
    return (
        time_value.hour * 3600
        + time_value.minute * 60
        + time_value.second
        + time_value.microsecond / 1_000_000
    )

def debriefing_phrase_score(text):
    """Scores phrases that commonly indicate the start of debriefing."""
    text = str(text).lower()

    # These phrases are treated as direct evidence that the transcript has
    # moved into the debriefing section. Higher weights mean stronger evidence.
    phrase_weights = {
        "debriefing": 5,
        "debrief": 5,
        "reflection": 5,
        "reflect": 5,
        "discussion": 3,
        "feedback": 3,
        "encounter": 3,
    }

    score = 0
    for phrase, weight in phrase_weights.items():
        if phrase in text:
            score += weight

    return score

def has_entry_phrase(text):
    """Finds 'come in' style cues without matching inside longer words."""
    text = str(text).lower()

    # Entry phrases such as "come in" are common in normal speech, so they are
    # only used as candidate cues. The main detection function confirms them
    # with nearby debriefing-related language before splitting.
    entry_patterns = [
        r"\bcome\s+(?:on\s+)?in\b",
        r"\bcome\s+back\s+in\b",
        r"\byou\s+can\s+come\s+in\b",
        r"\bbring\s+.*\bin\b",
        r"\bjoin\s+us\b",
    ]

    return any(re.search(pattern, text) for pattern in entry_patterns)

def find_debriefing_start_index(df):
    """
    Finds the first row that should belong to debriefing.

    The cut is always made at a row boundary, never inside a text cell. "Come in"
    is treated as a candidate cue only when nearby debriefing language supports it.
    """
    if df.empty:
        return 0, "empty_file", 0

    working_df = df.copy()
    working_df["_start_seconds"] = working_df["start time"].apply(srt_time_to_seconds)
    total_duration = srt_time_to_seconds(working_df["end time"].iloc[-1])

    # Do not look for debriefing before 20 minutes. This reduces false positives
    # from words like "feedback" or "come in" during the simulation itself.
    search_after_seconds = 20 * 60

    # If a candidate entry phrase is found, look forward this many seconds to
    # check whether debriefing language appears soon after it.
    context_window_seconds = 120

    for idx, row in working_df.iterrows():
        row_start = row["_start_seconds"]
        if row_start < search_after_seconds:
            continue

        row_text = row["text"]
        row_score = debriefing_phrase_score(row_text)

        # Create a look-ahead window from the current row. This lets the script
        # treat "come in" as a debriefing start only when the following context
        # also sounds like debriefing.
        window_df = working_df[
            (working_df["_start_seconds"] >= row_start)
            & (working_df["_start_seconds"] <= row_start + context_window_seconds)
        ]
        window_score = sum(debriefing_phrase_score(text) for text in window_df["text"])

        # Strong debriefing phrases can define the split point by themselves.
        if row_score >= 5:
            return idx, "strong_debriefing_phrase", min(row_score, 10)

        # Entry phrases need supporting debriefing context within the look-ahead
        # window before they are accepted as the split point.
        if has_entry_phrase(row_text) and window_score >= 5:
            return idx, "entry_phrase_with_debriefing_context", min(window_score, 10)

    # Fallback: if no phrase-based split is found, assume the final 17 minutes
    # are likely debriefing, but never split earlier than the 20-minute mark.
    fallback_start_seconds = max(search_after_seconds, total_duration - 17 * 60)
    fallback_candidates = working_df[working_df["_start_seconds"] >= fallback_start_seconds]
    if not fallback_candidates.empty:
        return fallback_candidates.index[0], "duration_fallback", 1

    # Last resort for unusual or very short files.
    return working_df.index[-1], "end_fallback", 0

def split_simulation_and_debriefing(df):
    """Splits the transcript into simulation+transition and debriefing DataFrames."""
    debriefing_start_idx, method, confidence = find_debriefing_start_index(df)

    # Split only between CSV/SRT rows. This prevents cutting a subtitle block or
    # a text cell in the middle of a sentence.
    simulation_df = df.loc[df.index < debriefing_start_idx].copy()
    debriefing_df = df.loc[df.index >= debriefing_start_idx].copy()

    simulation_df["phase"] = "simulation_transition"
    debriefing_df["phase"] = "debriefing"

    return simulation_df, debriefing_df, method, confidence

# --- BATCH EXECUTION LOGIC ---

INPUT_FOLDER = "srt_data"
OUTPUT_FOLDER = "processed_srt_to_csvs"
SIMULATION_OUTPUT_FOLDER = "srt_to_csv_simulation"
DEBRIEFING_OUTPUT_FOLDER = "srt_csv_debriefing"

def batch_process():
    for output_folder in [
        OUTPUT_FOLDER,
        SIMULATION_OUTPUT_FOLDER,
        DEBRIEFING_OUTPUT_FOLDER,
    ]:
        if not os.path.exists(output_folder):
            os.makedirs(output_folder)
        
    search_pattern = os.path.join(INPUT_FOLDER, "*.srt")
    srt_files = glob.glob(search_pattern)
    
    if len(srt_files) == 0:
        print(f"No .srt files found in '{INPUT_FOLDER}'.")
        return

    print(f"Found {len(srt_files)} files. Starting batch process...\n")
    
    for file_path in srt_files:
        base_name = os.path.splitext(os.path.basename(file_path))[0]
        print(f"Processing: {base_name}.srt...")
        output_path = os.path.join(OUTPUT_FOLDER, f"{base_name}_full_uncut.csv")
        
        try:
            df = process_srt_to_df(file_path)
            df.to_csv(output_path, index=False, encoding='utf-8')
            simulation_df, debriefing_df, method, confidence = split_simulation_and_debriefing(df)

            simulation_output_path = os.path.join(
                SIMULATION_OUTPUT_FOLDER,
                f"{base_name}_simulation_transition_cut.csv"
            )
            debriefing_output_path = os.path.join(
                DEBRIEFING_OUTPUT_FOLDER,
                f"{base_name}_debriefing_cut.csv"
            )

            simulation_df.to_csv(simulation_output_path, index=False, encoding='utf-8')
            debriefing_df.to_csv(debriefing_output_path, index=False, encoding='utf-8')

            print(f"✅ Success: {base_name}.srt -> Saved to {OUTPUT_FOLDER}")
            print(f"   Split: {method}, confidence={confidence}")
            print(f"   Simulation+transition -> {SIMULATION_OUTPUT_FOLDER}")
            print(f"   Debriefing -> {DEBRIEFING_OUTPUT_FOLDER}")
        except Exception as e:
            print(f"❌ Error processing {base_name}.srt: {e}")
            
    print("\nBatch processing complete!")

if __name__ == "__main__":
    batch_process()
