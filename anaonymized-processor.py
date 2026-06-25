import glob
import os
import re
import json
import pandas as pd


# --- CONFIGURATION ---
INPUT_FOLDER = "/mnt/c/Users/stelh/Downloads/hippa"
OUTPUT_FOLDER = "/mnt/c/Users/stelh/Downloads/processed_anonymized_csvs"
SIMULATION_FOLDER = os.path.join(OUTPUT_FOLDER, "simulation")
DEBRIEFING_FOLDER = os.path.join(OUTPUT_FOLDER, "debriefing")
CONFIDENCE_REPORT_PATH = os.path.join(OUTPUT_FOLDER, "split_confidence_report.csv")


def time_to_seconds(time_str):
    """Converts a timestamp string into seconds from the start."""
    text = str(time_str).strip().replace(",", ".")
    parts = text.split(":")

    if len(parts) == 3:
        return int(parts[0]) * 3600 + int(parts[1]) * 60 + float(parts[2])
    if len(parts) == 2:
        return int(parts[0]) * 60 + float(parts[1])
    return float(text)


def debriefing_phrase_score(text):
    """Scores phrases that commonly indicate the start of debriefing."""
    text = str(text).lower()

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

    The cut is always made at a row boundary, never inside a text cell.
    """
    if df.empty:
        return 0, "empty_file", 0

    working_df = df.reset_index(drop=True).copy()
    working_df["_start_seconds"] = working_df["start time"].apply(time_to_seconds)
    total_duration = time_to_seconds(working_df["end time"].iloc[-1])

    search_after_seconds = 20 * 60
    context_window_seconds = 120

    for idx, row in working_df.iterrows():
        row_start = row["_start_seconds"]
        if row_start < search_after_seconds:
            continue

        row_text = row["text"]
        row_score = debriefing_phrase_score(row_text)

        window_df = working_df[
            (working_df["_start_seconds"] >= row_start)
            & (working_df["_start_seconds"] <= row_start + context_window_seconds)
        ]
        window_score = sum(debriefing_phrase_score(text) for text in window_df["text"])

        if row_score >= 5:
            return idx, "strong_debriefing_phrase", min(row_score, 10)

        if has_entry_phrase(row_text) and window_score >= 5:
            return idx, "entry_phrase_with_debriefing_context", min(window_score, 10)

    fallback_start_seconds = max(search_after_seconds, total_duration - 17 * 60)
    fallback_candidates = working_df[working_df["_start_seconds"] >= fallback_start_seconds]
    if not fallback_candidates.empty:
        return fallback_candidates.index[0], "duration_fallback", 1

    return len(working_df) - 1, "end_fallback", 0


def split_and_save_csv(file_path):
    """Splits one CSV into simulation and debriefing files."""
    df = pd.read_csv(file_path)
    split_idx, reason, confidence = find_debriefing_start_index(df)

    base_name = os.path.splitext(os.path.basename(file_path))[0]
    simulation_path = os.path.join(SIMULATION_FOLDER, f"{base_name}_SIMULATION.csv")
    debriefing_path = os.path.join(DEBRIEFING_FOLDER, f"{base_name}_DEBRIEFING.csv")

    df.iloc[:split_idx].to_csv(simulation_path, index=False)
    df.iloc[split_idx:].to_csv(debriefing_path, index=False)

    print(f"{base_name}: {reason}, confidence={confidence}")

    split_row = df.iloc[split_idx] if not df.empty and split_idx < len(df) else None
    return {
        "file_name": os.path.basename(file_path),
        "input_path": file_path,
        "total_rows": len(df),
        "simulation_rows": split_idx,
        "debriefing_rows": len(df) - split_idx,
        "split_index": split_idx,
        "split_start_time": "" if split_row is None else split_row.get("start time", ""),
        "split_end_time": "" if split_row is None else split_row.get("end time", ""),
        "split_text_preview": "" if split_row is None else str(split_row.get("text", ""))[:200],
        "split_reason": reason,
        "confidence": confidence,
        "simulation_output_path": simulation_path,
        "debriefing_output_path": debriefing_path,
        "status": "processed",
        "error": "",
    }


def process_files():
    """Processes all CSV files in the input folder."""
    os.makedirs(SIMULATION_FOLDER, exist_ok=True)
    os.makedirs(DEBRIEFING_FOLDER, exist_ok=True)

    files = glob.glob(os.path.join(INPUT_FOLDER, "*.csv"))
    print(f"Found {len(files)} CSV files.")

    report_rows = []

    for file_path in files:
        try:
            report_rows.append(split_and_save_csv(file_path))
        except Exception as exc:
            print(f"{os.path.basename(file_path)}: error={exc}")
            report_rows.append({
                "file_name": os.path.basename(file_path),
                "input_path": file_path,
                "total_rows": "",
                "simulation_rows": "",
                "debriefing_rows": "",
                "split_index": "",
                "split_start_time": "",
                "split_end_time": "",
                "split_text_preview": "",
                "split_reason": "error",
                "confidence": "",
                "simulation_output_path": "",
                "debriefing_output_path": "",
                "status": "error",
                "error": str(exc),
            })

    pd.DataFrame(report_rows).to_csv(CONFIDENCE_REPORT_PATH, index=False)
    print(f"Saved confidence report: {CONFIDENCE_REPORT_PATH}")


if __name__ == "__main__":
    process_files()
