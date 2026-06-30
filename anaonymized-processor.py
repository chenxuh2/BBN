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


# --- REVIEW TRIAGE ---
def assign_review_flags(report_rows):
    """Tags each report row with 'needs_review' (yes/no) and 'review_reason'.

    Only force a review on genuinely low-confidence splits (the fallbacks,
    failures, and broken/empty debriefings); everything the splitter matched
    by phrase is trusted and left to its confidence rating.
    """
    for row in report_rows:
        reason = row.get("split_reason", "")
        deb_rows = row.get("debriefing_rows")

        # 1. Failures and the no-evidence fallbacks (confidence 0/1).
        if reason in ("error", "empty_file", "duration_fallback", "end_fallback"):
            row["needs_review"] = "yes"
            row["review_reason"] = f"low_confidence:{reason}"
            continue

        # 2. Phrase matched but produced an empty/near-empty debriefing -> likely broken.
        if isinstance(deb_rows, int) and deb_rows <= 1:
            row["needs_review"] = "yes"
            row["review_reason"] = "empty_debriefing"
            continue

        # Otherwise it's a confident phrase-based split; rely on its confidence rating.
        row["needs_review"] = "no"
        row["review_reason"] = ""


# --- REVIEW CONTEXT ---
CONTEXT_BEFORE = 3   # simulation rows to show before the cut
CONTEXT_AFTER = 5    # debriefing rows to show after the cut sentence
REVIEW_CONTEXT_PATH = os.path.join(OUTPUT_FOLDER, "split_review_context.csv")

# Human-corrected cut points. CSV columns: file_name, and ONE of
#   row_index         -> the row_index (copied from split_review_context.csv), OR
#   split_start_time  -> the 'start time' of the first debriefing row,
#                        read straight off the original input CSV.
# Use split_start_time when the correct cut is far from the heuristic's guess
# and therefore not visible in the context window.
MANUAL_OVERRIDES_PATH = os.path.join(OUTPUT_FOLDER, "manual_overrides.csv")


def load_manual_overrides():
    """Reads file_name -> {row_index, split_start_time} from the overrides CSV."""
    if not os.path.exists(MANUAL_OVERRIDES_PATH):
        return {}
    overrides = {}
    df = pd.read_csv(MANUAL_OVERRIDES_PATH, dtype=str).fillna("")
    for _, r in df.iterrows():
        name = str(r.get("file_name", "")).strip()
        if not name:
            continue
        overrides[name] = {
            "row_index": str(r.get("row_index", "")).strip(),
            "split_start_time": str(r.get("split_start_time", "")).strip(),
        }
    return overrides


def resolve_override_index(df, override):
    """Turns a manual override into a row index, or None if it can't be applied.

    Prefers an explicit row index; otherwise matches the given start time
    (exact string first, then the first row at/after that time).
    """
    if override.get("row_index"):
        return max(0, min(int(float(override["row_index"])), len(df)))

    target = override.get("split_start_time", "")
    if target and "start time" in df.columns:
        exact = df.index[df["start time"].astype(str).str.strip() == target]
        if len(exact):
            return int(exact[0])
        # Fall back to the first row whose start time is at/after the target.
        target_seconds = time_to_seconds(target)
        seconds = df["start time"].apply(time_to_seconds)
        at_or_after = df.index[seconds >= target_seconds]
        if len(at_or_after):
            return int(at_or_after[0])

    return None  # unusable override -> caller falls back to the heuristic


def build_context_rows(df, split_idx, base_name, reason, confidence):
    """Builds a long-format window of rows around the cut for quick eyeballing.

    The cut row (split_idx) is the FIRST debriefing row; rows before it are the
    tail of the simulation. One dict per row, ready to stack across all files.
    """
    rows = []
    start = max(0, split_idx - CONTEXT_BEFORE)
    end = min(len(df), split_idx + CONTEXT_AFTER + 1)  # +1 to include the cut row itself
    for i in range(start, end):
        r = df.iloc[i]
        offset = i - split_idx  # <0 simulation, 0 = cut row, >0 debriefing
        rows.append({
            "file_name": f"{base_name}.csv",
            "needs_review": "",   # stamped in after triage
            "review_reason": "",  # stamped in after triage
            "split_reason": reason,
            "confidence": confidence,
            "marker": ">>> CUT" if offset == 0 else "",
            "phase": "SIMULATION" if offset < 0 else "DEBRIEFING",
            "offset": offset,
            "row_index": i,  # put this number into manual_overrides.csv to force the cut here

            "start time": r.get("start time", ""),
            "end time": r.get("end time", ""),
            "text": r.get("text", ""),
        })
    return rows


def split_and_save_csv(file_path, overrides=None):
    """Splits one CSV into simulation and debriefing files."""
    df = pd.read_csv(file_path)

    # A human-supplied cut wins over the heuristic (by row index or start time);
    # if it can't be resolved, fall back to the heuristic.
    name = os.path.basename(file_path)
    override_idx = resolve_override_index(df, overrides[name]) if overrides and name in overrides else None
    if override_idx is not None:
        split_idx, reason, confidence = override_idx, "manual_override", 10
    else:
        split_idx, reason, confidence = find_debriefing_start_index(df)

    base_name = os.path.splitext(os.path.basename(file_path))[0]
    simulation_path = os.path.join(SIMULATION_FOLDER, f"{base_name}_SIMULATION.csv")
    debriefing_path = os.path.join(DEBRIEFING_FOLDER, f"{base_name}_DEBRIEFING.csv")

    df.iloc[:split_idx].to_csv(simulation_path, index=False)
    df.iloc[split_idx:].to_csv(debriefing_path, index=False)

    print(f"{base_name}: {reason}, confidence={confidence}")

    context_rows = build_context_rows(df, split_idx, base_name, reason, confidence)
    split_row = df.iloc[split_idx] if not df.empty and split_idx < len(df) else None
    report_row = {
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
    return report_row, context_rows


def process_files():
    """Processes all CSV files in the input folder."""
    os.makedirs(SIMULATION_FOLDER, exist_ok=True)
    os.makedirs(DEBRIEFING_FOLDER, exist_ok=True)

    files = glob.glob(os.path.join(INPUT_FOLDER, "*.csv"))
    print(f"Found {len(files)} CSV files.")

    overrides = load_manual_overrides()
    if overrides:
        print(f"Loaded {len(overrides)} manual override(s).")

    report_rows = []
    context_rows = []

    for file_path in files:
        try:
            report_row, ctx = split_and_save_csv(file_path, overrides)
            report_rows.append(report_row)
            context_rows.extend(ctx)
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

    # Tag which files a human should manually re-check.
    assign_review_flags(report_rows)
    to_review = sum(1 for r in report_rows if r.get("needs_review") == "yes")

    # Stamp each file's review flag onto its context rows so the long-format
    # CSV can be filtered/sorted by needs_review while browsing.
    review_map = {
        r["file_name"]: (r.get("needs_review", ""), r.get("review_reason", ""))
        for r in report_rows
    }
    for c in context_rows:
        c["needs_review"], c["review_reason"] = review_map.get(c["file_name"], ("", ""))

    pd.DataFrame(report_rows).to_csv(CONFIDENCE_REPORT_PATH, index=False)
    print(f"Saved confidence report: {CONFIDENCE_REPORT_PATH}")
    pd.DataFrame(context_rows).to_csv(REVIEW_CONTEXT_PATH, index=False)
    print(f"Saved review context: {REVIEW_CONTEXT_PATH}")
    print(f"Flagged {to_review}/{len(report_rows)} files for manual review.")

    # Breakdown of why files were flagged, so the triage stays transparent.
    breakdown = {}
    for r in report_rows:
        if r.get("needs_review") == "yes":
            breakdown[r["review_reason"]] = breakdown.get(r["review_reason"], 0) + 1
    for review_reason, count in sorted(breakdown.items(), key=lambda kv: -kv[1]):
        print(f"   {count:>4}  {review_reason}")


if __name__ == "__main__":
    process_files()
