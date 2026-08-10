"""
CONFIG 1/4: SUMMARY, CORE-ONLY.
Conversation-level. One prompt reads the WHOLE debrief and reports only the
reflected (goal + item) set. NO valence, NO level. Used to test whether adding
valence/level in the other configs degrades goal-action detection.

Output: one row per (goal, item) reflected in the conversation.
"""
import os
import glob
import json
from tqdm import tqdm
import pandas as pd

import reflection_common as rc

INPUT_FOLDER = os.environ.get(
    "REFLECTION_INPUT",
    "/mnt/c/Users/stelh/Downloads/processed_anonymized_csvs/debriefing_sets/development",
)
OUTPUT_FOLDER = "ablation_outputs"
CONFIG_NAME = "summary_core_only"


def analyze(transcript):
    prompt = f"""
You are an expert qualitative researcher analyzing a full medical debriefing conversation.
A team of learners (including a MEDICAL STUDENT) just finished a BREAKING BAD NEWS simulation
(notifying a bereaved family of a death) and is now debriefing / reflecting on how it went.

TAXONOMY OF GOALS AND ACTIONS:
{json.dumps(rc.TAXONOMY, indent=2)}

FULL DEBRIEFING CONVERSATION (speakers are NOT reliably labeled by role):
{transcript}

TASKS:
1. Infer which speaker is most likely the MEDICAL STUDENT (the learner reflecting on their own
   clinical actions), with a brief justification.
2. Report which specific (GOAL + ITEM) reflections actually occur. Be STRICT and SPECIFIC:
   - Count an item only if a speaker explicitly evaluates/recalls/reflects on that action AND you
     can quote a direct supporting line. Exclude small talk, logistics, generic feelings.
   - "goal" MUST be exactly one of: {json.dumps(rc.GOAL_NAMES)} (never invent a new goal).
   - "item" MUST be the exact taxonomy item string, or "Other".
   Prefer a SMALL well-supported set; leaving items out is correct.

OUTPUT (Strict JSON, no prose):
{{
    "likely_medical_student": "speaker label",
    "medical_student_reasoning": "why, with brief evidence",
    "reflections": [
        {{"goal": "Goal N: ...", "item": "exact item or Other",
          "reasoning": "why this goal+item", "evidence": ["short quote"],
          "by_medical_student": true}}
    ]
}}
"""
    return rc.chat_json(prompt)


def run():
    os.makedirs(OUTPUT_FOLDER, exist_ok=True)
    files = sorted(glob.glob(os.path.join(INPUT_FOLDER, "*.csv"))
                   + glob.glob(os.path.join(INPUT_FOLDER, "*.tsv")))
    print(f"[{CONFIG_NAME}] model={rc.MODEL}  files={len(files)}  in={INPUT_FOLDER}")

    rows = []
    for fp in tqdm(files):
        base = os.path.basename(fp)
        df = rc.read_transcript_df(fp)
        if df is None:
            continue
        speaker_col = rc.find_col(df, "speaker", "role")
        text_col = rc.find_col(df, "text", "utterance")
        turn_col = rc.find_col(df, "turn")
        if text_col is None:
            print(f"  skip {base}: no text column {list(df.columns)}")
            continue
        transcript = rc.build_transcript(df, speaker_col, text_col, turn_col)
        if not transcript.strip():
            continue

        result = analyze(transcript)
        student = (result or {}).get("likely_medical_student", "Error")
        reflections = (result or {}).get("reflections", []) or []
        if not reflections:
            rows.append({"config": CONFIG_NAME, "model": rc.MODEL, "file": base,
                         "likely_medical_student": student, "goal": "None", "item": "None",
                         "evidence": "", "reasoning": "", "by_medical_student": ""})
            continue
        for r in reflections:
            goal, item = rc.correct_goal(r.get("goal", "Other"), r.get("item", "Other"))
            ev = r.get("evidence", [])
            ev = " | ".join(str(e).strip() for e in ev if str(e).strip()) if isinstance(ev, list) else str(ev)
            rows.append({"config": CONFIG_NAME, "model": rc.MODEL, "file": base,
                         "likely_medical_student": student, "goal": goal, "item": item,
                         "evidence": ev, "reasoning": str(r.get("reasoning", "")).strip(),
                         "by_medical_student": r.get("by_medical_student", "")})

    out = pd.DataFrame(rows, columns=["config", "model", "file", "likely_medical_student",
                                      "goal", "item", "evidence", "reasoning", "by_medical_student"])
    safe_model = rc.MODEL.replace(":", "-").replace("/", "-")
    path = os.path.join(OUTPUT_FOLDER, f"{CONFIG_NAME}__{safe_model}.csv")
    out.to_csv(path, index=False)
    print(f"[{CONFIG_NAME}] wrote {len(out)} rows -> {path}")


if __name__ == "__main__":
    run()
