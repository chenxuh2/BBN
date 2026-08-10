"""
CONFIG 3/4: SUMMARY, FULL, TWO-STAGE.
Conversation-level, split into two prompts:
  Stage 1: read the whole debrief -> detect reflected (goal + item) set (+ evidence).
  Stage 2: for EACH confirmed reflection, code valence + level in isolation
           (the labeler is not distracted by the detection decision).

Output: one row per (goal, item) with valence + level  (same columns as one-stage).
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
CONFIG_NAME = "summary_full_two_stage"


def stage1_detect(transcript):
    prompt = f"""
You are an expert qualitative researcher analyzing a full medical debriefing conversation after a
BREAKING BAD NEWS simulation. A team of learners (including a MEDICAL STUDENT) is reflecting on it.

TAXONOMY OF GOALS AND ACTIONS:
{json.dumps(rc.TAXONOMY, indent=2)}

FULL DEBRIEFING CONVERSATION (speakers not reliably labeled):
{transcript}

TASKS:
1. Infer which speaker is most likely the MEDICAL STUDENT, with a brief justification.
2. Report each specific (GOAL + ITEM) reflection. Be STRICT: count an item only if a speaker
   explicitly evaluates/recalls/reflects on that action AND you can quote a direct supporting line.
   Exclude small talk, logistics, generic feelings.
   - "goal" MUST be exactly one of: {json.dumps(rc.GOAL_NAMES)} (never invent a goal).
   - "item" MUST be the exact taxonomy item, or "Other".
   Prefer a SMALL well-supported set.

OUTPUT (Strict JSON):
{{
    "likely_medical_student": "speaker label",
    "medical_student_reasoning": "why, with brief evidence",
    "reflections": [
        {{"goal": "Goal N: ...", "item": "exact item or Other",
          "reasoning": "why", "evidence": ["short quote"], "by_medical_student": true}}
    ]
}}
"""
    return rc.chat_json(prompt)


def stage2_label(transcript, goal, item, evidence):
    prompt = f"""
You are coding ONE already-confirmed reflection from a medical debriefing (BREAKING BAD NEWS sim).
Do NOT question whether it is a reflection - it is. Only assign valence and level.

FULL CONVERSATION (for context):
{transcript}

THE CONFIRMED REFLECTION:
  Goal: {goal}
  Item: {item}
  Supporting evidence: {evidence}

Assign:
- "valence" = ONE of {json.dumps(rc.VALENCE_VALUES)}:
{rc.VALENCE_DEFS}
- "level" = ONE of {json.dumps(rc.LEVEL_VALUES)} (CODE THE HIGHEST THAT APPLIES):
{rc.levels_block()}

OUTPUT (Strict JSON): {{"valence": "...", "level": "R#"}}
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

        s1 = stage1_detect(transcript)
        student = (s1 or {}).get("likely_medical_student", "Error")
        reflections = (s1 or {}).get("reflections", []) or []
        if not reflections:
            rows.append({"config": CONFIG_NAME, "model": rc.MODEL, "file": base,
                         "likely_medical_student": student, "goal": "None", "item": "None",
                         "valence": "None", "level": "None", "evidence": "", "reasoning": "",
                         "by_medical_student": ""})
            continue

        for r in reflections:
            goal, item = rc.correct_goal(r.get("goal", "Other"), r.get("item", "Other"))
            ev = r.get("evidence", [])
            ev = " | ".join(str(e).strip() for e in ev if str(e).strip()) if isinstance(ev, list) else str(ev)

            s2 = stage2_label(transcript, goal, item, ev) or {}
            rows.append({"config": CONFIG_NAME, "model": rc.MODEL, "file": base,
                         "likely_medical_student": student, "goal": goal, "item": item,
                         "valence": rc.normalize_valence(s2.get("valence", "None")),
                         "level": rc.normalize_level(s2.get("level", "None")),
                         "evidence": ev, "reasoning": str(r.get("reasoning", "")).strip(),
                         "by_medical_student": r.get("by_medical_student", "")})

    out = pd.DataFrame(rows, columns=["config", "model", "file", "likely_medical_student",
                                      "goal", "item", "valence", "level", "evidence",
                                      "reasoning", "by_medical_student"])
    safe_model = rc.MODEL.replace(":", "-").replace("/", "-")
    path = os.path.join(OUTPUT_FOLDER, f"{CONFIG_NAME}__{safe_model}.csv")
    out.to_csv(path, index=False)
    print(f"[{CONFIG_NAME}] wrote {len(out)} rows -> {path}")


if __name__ == "__main__":
    run()
