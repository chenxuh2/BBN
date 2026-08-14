"""
CONFIG 2/4: SUMMARY, FULL, ONE-STAGE (the anchor config).
Conversation-level. ONE prompt reads the whole debrief and reports each reflected
(goal + item) together with valence and level.

Output: one row per (goal, item) with valence + level.
"""
import os
import glob
import json
from tqdm import tqdm
import pandas as pd

import reflection_common as rc

INPUT_FOLDER = os.environ.get("REFLECTION_INPUT", rc.DEFAULT_INPUT)
OUTPUT_FOLDER = "ablation_outputs"
CONFIG_NAME = "summary_full_one_stage"

# By default run ALL candidate models into one combined CSV; OLLAMA_MODEL picks a single one.
MODELS = [os.environ["OLLAMA_MODEL"]] if os.environ.get("OLLAMA_MODEL") else rc.CANDIDATE_MODELS


def analyze(transcript, model):
    prompt = f"""
You are an expert qualitative researcher analyzing a full medical debriefing conversation.
A team of learners (including a MEDICAL STUDENT) just finished a BREAKING BAD NEWS simulation
(notifying a bereaved family of a death) and is now debriefing / reflecting on how it went.

TAXONOMY (pick an item CODE below; do NOT invent or paraphrase item text):
{rc.coded_taxonomy_text()}

FULL DEBRIEFING CONVERSATION (speakers are NOT reliably labeled by role):
{transcript}

TASK: Report each specific reflection in the conversation. Be STRICT and SPECIFIC:
   - Count a reflection only if a speaker explicitly evaluates/recalls/reflects on that action AND
     you can quote a direct supporting line. Exclude small talk, logistics, generic feelings.
   - Decide each reflection in THIS ORDER: (1) quote the supporting evidence, (2) reason which
     GOAL that evidence really shows (e.g. silence/pauses and touch are emotional-response actions,
     NOT setup or assessment), (3) THEN pick the single most specific code.
   - "item_code" MUST be one of the codes listed above (codes span G1.1 through G5.x, plus
     G#.OTHER and OTHER). Do NOT default to a goal's first item; pick the item that actually matches.
     Do NOT write item text and do NOT invent new codes.
   - CONSISTENCY: the item_code MUST follow from the evidence you quote. If the quote does not
     clearly express the coded action, pick a different code or drop the reflection - never pair a
     quote with an unrelated code.
   Include EVERY reflection that a direct quote clearly supports - a long debrief may have many, and
   that is fine. Do NOT pad the list with weak, forced, or unsupported mappings.
   For each reflection also code:
   - "valence" = ONE of {json.dumps(rc.VALENCE_VALUES)}:
{rc.VALENCE_DEFS}
   - "level" = ONE of {json.dumps(rc.LEVEL_VALUES)} (CODE THE HIGHEST THAT APPLIES):
{rc.levels_block()}

OUTPUT (Strict JSON, no prose). Fill the keys in the given order so evidence/reasoning come BEFORE the code:
{{
    "reflections": [
        {{"evidence": ["1-5 short verbatim quotes that ARE this reflection - the exact words that justify THIS code"],
          "reasoning": "which goal these quoted words show, and why this specific item over nearby ones",
          "item_code": "<the single best code from the taxonomy above>",
          "valence": "positive|negative|neutral|mixed",
          "level": "R0|R1|R2|R3|R4"}}
    ]
}}
"""
    return rc.chat_json(prompt, model=model)


def run():
    files = sorted(glob.glob(os.path.join(INPUT_FOLDER, "*.csv"))
                   + glob.glob(os.path.join(INPUT_FOLDER, "*.tsv")))
    print(f"[{CONFIG_NAME}] models={MODELS}  files={len(files)}  in={INPUT_FOLDER}")

    out_dir = os.path.join(OUTPUT_FOLDER, rc.input_set_name(INPUT_FOLDER))
    os.makedirs(out_dir, exist_ok=True)
    tag = MODELS[0].replace(":", "-").replace("/", "-") if len(MODELS) == 1 else "compare"
    path = os.path.join(out_dir, f"{CONFIG_NAME}__{tag}.csv")
    COLUMNS = ["config", "model", "file", "goal", "item",
               "valence", "level", "evidence", "reasoning"]

    def save(rows):
        pd.DataFrame(rows, columns=COLUMNS).to_csv(path, index=False)

    rows = []
    for model in MODELS:
        print(f"  === model: {model} ===")
        for fp in tqdm(files, desc=model):
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

            result = analyze(transcript, model)
            if result is None:
                print(f"  !! FAILED (bad/oversized JSON): {base} [{model}]")
                rows.append({"config": CONFIG_NAME, "model": model, "file": base,
                             "goal": "Error", "item": "Error", "valence": "Error",
                             "level": "Error", "evidence": "", "reasoning": ""})
                continue
            reflections = result.get("reflections", []) or []
            if not reflections:
                rows.append({"config": CONFIG_NAME, "model": model, "file": base,
                             "goal": "None", "item": "None", "valence": "None",
                             "level": "None", "evidence": "", "reasoning": ""})
                continue
            for r in reflections:
                goal, item = rc.resolve_code(r.get("item_code", "OTHER"))
                ev = r.get("evidence", [])
                ev = " | ".join(str(e).strip() for e in ev if str(e).strip()) if isinstance(ev, list) else str(ev)
                rows.append({"config": CONFIG_NAME, "model": model, "file": base,
                             "goal": goal, "item": item,
                             "valence": rc.normalize_valence(r.get("valence", "None")),
                             "level": rc.normalize_level(r.get("level", "None")),
                             "evidence": ev, "reasoning": str(r.get("reasoning", "")).strip()})
        save(rows)  # write after each model so the CSV on disk is never stale mid-run
        print(f"  saved after {model}: {len(rows)} rows -> {path}")

    print(f"[{CONFIG_NAME}] DONE {len(rows)} rows ({len(MODELS)} models) -> {path}")


if __name__ == "__main__":
    run()
