"""
CONFIG 4/4: CONTEXTUAL, FULL, ONE-STAGE.
Per-utterance. Each target utterance is judged with a +/-2 sliding context window:
is_reflection -> goal+action -> valence -> level, in ONE prompt.

Output: the original transcript with added columns
  Is_Reflection | Reflected_Goals | Reflected_Actions | Valence | Level | Other_Summary
(For comparison this is later aggregated to conversation-level goal-action sets.)
"""
import os
import glob
import re
import json
from tqdm import tqdm

import reflection_common as rc

INPUT_FOLDER = os.environ.get("REFLECTION_INPUT", rc.DEFAULT_INPUT)
OUTPUT_FOLDER = "ablation_outputs_contextual"
CONFIG_NAME = "contextual_full_one_stage"

# Contextual is per-utterance = heavy, so run ONE model per invocation (set OLLAMA_MODEL to pick it).
# Output is per-transcript and RESUMABLE (already-done files are skipped), so a large validation run
# can be stopped and continued without redoing finished transcripts.
MODEL = rc.MODEL


def classify(prev2_role, prev2_text, prev1_role, prev1_text,
             cur_role, cur_text, next_role, next_text, model):
    # Cheap pre-filter: empty or pure-filler backchannels skip the LLM call entirely.
    if rc.looks_like_filler(cur_text):
        return dict(is_reflection=False, goals="None", actions="None",
                    valence="None", level="None", other_summary="None")

    prompt = f"""
You are an expert qualitative researcher analyzing a medical debriefing transcript after a
BREAKING BAD NEWS simulation. Speakers are now reflecting on how it went.

TAXONOMY (map to an item CODE below; do NOT invent or paraphrase item text):
{rc.coded_taxonomy_text()}

CONVERSATION CONTEXT:
[2 ago] {prev2_role}: "{prev2_text}"
[1 ago] {prev1_role}: "{prev1_text}"
---
[TARGET UTTERANCE] {cur_role}: "{cur_text}"
---
[next] {next_role}: "{next_text}"

RULES:
1. ONLY REFLECTIONS: annotate the TARGET only when the speaker evaluates/recalls/reflects on a
   clinical action from the simulation. Otherwise set "is_reflection" false and return empty lists.
2. IGNORE SMALL TALK: filler ("yeah", "okay"), logistics, bare acknowledgements -> is_reflection false.
3. INHERIT CONTEXT: a generic agreement ("exactly") right after a specific reflection inherits it.
4. MAP TO ITEM CODES: first reason which GOAL the utterance really shows (e.g. silence/pauses and
   touch are emotional-response actions, NOT setup or assessment), THEN put the taxonomy code(s) in
   "reflected_item_codes".
   - Each entry MUST be one of the codes in the taxonomy above (codes span G1.1 through G5.x,
     plus G#.OTHER and OTHER). Do NOT default to a goal's first item; pick the item that matches.
   - Do NOT write item text and do NOT invent new codes.
   - CONSISTENCY: each code must be supported by the words shown in the CONTEXT WINDOW (the target
     plus the surrounding utterances). If nothing in that window clearly expresses the action, pick a
     different code or set is_reflection false.
   - Usually one code; use several only if the utterance clearly reflects on several actions.
5. When an OTHER code (G#.OTHER or OTHER) is used, write a 3-7 word "other_summary".
6. "valence" = ONE of {json.dumps(rc.VALENCE_VALUES)} (or "None" if not a reflection):
{rc.VALENCE_DEFS}
7. "level" = ONE of {json.dumps(rc.LEVEL_VALUES)} (CODE THE HIGHEST THAT APPLIES; "None" if not a reflection):
{rc.levels_block()}

OUTPUT (Strict JSON):
{{
    "is_reflection": true or false,
    "reason": "which goal the utterance shows and why (decide this BEFORE the codes)",
    "reflected_item_codes": ["<best code(s) from the taxonomy above>"],
    "valence": "positive|negative|neutral|mixed|None",
    "level": "R0|R1|R2|R3|R4|None",
    "other_summary": "short summary if Other else empty"
}}
"""
    result = rc.chat_json(prompt, model=model)
    if result is None:
        return dict(is_reflection="Error", goals="Error", actions="Error",
                    valence="Error", level="Error", other_summary="Error")

    is_refl = bool(result.get("is_reflection", False))
    codes = result.get("reflected_item_codes", []) or []
    if isinstance(codes, str):
        codes = [codes]
    # Codes map deterministically back to canonical (goal, item) - no paraphrase/hallucination.
    pairs = [rc.resolve_code(c) for c in codes]
    goals = [g for g, _ in pairs]
    actions = [it for _, it in pairs]

    clean_goals = " | ".join(goals) if goals else "None"
    clean_actions = " | ".join(actions) if actions else "None"

    valence = rc.normalize_valence(result.get("valence", "None"))
    level = rc.normalize_level(result.get("level", "None"))

    if not is_refl or (clean_goals == "None" and clean_actions == "None"):
        is_refl = False
        clean_goals, clean_actions, valence, level = "None", "None", "None", "None"

    other = str(result.get("other_summary", "")).strip() or "None"
    if ("Other" not in clean_goals and "Other" not in clean_actions) and other != "None":
        other = "None"
    if ("Other" in clean_goals or "Other" in clean_actions) and other == "None":
        other = "Unspecified other reflection"

    return dict(is_reflection=is_refl, goals=clean_goals, actions=clean_actions,
                valence=valence, level=level, other_summary=other)


def run():
    files = glob.glob(os.path.join(INPUT_FOLDER, "*_DEBRIEF*")) \
        or sorted(glob.glob(os.path.join(INPUT_FOLDER, "*.csv"))
                  + glob.glob(os.path.join(INPUT_FOLDER, "*.tsv")))
    out_dir = os.path.join(OUTPUT_FOLDER, rc.input_set_name(INPUT_FOLDER))
    os.makedirs(out_dir, exist_ok=True)
    safe_model = MODEL.replace(":", "-").replace("/", "-")
    print(f"[{CONFIG_NAME}] model={MODEL}  files={len(files)}  in={INPUT_FOLDER}")

    for fp in files:
        base = os.path.basename(fp)
        save = f"{CONFIG_NAME}__{safe_model}__{base.split('.')[0]}.csv"
        out_path = os.path.join(out_dir, save)
        if os.path.exists(out_path):
            print(f"  skip {base}: already done ({save})")   # resumable
            continue

        print(f"  annotating {base} ...")
        df = rc.read_transcript_df(fp)
        if df is None:
            continue
        role_col = rc.find_col(df, "role", "speaker")
        text_col = rc.find_col(df, "utterance", "text")
        if role_col is None or text_col is None:
            print(f"  skip {base}: need role/speaker + utterance/text, got {list(df.columns)}")
            continue

        roles = df[role_col].fillna("None")
        texts = df[text_col].fillna("None")
        p2r, p2t = roles.shift(2).fillna("None"), texts.shift(2).fillna("None")
        p1r, p1t = roles.shift(1).fillna("None"), texts.shift(1).fillna("None")
        nr, nt = roles.shift(-1).fillna("None"), texts.shift(-1).fillna("None")

        cols = {k: [] for k in ["Is_Reflection", "Reflected_Goals", "Reflected_Actions",
                                "Valence", "Level", "Other_Summary"]}
        for i in tqdm(range(len(df)), total=len(df), desc=base):
            res = classify(p2r.iloc[i], p2t.iloc[i], p1r.iloc[i], p1t.iloc[i],
                           roles.iloc[i], texts.iloc[i], nr.iloc[i], nt.iloc[i], MODEL)
            cols["Is_Reflection"].append(res["is_reflection"])
            cols["Reflected_Goals"].append(res["goals"])
            cols["Reflected_Actions"].append(res["actions"])
            cols["Valence"].append(res["valence"])
            cols["Level"].append(res["level"])
            cols["Other_Summary"].append(res["other_summary"])

        df.insert(0, "model", MODEL)
        for k, v in cols.items():
            df[k] = v
        df.to_csv(out_path, index=False)   # save per transcript -> partial progress is never lost


if __name__ == "__main__":
    run()
