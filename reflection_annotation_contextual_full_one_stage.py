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
import pandas as pd

import reflection_common as rc

INPUT_FOLDER = os.environ.get("REFLECTION_INPUT", "processed_csvs")
OUTPUT_FOLDER = "ablation_outputs_contextual"
CONFIG_NAME = "contextual_full_one_stage"


def classify(prev2_role, prev2_text, prev1_role, prev1_text,
             cur_role, cur_text, next_role, next_text):
    if pd.isna(cur_text) or str(cur_text).strip() == "":
        return dict(is_reflection=False, goals="None", actions="None",
                    valence="None", level="None", other_summary="None")

    prompt = f"""
You are an expert qualitative researcher analyzing a medical debriefing transcript after a
BREAKING BAD NEWS simulation. Speakers are now reflecting on how it went.

TAXONOMY OF GOALS AND ACTIONS:
{json.dumps(rc.TAXONOMY, indent=2)}

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
4. GOAL FIRST, THEN ACTION:
   - "reflected_goals" MUST use ONLY: {json.dumps(rc.GOAL_NAMES)} (never invent a goal;
     "offering support"/"answering questions" -> Goal 5).
   - "reflected_actions": pick the matching item under that goal, or "Other".
5. When "Other" is used, write a 3-7 word "other_summary".
6. "valence" = ONE of {json.dumps(rc.VALENCE_VALUES)} (or "None" if not a reflection):
{rc.VALENCE_DEFS}
7. "level" = ONE of {json.dumps(rc.LEVEL_VALUES)} (CODE THE HIGHEST THAT APPLIES; "None" if not a reflection):
{rc.levels_block()}

OUTPUT (Strict JSON):
{{
    "is_reflection": true or false,
    "reflected_goals": ["..."],
    "reflected_actions": ["..."],
    "valence": "positive|negative|neutral|mixed|None",
    "level": "R0|R1|R2|R3|R4|None",
    "other_summary": "short summary if Other else empty"
}}
"""
    result = rc.chat_json(prompt)
    if result is None:
        return dict(is_reflection="Error", goals="Error", actions="Error",
                    valence="Error", level="Error", other_summary="Error")

    is_refl = bool(result.get("is_reflection", False))
    goals = result.get("reflected_goals", []) or []
    actions = result.get("reflected_actions", []) or []

    # Auto-correct hallucinated goals from the actions
    fixed_goals = list(goals)
    for a in actions:
        hit = rc.ITEM_TO_GOAL.get(rc._norm(a))
        if hit and hit[0] not in fixed_goals:
            fixed_goals = [hit[0]] if not goals else fixed_goals
    goals = fixed_goals or goals

    clean_goals = ", ".join(re.sub(r'[^\w\s]', '', g) for g in goals) if goals else "None"
    clean_actions = ", ".join(re.sub(r'[^\w\s]', '', a) for a in actions) if actions else "None"

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
    os.makedirs(OUTPUT_FOLDER, exist_ok=True)
    files = glob.glob(os.path.join(INPUT_FOLDER, "*_DEBRIEF*")) \
        or sorted(glob.glob(os.path.join(INPUT_FOLDER, "*.csv"))
                  + glob.glob(os.path.join(INPUT_FOLDER, "*.tsv")))
    print(f"[{CONFIG_NAME}] model={rc.MODEL}  files={len(files)}  in={INPUT_FOLDER}")

    for fp in files:
        base = os.path.basename(fp)
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
        for i in tqdm(range(len(df)), total=len(df)):
            res = classify(p2r.iloc[i], p2t.iloc[i], p1r.iloc[i], p1t.iloc[i],
                           roles.iloc[i], texts.iloc[i], nr.iloc[i], nt.iloc[i])
            cols["Is_Reflection"].append(res["is_reflection"])
            cols["Reflected_Goals"].append(res["goals"])
            cols["Reflected_Actions"].append(res["actions"])
            cols["Valence"].append(res["valence"])
            cols["Level"].append(res["level"])
            cols["Other_Summary"].append(res["other_summary"])

        for k, v in cols.items():
            df[k] = v

        safe_model = rc.MODEL.replace(":", "-").replace("/", "-")
        save = f"{CONFIG_NAME}__{safe_model}__{base.split('.')[0]}.csv"
        df.to_csv(os.path.join(OUTPUT_FOLDER, save), index=False)


if __name__ == "__main__":
    run()
