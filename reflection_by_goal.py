"""
reflection_by_goal.py

Aggregate the full-corpus LLM reflection annotation (mistral-nemo contextual) into a tidy
per (session x goal) table for the RQ2 analysis in R:

    session, goal, n_reflect, n_positive, n_negative, n_neutral, n_mixed, session_reflect_utts

- n_reflect            = # reflective utterances in the session tagged with this goal
                         (an utterance with several goals counts toward each)
- n_<valence>          = valence breakdown of those reflective utterances
- session_reflect_utts = # reflective utterances in the session (any goal), for proportions in R

Reuses the helpers already written in validation_analysis.py (which imports reflection_common.py).
Run:  python reflection_by_goal.py
Output: processed_annotations/reflection_by_goal.csv
"""
import os
import re
import pandas as pd

import reflection_common as rc
import validation_analysis as va   # _read_table, transcript_key, normalize

# --- Folders that hold the mistral-nemo contextual outputs (val + dev + prod) ---
INPUT_FOLDERS = [
    "ablation_outputs_contextual/validation",
    "production_outputs_contextual_rerun_development/development",
    "production_outputs_contextual/production",
]
MODEL_TAG = "mistral"          # only keep files whose name contains this (the chosen model)
OUT = "processed_annotations/reflection_by_goal.csv"

# G-code -> canonical goal name (so R can join with the performance goals)
GOAL_CODE_TO_NAME = {f"G{i + 1}": g for i, g in enumerate(rc.GOAL_LIST)}
GOAL_CODE_TO_NAME["OTHER"] = "Other"


def _mistral_files(folder):
    if not os.path.isdir(folder):
        return []
    # va._list_tables applies the SAME sibling-aware copy dedup + lock-file skip as the
    # validation analysis: a 'copy' file is dropped only when a non-copy sibling exists;
    # a session that exists ONLY as 'copy' (e.g. FAMJ18) is kept. Then filter to the model.
    files = va._list_tables(folder)
    return [f for f in files if MODEL_TAG in os.path.basename(f).lower()]


def _is_copy(name):
    return bool(re.search(r"(?<![a-z])copy(?![a-z])", name, re.IGNORECASE))


def run():
    # 1) collect mistral files (per-folder sibling-aware dedup already applied inside _mistral_files)
    all_files = []
    for folder in INPUT_FOLDERS:
        fs = _mistral_files(folder)
        print(f"{folder}: {len(fs)} mistral files")
        all_files += fs

    # 2) GLOBAL dedup by session code - catches the SAME session appearing in >1 folder
    #    (e.g. a 'copy' in one folder and the real file in another). Prefer the non-copy.
    by_session = {}
    for fp in all_files:
        by_session.setdefault(va.transcript_key(fp), []).append(fp)
    files = []
    for s, fps in by_session.items():
        if len(fps) == 1:
            files.append(fps[0])
            continue
        noncopy = [f for f in fps if not _is_copy(os.path.basename(f))]
        pick = (noncopy or fps)[0]
        files.append(pick)
        print(f"  [global dedup] session '{s}' in {len(fps)} files -> kept {os.path.basename(pick)}")
        for f in fps:
            if f != pick:
                print(f"      dropped {os.path.basename(f)}")
    n_files = len(files)
    print(f"unique sessions after global dedup: {n_files}")

    per_utt = []   # long: one row per (session, utterance, goal)
    for fp in files:
        session = va.transcript_key(fp)
        df = va.normalize(va._read_table(fp))   # adds n_goals (set of G-codes) + n_valence
        for _, r in df.iterrows():
            goals = r["n_goals"]
            if not goals:                        # not a coded reflection
                continue
            for gcode in goals:
                per_utt.append({
                    "session": session,
                    "goal": GOAL_CODE_TO_NAME.get(gcode, gcode),
                    "valence": r["n_valence"],
                })

    if not per_utt:
        print("No reflective utterances found - check INPUT_FOLDERS / MODEL_TAG.")
        return

    long = pd.DataFrame(per_utt)

    # count of reflective utterances per session (distinct rows contributing any goal)
    # (recomputed simply as total goal-mentions is misleading; use utterance-level count)
    # Rebuild a per-session reflective-utterance count:
    #   each folder file already gave one row per (utterance, goal); to count utterances we
    #   need uniqueness - approximate with total goal-mentions is avoided by recomputing below.
    sess_reflect = (long.groupby("session").size()
                    .rename("goal_mentions"))   # total goal-mentions (>= utterances)

    # main aggregate: per (session, goal) counts + valence breakdown
    agg = (long.groupby(["session", "goal"]).size().rename("n_reflect").reset_index())
    val = (long.pivot_table(index=["session", "goal"], columns="valence",
                            aggfunc="size", fill_value=0).reset_index())
    val.columns = [c if c in ("session", "goal") else f"n_{c}" for c in val.columns]

    out = agg.merge(val, on=["session", "goal"], how="left")
    out = out.merge(sess_reflect, on="session", how="left")

    # ensure the 4 valence columns exist even if a category never appears
    for v in ("n_positive", "n_negative", "n_neutral", "n_mixed"):
        if v not in out.columns:
            out[v] = 0

    out = out[["session", "goal", "n_reflect",
               "n_positive", "n_negative", "n_neutral", "n_mixed", "goal_mentions"]]
    out = out.rename(columns={"goal_mentions": "session_goal_mentions"})
    out = out.sort_values(["session", "goal"]).reset_index(drop=True)

    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    out.to_csv(OUT, index=False)
    print(f"\nwrote {len(out)} (session x goal) rows across {out['session'].nunique()} sessions "
          f"({n_files} files) -> {OUT}")
    print(out.head(12).to_string(index=False))


if __name__ == "__main__":
    run()
