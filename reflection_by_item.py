"""
reflection_by_item.py

Like reflection_by_goal.py, but aggregates at the ITEM-CODE level (e.g. G4.4), so RQ2 can
measure, within each goal, HOW MANY distinct items were reflected on (a content/depth measure
that discriminates across goals, unlike goal-level 0/1 coverage which saturates).

Output: processed_annotations/reflection_by_item.csv
  session, code, goal, n_reflect, n_positive, n_negative, n_neutral, n_mixed

Reuses validation_analysis helpers + the same sibling-aware/global copy dedup.
Run:  python reflection_by_item.py
"""
import os
import re
import pandas as pd

import validation_analysis as va   # _list_tables, _read_table, transcript_key, normalize

INPUT_FOLDERS = [
    "ablation_outputs_contextual/validation",
    "production_outputs_contextual_rerun_development/development",
    "production_outputs_contextual/production",
]
MODEL_TAG = "mistral"
OUT = "processed_annotations/reflection_by_item.csv"


def _is_copy(name):
    return bool(re.search(r"(?<![a-z])copy(?![a-z])", name, re.IGNORECASE))


def _mistral_files(folder):
    if not os.path.isdir(folder):
        return []
    return [f for f in va._list_tables(folder) if MODEL_TAG in os.path.basename(f).lower()]


def run():
    # collect + global dedup by session (same logic as reflection_by_goal.py)
    all_files = []
    for folder in INPUT_FOLDERS:
        fs = _mistral_files(folder)
        print(f"{folder}: {len(fs)} mistral files")
        all_files += fs
    by_session = {}
    for fp in all_files:
        by_session.setdefault(va.transcript_key(fp), []).append(fp)
    files = []
    for s, fps in by_session.items():
        noncopy = [f for f in fps if not _is_copy(os.path.basename(f))]
        files.append((noncopy or fps)[0])

    per_utt = []   # one row per (session, utterance, item-code)
    for fp in files:
        session = va.transcript_key(fp)
        df = va.normalize(va._read_table(fp))   # n_codes = set of item codes (e.g. {"G4.4"})
        for _, r in df.iterrows():
            for code in r["n_codes"]:
                per_utt.append({"session": session, "code": code, "valence": r["n_valence"]})

    if not per_utt:
        print("No reflective item codes found - check INPUT_FOLDERS / MODEL_TAG.")
        return

    long = pd.DataFrame(per_utt)
    long["goal"] = long["code"].str.split(".").str[0]   # G4.4 -> G4 ; OTHER -> OTHER

    agg = long.groupby(["session", "code", "goal"]).size().rename("n_reflect").reset_index()
    val = (long.pivot_table(index=["session", "code"], columns="valence",
                            aggfunc="size", fill_value=0).reset_index())
    val.columns = [c if c in ("session", "code") else f"n_{c}" for c in val.columns]

    out = agg.merge(val, on=["session", "code"], how="left")
    for v in ("n_positive", "n_negative", "n_neutral", "n_mixed"):
        if v not in out.columns:
            out[v] = 0
    out = out[["session", "code", "goal", "n_reflect",
               "n_positive", "n_negative", "n_neutral", "n_mixed"]]
    out = out.sort_values(["session", "code"]).reset_index(drop=True)

    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    out.to_csv(OUT, index=False)
    print(f"\nwrote {len(out)} (session x code) rows across {out['session'].nunique()} sessions "
          f"-> {OUT}")
    print(out.head(12).to_string(index=False))


if __name__ == "__main__":
    run()
