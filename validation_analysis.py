"""
validation_analysis.py

Compare annotations on the VALIDATION set:
  - 3 HUMAN annotators (each in their own folder; coded per the contextual, utterance-level instruction)
  - LLM annotations (produced by the reflection_annotation_* pipeline)

Built step by step. Fill in the local paths below; they stay on your machine.
Run:  python validation_analysis.py
"""
import os
import glob
import pandas as pd

import reflection_common as rc  # shared taxonomy / valence / level definitions (for alignment)

# ============================================================================
# >>> FILL IN YOUR PATHS <<<  (local Downloads paths; leave "" until ready)
# ----------------------------------------------------------------------------
# One folder per human annotator. Each folder holds that annotator's coded
# validation transcripts (utterance-level, with the codebook columns).
HUMAN_ANNOTATOR_DIRS = {
    "annotator_1": "",   # e.g. /mnt/c/Users/stelh/Downloads/.../validation_ann1
    "annotator_2": "",
    "annotator_3": "",
}

# LLM outputs already written by the pipeline for the validation set.
LLM_SUMMARY_DIR = "ablation_outputs/validation"              # summary configs -> *__compare.csv
LLM_CONTEXTUAL_DIR = "ablation_outputs_contextual/validation"  # contextual -> per-file CSVs
# ============================================================================


def _list_csvs(folder):
    if not folder:
        return []
    return sorted(glob.glob(os.path.join(folder, "*.csv")) + glob.glob(os.path.join(folder, "*.tsv")))


def check_paths():
    """STEP 0 - verify each configured path exists and show what's inside
    (file counts + the columns of the first file), so we confirm the schema
    before writing any analysis. Prints metadata only."""
    print("=== HUMAN annotator folders ===")
    for name, folder in HUMAN_ANNOTATOR_DIRS.items():
        if not folder:
            print(f"  {name}: (not set)")
            continue
        files = _list_csvs(folder)
        exists = os.path.isdir(folder)
        print(f"  {name}: exists={exists}  files={len(files)}  path={folder}")
        if files:
            try:
                cols = list(pd.read_csv(files[0], nrows=0).columns)
                print(f"      first file: {os.path.basename(files[0])}  columns={cols}")
            except Exception as e:
                print(f"      (could not read columns: {e})")

    print("=== LLM output folders ===")
    for label, folder in [("summary", LLM_SUMMARY_DIR), ("contextual", LLM_CONTEXTUAL_DIR)]:
        files = _list_csvs(folder)
        print(f"  {label}: exists={os.path.isdir(folder)}  files={len(files)}  path={folder}")
        if files:
            cols = list(pd.read_csv(files[0], nrows=0).columns)
            print(f"      first file: {os.path.basename(files[0])}  columns={cols}")


# --- steps to build next (stubs) ---
def load_human():
    """STEP 1 - load the 3 annotators into aligned per-utterance tables. TODO."""
    pass


def load_llm():
    """STEP 2 - load the LLM outputs (summary + contextual). TODO."""
    pass


def main():
    check_paths()
    # PLAN (we fill these one at a time):
    #   1. load_human()  -> 3 annotators aligned per utterance
    #   2. inter-annotator agreement (human vs human) = upper bound
    #   3. load_llm()
    #   4. human (consensus) vs LLM agreement: goal-level, item-level, valence, level
    #   5. failure / None-rate accounting and exclusions


if __name__ == "__main__":
    main()
