import glob
import os
import random
import shutil
import pandas as pd


# --- CONFIGURATION ---
# Reads the already-cut debriefing files and randomly assigns each SESSION to one
# of three sets (per the README's debriefing LLM annotation workflow):
#   development -> prompt/codebook development
#   validation  -> locked validation set
#   production  -> all remaining sessions
#
# Splitting is by session (one debriefing file == one session), so language from
# the same session never leaks across sets.
OUTPUT_FOLDER = "/mnt/c/Users/stelh/Downloads/processed_anonymized_csvs"
DEBRIEFING_FOLDER = os.path.join(OUTPUT_FOLDER, "debriefing")

# Where the per-set copies and the assignment table are written.
SET_ROOT = os.path.join(OUTPUT_FOLDER, "debriefing_sets")
ASSIGNMENT_CSV = os.path.join(OUTPUT_FOLDER, "debriefing_set_assignments.csv")

# Set sizes, by session. Everything left over goes to the production set.
DEV_SIZE = 12          # prompt/codebook development
VAL_SIZE = 25          # locked validation

# Fixed seed -> the split is reproducible: re-running gives the SAME assignment,
# which keeps the locked validation set stable. Change it only to draw a new split.
RANDOM_SEED = 42

SETS = ("development", "validation", "production")


def session_name_from_file(path):
    """Session id = debriefing file name without the _DEBRIEFING.csv suffix."""
    base = os.path.basename(path)
    suffix = "_DEBRIEFING.csv"
    return base[:-len(suffix)] if base.endswith(suffix) else os.path.splitext(base)[0]


def main():
    files = sorted(glob.glob(os.path.join(DEBRIEFING_FOLDER, "*_DEBRIEFING.csv")))
    total = len(files)
    print(f"Found {total} debriefing files in '{DEBRIEFING_FOLDER}'.")

    if total < DEV_SIZE + VAL_SIZE:
        raise SystemExit(
            f"Need at least {DEV_SIZE + VAL_SIZE} sessions "
            f"(dev {DEV_SIZE} + val {VAL_SIZE}) but found only {total}."
        )

    # Shuffle a copy deterministically, then slice into the three sets.
    shuffled = files[:]
    random.Random(RANDOM_SEED).shuffle(shuffled)

    assignments = []
    for i, path in enumerate(shuffled):
        if i < DEV_SIZE:
            set_name = "development"
        elif i < DEV_SIZE + VAL_SIZE:
            set_name = "validation"
        else:
            set_name = "production"
        assignments.append({
            "session": session_name_from_file(path),
            "set": set_name,
            "source_file": os.path.basename(path),
        })

    # Make the three set folders and copy each debriefing file into its set.
    # copy2 is a whole-file copy: the CSV contents are never opened or parsed here.
    for set_name in SETS:
        os.makedirs(os.path.join(SET_ROOT, set_name), exist_ok=True)
    for row, path in zip(assignments, shuffled):
        dest = os.path.join(SET_ROOT, row["set"], os.path.basename(path))
        shutil.copy2(path, dest)

    # Write the assignment table, sorted by session for easy reading.
    df = pd.DataFrame(assignments).sort_values("session").reset_index(drop=True)
    df.to_csv(ASSIGNMENT_CSV, index=False)

    counts = {s: sum(1 for a in assignments if a["set"] == s) for s in SETS}
    print(f"Saved assignments: {ASSIGNMENT_CSV}")
    print(f"Copied debriefing files into: {os.path.join(SET_ROOT, '(' + '|'.join(SETS) + ')')}")
    print(f"Set sizes: {counts}")


if __name__ == "__main__":
    main()
