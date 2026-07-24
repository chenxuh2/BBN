"""
assign_sets.py

List the session files in a folder, assign them to annotators with a shared
IRR overlap set, and write an assignment sheet collaborators can look up.

Design (defaults): all annotators code the SHARED set (for inter-rater
agreement); the rest is split evenly (round-robin), each session covered once.
Assignment is reproducible (fixed SEED).

    per-person load  =  N_SHARED + (n_sessions - N_SHARED) / n_annotators

Run:  python assign_sets.py     (only reads file NAMES, not file contents)
"""

import os
import glob
import csv
import random

# ============================ SET THESE ============================
FOLDER = "~/Downloads/BBN_Delft_2025/debriefing_sets/validation"   # folder with the session files
FILE_GLOB = "*.csv"                       # "*.csv" or "*.xlsx" — only the base name is used
ANNOTATORS = ["VP", "SN", "CH"]              # put real names/initials here
N_SHARED = 7                              # sessions coded by ALL annotators (IRR overlap)
SEED = 42                                 # fixed -> same assignment every run
OUTPUT = "assignment_sheet.csv"           # written in the current directory
# ==================================================================


def main():
    folder = os.path.expanduser(FOLDER)
    sessions = sorted({os.path.splitext(os.path.basename(p))[0]
                       for p in glob.glob(os.path.join(folder, FILE_GLOB))})
    n = len(sessions)
    if n == 0:
        print(f"No files matched {os.path.join(folder, FILE_GLOB)}")
        return
    if N_SHARED > n:
        print(f"N_SHARED ({N_SHARED}) > number of sessions ({n}).")
        return

    rng = random.Random(SEED)
    order = sessions[:]
    rng.shuffle(order)

    shared = set(order[:N_SHARED])            # coded by everyone (IRR)
    rest = order[N_SHARED:]                   # split uniquely, round-robin

    coders = {s: set() for s in sessions}
    for s in shared:
        coders[s].update(ANNOTATORS)
    for i, s in enumerate(rest):
        coders[s].add(ANNOTATORS[i % len(ANNOTATORS)])

    # --- matrix sheet: one row per session, a column per annotator ---
    with open(OUTPUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["session", "IRR_shared"] + ANNOTATORS + ["n_coders"])
        for s in sessions:                    # sorted order for easy lookup
            row = [s, "yes" if s in shared else ""]
            row += ["X" if a in coders[s] else "" for a in ANNOTATORS]
            row += [len(coders[s])]
            w.writerow(row)

    # --- console summary ---
    print(f"{n} sessions | {len(ANNOTATORS)} annotators | {N_SHARED} shared (IRR)")
    for a in ANNOTATORS:
        mine = sorted(s for s in sessions if a in coders[s])
        n_shared_mine = sum(1 for s in mine if s in shared)
        print(f"  {a}: {len(mine)} sessions ({n_shared_mine} shared + {len(mine) - n_shared_mine} unique)")
    print(f"Wrote {OUTPUT}")


if __name__ == "__main__":
    main()
