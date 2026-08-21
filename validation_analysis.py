"""
validation_analysis.py

Compare annotations on the VALIDATION set:
  - 3 HUMAN annotators (each in their own folder; coded per the contextual, utterance-level instruction)
  - LLM annotations (produced by the reflection_annotation_* pipeline)

Built step by step. Fill in the local paths below; they stay on your machine.
Run:  python validation_analysis.py
"""
import os
import re
import glob
import pandas as pd

import reflection_common as rc  # shared taxonomy / valence / level definitions (for alignment)

# ============================================================================
# >>> FILL IN YOUR PATHS <<<  (local Downloads paths; leave "" until ready)
# ----------------------------------------------------------------------------
# One folder per human annotator. Each folder holds that annotator's coded
# validation transcripts (utterance-level, with the codebook columns).
HUMAN_ANNOTATOR_DIRS = {
    "annotator_1": "/mnt/c/Users/stelh/Documents/BBN-PC-local/annotation_CH",    
    "annotator_2": "/mnt/c/Users/stelh/Documents/BBN-PC-local/annotations_SN",    
    "annotator_3": "/mnt/c/Users/stelh/Documents/BBN-PC-local/annotation_VP",    
}

# LLM outputs already written by the pipeline for the validation set.
LLM_SUMMARY_DIR = "ablation_outputs/validation"              # summary configs -> *__compare.csv
LLM_CONTEXTUAL_DIR = "ablation_outputs_contextual/validation"  # contextual -> per-file CSVs

# Optional: the SOURCE validation transcripts the LLM ran on (to catch upstream duplicates).
SOURCE_VALIDATION_DIR = ""   # e.g. /mnt/c/Users/.../debriefing_sets/validation
# ============================================================================


def _is_copy(name):
    """True if the filename carries a 'copy' marker: 'FAMJ18 copy_...', '- Copy', 'copy (2)'.
    (\\b fails here because '_' is a word char, so 'copy_full' has no boundary.)"""
    return bool(re.search(r"(?<![a-z])copy(?![a-z])", name, re.IGNORECASE))


def _dedup_key(path):
    """Identity of a file IGNORING the copy marker, so 'FAMJ18 copy_...' and 'FAMJ18_...'
    share a key (and thus dedup), while different models/transcripts stay distinct."""
    stem = os.path.splitext(os.path.basename(path))[0].lower()
    stem = re.sub(r"(?<![a-z])copy(?![a-z])", "", stem)   # drop the copy token
    return re.sub(r"[\s_\-]+", "_", stem).strip("_")       # normalize separators


def _list_tables(folder):
    """List .xlsx (human) / .csv / .tsv (LLM) files. Drops Excel lock files (~$...).
    Sibling-aware copy dedup: a 'copy' file is dropped ONLY when a non-copy sibling with the
    same identity exists; a session that exists ONLY as 'copy' is kept (it is not redundant)."""
    if not folder:
        return []
    files = []
    for p in ("*.xlsx", "*.csv", "*.tsv"):
        files += glob.glob(os.path.join(folder, p))
    files = [f for f in files if not os.path.basename(f).startswith("~$")]  # lock files

    groups = {}
    for f in files:
        groups.setdefault(_dedup_key(f), []).append(f)
    kept, dropped = [], []
    for g in groups.values():
        noncopy = [f for f in g if not _is_copy(os.path.basename(f))]
        if noncopy:                 # a real version exists -> keep it, drop the copy(ies)
            kept += noncopy
            dropped += [f for f in g if _is_copy(os.path.basename(f))]
        else:                       # only a copy-named version exists -> keep it
            kept += g
    if dropped:
        print(f"  [dropped {len(dropped)} redundant copy(ies) in {os.path.basename(folder)}]: "
              f"{[os.path.basename(f) for f in dropped]}")
    return sorted(kept)


def _read_table(path, nrows=None):
    """Read a table by extension: xlsx -> read_excel (first sheet), else read_csv/tsv."""
    ext = os.path.splitext(path)[1].lower()
    if ext == ".xlsx":
        return pd.read_excel(path, nrows=nrows)         # needs openpyxl
    sep = "\t" if ext == ".tsv" else ","
    return pd.read_csv(path, sep=sep, nrows=nrows)


def audit_files():
    """List every folder's TOTAL files and which are 'copy' duplicates / lock files, so
    duplicates can be removed BEFORE analysis (especially if they live in the source set)."""
    folders = {f"human/{name}": f for name, f in HUMAN_ANNOTATOR_DIRS.items()}
    folders["llm/summary"] = LLM_SUMMARY_DIR
    folders["llm/contextual"] = LLM_CONTEXTUAL_DIR
    if SOURCE_VALIDATION_DIR:
        folders["source/validation"] = SOURCE_VALIDATION_DIR

    print("=== FILE AUDIT (total files + copy/lock duplicates per folder) ===")
    grand = 0
    for label, folder in folders.items():
        if not folder or not os.path.isdir(folder):
            print(f"  {label:20} : (not set / missing) {folder}")
            continue
        all_files = []
        for p in ("*.xlsx", "*.csv", "*.tsv"):
            all_files += glob.glob(os.path.join(folder, p))
        grand += len(all_files)
        names = [os.path.basename(f) for f in all_files]
        copies = [n for n in names if re.search(r"(?<![a-z])copy(?![a-z])", n, re.IGNORECASE)]
        locks = [n for n in names if n.startswith("~$")]
        print(f"  {label:20} : total={len(all_files)}  copies={len(copies)}  locks={len(locks)}")
        if copies:
            print(f"      COPY -> {copies}")
        if locks:
            print(f"      LOCK -> {locks}")
    print(f"  GRAND TOTAL files across folders: {grand}")


def check_paths():
    """STEP 0 - verify each configured path exists and show what's inside
    (file counts + the columns of the first file), so we confirm the schema
    before writing any analysis. Prints metadata only."""
    print("=== HUMAN annotator folders ===")
    for name, folder in HUMAN_ANNOTATOR_DIRS.items():
        if not folder:
            print(f"  {name}: (not set)")
            continue
        files = _list_tables(folder)
        exists = os.path.isdir(folder)
        print(f"  {name}: exists={exists}  files={len(files)}  path={folder}")
        if files:
            try:
                cols = list(_read_table(files[0], nrows=0).columns)
                print(f"      first file: {os.path.basename(files[0])}  columns={cols}")
            except Exception as e:
                print(f"      (could not read columns: {e})")

    print("=== LLM output folders ===")
    for label, folder in [("summary", LLM_SUMMARY_DIR), ("contextual", LLM_CONTEXTUAL_DIR)]:
        files = _list_tables(folder)
        print(f"  {label}: exists={os.path.isdir(folder)}  files={len(files)}  path={folder}")
        if files:
            cols = list(_read_table(files[0], nrows=0).columns)
            print(f"      first file: {os.path.basename(files[0])}  columns={cols}")


def _annotator_suffix(folder):
    """Derive the annotator code from the folder name: 'annotation_CH' -> 'CH'."""
    return os.path.basename(str(folder).rstrip("/\\")).split("_")[-1].upper()


def transcript_key(path, strip_suffix=None):
    """Normalize a filename to a session key that matches across human & LLM files.
    - LLM files 'contextual_..__model__DBCH18_..._DEBRIEFING.csv' -> keep part after last '__'.
    - Human files carry an annotator suffix (e.g. 'NAME_CH.xlsx'); strip it so the same
      transcript coded by different annotators collapses to one key."""
    stem = os.path.splitext(os.path.basename(path))[0]
    if "__" in stem:
        stem = stem.split("__")[-1]
    stem = stem.strip()
    if strip_suffix:
        for sep in ("_", "-", " "):
            tail = f"{sep}{strip_suffix}".lower()
            if stem.lower().endswith(tail):
                stem = stem[: -len(tail)]
                break
    stem = stem.strip().lower()
    # Collapse to the leading SESSION CODE (letters+digits) so naming variants like
    # 'jjfw17_sh_...' and 'jjfw17_...' map to the same key. Fall back to full stem.
    # NOTE: letters-only codes (e.g. 'cnip') fall through to the full stem here; that odd
    # naming is reconciled at the session<->Team ID join step, not in the keying.
    m = re.match(r"^[a-z]+\d+", stem)
    return m.group(0) if m else stem


HUMAN_LABEL_COLS = ["is_reflection", "goal_action", "level", "valence"]


def load_human():
    """STEP 1 - load all 3 annotators into ONE long per-utterance table and print an
    inventory + the label vocabulary (so we can confirm formats before aligning)."""
    frames = []
    for annotator, folder in HUMAN_ANNOTATOR_DIRS.items():
        suffix = _annotator_suffix(folder)   # CH / SN / VP, stripped from the transcript key
        files = _list_tables(folder)
        for fp in files:
            df = _read_table(fp)
            df.columns = [str(c).strip() for c in df.columns]
            df["annotator"] = annotator
            df["transcript"] = transcript_key(fp, strip_suffix=suffix)
            frames.append(df)

    if not frames:
        print("load_human: no files found - check HUMAN_ANNOTATOR_DIRS.")
        return None

    human = pd.concat(frames, ignore_index=True)

    # --- inventory ---
    print("\n=== HUMAN inventory ===")
    for annotator in HUMAN_ANNOTATOR_DIRS:
        sub = human[human["annotator"] == annotator]
        n_tx = sub["transcript"].nunique()
        print(f"  {annotator}: {n_tx} transcripts, {len(sub)} rows")
    print(f"  TOTAL transcripts (unique keys): {human['transcript'].nunique()}")
    # transcripts each annotator covers (to check they overlap)
    per = {a: set(human[human['annotator'] == a]['transcript']) for a in HUMAN_ANNOTATOR_DIRS}
    common = set.intersection(*per.values()) if all(per.values()) else set()
    print(f"  transcripts annotated by ALL 3: {len(common)}")

    # --- label vocabulary (small controlled sets; helps align to the LLM output) ---
    print("\n=== HUMAN label vocabulary ===")
    for col in HUMAN_LABEL_COLS:
        if col not in human.columns:
            print(f"  {col}: (column missing)")
            continue
        vals = human[col].dropna().astype(str).str.strip()
        vals = vals[vals != ""]
        uniq = sorted(vals.unique())
        if col == "goal_action" and len(uniq) > 25:
            print(f"  {col}: {len(uniq)} unique, e.g. {uniq[:25]}")
        else:
            print(f"  {col}: {uniq}")

    return human


# ============================================================================
# LABEL NORMALIZATION - map human & LLM labels into one shared space, per utterance:
#   is_reflection -> bool | None
#   goal_action   -> frozenset of codes, e.g. {"G1.7","G2.1"}   (+ goal-level {"G1","G2"})
#   valence       -> one of positive/negative/neutral/mixed | None
#   level         -> one of R0..R4 | None
# ============================================================================
VALENCE_SET = set(rc.VALENCE_VALUES)   # positive / negative / neutral / mixed
LEVEL_SET = set(rc.LEVEL_VALUES)       # R0 .. R4
_BLANK = {"", "nan", "none", "no", "n/a", "na"}


def norm_is_reflection(v):
    s = str(v).strip().lower()
    if s in ("yes", "true", "1", "y", "t"):
        return True
    if s in ("no", "false", "0", "n", "f"):
        return False
    return None


def _norm_code_token(tok):
    """'g1-7' / 'G1.7' / 'g1 7' -> 'G1.7'.  'other' -> 'OTHER'."""
    return str(tok).strip().upper().replace(" ", "").replace("-", ".")


def norm_codes(value):
    """'g1-7, g2-1' -> frozenset({'G1.7','G2.1'}); blanks -> empty set."""
    if value is None:
        return frozenset()
    s = str(value).strip()
    if s.lower() in _BLANK:
        return frozenset()
    codes = {_norm_code_token(t) for t in re.split(r"[,;/|]+", s)}
    return frozenset(c for c in codes if c)


def goals_from_codes(codes):
    """{'G1.7','G2.1'} -> {'G1','G2'};  'OTHER' stays 'OTHER'."""
    return frozenset(c.split(".")[0] for c in codes)


def norm_valence(v):
    s = str(v).strip().lower()
    return s if s in VALENCE_SET else None


def norm_level(v):
    s = str(v).strip().upper()[:2]
    return s if s in LEVEL_SET else None


# Inverse maps for the LLM CONTEXTUAL output, which stored goal NAMES + item TEXT (not codes).
# We map that text back to the code space so it aligns with the human 'g1-1' codes.
_GOALITEM_TO_CODE = {}   # (goal name, item text) -> code
_ITEMTEXT_TO_CODE = {}   # normalized item text -> code (unique for non-"Other" items)
for _code, (_g, _it) in rc.CODE_TO_GOAL_ITEM.items():
    _GOALITEM_TO_CODE[(_g, _it)] = _code
    if _it != "Other":
        _ITEMTEXT_TO_CODE[rc._norm(_it)] = _code


def codes_from_llm(goals_str, actions_str):
    """LLM contextual stored 'Reflected_Goals' (names) and 'Reflected_Actions' (item text),
    both '|'-joined in the same order. Zip them and map each (goal,item) back to a code."""
    def split(s):
        return [] if (s is None or str(s).lower() in _BLANK) else [x.strip() for x in str(s).split("|")]
    goals, items = split(goals_str), split(actions_str)
    codes = set()
    for g, it in zip(goals, items):
        if str(it).lower() in _BLANK:
            continue
        code = _GOALITEM_TO_CODE.get((g, it)) or _ITEMTEXT_TO_CODE.get(rc._norm(it))
        if code is None and it == "Other":
            code = _GOALITEM_TO_CODE.get((g, "Other"))   # goal-level Other -> G#.OTHER
        if code:
            codes.add(code)
    return frozenset(codes)


def normalize(df):
    """Add normalized columns (n_*) to a per-utterance table (human or LLM contextual).
    Human schema uses codes in 'goal_action'; LLM schema uses text in Reflected_Goals/Actions."""
    def col(*names):
        for n in names:
            if n in df.columns:
                return df[n]
        return pd.Series([None] * len(df))

    df = df.copy()
    df["n_is_reflection"] = col("is_reflection", "Is_Reflection").map(norm_is_reflection)
    if "goal_action" in df.columns:                         # HUMAN: codes like 'g1-1, g2-1'
        df["n_codes"] = df["goal_action"].map(norm_codes)
    else:                                                    # LLM contextual: text -> codes
        g, a = col("Reflected_Goals"), col("Reflected_Actions")
        df["n_codes"] = [codes_from_llm(gg, aa) for gg, aa in zip(g, a)]
    df["n_goals"] = df["n_codes"].map(goals_from_codes)
    df["n_valence"] = col("valence", "Valence").map(norm_valence)
    df["n_level"] = col("level", "Level").map(norm_level)
    return df


def show_normalized(df, title):
    """Print the normalized labels for inspection (labels only, no free text)."""
    print(f"\n=== NORMALIZED: {title} ===")
    n_refl = df["n_is_reflection"]
    print(f"  rows={len(df)}  is_reflection: Yes={int((n_refl == True).sum())} "
          f"No={int((n_refl == False).sum())} blank={int(n_refl.isna().sum())}")

    # code-validity check: any normalized tokens that are NOT known taxonomy codes?
    all_codes = set().union(*df["n_codes"]) if len(df) else set()
    valid = set(rc.CODE_TO_GOAL_ITEM)
    unknown = sorted(all_codes - valid)
    print(f"  distinct codes used: {len(all_codes)}  |  UNKNOWN (not in taxonomy): {unknown}")

    print(f"  valence counts: {df['n_valence'].value_counts(dropna=False).to_dict()}")
    print(f"  level counts:   {df['n_level'].value_counts(dropna=False).to_dict()}")

    # a few reflective examples: original -> normalized (codes/level/valence only)
    src_ga = "goal_action" if "goal_action" in df.columns else "Reflected_Actions"
    sample = df[df["n_codes"].map(len) > 0].head(8)
    print("  examples (orig goal_action -> n_codes | n_level | n_valence):")
    for _, r in sample.iterrows():
        print(f"    {str(r.get(src_ga)):<22} -> {sorted(r['n_codes'])} | {r['n_level']} | {r['n_valence']}")


# ============================================================================
# AGGREGATION - distill a per-utterance normalized table into ONE ROW PER
# (annotator/model, transcript, code): the "processed" representation.
# ============================================================================
PROCESSED_DIR = "processed_annotations"


def level_max(levels):
    """Highest R-level across occurrences (R0<R1<...<R4). None if none."""
    ls = [l for l in levels if l]
    return max(ls, key=lambda x: int(x[1:])) if ls else None


def valence_mixed(vals):
    """Rule A: any positive+negative together -> 'mixed'; neutral yields to polarity."""
    vs = {v for v in vals if v}
    if not vs:
        return None
    if "mixed" in vs:
        return "mixed"
    pol = vs & {"positive", "negative"}
    if pol == {"positive", "negative"}:
        return "mixed"
    if pol == {"positive"}:
        return "positive"
    if pol == {"negative"}:
        return "negative"
    return "neutral"  # only neutral present


def valence_majority(vals):
    """Rule B: most frequent valence; a tie (no clear majority) -> 'mixed'."""
    vals = [v for v in vals if v]
    if not vals:
        return None
    counts = pd.Series(vals).value_counts()
    top = counts[counts == counts.max()].index.tolist()
    return top[0] if len(top) == 1 else "mixed"


def aggregate_by_code(dfn, keys=("annotator", "transcript")):
    """Explode codes, then one row per (keys..., code) with count/level_max/valence rules."""
    keys = list(keys)
    ex = dfn.copy()
    ex["code"] = ex["n_codes"].map(lambda s: list(s) if s else [])
    ex = ex.explode("code")
    ex = ex[ex["code"].notna() & (ex["code"] != "")]
    if ex.empty:
        return pd.DataFrame(columns=keys + ["goal", "code", "count", "level_max",
                                            "valence_mixed", "valence_majority"])
    out = (ex.groupby(keys + ["code"])
             .agg(count=("code", "size"),
                  level_max=("n_level", lambda s: level_max(list(s))),
                  valence_mixed=("n_valence", lambda s: valence_mixed(list(s))),
                  valence_majority=("n_valence", lambda s: valence_majority(list(s))))
             .reset_index())
    out["goal"] = out["code"].str.split(".").str[0]
    return out[keys + ["goal", "code", "count", "level_max", "valence_mixed", "valence_majority"]]


def build_human_processed(human_norm):
    """Aggregate the normalized human table and write the processed CSV."""
    proc = aggregate_by_code(human_norm, keys=("annotator", "transcript"))
    os.makedirs(PROCESSED_DIR, exist_ok=True)
    path = os.path.join(PROCESSED_DIR, "human_by_code.csv")
    proc.to_csv(path, index=False)

    print(f"\n=== HUMAN processed (per annotator x transcript x code) ===")
    print(f"  {len(proc)} rows -> {path}")
    print(f"  where mixed != majority: {int((proc['valence_mixed'] != proc['valence_majority']).sum())} rows")
    print("  sample:")
    print(proc.head(10).to_string(index=False))
    return proc


# ============================================================================
# INTER-ANNOTATOR AGREEMENT (IAA) - the human "ceiling".
# Computed on the 7 triple-coded transcripts. Pairwise across the 3 annotators,
# then averaged over the (up to) 3 pairs.
#   Dim 1  which codes/goals were reflected  -> set overlap (Jaccard), unconditioned
#   Dim 2  level (R0-R4, ordinal)            -> quadratic-weighted kappa, on shared codes
#   Dim 3  valence (mixed & majority rules)  -> nominal kappa, on shared codes
# ============================================================================
from itertools import combinations

try:
    from sklearn.metrics import cohen_kappa_score
except ImportError:
    cohen_kappa_score = None

ANNOTATORS = list(HUMAN_ANNOTATOR_DIRS)


def triple_coded_transcripts(proc):
    """Return the transcript keys coded by ALL 3 annotators (the IAA set)."""
    counts = proc.groupby("transcript")["annotator"].nunique()
    return counts[counts == len(ANNOTATORS)].index.tolist()


def _jaccard(a, b):
    """Set overlap |A n B| / |A u B|. Two empty sets count as full agreement."""
    a, b = set(a), set(b)
    if not a and not b:
        return 1.0
    return len(a & b) / len(a | b)


def iaa_sets(proc, transcripts, field):
    """Dim 1 - agreement on the code/goal SETS each annotator gave per transcript.
    Returns BOTH:
      - mean pairwise Jaccard: average of the 2-way |A n B|/|A u B| over all annotator pairs
      - mean 3-way Jaccard:   |A n B n C| / |A u B u C| per transcript, averaged
    field='code' (item-level) or 'goal' (G1-G5 level). Unconditioned: all codes participate."""
    sub = proc[proc["transcript"].isin(transcripts)]
    grp = sub.groupby(["transcript", "annotator"])[field].agg(frozenset)
    pair_scores, triple_scores = [], []
    for t in transcripts:
        sets = [set(grp.get((t, a), frozenset())) for a in ANNOTATORS]
        for x, y in combinations(sets, 2):          # 2-way, every pair
            pair_scores.append(_jaccard(x, y))
        inter = set.intersection(*sets) if sets else set()   # 3-way: all agree / anyone mentioned
        union = set.union(*sets) if sets else set()
        triple_scores.append(len(inter) / len(union) if union else 1.0)
    mean_pair = sum(pair_scores) / len(pair_scores) if pair_scores else float("nan")
    mean_triple = sum(triple_scores) / len(triple_scores) if triple_scores else float("nan")
    return mean_pair, len(pair_scores), mean_triple, len(triple_scores)


def _pairwise_label_agreement(wide, weights=None):
    """Given a wide table (rows = items, columns = annotators, values = a label),
    compute per-pair exact-agreement and kappa on the rows both annotators labelled
    (i.e. the pair's shared items). Returns list of (pair, n, exact, kappa)."""
    out = []
    for a, b in combinations(ANNOTATORS, 2):
        if a not in wide.columns or b not in wide.columns:
            continue
        pair = wide[[a, b]].dropna()          # <- conditioning: only items BOTH labelled
        n = len(pair)
        if n == 0:
            out.append((f"{a}|{b}", 0, float("nan"), float("nan")))
            continue
        y1, y2 = pair[a].astype(str), pair[b].astype(str)
        exact = float((y1.values == y2.values).mean())
        # kappa is undefined when there is no variance (all identical) -> report nan
        if cohen_kappa_score is None or (y1.nunique() == 1 and y2.nunique() == 1):
            kappa = float("nan")
        else:
            kappa = cohen_kappa_score(y1, y2, weights=weights)
        out.append((f"{a}|{b}", n, exact, kappa))
    return out


def iaa_per_code(proc, transcripts, field, weights=None):
    """Dim 2/3 - agreement on a per-code attribute (level_max / valence_*), CONDITIONED on
    the codes a pair both flagged. Also reports the strict 3-way intersection."""
    sub = proc[proc["transcript"].isin(transcripts)]
    # wide: one row per (transcript, code); a column per annotator holding the attribute
    wide = sub.pivot_table(index=["transcript", "code"], columns="annotator",
                           values=field, aggfunc="first")
    pairwise = _pairwise_label_agreement(wide, weights=weights)
    # strict: codes flagged by ALL 3 -> exact 3-way match rate
    cols = [a for a in ANNOTATORS if a in wide.columns]
    triple = wide[cols].dropna() if len(cols) == len(ANNOTATORS) else wide.iloc[0:0]
    triple_n = len(triple)
    triple_exact = float((triple.nunique(axis=1) == 1).mean()) if triple_n else float("nan")
    return pairwise, triple_n, triple_exact


def iaa_reflection(human_norm, transcripts):
    """Dim 4 - per-utterance is_reflection agreement, aligned by (transcript, turn)."""
    sub = human_norm[human_norm["transcript"].isin(transcripts)]
    wide = sub.pivot_table(index=["transcript", "turn"], columns="annotator",
                           values="n_is_reflection", aggfunc="first")
    return _pairwise_label_agreement(wide)   # binary, unweighted


def _fmt_pairwise(pairwise):
    """Average the pairwise (exact, kappa) for the summary line, keep per-pair detail."""
    import math
    exacts = [e for _, n, e, k in pairwise if n > 0 and not math.isnan(e)]
    kappas = [k for _, n, e, k in pairwise if n > 0 and not math.isnan(k)]
    avg_e = sum(exacts) / len(exacts) if exacts else float("nan")
    avg_k = sum(kappas) / len(kappas) if kappas else float("nan")
    ns = [n for _, n, _, _ in pairwise]
    return avg_e, avg_k, ns


def run_iaa(proc):
    """Run the IAA dimensions on the 7 triple-coded transcripts and print a summary.
    (is_reflection / per-utterance turn alignment is intentionally NOT evaluated: reflection
    boundaries are inherently subjective and the annotators' turn numbering does not line up.)"""
    seven = triple_coded_transcripts(proc)
    print(f"\n========== INTER-ANNOTATOR AGREEMENT (n={len(seven)} triple-coded transcripts) ==========")
    if cohen_kappa_score is None:
        print("  (sklearn not installed -> kappa is NaN; run: pip install scikit-learn)")
    if not seven:
        print("  no triple-coded transcripts found.")
        return

    # --- Dim 1: which codes / goals (set overlap, Jaccard) ---
    for lvl, field in [("goal", "goal"), ("code", "code")]:
        jp, npairs, jt, ntrip = iaa_sets(proc, seven, field)
        print(f"  [Dim1 sets] {lvl:4} : pairwise Jaccard = {jp:.3f} (over {npairs} pairs)"
              f"  | 3-way Jaccard = {jt:.3f} (over {ntrip} transcripts)")

    # --- Dim 2: level (ordinal, quadratic-weighted kappa) ---
    pw, tn, te = iaa_per_code(proc, seven, "level_max", weights="quadratic")
    e, k, ns = _fmt_pairwise(pw)
    print(f"  [Dim2 level]        : exact={e:.3f}  weighted-kappa={k:.3f}  shared-codes/pair={ns}"
          f"  | 3-way n={tn} exact={te:.3f}")

    # --- Dim 3: valence, both aggregation rules (nominal kappa) ---
    for rule in ["valence_mixed", "valence_majority"]:
        pw, tn, te = iaa_per_code(proc, seven, rule)
        e, k, ns = _fmt_pairwise(pw)
        print(f"  [Dim3 {rule:16}]: exact={e:.3f}  kappa={k:.3f}  shared-codes/pair={ns}"
              f"  | 3-way n={tn} exact={te:.3f}")


def load_llm_contextual():
    """STEP 4 - load LLM contextual outputs (per-utterance, one file per model x transcript),
    map their text back to codes, and aggregate to per (model, transcript, code)."""
    files = _list_tables(LLM_CONTEXTUAL_DIR)
    if not files:
        print("\nload_llm_contextual: no files in LLM_CONTEXTUAL_DIR")
        return None, None
    frames = []
    for fp in files:
        df = _read_table(fp)
        df.columns = [str(c).strip() for c in df.columns]
        df["transcript"] = transcript_key(fp)    # LLM filename -> part after last '__'
        if "model" not in df.columns:
            df["model"] = "unknown"
        frames.append(df)
    llm = normalize(pd.concat(frames, ignore_index=True))
    proc = aggregate_by_code(llm, keys=("model", "transcript"))
    proc["config"] = "contextual_full_one_stage"   # tag so it unifies with the summary configs

    os.makedirs(PROCESSED_DIR, exist_ok=True)
    path = os.path.join(PROCESSED_DIR, "llm_by_code.csv")
    proc.to_csv(path, index=False)

    print("\n=== LLM contextual processed ===")
    print(f"  {len(proc)} rows -> {path}")
    for m in sorted(proc["model"].unique()):
        sub = proc[proc["model"] == m]
        print(f"  model {m}: {sub['transcript'].nunique()} transcripts, {len(sub)} code-rows")
    return proc, llm


def load_llm_summary():
    """Load the summary configs' *__compare.csv (conversation-level, one row per reflection),
    map (goal name, item text) back to codes, and aggregate to per (config, model, transcript, code)."""
    files = _list_tables(LLM_SUMMARY_DIR)
    if not files:
        print("\nload_llm_summary: no files in LLM_SUMMARY_DIR")
        return None
    s = pd.concat([_read_table(fp) for fp in files], ignore_index=True)
    s.columns = [str(c).strip() for c in s.columns]
    s["transcript"] = s["file"].map(lambda f: transcript_key(str(f)))

    def to_code(g, it):
        g, it = str(g).strip(), str(it).strip()
        c = _GOALITEM_TO_CODE.get((g, it)) or _ITEMTEXT_TO_CODE.get(rc._norm(it))
        if c is None and it == "Other":
            c = _GOALITEM_TO_CODE.get((g, "Other"))
        return c

    s["n_codes"] = [frozenset([c]) if (c := to_code(g, i)) else frozenset()
                    for g, i in zip(s["goal"], s["item"])]
    s["n_valence"] = s["valence"].map(norm_valence) if "valence" in s.columns else None
    s["n_level"] = s["level"].map(norm_level) if "level" in s.columns else None
    proc = aggregate_by_code(s, keys=("config", "model", "transcript"))

    print("\n=== LLM summary processed ===")
    for (cfg, m), sub in proc.groupby(["config", "model"]):
        print(f"  {cfg} / {m}: {sub['transcript'].nunique()} transcripts, {len(sub)} code-rows")
    return proc


def report_key_overlap(human_proc, llm_proc):
    """Verify transcript keys line up between human and LLM (else no comparison is possible)."""
    h = set(human_proc["transcript"])
    l = set(llm_proc["transcript"])
    print("\n=== transcript-key overlap (human vs LLM) ===")
    print(f"  human keys: {len(h)}  |  LLM keys: {len(l)}  |  matched: {len(h & l)}")
    only_h, only_l = sorted(h - l), sorted(l - h)
    if only_h:
        print(f"  HUMAN-only ({len(only_h)}): {only_h[:6]}{' ...' if len(only_h) > 6 else ''}")
    if only_l:
        print(f"  LLM-only   ({len(only_l)}): {only_l[:6]}{' ...' if len(only_l) > 6 else ''}")


# ============================================================================
# STEP 5 - LLM vs HUMAN on the 7 triple-coded transcripts.
#   set P/R/F1 of the LLM code set vs 3 human gold definitions (a range):
#      union (any-1 human = lenient floor) / majority (>=2 = main) / all-3 (strict)
#   LLM as a "4th annotator": mean pairwise Jaccard vs each human, next to the human IAA
#   valence agreement vs each human on shared codes (both aggregation rules)
# ============================================================================
from collections import Counter


def _mean(xs):
    xs = [x for x in xs if x is not None]
    return sum(xs) / len(xs) if xs else float("nan")


def _ann_sets(hproc, transcripts, field):
    """(transcript, annotator) -> frozenset of codes ('code') or goals ('goal')."""
    sub = hproc[hproc["transcript"].isin(transcripts)]
    return sub.groupby(["transcript", "annotator"])[field].agg(frozenset).to_dict()


def _llm_sets(llm_proc, config, model, transcripts, field):
    """transcript -> frozenset for one (config, model)."""
    sub = llm_proc[(llm_proc["config"] == config) & (llm_proc["model"] == model)
                   & (llm_proc["transcript"].isin(transcripts))]
    return sub.groupby("transcript")[field].agg(frozenset).to_dict()


def _human_gold(hproc, transcripts, field):
    """transcript -> {'union','majority','all3'} human gold sets."""
    asets = _ann_sets(hproc, transcripts, field)
    out = {}
    for t in transcripts:
        per = [set(asets.get((t, a), frozenset())) for a in ANNOTATORS]
        cnt = Counter()
        for s in per:
            cnt.update(s)
        out[t] = {
            "union": set(cnt),                              # flagged by >=1 (lenient)
            "majority": {c for c, n in cnt.items() if n >= 2},  # flagged by >=2 (main)
            "all3": set.intersection(*per) if per else set(),   # flagged by all 3 (strict)
        }
    return out


def _prf(L, G):
    """precision/recall/F1 of predicted set L against gold set G."""
    L, G = set(L), set(G)
    inter = len(L & G)
    p = inter / len(L) if L else (1.0 if not G else 0.0)
    r = inter / len(G) if G else (1.0 if not L else 0.0)
    f = 2 * p * r / (p + r) if (p + r) else 0.0
    return p, r, f


def run_llm_vs_human_7(hproc, llm_proc):
    seven = triple_coded_transcripts(hproc)
    print(f"\n========== LLM vs HUMAN (n={len(seven)} triple-coded transcripts) ==========")
    if not seven or llm_proc is None or llm_proc.empty:
        print("  nothing to compare.")
        return

    # human-human IAA (for the 4th-annotator column)
    iaa = {f: iaa_sets(hproc, seven, f)[0] for f in ("goal", "code")}

    GOLDS = [("union", "any-1 (lenient floor)"),
             ("majority", ">=2  (main)"),
             ("all3", "all-3 (strict)")]

    combos = sorted(set(zip(llm_proc["config"], llm_proc["model"])))
    asets_by_lvl = {lvl: _ann_sets(hproc, seven, lvl) for lvl in ("goal", "code")}
    golds_by_lvl = {lvl: _human_gold(hproc, seven, lvl) for lvl in ("goal", "code")}

    for config, model in combos:
        print(f"\n--- config: {config}  |  model: {model} ---")
        for lvl in ("goal", "code"):
            golds = golds_by_lvl[lvl]
            lsets = _llm_sets(llm_proc, config, model, seven, lvl)
            print(f"  [{lvl}] LLM-vs-human set P/R/F1 (macro over {len(seven)}):")
            for gkey, gname in GOLDS:
                prf = [_prf(lsets.get(t, frozenset()), golds[t][gkey]) for t in seven]
                print(f"      vs {gname:22}: P={_mean([p for p,_,_ in prf]):.3f} "
                      f"R={_mean([r for _,r,_ in prf]):.3f} F1={_mean([f for *_,f in prf]):.3f}")
            js = [_jaccard(lsets.get(t, frozenset()), asets_by_lvl[lvl].get((t, a), frozenset()))
                  for t in seven for a in ANNOTATORS]
            print(f"      [4th-annotator] LLM-human Jaccard = {_mean(js):.3f}"
                  f"   (human-human IAA = {iaa[lvl]:.3f})")

        # valence agreement vs each human, on codes both flagged (both rules)
        lsub = llm_proc[(llm_proc["config"] == config) & (llm_proc["model"] == model)]
        hsub = hproc[hproc["transcript"].isin(seven)]
        for rule in ("valence_mixed", "valence_majority"):
            lval = lsub.set_index(["transcript", "code"])[rule].to_dict()
            hits = [1.0 if str(lval[(r.transcript, r.code)]) == str(getattr(r, rule)) else 0.0
                    for r in hsub.itertuples() if (r.transcript, r.code) in lval]
            print(f"  [valence {rule}] LLM-human exact on shared codes = "
                  f"{_mean(hits):.3f}  (N={len(hits)})")


def main():
    audit_files()
    check_paths()
    human = load_human()
    if human is None:
        return
    human = normalize(human)
    show_normalized(human, "HUMAN (all 3 annotators)")
    proc = build_human_processed(human)
    run_iaa(proc)

    # STEP 4: load LLM outputs (contextual + summary configs), unify, align to human
    llm_ctx, llm_norm = load_llm_contextual()
    llm_sum = load_llm_summary()
    llm_all = pd.concat([x for x in (llm_ctx, llm_sum) if x is not None], ignore_index=True)
    if not llm_all.empty:
        report_key_overlap(proc, llm_all)
        # STEP 5: LLM vs human on the 7 triple-coded transcripts, per (config, model)
        run_llm_vs_human_7(proc, llm_all)
    # PLAN (we fill these one at a time):
    #   1. [done] load_human() + normalize() -> shared label space
    #   2. [done] aggregate_by_code() -> processed_annotations/human_by_code.csv
    #   3. inter-annotator agreement on the 7 triple-coded transcripts (from the processed table)
    #   4. load_llm() + aggregate the same way
    #   5. human (consensus) vs LLM: goal-level, item-level, valence, level
    #   6. failure / None-rate accounting and exclusions


if __name__ == "__main__":
    main()
