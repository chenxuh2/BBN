"""
Shared definitions for the reflection-annotation experiments.

Four annotation configs import from here so the taxonomy / valence / level
definitions live in ONE place (no drift across configs):

  reflection_annotation_summary_core_only.py       - conversation-level, goal-action only
  reflection_annotation_summary_full_one_stage.py  - conversation-level, +valence +level (1 prompt)
  reflection_annotation_summary_full_two_stage.py  - conversation-level, +valence +level (2 prompts)
  reflection_annotation_contextual_full_one_stage.py - per-utterance, full

Swap the local model for ALL configs at once via env var, e.g.:
    OLLAMA_MODEL=gemma3:27b python reflection_annotation_summary_full_one_stage.py
"""
import os
import re
import json
import pandas as pd
import ollama

# ============================================================================
# >>> FILL IN / CHECK THESE TWO THINGS <<<
# Used when you click "Run Python File" in VS Code (i.e. no env var set).
# Env vars REFLECTION_INPUT / OLLAMA_MODEL still override these if present.
# ----------------------------------------------------------------------------
# 1) Folder of debrief CSVs to annotate.
#    Prompt-development / iteration -> DEVELOPMENT set (shown below). Do all tuning here.
#    Final evaluation               -> switch to the VALIDATION set (keep it locked; run ONCE).
#    Production                     -> switch to your production set.
DEFAULT_INPUT = "/mnt/c/Users/stelh/Downloads/processed_anonymized_csvs/debriefing_sets/validation"

# 2) Which local model to run (change this to sweep qwen2.5:14b / gemma3:12b / mistral-nemo:12b).
DEFAULT_MODEL = "qwen2.5:14b"
# ============================================================================

# --- MODEL (override for every config via the OLLAMA_MODEL env var) ---
MODEL = os.environ.get("OLLAMA_MODEL", DEFAULT_MODEL)

# Candidate local models for the model-selection sweep.
# Design: hold scale ~constant (12-14B, the largest tier that runs FULLY inside a
# 12 GB consumer GPU / RTX 5070, Q4_K_M) and vary the training family/lineage, to
# isolate family/instruction-tuning effects under a realistic single-GPU deployment.
#   qwen2.5:14b     - Alibaba (incumbent baseline; strong structured output)
#   gemma3:12b      - Google  (different pretraining / instruction tuning)
#   mistral-nemo:12b- Mistral/NVIDIA (clean JSON/structured output)
CANDIDATE_MODELS = ["qwen2.5:14b", "gemma3:12b", "mistral-nemo:12b"]

# --- TAXONOMY ---
TAXONOMY = {
    "Goal 1: Establish a Supportive and Professional Environment": [
        "Addressed family member by name.",
        "Introduced him/herself by name and role.",
        "Clearly stated the name of the deceased family member.",
        "Sat down. (Body language/Eye contact)",
        "Displayed professional attire/presence.",
        "Handled interruptions in non-disruptive manner.",
        "Conducted interaction in organized manner."
    ],
    "Goal 2: Assess and Align Expectations": [
        "Ensured all important survivors were present.",
        "Determined knowledge survivors possessed.",
        "Involved me when discussing reason for visit.",
        "Elicited patient perspective of health situation."
    ],
    "Goal 3: Deliver the News": [
        "Provided appropriate opening statement (warning shot).",
        "Accurately/succinctly chronicled events leading to death.",
        "Used phrase 'dead' or 'died' (avoided euphemisms).",
        "Avoided jargon or explained terms."
    ],
    "Goal 4: Manage the Emotional Response": [
        "Paused to allow family to assimilate information.",
        "Responded to cues with appropriate touch.",
        "Emotional response did not interfere with communication.",
        "Legitimized my emotions.",
        "Reinforced positive behaviors."
    ],
    "Goal 5: Ensure Understanding and Facilitate Closure": [
        "Offered viewing of the deceased.",
        "Established availability to answer questions.",
        "Encouraged questions/concerns.",
        "Summarized the interview.",
        "Checked for accuracy during interview.",
        "Reviewed next step(s).",
        "Verified patient's understanding."
    ],
    "Other": [
        "Other"
    ]
}
GOAL_NAMES = list(TAXONOMY.keys())

# --- VALENCE (exact gold definitions) ---
VALENCE_VALUES = ["positive", "negative", "neutral", "mixed"]
VALENCE_DEFS = (
    '      - "positive" = appraises the behavior as a strength / good choice;\n'
    '      - "negative" = appraises it as a shortcoming / mistake / regret / something to change;\n'
    '      - "neutral"  = pure description, no positive or negative appraisal;\n'
    '      - "mixed"    = both a positive AND a negative in the same reflection.'
)

# --- LEVEL (exact gold definitions). CODE THE HIGHEST THAT APPLIES. ---
LEVELS = {
    "R0": "Description — only what happened, or a bare 'I did well', with no reason.",
    "R1": "Reflective Description — what + why (gives a reason), but not questioned.",
    "R2": "Dialogic — questions own reading / weighs an alternative / reads family's hidden state.",
    "R3": "Transformative — commits to change or a changed view of self ('next time I'll...').",
    "R4": "Critical — beyond this encounter: culture / ethics / training system (rare).",
}
LEVEL_VALUES = list(LEVELS.keys())


def levels_block():
    return "\n".join(f'      - "{k}": {v}' for k, v in LEVELS.items())


# --- Helpers ---
def _norm(s):
    return re.sub(r'[^a-z0-9]', '', str(s).lower())


# Reverse lookup: item -> (canonical goal, canonical item). Auto-corrects
# hallucinated goals (e.g. "Goal 6") when the item is a real taxonomy item.
ITEM_TO_GOAL = {}
for _goal, _items in TAXONOMY.items():
    for _it in _items:
        ITEM_TO_GOAL[_norm(_it)] = (_goal, _it)


def correct_goal(goal, item):
    """Return (canonical_goal, canonical_item), fixing hallucinated goals."""
    goal = str(goal).strip()
    item = str(item).strip()
    hit = ITEM_TO_GOAL.get(_norm(item))
    if hit:
        return hit[0], hit[1]
    if goal not in GOAL_NAMES:
        goal = f"UNMAPPED ({goal})"
    return goal, item


# ---------------------------------------------------------------------------
# CODED taxonomy. The model must pick an item CODE (e.g. "G4.4") instead of
# free-typing the item text, which it paraphrases/invents. Codes map back to the
# exact taxonomy strings deterministically, so outputs always use canonical items.
#   G<goal>.<item>  -> a specific listed action
#   G<goal>.OTHER   -> fits that goal but none of its listed actions (Scenario A)
#   OTHER           -> fits none of Goals 1-5 (Scenario B)
# ---------------------------------------------------------------------------
GOAL_LIST = [g for g in GOAL_NAMES if g != "Other"]  # Goal 1..5, excludes the "Other" bucket

CODE_TO_GOAL_ITEM = {}
for _gi, _goal in enumerate(GOAL_LIST, start=1):
    for _ii, _item in enumerate(TAXONOMY[_goal], start=1):
        CODE_TO_GOAL_ITEM[f"G{_gi}.{_ii}"] = (_goal, _item)
    CODE_TO_GOAL_ITEM[f"G{_gi}.OTHER"] = (_goal, "Other")
CODE_TO_GOAL_ITEM["OTHER"] = ("Other", "Other")


def coded_taxonomy_text():
    """Render the taxonomy with item codes, for embedding in a prompt."""
    lines = []
    for _gi, _goal in enumerate(GOAL_LIST, start=1):
        lines.append(_goal)
        for _ii, _item in enumerate(TAXONOMY[_goal], start=1):
            lines.append(f"  G{_gi}.{_ii} = {_item}")
        lines.append(f"  G{_gi}.OTHER = (reflects this goal but none of the specific actions above)")
    lines.append("OTHER = (does not fit any of Goals 1-5 at all)")
    return "\n".join(lines)


def resolve_code(code):
    """Map an item code (e.g. 'G4.4', 'G2.OTHER', 'OTHER') to (goal, item).
    Tolerates '-' vs '.', spaces, and case. Unknown codes fall back sensibly."""
    c = str(code).strip().upper().replace(" ", "").replace("-", ".")
    if c in CODE_TO_GOAL_ITEM:
        return CODE_TO_GOAL_ITEM[c]
    m = re.match(r"^G([1-5])\b", c) or re.match(r"^G([1-5])\.", c)
    if m:
        gi = int(m.group(1))
        # valid goal, unrecognized item slot -> treat as that goal's "Other"
        return (GOAL_LIST[gi - 1], "Other")
    return ("Other", "Other")


def normalize_valence(valence, is_reflection=True):
    v = str(valence).strip().lower()
    m = {x.lower(): x for x in VALENCE_VALUES}
    if v in m:
        return m[v]
    return "None"


def normalize_level(level, is_reflection=True):
    # accepts "R2" or "R2 Dialogic ..." -> "R2"
    key = _norm(level)[:2]
    m = {x.lower(): x for x in LEVEL_VALUES}
    if key in m:
        return m[key]
    return "None"


# Pure backchannel / filler tokens. An utterance of <=3 words made ENTIRELY of these is
# treated as non-reflection WITHOUT an LLM call (big speedup for per-utterance annotation).
# Trade-off: short agreements ("exactly", "I agree") are skipped, so INHERIT-CONTEXT
# reflections that ride on them are lost - an accepted cost for speed.
_FILLER = {
    "yeah", "yea", "yes", "yep", "yup", "no", "nope", "ok", "okay", "k", "kay",
    "right", "alright", "mm", "mmhm", "mhm", "mmm", "hmm", "hm", "uh", "um", "er",
    "uhhuh", "huh", "ah", "oh", "so", "well", "sure", "exactly", "agree", "agreed",
    "true", "correct", "totally", "definitely", "absolutely", "gotcha", "cool",
    "nice", "wow", "great", "good", "thanks", "thank", "you", "yourself", "same",
    "of", "course", "for", "i", "me", "too",
}


def looks_like_filler(text):
    """True if the utterance is empty or a short pure-backchannel (skip the LLM call)."""
    t = str(text).strip().lower()
    if not t:
        return True
    words = re.findall(r"[a-z']+", t)
    if not words:
        return True  # only punctuation / numbers
    if len(words) > 3:
        return False
    return all(w.replace("'", "") in _FILLER for w in words)


def input_set_name(input_folder):
    """Folder name of the input set (e.g. 'validation') to tag/segregate outputs."""
    return os.path.basename(str(input_folder).rstrip("/\\")) or "default"


def find_col(df, *candidates):
    norm_map = {_norm(c): c for c in df.columns}
    for cand in candidates:
        if _norm(cand) in norm_map:
            return norm_map[_norm(cand)]
    return None


def read_transcript_df(file_path):
    """Read a debrief csv/tsv robustly. Returns df or None."""
    try:
        df = pd.read_csv(file_path)
        if len(df.columns) == 1:
            df = pd.read_csv(file_path, sep='\t')
        return df
    except Exception as e:
        print(f"Could not read {file_path}: {e}")
        return None


def build_transcript(df, speaker_col, text_col, turn_col=None):
    """Turn a dataframe into a numbered, readable transcript string."""
    lines = []
    for i, row in df.iterrows():
        speaker = row.get(speaker_col, "Unknown") if speaker_col else "Unknown"
        text = row.get(text_col, "")
        if pd.isna(text) or str(text).strip() == "":
            continue
        turn = row.get(turn_col) if turn_col else None
        tag = f"[Turn {turn}]" if (turn is not None and not pd.isna(turn)) else f"[Line {i}]"
        lines.append(f'{tag} {speaker}: "{str(text).strip()}"')
    return "\n".join(lines)


# Hard cap on generated tokens. A legit reflections JSON is well under this; the cap stops
# runaway/looping generation (which otherwise balloons to 100k+ chars and truncates into
# invalid JSON) from wasting time.
MAX_OUTPUT_TOKENS = 4096


# Temperatures tried in order. Start deterministic (0.0); if the JSON is malformed (usually a
# repetition loop), retry with a bit of randomness to break the loop. Give up after the last one.
RETRY_TEMPS = [0.0, 0.4, 0.6]


def chat_json(prompt, model=None, label=""):
    """Call the local model, force JSON. Returns dict or None.
    Retries with escalating temperature to recover from malformed/looping JSON.
    keep_alive keeps the model resident in VRAM; num_predict caps runaway output."""
    tag = f" [{label}]" if label else ""
    last_err = None
    for i, temp in enumerate(RETRY_TEMPS):
        try:
            response = ollama.chat(
                model=model or MODEL,
                messages=[{'role': 'user', 'content': prompt}],
                format="json",
                options={"temperature": temp, "num_predict": MAX_OUTPUT_TOKENS},
                keep_alive="30m",
            )
            return json.loads(response['message']['content'])
        except Exception as e:
            last_err = e
            if i + 1 < len(RETRY_TEMPS):
                print(f"  retry{tag} at temp={RETRY_TEMPS[i + 1]} after: {str(e)[:70]}")
    print(f"  Model/JSON error{tag} (gave up after {len(RETRY_TEMPS)} tries): {last_err}")
    return None
