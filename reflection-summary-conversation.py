import pandas as pd
import ollama
import json
from tqdm import tqdm
import os
import glob
import re

# --- CONFIGURATION ---
# Folder with the anonymized debriefing conversations.
# Each file = one full debriefing conversation.
# Expected columns (case/spacing tolerant): turn, speaker, start time, end time, duration, text
INPUT_FOLDER = "/mnt/c/Users/stelh/Downloads/processed_anonymized_csvs/debriefing_sets/development"
OUTPUT_FOLDER = "conversation_summaries"
OUTPUT_CSV = os.path.join(OUTPUT_FOLDER, "reflection_summary_by_goal.csv")

MODEL = "qwen2.5:14b"

# Same taxonomy used by the utterance-level annotator (reflection-annotation-contextual.py)
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

# Canonical goal names (used to keep the output tidy / matchable)
GOAL_NAMES = list(TAXONOMY.keys())

# Reverse lookup: normalized item text -> (canonical goal, canonical item).
# Used to auto-correct the goal when the LLM invents a non-existent goal (e.g. "Goal 6")
# for an item that actually belongs to a real goal in the taxonomy.
def _norm_item(s):
    return re.sub(r'[^a-z0-9]', '', str(s).lower())

ITEM_TO_GOAL = {}
for _goal, _items in TAXONOMY.items():
    for _it in _items:
        ITEM_TO_GOAL[_norm_item(_it)] = (_goal, _it)


def _find_col(df, *candidates):
    """Find a column by fuzzy name match (case & non-alphanumeric insensitive)."""
    def norm(s):
        return re.sub(r'[^a-z0-9]', '', str(s).lower())
    norm_map = {norm(c): c for c in df.columns}
    for cand in candidates:
        key = norm(cand)
        if key in norm_map:
            return norm_map[key]
    return None


def build_transcript(df, speaker_col, text_col, turn_col=None):
    """Turn the dataframe into a readable, numbered transcript string."""
    lines = []
    for i, row in df.iterrows():
        speaker = row.get(speaker_col, "Unknown") if speaker_col else "Unknown"
        text = row.get(text_col, "")
        if pd.isna(text) or str(text).strip() == "":
            continue
        turn = row.get(turn_col) if turn_col else None
        tag = f"[Turn {turn}]" if turn is not None and not pd.isna(turn) else f"[Line {i}]"
        lines.append(f'{tag} {speaker}: "{str(text).strip()}"')
    return "\n".join(lines)


def analyze_conversation(transcript, file_name):
    prompt = f"""
You are an expert qualitative researcher analyzing a full medical debriefing conversation.
A team of learners (which includes a MEDICAL STUDENT) has just finished a simulation about
BREAKING BAD NEWS to a bereaved family member, and they are now debriefing / reflecting on how it went.

TAXONOMY OF GOALS AND ACTIONS (the skills being trained in the simulation):
{json.dumps(TAXONOMY, indent=2)}

FULL DEBRIEFING CONVERSATION (speakers are NOT reliably labeled by role):
{transcript}

YOUR TASKS:

1. IDENTIFY THE MEDICAL STUDENT.
   The speakers are NOT explicitly labeled by role. Based on the taxonomy and the content of the
   conversation, infer which speaker is most likely the MEDICAL STUDENT (i.e., the learner who
   actually performed / is reflecting on their own clinical actions during the death-notification
   simulation, as opposed to an observer, facilitator/SP observer, or the standardized family member).
   Give the speaker label you inferred and a brief justification with evidence.

2. IDENTIFY SPECIFIC (GOAL + ITEM) REFLECTIONS.
   This analysis will be used to find the GAP between how the student actually PERFORMED and what
   they REFLECTED on. That gap only appears if you are STRICT and SPECIFIC. Do NOT be generous.

   Map each genuine reflection to a SPECIFIC ITEM inside a Goal (not just the Goal). Example:
   Goal 4: Manage the Emotional Response -> item "Legitimized my emotions."

   A reflection COUNTS only if ALL of these hold:
     - A speaker explicitly EVALUATES, RECALLS, or REFLECTS on that specific clinical/communication
       action from the simulation (e.g. "I forgot to introduce myself", "I said 'passed away' instead
       of 'died'", "I sat down and made eye contact").
     - You can quote a DIRECT verbatim line from the transcript that supports it. If you cannot quote
       specific supporting text, DO NOT output the item.

   A reflection does NOT count (exclude it) if it is:
     - small talk, greetings, logistics, or scheduling ("we have 15 minutes");
     - a generic feeling with no specific action ("I was nervous") UNLESS tied to a concrete taxonomy action;
     - a facilitator's leading question that the student never actually engages with;
     - your own inference about what "probably" happened in the simulation — only what is REFLECTED ON here.

   Prefer a SMALL number of well-supported items over a long generous list. It is expected and fine
   for most conversations to reflect on only a FEW items, not all of them. Leaving items out is correct.

   For EACH qualifying reflection, provide:
     - "goal": MUST be EXACTLY one of these strings (copy verbatim, do NOT invent any other goal such
        as "Goal 6", "Offer support", "Provide resources", etc. — those do not exist in this taxonomy):
{json.dumps(GOAL_NAMES, indent=8)}
        If a reflection seems like "offering support" or "answering questions", it belongs to
        "Goal 5: Ensure Understanding and Facilitate Closure", NOT a new goal.
     - "item": the exact ITEM string from the taxonomy under that goal (verbatim). If it fits the goal
        but is not on the checklist, use "Other". If it fits no goal at all, use goal "Other" + item "Other".
     - "stance": one of "did well" | "did poorly" | "found difficult" | "neutral mention" —
        how the SPEAKER characterizes their own handling of this action. This is what powers the gap analysis.
     - "reasoning": 1-2 sentences on why this maps to this specific goal+item.
     - "evidence": a list of 1-3 SHORT verbatim quotes from the transcript.
     - "by_medical_student": true/false — whether the person you identified as the medical student
        is the one reflecting on it (someone else praising them does not count as the student's reflection).

OUTPUT FORMAT (Strict JSON, no extra prose):
{{
    "likely_medical_student": "speaker label you inferred",
    "medical_student_reasoning": "why you think this speaker is the medical student, with brief evidence",
    "reflections": [
        {{
            "goal": "Goal N: ...",
            "item": "exact taxonomy item, or Other",
            "stance": "did well | did poorly | found difficult | neutral mention",
            "reasoning": "...",
            "evidence": ["short quote 1", "short quote 2"],
            "by_medical_student": true
        }}
    ]
}}
"""

    try:
        response = ollama.chat(
            model=MODEL,
            messages=[{'role': 'user', 'content': prompt}],
            format="json",
            options={"temperature": 0.0}
        )
        result = json.loads(response['message']['content'])
        return result
    except Exception as e:
        print(f"  Error analyzing {file_name}: {e}")
        return None


def process_conversations():
    if not os.path.exists(OUTPUT_FOLDER):
        os.makedirs(OUTPUT_FOLDER)

    # Grab every csv/tsv in the folder (each file = one conversation)
    files = sorted(
        glob.glob(os.path.join(INPUT_FOLDER, "*.csv"))
        + glob.glob(os.path.join(INPUT_FOLDER, "*.tsv"))
    )
    print(f"Found {len(files)} conversation files in {INPUT_FOLDER}.")

    all_rows = []

    for file_path in tqdm(files):
        base = os.path.basename(file_path)

        try:
            df = pd.read_csv(file_path)
            if len(df.columns) == 1:
                df = pd.read_csv(file_path, sep='\t')
        except Exception as e:
            print(f"Could not read {file_path}. Error: {e}")
            continue

        speaker_col = _find_col(df, "speaker", "role")
        text_col = _find_col(df, "text", "utterance")
        turn_col = _find_col(df, "turn")

        if text_col is None:
            print(f"WARNING: {base} has no text/utterance column ({list(df.columns)}). Skipping.")
            continue

        transcript = build_transcript(df, speaker_col, text_col, turn_col)
        if not transcript.strip():
            print(f"WARNING: {base} produced an empty transcript. Skipping.")
            continue

        result = analyze_conversation(transcript, base)
        if result is None:
            all_rows.append({
                "file": base,
                "likely_medical_student": "Error",
                "medical_student_reasoning": "Error",
                "goal": "Error",
                "item": "Error",
                "stance": "",
                "reasoning": "",
                "evidence": "",
                "by_medical_student": ""
            })
            continue

        student = result.get("likely_medical_student", "Unknown")
        student_reason = result.get("medical_student_reasoning", "")
        reflections = result.get("reflections", []) or []

        if not reflections:
            # No reflection detected — still record a row so the file is accounted for.
            all_rows.append({
                "file": base,
                "likely_medical_student": student,
                "medical_student_reasoning": student_reason,
                "goal": "None",
                "item": "None",
                "stance": "",
                "reasoning": "No specific goal+item reflection detected.",
                "evidence": "",
                "by_medical_student": ""
            })
            continue

        for r in reflections:
            evidence = r.get("evidence", [])
            if isinstance(evidence, list):
                evidence_str = " | ".join(str(e).strip() for e in evidence if str(e).strip())
            else:
                evidence_str = str(evidence).strip()

            goal = str(r.get("goal", "Other")).strip()
            item = str(r.get("item", "Other")).strip()

            # --- AUTO-CORRECT the goal ---
            # 1) If the item matches a real taxonomy item, trust the item and derive its canonical
            #    goal (fixes hallucinated goals like "Goal 6: Offer support").
            lookup = ITEM_TO_GOAL.get(_norm_item(item))
            if lookup:
                goal, item = lookup
            # 2) Otherwise, if the goal itself is not one of the allowed goals, flag it instead of
            #    silently keeping a hallucinated goal name.
            elif goal not in GOAL_NAMES:
                goal = f"UNMAPPED ({goal})"

            all_rows.append({
                "file": base,
                "likely_medical_student": student,
                "medical_student_reasoning": student_reason,
                "goal": goal,
                "item": item,
                "stance": str(r.get("stance", "")).strip(),
                "reasoning": str(r.get("reasoning", "")).strip(),
                "evidence": evidence_str,
                "by_medical_student": r.get("by_medical_student", "")
            })

    out_df = pd.DataFrame(all_rows, columns=[
        "file",
        "likely_medical_student",
        "medical_student_reasoning",
        "goal",
        "item",
        "stance",
        "reasoning",
        "evidence",
        "by_medical_student",
    ])
    out_df.to_csv(OUTPUT_CSV, index=False)
    print(f"\nSaved conversation-level reflection summary to {OUTPUT_CSV}")
    print(f"  {len(files)} conversations -> {len(out_df)} (goal+item) rows.")


if __name__ == "__main__":
    process_conversations()
