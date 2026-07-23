"""
codebook_generator.py

Generates reflection_codebook.csv -- the human annotation codebook for the
Breaking Bad News (BBN) debriefing validation set.

The codebook covers four coding dimensions plus metadata:
  D1  Reflection Gate        (is_reflection)   -- mirrors the LLM's implicit gate
  D2  Goal                   (H_goal)          -- 5 goals + Other, from the LLM prompt taxonomy
  D3  Action                 (H_action)        -- checklist items per goal + Other
  D4  Reflection Level       (H_level)         -- R0-R4, Fleck & Fitzpatrick (2010)
  META / PROCEDURE                             -- speaker, episode, roll-up, rules

The five goals and their action strings are copied verbatim from
reflection-annotation-contextual.py so the human codes align with the LLM output.
Reflection-level definitions and BBN examples follow Fleck & Fitzpatrick (2010),
"Reflecting on reflection: framing a design landscape" (OzCHI '10).

Run:  python codebook_generator.py   ->   writes reflection_codebook.csv
This script only WRITES a new CSV; it does not modify any existing file.
"""

import csv

OUTPUT_FILE = "reflection_codebook.csv"

FIELDNAMES = [
    "dimension",
    "field_name",
    "code",
    "definition",
    "decision_rule",
    "boundary_rule",
    "example_positive",
    "example_negative",
    "source",
]

SRC_LLM = "LLM prompt (reflection-annotation-contextual.py)"
SRC_FLECK = "Fleck & Fitzpatrick (2010), OzCHI; examples adapted to BBN"
SRC_DERIVED = "Derived for human validation"

ROWS = []


def add(dimension, field_name, code, definition, decision_rule="",
        boundary_rule="", example_positive="", example_negative="", source=""):
    ROWS.append({
        "dimension": dimension,
        "field_name": field_name,
        "code": code,
        "definition": definition,
        "decision_rule": decision_rule,
        "boundary_rule": boundary_rule,
        "example_positive": example_positive,
        "example_negative": example_negative,
        "source": source,
    })


# ---------------------------------------------------------------------------
# 0. PROCEDURE / GLOBAL RULES
# ---------------------------------------------------------------------------
add("0 PROCEDURE", "unit_of_analysis", "one utterance",
    "The coding unit is a single utterance: one row of a *_DEBRIEF*.csv file "
    "(columns Role, Timestamp, Utterance). Every row gets an is_reflection code.",
    decision_rule="Judge only the TARGET utterance, but use the context window to interpret it.",
    source=SRC_DERIVED)

add("0 PROCEDURE", "context_window", "2 before + target + 1 after",
    "Before coding, read the 2 utterances before the target and the 1 utterance "
    "after it. This is the same window the LLM sees, so human and model judge the "
    "target with identical context.",
    source=SRC_LLM)

add("0 PROCEDURE", "top_down_rule", "goal first, then action",
    "Classify top-down: first decide the Goal (D2), then pick the Action (D3) ONLY "
    "from the item list under that goal. Never choose an action from a different goal.",
    source=SRC_LLM)

add("0 PROCEDURE", "inheritance_rule", "inherit on generic agreement",
    "If the target utterance is a generic agreement ('I agree', 'exactly', 'yeah, "
    "totally') AND the immediately preceding utterance was a specific reflection, "
    "inherit that preceding utterance's Goal and Action and set is_reflection = Yes.",
    boundary_rule="A bare 'yeah'/'okay' with NO preceding specific reflection stays is_reflection = No.",
    source=SRC_LLM)

add("0 PROCEDURE", "multilabel_rule", "goal may be multi-valued",
    "H_goal may hold more than one goal when the utterance genuinely reflects on "
    "several goals. Separate multiple values with '; '. Prefer a single primary goal "
    "when the reflection has one clear focus.",
    source=SRC_LLM)

add("0 PROCEDURE", "highest_level_rule", "code the single highest level",
    "Reflection levels (D4) are cumulative: a higher level presupposes the lower "
    "ones. Record only the single HIGHEST level evident in the utterance.",
    source=SRC_FLECK)

add("0 PROCEDURE", "rollup_rule", "group by episode_id, take max level",
    "For depth analysis, group utterances by episode_id and take the max level "
    "(episode-level depth). Roll up further by speaker x goal for the blind-spot grid, "
    "or by session for cross-session comparison. Roll-up is an analysis step, not a "
    "coding step: the raw per-utterance level is always kept for LLM comparison.",
    source=SRC_DERIVED)

add("0 PROCEDURE", "firstperson_note", "'me'/'my' = family member",
    "Checklist items phrased in the first person ('Involved me...', 'Legitimized my "
    "emotions', 'Elicited patient perspective') are written from the standardized "
    "family member's (SP's) point of view. 'me/my' means the bereaved family member, "
    "NOT the learner.",
    source=SRC_LLM)

add("0 PROCEDURE", "scope_note", "gate every row, analyze learners",
    "The LLM annotates every row regardless of speaker. Code is_reflection on every "
    "row so false positives are measurable, but restrict depth and blind-spot analysis "
    "to speaker_type = learner (the person being debriefed).",
    source=SRC_DERIVED)

add("0 PROCEDURE", "depth_is_human_only", "D4 not in LLM output",
    "The current LLM prompt does NOT emit a reflection level. D4 (R0-R4) is a "
    "human-only layer used for the depth/blind-spot analysis. It is NOT scored against "
    "the LLM unless the level is later added to the prompt.",
    source=SRC_DERIVED)


# ---------------------------------------------------------------------------
# D1. REFLECTION GATE
# ---------------------------------------------------------------------------
add("D1 Reflection Gate", "is_reflection", "Yes",
    "The speaker is actively evaluating, recalling, or reflecting on a clinical or "
    "emotional action from the simulation (what they did, why, how it went, how it "
    "felt, or what it meant).",
    decision_rule="Assign Yes for any on-topic recall/evaluation of the encounter, "
    "including plain description (R0). Also Yes for a generic agreement that inherits "
    "a preceding reflection (see inheritance_rule).",
    boundary_rule="On-topic description of the encounter is Yes (level R0), NOT No. "
    "No is reserved for content that is not about reflecting on the encounter.",
    example_positive="'I think overall I did well... when you asked me what compressions "
    "were, I was like, oh gosh, how do I describe that?'",
    example_negative="'Come on in. Hi, my name is [NAME].' (greeting/room logistics)",
    source=SRC_LLM)

add("D1 Reflection Gate", "is_reflection", "No",
    "The utterance is conversational filler, acknowledgement, logistics/room talk, or "
    "social pleasantry, and is not (and does not inherit) a reflection on the encounter.",
    decision_rule="Assign No to 'yeah', 'okay', 'right', 'um', scheduling talk "
    "('we only have 15 minutes'), and greetings, UNLESS the inheritance_rule applies.",
    boundary_rule="Do NOT force a goal/action mapping onto a No row: leave H_goal, "
    "H_action, H_level empty.",
    example_positive="'Yeah.' / 'Okay.' / 'So we only have about 15 minutes.'",
    example_negative="'I used the word died on purpose.' (this is a reflection -> Yes)",
    source=SRC_LLM)


# ---------------------------------------------------------------------------
# D2. GOAL
# ---------------------------------------------------------------------------
add("D2 Goal", "H_goal", "Goal 1: Establish a Supportive and Professional Environment",
    "Reflection about setting up the encounter: greeting and identifying people, "
    "physical/nonverbal presence (sitting, eye contact, attire), and running the "
    "conversation in a calm, organized, non-disruptive way.",
    decision_rule="Choose when the reflection is about the FRAME of the encounter "
    "(who, where, how it was set up and conducted) rather than the news content or emotion.",
    boundary_rule="vs Goal 4: general professional calm/organization is Goal 1; "
    "specifically handling emotion in the room is Goal 4.",
    example_positive="'I forgot to introduce myself and my role before I started talking.'",
    source=SRC_LLM)

add("D2 Goal", "H_goal", "Goal 2: Assess and Align Expectations",
    "Reflection about finding out who is present, what the family already knows or "
    "suspects, and eliciting/aligning with the family's perspective before delivering news.",
    decision_rule="Choose when the reflection is about ASSESSING the family's knowledge/"
    "perspective or making sure the right people are there, before the disclosure.",
    boundary_rule="vs Goal 3: assessing what they already know is Goal 2; the warning "
    "shot and the disclosure itself are Goal 3.",
    example_positive="'I should have asked what she already knew before I launched in.'",
    source=SRC_LLM)

add("D2 Goal", "H_goal", "Goal 3: Deliver the News",
    "Reflection about the act of delivering the death notification: giving a warning "
    "shot, chronicling the clinical events, using explicit words ('dead'/'died'), and "
    "avoiding or explaining jargon.",
    decision_rule="Choose when the reflection is about HOW the news itself was told "
    "(wording, sequence, clarity of the disclosure).",
    boundary_rule="vs Goal 5: delivering the news content is Goal 3; checking the family "
    "understood it or summarizing is Goal 5.",
    example_positive="'I said died instead of passed away because euphemisms can confuse people.'",
    source=SRC_LLM)

add("D2 Goal", "H_goal", "Goal 4: Manage the Emotional Response",
    "Reflection about handling emotion in the room: pausing for the family to absorb "
    "the news, responding to emotional cues (including appropriate touch), keeping "
    "one's own emotion from interfering, validating the family's emotions, and "
    "reinforcing their positive coping.",
    decision_rule="Choose when the reflection is about EMOTION -- the family's reaction "
    "and how the learner responded to or managed it.",
    boundary_rule="vs Other: managing the FAMILY's emotion (or the learner's emotion as "
    "it affected the family) is Goal 4; the learner's purely internal anxiety that was "
    "never expressed or managed is Other (Scenario B).",
    example_positive="'She started crying and I just froze instead of giving her a moment.'",
    source=SRC_LLM)

add("D2 Goal", "H_goal", "Goal 5: Ensure Understanding and Facilitate Closure",
    "Reflection about wrapping up: offering to view the deceased, stating availability, "
    "inviting questions, summarizing, checking accuracy/understanding, and reviewing "
    "next steps.",
    decision_rule="Choose when the reflection is about CLOSING the encounter and "
    "confirming understanding/next steps.",
    boundary_rule="vs Goal 3: the disclosure is Goal 3; confirming the family understood "
    "it and what comes next is Goal 5.",
    example_positive="'I never checked whether she actually understood what I told her.'",
    source=SRC_LLM)

add("D2 Goal", "H_goal", "Other",
    "The reflection is on something entirely outside all five goals -- e.g., the "
    "learner's internal anxiety, time pressure, general preparation/nerves, or "
    "simulation logistics. This is the goal-level 'Other' (Scenario B).",
    decision_rule="Use Goal = Other ONLY when no goal fits at all. Pair with Action = "
    "Other and fill H_other_summary. Do NOT use goal-level Other when a goal fits but "
    "only the action is off-checklist (that is Scenario A: keep the matched goal).",
    boundary_rule="Scenario A (goal fits, action off-checklist) keeps the real goal + "
    "Action Other; Scenario B (nothing fits) uses Goal Other + Action Other.",
    example_positive="'My plan just went out the window the second I walked in, I was so "
    "anxious.' (internal anxiety, no goal fits)",
    source=SRC_LLM)


# ---------------------------------------------------------------------------
# D3. ACTION  (grouped by goal; choose only within the selected goal)
# ---------------------------------------------------------------------------
ACTION_DEFS = {
    "Goal 1: Establish a Supportive and Professional Environment": [
        ("Addressed family member by name.",
         "The learner used the family member's name when speaking to them."),
        ("Introduced him/herself by name and role.",
         "The learner stated their own name and clinical role (e.g., 'I'm the ER physician')."),
        ("Clearly stated the name of the deceased family member.",
         "The learner named the deceased explicitly rather than referring to them vaguely."),
        ("Sat down. (Body language/Eye contact)",
         "The learner sat down and used supportive nonverbal behavior (eye contact, open posture)."),
        ("Displayed professional attire/presence.",
         "The learner's dress and demeanor conveyed professionalism."),
        ("Handled interruptions in non-disruptive manner.",
         "The learner managed interruptions (pager, door, side questions) without derailing the conversation."),
        ("Conducted interaction in organized manner.",
         "The encounter had a coherent structure/flow rather than being scattered or improvised."),
    ],
    "Goal 2: Assess and Align Expectations": [
        ("Ensured all important survivors were present.",
         "The learner checked for or arranged the presence of key family members before delivering news."),
        ("Determined knowledge survivors possessed.",
         "The learner found out what the family already knew or suspected before telling them."),
        ("Involved me when discussing reason for visit.",
         "The learner engaged the family member (the SP) in the conversation about why they were called in."),
        ("Elicited patient perspective of health situation.",
         "The learner asked for the family's own understanding or view of the situation."),
    ],
    "Goal 3: Deliver the News": [
        ("Provided appropriate opening statement (warning shot).",
         "The learner signaled that serious news was coming before delivering it."),
        ("Accurately/succinctly chronicled events leading to death.",
         "The learner gave a clear, concise account of the clinical events leading to the death."),
        ("Used phrase 'dead' or 'died' (avoided euphemisms).",
         "The learner used explicit death language rather than euphemisms ('passed', 'lost', 'gone')."),
        ("Avoided jargon or explained terms.",
         "The learner avoided unexplained medical jargon, or defined the terms used (e.g., 'compressions')."),
    ],
    "Goal 4: Manage the Emotional Response": [
        ("Paused to allow family to assimilate information.",
         "The learner allowed silence/time for the news to sink in."),
        ("Responded to cues with appropriate touch.",
         "The learner responded to emotional cues, including appropriate physical comfort where welcome."),
        ("Emotional response did not interfere with communication.",
         "The learner's own emotional reaction did not obstruct clear communication."),
        ("Legitimized my emotions.",
         "The learner acknowledged/validated the family member's emotional reaction as understandable."),
        ("Reinforced positive behaviors.",
         "The learner affirmed the family's healthy coping or reactions."),
    ],
    "Goal 5: Ensure Understanding and Facilitate Closure": [
        ("Offered viewing of the deceased.",
         "The learner offered the family the chance to see the body."),
        ("Established availability to answer questions.",
         "The learner made clear they remained available for further questions."),
        ("Encouraged questions/concerns.",
         "The learner actively invited the family to ask questions or raise concerns."),
        ("Summarized the interview.",
         "The learner recapped the key points of the conversation."),
        ("Checked for accuracy during interview.",
         "The learner confirmed facts or understanding along the way."),
        ("Reviewed next step(s).",
         "The learner explained what would happen next (paperwork, belongings, contacts, etc.)."),
        ("Verified patient's understanding.",
         "The learner confirmed the family understood what was communicated."),
    ],
}

for goal, actions in ACTION_DEFS.items():
    for action_str, action_def in actions:
        add("D3 Action", "H_action", action_str, action_def,
            decision_rule="Select only if H_goal = '%s'." % goal,
            source=SRC_LLM)

# Action-level Other (two scenarios)
add("D3 Action", "H_action", "Other (Scenario A: goal fits, action off-checklist)",
    "The reflection fits one of the five goals but the specific behavior is not a "
    "listed checklist item under that goal (e.g., handing the family a tissue -> "
    "Goal 4 / Action Other).",
    decision_rule="Keep H_goal = the matched goal; set H_action = Other; fill "
    "H_other_summary with a 3-7 word summary of the behavior.",
    boundary_rule="If NO goal fits either, use Scenario B (Goal Other + Action Other) instead.",
    example_positive="'I handed her a box of tissues without thinking about it.' "
    "(Goal 4, action not on checklist)",
    source=SRC_LLM)

add("D3 Action", "H_action", "Other (Scenario B: outside all goals)",
    "The reflection is entirely outside the five goals (e.g., internal anxiety, running "
    "out of time, forgetting a plan).",
    decision_rule="Set H_goal = Other AND H_action = Other; fill H_other_summary with a "
    "3-7 word summary.",
    boundary_rule="If a goal DOES fit and only the action is off-checklist, use Scenario A.",
    example_positive="'I was so nervous I completely forgot the plan I had rehearsed.'",
    source=SRC_LLM)


# ---------------------------------------------------------------------------
# D4. REFLECTION LEVEL  (Fleck & Fitzpatrick 2010)
# ---------------------------------------------------------------------------
add("D4 Reflection Level", "H_level", "R0 - Description",
    "A purely descriptive revisiting of events: recounting what happened with no "
    "indication of reflective thought -- no reasons, no interpretation, no evaluation. "
    "(Fleck treats R0 as not yet reflection, but it is still on-topic, so is_reflection = Yes.)",
    decision_rule="Assign when the utterance only reports what happened.",
    boundary_rule="R0 -> R1: assign R1 the moment a justification/reason appears "
    "(because / so that / in order to).",
    example_positive="'I walked in, introduced myself, asked her to sit down, and then I "
    "told her that her husband had died. After that I explained what had happened in the ER.'",
    source=SRC_FLECK)

add("D4 Reflection Level", "H_level", "R1 - Reflective Description",
    "Description accompanied by justification or reasons for events and actions, but "
    "with limited analysis and no change of perspective -- the learner explains WHY they "
    "did something, reportively, without questioning that explanation.",
    decision_rule="Assign when the learner gives a reason for what they did but does not "
    "question it or consider alternatives.",
    boundary_rule="R1 -> R2: assign R2 only when the learner questions their own "
    "interpretation, considers alternatives, or takes the family's perspective "
    "('I wonder if...', 'maybe she was actually...').",
    example_positive="'I used the word died instead of passed away because we were taught "
    "that euphemisms can leave the family unsure about what actually happened.'",
    source=SRC_FLECK)

add("D4 Reflection Level", "H_level", "R2 - Dialogic Reflection",
    "Goes beyond description-with-explanation to probe relationships within the "
    "experience; dialogic in that it involves stepping back and seeing things from a "
    "different perspective, entertaining new points of view -- considering alternative "
    "explanations or the other party's internal state.",
    decision_rule="Assign when the learner entertains an alternative reading, or infers "
    "the family member's internal state, without yet committing to a change.",
    boundary_rule="R2 -> R3: stay at R2 if the learner only explores possibilities; "
    "assign R3 only when there is an explicit future change or revised self-understanding.",
    example_positive="'She went silent after I said it, and at the time I assumed she "
    "understood. But now I wonder if she was actually in shock -- maybe her silence "
    "wasn't understanding at all. If I had asked what she already knew first, I might "
    "have read her reaction differently.'",
    source=SRC_FLECK)

add("D4 Reflection Level", "H_level", "R3 - Transformative Reflection",
    "Reflection that challenges one's assumptions and results in an actual change of "
    "practice -- revisiting the event with the intent to do things differently, arriving "
    "at a revised understanding of oneself or the task that carries forward.",
    decision_rule="Assign when there is an explicit, forward-looking change or a revised "
    "self-understanding ('next time I will...', 'I realized I was...').",
    boundary_rule="R3 -> R4: assign R4 only when the reflection goes beyond this single "
    "encounter to culture, ethics, institutions, or the training system itself.",
    example_positive="'I realized I jumped straight into the medical details after the "
    "disclosure because the silence made me uncomfortable -- I was managing my own "
    "anxiety, not her needs. Next time I'll hold the silence and let her reaction set "
    "the pace before offering information.'",
    source=SRC_FLECK)

add("D4 Reflection Level", "H_level", "R4 - Critical Reflection",
    "Takes into consideration aspects beyond the immediate situation -- moral and "
    "ethical issues and the wider socio-historical and politico-cultural context. "
    "Reaching this level is very rare in practice.",
    decision_rule="Assign when the reflection engages culture, ethics, institutions, or "
    "the training/checklist system itself, beyond this one encounter.",
    boundary_rule="Highest level; nothing is coded above R4. If the reflection stays "
    "within the single encounter, it is at most R3.",
    example_positive="'Our training treats death notification as one standardized script, "
    "but families from different cultural or religious backgrounds may need very "
    "different conversations... the checklist assumes there's one right way to grieve.'",
    source=SRC_FLECK)


# ---------------------------------------------------------------------------
# META FIELDS  (annotator-filled, per row)
# ---------------------------------------------------------------------------
add("META", "H_speaker_type", "learner",
    "The person being debriefed and doing the reflecting: the medical student or the "
    "social worker. Depth and blind-spot analysis are restricted to this speaker type.",
    source=SRC_DERIVED)

add("META", "H_speaker_type", "facilitator",
    "The debrief guide (e.g., 'SP Observer'): asks prompts and steers the session but "
    "is not the one being assessed for reflection depth.",
    source=SRC_DERIVED)

add("META", "H_speaker_type", "SP",
    "The standardized patient in the family role (e.g., 'Widow'): speaks from the "
    "family-member perspective or gives family-perspective feedback.",
    source=SRC_DERIVED)

add("META", "episode_id", "e.g. BESE11_ep03",
    "Identifier grouping a contiguous run of utterances that form ONE reflective thread "
    "(same reflector, same referent). Assign the same id across utterances that continue "
    "one reflection.",
    decision_rule="Start a NEW id when the referent/goal changes, the reflector changes, "
    "or the facilitator moves to a new topic. Bridge over backchannels and same-topic "
    "probing questions (keep the same id).",
    boundary_rule="If the same moment is revisited later after an intervening different "
    "topic, use a new id and cross-link the two in H_notes.",
    source=SRC_DERIVED)

add("META", "prompted_spontaneous", "spontaneous | prompted",
    "Whether the reflection level was reached spontaneously by the learner or elicited "
    "by a facilitator/SP prompt (e.g., 'what would you do next time?'). Attribute the "
    "level to the learner's utterance regardless; this flag records how it was produced.",
    decision_rule="'prompted' if the immediately preceding facilitator/SP turn asked for "
    "exactly the reasoning/change the learner then gave; otherwise 'spontaneous'.",
    source=SRC_DERIVED)

add("META", "H_other_summary", "3-7 word free text",
    "Short free-text summary of the reflected behavior, filled ONLY when H_action = "
    "Other (either scenario). Empty otherwise.",
    example_positive="'handed patient a tissue' / 'anxiety disrupted planned approach'",
    source=SRC_LLM)

add("META", "H_uncertain", "0 | 1",
    "Set to 1 if the annotator is unsure of ANY code on this row (flag for "
    "adjudication); otherwise 0.",
    source=SRC_DERIVED)

add("META", "H_notes", "free text",
    "Free text. Record episode linkage ('continues previous 2 utterances'), rationale "
    "for borderline calls, and anything needed for adjudication.",
    source=SRC_DERIVED)


def main():
    with open(OUTPUT_FILE, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDNAMES, quoting=csv.QUOTE_ALL)
        writer.writeheader()
        for row in ROWS:
            writer.writerow(row)
    print("Wrote %d codebook rows to %s" % (len(ROWS), OUTPUT_FILE))


if __name__ == "__main__":
    main()
