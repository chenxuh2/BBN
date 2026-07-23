# BBN Reflection Codebook

Unit = one utterance (one row). Read 2 lines before + 1 line after for context.
Only fill **goal / action / level** when `is_reflection = Yes`. Leave a field blank
when it has no value — don't force one.

---

## is_reflection — is this utterance a reflection?
- **Yes** — the speaker recalls / evaluates / reflects on something from the simulation
- **No** — filler, acknowledgement, small talk, logistics ("yeah", "okay", "we have 15 min")

## goal + action — what is being reflected on
Pick the **goal** (one or more) and its **action code** below. If it matches none of
the listed actions, use **Other** (code `other`) and describe it in `other_summary`.

**g1 — Establish a Supportive and Professional Environment**  *(setting up the encounter: greeting, names, sitting/eye contact, attire, organized manner)*
- `g1-1` Addressed family member by name.
- `g1-2` Introduced him/herself by name and role.
- `g1-3` Clearly stated the name of the deceased family member.
- `g1-4` Sat down. (Body language/Eye contact)
- `g1-5` Displayed professional attire/presence.
- `g1-6` Handled interruptions in non-disruptive manner.
- `g1-7` Conducted interaction in organized manner.

**g2 — Assess and Align Expectations**  *(before the news: who's present + what the family already knows)*
- `g2-1` Ensured all important survivors were present.
- `g2-2` Determined knowledge survivors possessed.
- `g2-3` Involved me when discussing reason for visit.
- `g2-4` Elicited patient perspective of health situation.

**g3 — Deliver the News**  *(the disclosure: warning shot, chronicle of events, saying "died", avoiding jargon)*
- `g3-1` Provided appropriate opening statement (warning shot).
- `g3-2` Accurately/succinctly chronicled events leading to death.
- `g3-3` Used phrase 'dead' or 'died' (avoided euphemisms).
- `g3-4` Avoided jargon or explained terms.

**g4 — Manage the Emotional Response**  *(handling emotion: pausing, touch, validating feelings, own emotion not interfering)*
- `g4-1` Paused to allow family to assimilate information.
- `g4-2` Responded to cues with appropriate touch.
- `g4-3` Emotional response did not interfere with communication.
- `g4-4` Legitimized my emotions.
- `g4-5` Reinforced positive behaviors.

**g5 — Ensure Understanding and Facilitate Closure**  *(closing: viewing, questions, summary, next steps, checking understanding)*
- `g5-1` Offered viewing of the deceased.
- `g5-2` Established availability to answer questions.
- `g5-3` Encouraged questions/concerns.
- `g5-4` Summarized the interview.
- `g5-5` Checked for accuracy during interview.
- `g5-6` Reviewed next step(s).
- `g5-7` Verified patient's understanding.

**Other** — fits none of the five goals / listed actions  *(e.g. internal anxiety, ran out of time, forgot the plan)*
- `other` Other → also fill `other_summary`

> Note: in the g2/g4 items, "me / my" is the **family member's** wording (from the SP's checklist), not the learner.

## level — depth of reflection (code the highest that applies)
- **R0 Description** — only says what happened, or a bare "I did well", with no reason
- **R1 Reflective Description** — says what + *why* (a reason), but doesn't question it (because / so that)
- **R2 Dialogic Reflection** — steps back: questions own reading, weighs an alternative, or reads the family's hidden state ("I wonder if she was in shock")
- **R3 Transformative Reflection** — commits to doing it differently, or a changed view of self ("next time I'll…", "I realized I was managing my own anxiety")
- **R4 Critical Reflection** — beyond this encounter, to culture / ethics / the training system (rare)

## valence — is the reflection positive or negative?
- **positive** — appraises the reflected-on behavior as a strength or good choice ("I'm glad I…", "that worked")
- **negative** — appraises it as a shortcoming, mistake, regret, or something to change ("I forgot…", "I should have…")
- **neutral** — pure description, no positive/negative appraisal
- **mixed** — both a positive and a negative in the same reflection

> Judge the appraisal of the *past* behavior: a constructive "next time I'll…" still counts **negative** (it flags a shortcoming).

## episode — grouping utterances into one reflection
Number reflection threads **within each file**: the first reflection episode = `ep1`,
the next new thread = `ep2`, and so on. Non-reflection rows get no episode.
- **Same episode** while the reflection stays on the same moment/topic (bridge over a short "mm-hm" or a prompt in between).
- **New episode** when the moment being reflected on changes.
- Purpose: in analysis, take the highest `level` within an episode as that reflection's depth.

## other_summary — when the goal is **Other**
3–7 word free-text description of the behavior. Blank otherwise.

## notes — optional
Free text for borderline calls or to link related episodes.
