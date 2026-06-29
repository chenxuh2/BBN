# BBN Transcript Processing and Annotation

This repository contains Python scripts for processing Breaking Bad News (BBN)
simulation/debriefing transcripts and preparing them for downstream annotation.


Data files are not stored in this repository. Raw transcripts, processed CSVs,
split simulation/debriefing files, and annotated outputs should remain in local
folders on your machine.

## Scripts

- `anaonymized-processor.py`: splits anonymized full-session CSV files into
  simulation and debriefing CSVs.
- `transcript_processor.py`: converts raw transcript text files into cleaned CSV
  files.
- `checklist-annotation-contextual.py`: annotates simulation utterances against
  the Breaking Bad News checklist using an LLM.
- `reflection-annotation-contextual.py`: annotates debriefing/reflection
  utterances against the reflection taxonomy using an LLM.

## Anonymized CSV Processor

`anaonymized-processor.py` takes full-session anonymized CSV files and cuts each
file into two outputs:

- simulation rows, saved under `processed_anonymized_csvs/simulation`
- debriefing rows, saved under `processed_anonymized_csvs/debriefing`

It also writes a run-level confidence report:

```text
processed_anonymized_csvs/split_confidence_report.csv
```

The report contains one row per input file, including the file name, cut index,
cut timestamp, cut reason, confidence score, a short preview of the cut row, and
the output file paths.

### Required Input Format

The input CSVs must include these columns:

```text
start time
end time
text
```

The script currently reads from the local Windows Downloads folder through WSL:

```python
INPUT_FOLDER = "/mnt/c/Users/stelh/Downloads/hippa"
OUTPUT_FOLDER = "/mnt/c/Users/stelh/Downloads/processed_anonymized_csvs"
```

Edit these paths in `anaonymized-processor.py` if your files are somewhere else.

### How to Run

Install the required Python package:

```bash
pip install pandas
```

Then run:

```bash
python anaonymized-processor.py
```

## Cut Rules

The processor looks for the first row that should belong to debriefing. The cut
is always made between rows; it never splits a text cell.

1. Empty files are cut at row 0 and marked with `confidence=0`.
2. The first 20 minutes are ignored when searching for debriefing cues.
3. After 20 minutes, a row is treated as the debriefing start if it contains a
   strong cue such as `debriefing`, `debrief`, `reflection`, or `reflect`.
4. A row can also be treated as the debriefing start if it contains an entry cue
   such as `come in`, `come back in`, `bring ... in`, or `join us`, and the next
   120 seconds contain debriefing-related language.
5. If no language-based cut is found, the script falls back to the start of the
   final 17 minutes of the session, but never before minute 20.
6. If no fallback candidate exists, the script cuts at the last row.

Confidence scores are heuristic quality flags, not probabilities:

- `10`: very strong language evidence
- `5`: clear debriefing/reflection cue
- `1`: duration fallback only; manually review these
- `0`: empty file or last-row fallback; manually review these first

## LLM Annotation Setup

The annotation scripts use Ollama for local LLM calls.

Install Ollama, then pull the configured model:

```bash
ollama pull qwen2.5:14b
```

Install Python dependencies:

```bash
pip install pandas ollama tqdm
```

```bash
python checklist-annotation-contextual.py
python reflection-annotation-contextual.py
```

Check and edit each script's `INPUT_FOLDER` and `OUTPUT_FOLDER` constants before
running annotation.

## To Do

### Cut QC

Use the `split_confidence_report.csv` file to audit cut quality before any
downstream annotation. Prioritize all files with `status=error`,
`confidence=0`, or `confidence=1`, because these cuts were made with weak or no
language evidence. Then review a stratified subset of higher-confidence cuts:
about 15-20 files with `confidence=5` and 8-10 files with `confidence=10`. For
each reviewed file, inspect roughly two minutes before and after the cut and
label the cut as `correct`, `too early`, `too late`, or `unclear`.

For the current dataset size, a first-pass QC target of about 35 sessions is a
reasonable minimum. This is large enough to cover all weak cuts plus a sample of
apparently strong cuts, while still being feasible for manual review.

### Potential LLM Checklist Auto-Assessment

It is worth testing whether an LLM can identify observable Breaking Bad News
checklist evidence from the simulation transcript. Treat this as
transcript-based evidence detection, not as a replacement for medical student
self-assessment. Recommended labels are `present`, `no evidence`, and
`not observable from transcript`, because some checklist items involve
non-verbal behavior that may not be visible in text.

This could support a useful comparison between student self-rating, human-coded
transcript evidence, and LLM-coded transcript evidence.

### Debriefing LLM Annotation Workflow

Split data by session rather than by utterance, so that language from the same
debriefing session does not leak across development and validation.

- Prompt/codebook development: 12-15 sessions. This is enough to expose common
  debriefing patterns and refine the prompt without using too much of the
  dataset for tuning.
- Locked validation set: 20-25 sessions. This gives a stable estimate of model
  performance across sessions and should not be used for prompt revision after
  it is locked.
- LLM production set: all remaining sessions. Run the locked prompt on these
  only after validation performance is acceptable.
- Post-hoc audit: at least 10% of the production set, plus all `Other`,
  uncertain, malformed, or error outputs. This keeps quality control focused on
  the highest-risk cases.

For a CHI/IUI-level analysis, use human annotation on the validation set and
report agreement against the LLM. A practical target is goal-level macro F1 of
at least 0.80 and action-level macro F1 of about 0.70-0.75 for common labels,
with rare labels and `Other` reported separately.
