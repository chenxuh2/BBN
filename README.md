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
