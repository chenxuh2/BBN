# BBN Simulation & Reflection Auto-Annotator

This repository contains Python scripts designed to process and auto-annotate qualitative transcripts from Breaking Bad News (BBN) simulation and reflection/debriefing sessions. It uses an LLM (via Ollama) to categorize utterances against a predefined taxonomy of goals and actions.

Current workflow note: **process the SRT files first**. The annotation scripts are not yet fully updated for the new SRT split workflow; they will need to be updated before the next full annotation run.

## 📋 Table of Contents
1. [Prerequisites & Setup](#-prerequisites--setup)
2. [Data Preparation](#-data-preparation)
3. [Script Overview](#-script-overview)
4. [How to Run](#-how-to-run)
    - [Step 1: Process SRT Files First](#step-1-process-srt-files-first)
    - [Step 2: Annotate Simulations](#step-2-annotate-simulations)
    - [Step 3: Annotate Debriefing / Reflections](#step-3-annotate-debriefing--reflections)
5. [Understanding the Output](#-understanding-the-output)

---

## 🛠 Prerequisites & Setup

Before running the scripts, you need to set up your environment and ensure the LLM engine is running locally.

### 1. Install Ollama
The annotation scripts rely on **Ollama** to run local Large Language Models. We updated the code for Qwen 2.5 due to its superior performance in structured data extraction.
* Download and install Ollama from [ollama.com](https://ollama.com/).
* Once installed, open your terminal and pull the model:
  ```bash
  ollama pull qwen2.5:14b
  ```
Note: The Ollama application MUST be running in the background for the annotation scripts to work.

### 2. Install Python Dependencies
Ensure you have Python installed. Then, install the required libraries:

```Bash
pip install pandas ollama tqdm
```

## 📁 Data Preparation

Because patient/student data is kept private and not tracked on GitHub, you will need to create specific folders on your local machine after cloning this repository. 

Create the following folder structure in the root directory:

```text
BBN-local/
│
├── srt_data/                 <-- 1. Drop raw .srt files here
├── processed_srt_to_csvs/    <-- 2. Full uncut SRT-to-CSV files appear here
├── srt_to_csv_simulation/    <-- 3. Simulation + transition CSVs appear here
├── srt_csv_debriefing/       <-- 4. Debriefing CSVs appear here
├── annotated_data/           <-- 5. Final simulation annotation outputs appear here
├── annotated_reflections/    <-- 6. Final reflection outputs appear here
│
├── srt-processor.py
├── checklist-annotation-contextual.py
├── reflection-annotation-contextual.py
├── transcript_processor.py
└── ...
```

## 💻 Script Overview
srt-processor.py: Converts raw `.srt` files into CSV files. It also splits each transcript into simulation+transition and debriefing portions using a rule-based debriefing-start detector.

transcript_processor.py: Cleans and prepares raw transcripts for analysis.

checklist-annotation-contextual.py: Analyzes Simulation transcripts. It looks strictly at MED STUDENT roles and maps their direct actions to the 27-item medical checklist (e.g., "Provided warning shot"), corresponding goals, and flags if the utterance is a Death Announcement. **Needs update for the new SRT split workflow.**

reflection-annotation-contextual.py: Analyzes Reflection/Debrief transcripts. It looks at all non-empty utterances across any role and maps the discussion against the higher-level taxonomy of Goals and Actions (e.g., "Reflecting on Goal 2: Delivery of Bad News"). **Needs update for the new SRT split workflow.**

## 🚀 How to Run
### Step 1: Process SRT Files First
Place raw `.srt` files in:

```text
srt_data/
```

Then run:

```bash
python srt-processor.py
```

This creates three CSV outputs for each SRT file:

```text
processed_srt_to_csvs/<original_name>_full_uncut.csv
srt_to_csv_simulation/<original_name>_simulation_transition_cut.csv
srt_csv_debriefing/<original_name>_debriefing_cut.csv
```

The split is approximate. The simulation output includes the transition period before debriefing. The debriefing start is detected using timing, phrase cues, and a look-ahead context window.

### Step 2: Annotate Simulations
Status: **needs update**. Planned for next week.

The simulation annotation scripts should be updated to read from:

```text
srt_to_csv_simulation/
```

The expected input files are:

```text
*_simulation_transition_cut.csv
```

Current script:

```bash
python checklist-annotation-contextual.py
```

What this does: 
1. Filters for the MEDICAL STUDENT role.
2. Uses Qwen 2.5 to output a multi-label JSON of identified actions.
3. Context: Uses the surrounding utterances (2 previous, 1 next).
4. Outputs new files prefixed with ANNOTATED_ in the designated output folder.

### Step 3: Annotate Debriefing / Reflections
Status: **needs update**. Planned for next week.

The reflection/debriefing annotation scripts should be updated to read from:

```text
srt_csv_debriefing/
```

The expected input files are:

```text
*_debriefing_cut.csv
```

Current script:

```bash
python reflection-annotation-contextual.py
```

What this does:
1. Evaluates all valid, non-empty utterances.
2. Maps meta-cognitive discussion to the Goal/Action taxonomy.
3. Context: Uses the surrounding utterances (2 previous, 1 next).
4. Outputs new files prefixed with ANNOTATED_ in the designated output folder.

## 📊 Understanding the Output
The annotated CSVs will contain new columns generated by the LLM.

Simulation Output:

Annotated_Goals: Broad goal categories (e.g., Goal 4: Manage Emotional Response). 

Annotated_Actions: Specific checklist actions.

Is_Death: A boolean (True/False) flagging the specific moment of the death announcement.

Reflection Output:

Reflected_Goals: A comma-separated string of the higher-level goals being discussed (e.g., Goal 2 Delivery of Bad News).

Reflected_Actions: A comma-separated string of the specific checklist actions being discussed.

---

### Next Steps
Update the annotation scripts so they read from the new SRT split folders and write outputs using the current naming convention.
