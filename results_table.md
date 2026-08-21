# LLM–Human Agreement Results (7 triple-coded validation sessions)

**Reference — human inter-annotator ceiling (IAA):** goal-level Jaccard = **0.76**, item-level = **0.39**;
valence exact ≈ **0.48** (κ ≈ 0.25). All LLM numbers below are read against this ceiling.

**Legend:**
- **F1** = the LLM's per-session code set vs the human **majority (≥2/3)** gold, macro-averaged over the 7 sessions.
- **J⁴** = LLM as a *"4th annotator"*: mean pairwise Jaccard between the LLM and each human — directly comparable to the human–human IAA.
- **Valence** = exact agreement on shared codes (mixed rule); N = number of shared codes. Cells with small N (< ~15) are not reliable.

---

## LaTeX table (appendix, paste-ready)

```latex
\begin{table*}[t]
\centering
\caption{LLM--human agreement on the seven triple-coded validation sessions, by annotation
configuration and model. F1 is the LLM's per-session code set against the human majority
($\geq$2/3) gold; J$^{4}$ is the LLM's mean pairwise Jaccard with each annotator (LLM as a
``4th annotator''), read against the human inter-annotator ceiling (goal $=0.76$, item $=0.39$).
Valence is exact agreement on shared codes (mixed rule); $N$ is the number of shared codes
(valence with small $N$ is unreliable). The selected configuration/model is in bold.}
\label{tab:llm-human}
\begin{tabular}{llcccccc}
\toprule
 & & \multicolumn{2}{c}{Goal level} & \multicolumn{2}{c}{Item level} & \multicolumn{2}{c}{Valence} \\
\cmidrule(lr){3-4}\cmidrule(lr){5-6}\cmidrule(lr){7-8}
Config & Model & F1 & J$^{4}$ & F1 & J$^{4}$ & exact & $N$ \\
\midrule
\multicolumn{2}{l}{\emph{Human ceiling (IAA)}} & -- & 0.76 & -- & 0.39 & 0.48 & -- \\
\midrule
Contextual & gemma3:12b        & 0.90 & 0.81 & 0.44 & 0.29 & 0.40 & 166 \\
Contextual & mistral-nemo:12b  & 0.91 & 0.81 & 0.45 & 0.28 & 0.28 & 151 \\
\textbf{Contextual} & \textbf{qwen2.5:14b} & \textbf{0.90} & \textbf{0.81} & \textbf{0.46} & \textbf{0.31} & \textbf{0.36} & \textbf{172} \\
\midrule
Summary (core)    & gemma3:12b       & 0.51 & 0.33 & 0.06 & 0.04 & 0.00 & 12 \\
Summary (core)    & mistral-nemo:12b & 0.74 & 0.61 & 0.04 & 0.02 & 0.00 & 5  \\
Summary (core)    & qwen2.5:14b      & 0.30 & 0.19 & 0.11 & 0.06 & 0.00 & 14 \\
\midrule
Summary (1-stage) & gemma3:12b       & 0.65 & 0.44 & 0.05 & 0.01 & 0.25 & 4  \\
Summary (1-stage) & mistral-nemo:12b & 0.69 & 0.59 & 0.23 & 0.12 & 0.13 & 32 \\
Summary (1-stage) & qwen2.5:14b      & 0.50 & 0.32 & 0.21 & 0.09 & 0.19 & 21 \\
\midrule
Summary (2-stage) & gemma3:12b       & 0.13 & 0.10 & 0.00 & 0.01 & 0.00 & 1  \\
Summary (2-stage) & mistral-nemo:12b & 0.70 & 0.57 & 0.20 & 0.10 & 0.31 & 26 \\
Summary (2-stage) & qwen2.5:14b      & 0.50 & 0.32 & 0.16 & 0.10 & 0.17 & 23 \\
\bottomrule
\end{tabular}
\end{table*}
```

---

## Same table, rendered

| Config | Model | Goal F1 | Goal J⁴ | Item F1 | Item J⁴ | Valence | N |
|---|---|--:|--:|--:|--:|--:|--:|
| *Human ceiling (IAA)* | — | — | **0.76** | — | **0.39** | 0.48 | — |
| Contextual | gemma3:12b | 0.90 | 0.81 | 0.44 | 0.29 | 0.40 | 166 |
| Contextual | mistral-nemo:12b | 0.91 | 0.81 | 0.45 | 0.28 | 0.28 | 151 |
| **Contextual** | **qwen2.5:14b** | **0.90** | **0.81** | **0.46** | **0.31** | **0.36** | **172** |
| Summary (core) | gemma3:12b | 0.51 | 0.33 | 0.06 | 0.04 | 0.00 | 12 |
| Summary (core) | mistral-nemo:12b | 0.74 | 0.61 | 0.04 | 0.02 | 0.00 | 5 |
| Summary (core) | qwen2.5:14b | 0.30 | 0.19 | 0.11 | 0.06 | 0.00 | 14 |
| Summary (1-stage) | gemma3:12b | 0.65 | 0.44 | 0.05 | 0.01 | 0.25 | 4 |
| Summary (1-stage) | mistral-nemo:12b | 0.69 | 0.59 | 0.23 | 0.12 | 0.13 | 32 |
| Summary (1-stage) | qwen2.5:14b | 0.50 | 0.32 | 0.21 | 0.09 | 0.19 | 21 |
| Summary (2-stage) | gemma3:12b | 0.13 | 0.10 | 0.00 | 0.01 | 0.00 | 1 |
| Summary (2-stage) | mistral-nemo:12b | 0.70 | 0.57 | 0.20 | 0.10 | 0.31 | 26 |
| Summary (2-stage) | qwen2.5:14b | 0.50 | 0.32 | 0.16 | 0.10 | 0.17 | 23 |

---

## Appendix — full precision/recall/F1 (vs majority gold, 3 decimals)

| Config | Model | Goal P | Goal R | Goal F1 | Goal J⁴ | Item P | Item R | Item F1 | Item J⁴ | Val(mix) | Val(maj) | N |
|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| Contextual | gemma3:12b | 0.914 | 0.893 | 0.898 | 0.806 | 0.297 | 0.848 | 0.437 | 0.290 | 0.398 | 0.355 | 166 |
| Contextual | mistral-nemo:12b | 0.914 | 0.917 | 0.911 | 0.814 | 0.313 | 0.796 | 0.446 | 0.284 | 0.278 | 0.265 | 151 |
| **Contextual** | **qwen2.5:14b** | 0.914 | 0.893 | 0.898 | 0.806 | 0.317 | 0.886 | 0.464 | 0.305 | 0.360 | 0.279 | 172 |
| Summary (core) | gemma3:12b | 0.952 | 0.364 | 0.510 | 0.330 | 0.100 | 0.044 | 0.061 | 0.041 | 0.000 | 0.000 | 12 |
| Summary (core) | mistral-nemo:12b | 0.907 | 0.652 | 0.737 | 0.605 | 0.071 | 0.032 | 0.044 | 0.018 | 0.000 | 0.000 | 5 |
| Summary (core) | qwen2.5:14b | 0.786 | 0.188 | 0.296 | 0.187 | 0.357 | 0.068 | 0.114 | 0.056 | 0.000 | 0.000 | 14 |
| Summary (1-stage) | gemma3:12b | 0.952 | 0.507 | 0.648 | 0.444 | 0.083 | 0.034 | 0.048 | 0.014 | 0.250 | 0.500 | 4 |
| Summary (1-stage) | mistral-nemo:12b | 0.831 | 0.664 | 0.688 | 0.587 | 0.338 | 0.176 | 0.227 | 0.122 | 0.125 | 0.156 | 32 |
| Summary (1-stage) | qwen2.5:14b | 0.905 | 0.371 | 0.500 | 0.321 | 0.381 | 0.146 | 0.207 | 0.088 | 0.190 | 0.286 | 21 |
| Summary (2-stage) | gemma3:12b | 0.429 | 0.076 | 0.129 | 0.098 | 0.000 | 0.000 | 0.000 | 0.006 | 0.000 | 0.000 | 1 |
| Summary (2-stage) | mistral-nemo:12b | 0.917 | 0.588 | 0.702 | 0.567 | 0.390 | 0.140 | 0.204 | 0.104 | 0.308 | 0.308 | 26 |
| Summary (2-stage) | qwen2.5:14b | 0.905 | 0.371 | 0.500 | 0.321 | 0.271 | 0.121 | 0.158 | 0.095 | 0.174 | 0.261 | 23 |

---

## Selected setup: Contextual (per-utterance) + qwen2.5:14b

Rationale (goal-level is tied across all three contextual models, so the decision is item-level + deployment):

- **Goal level:** all three contextual models are at/above the human ceiling (J⁴ 0.81 ≥ 0.76); config choice dominates model choice.
- **Item level (decisive):** qwen2.5:14b is strongest — F1 **0.46**, J⁴ **0.31** (closest to the human ceiling 0.39).
- **Valence:** qwen 0.36 (2nd; gemma 0.40 is marginally higher but see deployment).
- **Deployment:** qwen2.5:14b (~9 GB) fits fully in a 12 GB GPU; gemma3:12b (10 GB) spills to CPU.

### Methods — final sentence (paste-ready)

> Based on this validation, we adopt the per-utterance (contextual) configuration with
> **qwen2.5:14b** for the full-corpus annotation. On the seven triple-coded sessions it reaches
> goal-level agreement with the human consensus of F1 $=0.90$ (LLM–human Jaccard $=0.81$, at or
> above the human inter-annotator ceiling of $0.76$) and item-level F1 $=0.46$ (Jaccard $=0.31$,
> approaching the human ceiling of $0.39$); the three contextual models are comparable at the goal
> level, and qwen2.5:14b is selected for its strongest item-level agreement and because it runs
> fully within a single 12\,GB GPU (Appendix Table~\ref{tab:llm-human}).

---

## Headline takeaways
1. **Config ≫ model.** Contextual (per-utterance) reaches goal J⁴ 0.81 for **all three** models; the best summary is only 0.61. Summary under-reports (low recall) — it is the annotation *granularity*, not the model, that drives agreement.
2. **Goal-level meets/exceeds the human ceiling:** contextual J⁴ 0.81 > IAA 0.76 for every model; goal F1 0.90–0.91.
3. **Item-level approaches the ceiling but over-generates:** item J⁴ ≈ 0.28–0.31 vs IAA 0.39; high recall, low precision (over-labels items, consistent with the low human item-level IAA).
4. **Model choice is minor within contextual** (goal tied); qwen2.5:14b edges out on item-level and fits the GPU → selected.
5. **Two-stage ≠ better than one-stage** for summary; **valence is hard** (best 0.40 < human 0.48; summary valence N too small to trust).
