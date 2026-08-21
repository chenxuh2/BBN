# =============================================================================
# RQ2-analysis.R
# RQ2: How does actual reflection deviate from the goal-aware task structure?
# H4: reflective attention gravitates toward salient / checklist-explicit actions
#     rather than the goals where performance is weakest -> reflection blind spots.
#
# Inputs:
#   - processed_annotations/reflection_by_goal.csv   (LLM reflection, per session x goal;
#         cols: session, goal, n_reflect, n_positive, n_negative, n_neutral, n_mixed,
#               session_goal_mentions)
#   - performance objects from checklist-analysis.R  (results_self = per Team x Goal self%;
#         bias_stats; SP/graded per-goal if needed)
#
# Packages (already installed / loaded in checklist-analysis.R): tidyverse, rstatix
# Run checklist-analysis.R first (for results_self etc.), or re-load what is needed.
# =============================================================================

library(tidyverse)
library(rstatix)


# -----------------------------------------------------------------------------
# 0. SETUP: load the reflection table; make sure the performance table is available
# -----------------------------------------------------------------------------
# Run checklist-analysis.R first so `results_self` (per Team x Goal self %) is in memory,
# and the working directory is the project root.
refl_raw <- read.csv("processed_annotations/reflection_by_goal.csv",
                     stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(exists("results_self"))   # from checklist-analysis.R


# -----------------------------------------------------------------------------
# 1. LINK session <-> Team ID  (+ canonical goal codes G1-G5)
# -----------------------------------------------------------------------------
# Two keys must align: (a) session -> Team ID (same code, different case), and
# (b) goal names differ between sources, so map both to G1-G5.
refl <- refl_raw %>%
  mutate(
    team_id = toupper(sub("_.*", "", session)),       # dbch18 -> DBCH18 ; cnip_full_... -> CNIP
    gcode = case_when(
      str_detect(goal, "^Goal 1") ~ "G1",
      str_detect(goal, "^Goal 2") ~ "G2",
      str_detect(goal, "^Goal 3") ~ "G3",
      str_detect(goal, "^Goal 4") ~ "G4",
      str_detect(goal, "^Goal 5") ~ "G5",
      TRUE ~ "Other"
    )
  )

# performance (self%) -> Team ID + G-code
perf <- results_self %>%
  transmute(
    team_id = `Team ID`,
    performance = Percentage,
    gcode = case_when(
      Goal == "Establish a Supportive and Professional Environment" ~ "G1",
      Goal == "Assess and Align Expectations"                       ~ "G2",
      Goal == "Deliver the Traumatic News"                          ~ "G3",
      Goal == "Manage the Emotional Response"                       ~ "G4",
      Goal == "Ensure Understanding and Facilitate Closure"         ~ "G5",
      TRUE ~ NA_character_
    )
  )

# which teams appear in both sources?
teams_refl <- unique(refl$team_id)
teams_perf <- unique(perf$team_id)
matched    <- intersect(teams_refl, teams_perf)
cat("reflection teams:", length(teams_refl),
    " | performance teams:", length(teams_perf),
    " | matched:", length(matched), "\n")
cat("reflection-only (no perf):", paste(setdiff(teams_refl, teams_perf), collapse = ", "), "\n")

# grid: every matched team x the 5 goals, with reflection COUNT, SHARE, and thresholded coverage
goals5 <- c("G1", "G2", "G3", "G4", "G5")
refl_counts <- refl %>%
  filter(gcode %in% goals5, team_id %in% matched) %>%
  group_by(team_id, gcode) %>%
  summarise(n_reflect = sum(n_reflect), .groups = "drop")

grid <- expand_grid(team_id = matched, gcode = goals5) %>%
  left_join(refl_counts, by = c("team_id", "gcode")) %>%
  mutate(n_reflect = replace_na(n_reflect, 0L)) %>%
  left_join(perf, by = c("team_id", "gcode")) %>%
  group_by(team_id) %>%
  mutate(session_total = sum(n_reflect),                      # total goal-tagged reflection in session
         share    = ifelse(session_total > 0, n_reflect / session_total, NA_real_),
         covered1 = as.integer(n_reflect >= 1),               # touched at all (saturates ~1)
         covered3 = as.integer(n_reflect >= 3)) %>%           # substantively reflected
  ungroup()


# -----------------------------------------------------------------------------
# 2. REFLECTION COVERAGE ACROSS THE FIVE GOALS  (bullet 1)
# -----------------------------------------------------------------------------
# CONTENT, not volume. Reflection COUNTS are unreliable (annotators' counts varied a lot,
# but their goal SETS agreed: goal Jaccard = 0.76) -- and coverage is the unit we validated.
goal_labels <- c(G1 = "Establish\nEnvironment", G2 = "Assess &\nAlign",
                 G3 = "Deliver\nthe News", G4 = "Manage\nEmotional", G5 = "Ensure\nClosure")

# --- shared publication style (matches checklist-analysis.R: mako viridis + theme_bw) -------
theme_bbn <- theme_bw(base_size = 14) +
  theme(
    legend.position   = "none",
    text              = element_text(family = "sans"),   # Arial/Helvetica for ACM
    axis.title.x      = element_text(face = "bold", margin = margin(t = 8)),
    axis.title.y      = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x       = element_text(color = "black", size = 11, lineheight = 0.9),
    axis.text.y       = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.title        = element_blank(),
    strip.background  = element_rect(fill = "grey92", color = NA),
    strip.text        = element_text(face = "bold", color = "black")
  )
fill_mako  <- scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.9)
color_mako <- scale_color_viridis_d(option = "mako", begin = 0.3, end = 0.9)

# output folder for figures (PNG, 300 dpi, inches -- same as checklist-analysis.R)
fig_dir <- "figures_rq2"
dir.create(fig_dir, showWarnings = FALSE)

# (a) presence coverage saturates -> report as context ("reflection is broad but shallow")
grid %>% group_by(gcode) %>%
  summarise(coverage1 = mean(covered1), .groups = "drop") %>% print()

# (b) PRIMARY: reflective attention SHARE per goal (emphasis / distribution across goals)
#     + substantive coverage (>=3) as a robustness view
attention_by_goal <- grid %>%
  group_by(gcode) %>%
  summarise(mean_share    = mean(share, na.rm = TRUE),
            median_share  = median(share, na.rm = TRUE),
            coverage3     = mean(covered3), .groups = "drop")
print(attention_by_goal)

# FIGURE: attention share by goal (distribution across sessions)
p_share <- ggplot(grid, aes(x = gcode, y = share, fill = gcode)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.5, lwd = 0.7) +
  scale_x_discrete(labels = goal_labels) +
  scale_y_continuous(labels = scales::percent) +
  fill_mako +
  labs(x = NULL, y = "Reflective attention share\n(% of session's reflection)") +
  theme_bbn
p_share
ggsave(file.path(fig_dir, "RQ2_attention_share_by_goal.png"),
       p_share, width = 7, height = 5, units = "in", dpi = 300)


# (c) ITEM BREADTH within each goal (content/depth; discriminates where goal-level 0/1 saturates)
#     "of a goal's items, how many did the session actually reflect on?"  (run reflection_by_item.py)
#     NOTE: item-level LLM agreement is lower (Jaccard 0.45); this is a breadth summary, so we use it
#     for relative across-goal comparison and flag the caveat.
n_items_per_goal <- c(G1 = 7, G2 = 4, G3 = 4, G4 = 5, G5 = 7)   # real checklist items per goal

item_tbl <- read.csv("processed_annotations/reflection_by_item.csv",
                     stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(team_id = toupper(sub("_.*", "", session))) %>%
  filter(team_id %in% matched, goal %in% goals5, grepl("\\.[0-9]", code))  # real items only

items_tg <- item_tbl %>%
  group_by(team_id, goal) %>%
  summarise(n_items = n_distinct(code), .groups = "drop")

grid_item <- expand_grid(team_id = matched, gcode = goals5) %>%
  left_join(items_tg, by = c("team_id" = "team_id", "gcode" = "goal")) %>%
  mutate(n_items = replace_na(n_items, 0L),
         item_coverage = n_items / n_items_per_goal[gcode]) %>%
  left_join(perf, by = c("team_id", "gcode"))

# per-goal summary in the SAME format as RQ1 Fig 1 (N, Median, Q1, Q3, Min) for the boxplot text
item_breadth_by_goal <- grid_item %>%
  group_by(gcode) %>%
  summarise(N = n(),
            Median = median(item_coverage), Q1 = quantile(item_coverage, .25),
            Q3 = quantile(item_coverage, .75), Min = min(item_coverage), .groups = "drop")
print(item_breadth_by_goal)

p_itemcov <- ggplot(grid_item, aes(x = gcode, y = item_coverage, fill = gcode)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.5, lwd = 0.7) +
  scale_x_discrete(labels = goal_labels) +
  scale_y_continuous(labels = scales::percent) +
  fill_mako +
  labs(x = NULL, y = "Item coverage within goal\n(% of goal's items reflected)") +
  theme_bbn
p_itemcov
ggsave(file.path(fig_dir, "RQ2_item_coverage_by_goal.png"),
       p_itemcov, width = 7, height = 5, units = "in", dpi = 300)


# -----------------------------------------------------------------------------
# 3. REFLECTION (item breadth) vs PERFORMANCE: the blind-spot test  (bullet 2, H4)
# -----------------------------------------------------------------------------
# Adaptive reflection would put MORE reflection on the goals a team did WORSE on
# (negative perf<->reflection link). H4 (blind spot) predicts the opposite / none:
# reflection tracks salient, scripted goals (Deliver the News), not weakness.
sg <- grid_item %>% filter(!is.na(performance))   # matched team x goal cells with a score

# 3a. GOAL-LEVEL (5 goals): MEAN item-coverage vs MEAN performance.
#     Correlation across the 5 goal points -> ONE rho (not five). The 5 per-goal rhos are in 3b.
#     (This uses mean, per request; the boxplot's descriptive table above stays median/Q1/Q3/Min.)
goal_summary <- grid_item %>%
  group_by(gcode) %>%
  summarise(mean_coverage = mean(item_coverage),
            mean_perf     = mean(performance, na.rm = TRUE),
            n_teams       = sum(!is.na(performance)), .groups = "drop") %>%
  mutate(rank_perf = rank(-mean_perf),        # 1 = best-performed goal
         rank_cov  = rank(-mean_coverage))    # 1 = most-reflected goal
print(goal_summary)

# do the goals that get the most reflection tend to be the already-strong ones? (5 points -> 1 rho)
gl_cor <- cor.test(goal_summary$mean_perf, goal_summary$mean_coverage,
                   method = "spearman", exact = FALSE)
cat("\nGOAL-LEVEL Spearman(perf, item-coverage): rho =",
    round(gl_cor$estimate, 3), " p =", round(gl_cor$p.value, 3), "\n")
# NOTE: the 2-panel goal-level scatter (self + grader) is built in 3c, once glg is available.

# 3b. SESSION x GOAL: within the matched cells, does reflection breadth track performance?
sg_cor <- cor.test(sg$performance, sg$item_coverage, method = "spearman", exact = FALSE)
cat("\nCELL-LEVEL Spearman(perf, item-coverage): rho =",
    round(sg_cor$estimate, 3), " p =", signif(sg_cor$p.value, 3),
    " (n =", nrow(sg), "cells)\n")

# within each goal: report rho + FDR-corrected p (same convention as RQ1 H2)
per_goal_cor <- sg %>%
  group_by(gcode) %>%
  summarise(rho = cor(performance, item_coverage, method = "spearman"),
            p   = cor.test(performance, item_coverage,
                           method = "spearman", exact = FALSE)$p.value,
            n   = n(), .groups = "drop") %>%
  mutate(p_fdr = p.adjust(p, "fdr"))
print(per_goal_cor)

# FIGURE (per-goal ③): one panel per goal; one point per team; grey line = lm fit.
#   flat / noisy line -> reflection breadth does NOT track that goal's performance (blind spot).
#   rho label per panel; * = FDR-significant.
pg_lab <- per_goal_cor %>%
  mutate(lab = sprintf("rho=%.2f%s  n=%d", rho, ifelse(p_fdr < .05, "*", ""), n))
p_facet_self <- ggplot(sg, aes(performance, item_coverage)) +
  geom_jitter(aes(color = gcode), width = 1.5, height = 0.02, alpha = 0.55, size = 1.6) +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 0.7) +
  geom_text(data = pg_lab, aes(x = -Inf, y = Inf, label = lab),
            hjust = -0.08, vjust = 1.5, size = 3.2, family = "sans", inherit.aes = FALSE) +
  facet_wrap(~ gcode, nrow = 1, labeller = as_labeller(goal_labels)) +
  color_mako +
  scale_x_continuous(labels = scales::label_number(suffix = "%")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Performance (self %)", y = "Item coverage (reflection breadth)") +
  theme_bbn
p_facet_self
ggsave(file.path(fig_dir, "RQ2_perGoal_breadth_vs_self.png"),
       p_facet_self, width = 12, height = 3.6, units = "in", dpi = 300)

# 3c. ROBUSTNESS: repeat against OBJECTIVE expert-grader score (not self-rating).
#     Self% is lenient/ceiling-y; the blind-spot claim is stronger against graded weakness.
stopifnot(exists("grader_agg"))          # from checklist-analysis.R
perf_g <- grader_agg %>%
  transmute(team_id = `Team ID`, perf_g = Grader_Score,
            gcode = case_when(
              Goal == "Establish a Supportive and Professional Environment" ~ "G1",
              Goal == "Assess and Align Expectations"                       ~ "G2",
              Goal == "Deliver the Traumatic News"                          ~ "G3",
              Goal == "Manage the Emotional Response"                       ~ "G4",
              Goal == "Ensure Understanding and Facilitate Closure"         ~ "G5",
              TRUE ~ NA_character_))
if (all(is.na(perf_g$gcode)))
  cat("\n[grader] goal names did not map - check unique(grader_agg$Goal)\n")

gi_g <- grid_item %>% select(team_id, gcode, item_coverage) %>%
  left_join(perf_g, by = c("team_id", "gcode")) %>% filter(!is.na(perf_g))

goal_summary_g <- gi_g %>% group_by(gcode) %>%
  summarise(mean_coverage = mean(item_coverage),
            mean_perf_g   = mean(perf_g), .groups = "drop")
print(goal_summary_g)
glg <- cor.test(goal_summary_g$mean_perf_g, goal_summary_g$mean_coverage,
                method = "spearman", exact = FALSE)
cat("[grader] GOAL-LEVEL Spearman(perf, coverage): rho =",
    round(glg$estimate, 3), " p =", round(glg$p.value, 3), "\n")
sgg <- cor.test(gi_g$perf_g, gi_g$item_coverage, method = "spearman", exact = FALSE)
cat("[grader] CELL-LEVEL Spearman(perf, coverage): rho =",
    round(sgg$estimate, 3), " p =", signif(sgg$p.value, 3),
    " (n =", nrow(gi_g), "cells)\n")

# FIGURE (goal-level ①, 2 panels): breadth vs performance, self (left) and expert grader (right).
# Each panel = the 5 goal means; annotated with that panel's single Spearman rho.
goal_scatter_df <- bind_rows(
  goal_summary   %>% transmute(gcode, perf = mean_perf,   coverage = mean_coverage,
                               anchor = "Self-rating"),
  goal_summary_g %>% transmute(gcode, perf = mean_perf_g, coverage = mean_coverage,
                               anchor = "Standardized patient")
) %>% mutate(anchor = factor(anchor, levels = c("Self-rating", "Standardized patient")))

rho_lab <- tibble::tibble(
  anchor = factor(c("Self-rating", "Standardized patient"),
                  levels = c("Self-rating", "Standardized patient")),
  lab = c(sprintf("rho = %.2f, p = %.2f", gl_cor$estimate, gl_cor$p.value),
          sprintf("rho = %.2f, p = %.2f", glg$estimate,   glg$p.value)))

p_goalscatter <- ggplot(goal_scatter_df, aes(perf, coverage)) +
  geom_point(aes(color = gcode), size = 5) +
  ggrepel::geom_text_repel(aes(label = goal_labels[gcode]), size = 3.3,
                           lineheight = 0.85, fontface = "bold", family = "sans") +
  geom_text(data = rho_lab, aes(x = -Inf, y = Inf, label = lab),
            hjust = -0.06, vjust = 1.6, size = 3.4, family = "sans", inherit.aes = FALSE) +
  facet_wrap(~ anchor, scales = "free_x") +
  color_mako +
  scale_x_continuous(labels = scales::label_number(suffix = "%")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Performance", y = "Mean item coverage (reflection breadth)") +
  theme_bbn
p_goalscatter
ggsave(file.path(fig_dir, "RQ2_breadth_vs_performance_goal.png"),
       p_goalscatter, width = 9, height = 4.2, units = "in", dpi = 300)

# per-goal (③, grader): within each goal, across teams -- report rho + FDR-corrected p (as RQ1)
per_goal_cor_g <- gi_g %>%
  group_by(gcode) %>%
  summarise(rho = cor(perf_g, item_coverage, method = "spearman"),
            p   = cor.test(perf_g, item_coverage,
                           method = "spearman", exact = FALSE)$p.value,
            n   = n(), .groups = "drop") %>%
  mutate(p_fdr = p.adjust(p, "fdr"))
print(per_goal_cor_g)

# FIGURE (per-goal ③, grader): one panel per goal; one point per team; grey line = lm fit.
pg_lab_g <- per_goal_cor_g %>%
  mutate(lab = sprintf("rho=%.2f%s  n=%d", rho, ifelse(p_fdr < .05, "*", ""), n))
p_facet_grader <- ggplot(gi_g, aes(perf_g, item_coverage)) +
  geom_jitter(aes(color = gcode), width = 1.5, height = 0.02, alpha = 0.55, size = 1.6) +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 0.7) +
  geom_text(data = pg_lab_g, aes(x = -Inf, y = Inf, label = lab),
            hjust = -0.08, vjust = 1.5, size = 3.2, family = "sans", inherit.aes = FALSE) +
  facet_wrap(~ gcode, nrow = 1, labeller = as_labeller(goal_labels)) +
  color_mako +
  scale_x_continuous(labels = scales::label_number(suffix = "%")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Performance (standardized patient %)", y = "Item coverage (reflection breadth)") +
  theme_bbn
p_facet_grader
ggsave(file.path(fig_dir, "RQ2_perGoal_breadth_vs_grader.png"),
       p_facet_grader, width = 12, height = 3.6, units = "in", dpi = 300)


# -----------------------------------------------------------------------------
# 4. OVER- / UNDER-REFLECTED GOALS  (bullet 3)
# -----------------------------------------------------------------------------
# Rank goals by performance and by reflection breadth; flag the mismatches.
over_under <- goal_summary %>%
  mutate(rank_gap = rank_perf - rank_cov,          # >0: reflected MORE than its perf rank
         label = case_when(
           rank_cov <= 2 & rank_perf <= 2 ~ "over-reflected (already strong)",
           rank_cov >= 4 & rank_perf >= 4 ~ "blind spot (weak & under-reflected)",
           rank_cov >= 4 & rank_perf <= 2 ~ "under-reflected despite strength",
           rank_cov <= 2 & rank_perf >= 4 ~ "adaptive (weak & reflected)",
           TRUE ~ "-")) %>%
  arrange(rank_perf) %>%
  select(gcode, mean_perf, mean_coverage, rank_perf, rank_cov, rank_gap, label)
print(over_under)


# -----------------------------------------------------------------------------
# 5. (OPTIONAL) VALENCE TIE-IN to RQ1 miscalibration
# -----------------------------------------------------------------------------
# Connect RQ1 (students under-rate themselves on Ensure Closure) to RQ2:
# - for the under-reflected / weak goals, what is the valence mix of the reflection
#   that does occur? (n_positive / n_negative / n_neutral / n_mixed per goal)
# - story: weak goals are both under-reflected AND (mis)judged -> blind spot mechanism.
# TODO


# -----------------------------------------------------------------------------
# 6. (OPTIONAL) QUALITATIVE EXAMPLES
# -----------------------------------------------------------------------------
# - pull a few example reflective utterances for an over-reflected goal (e.g. Deliver News)
#   and an under-reflected goal (e.g. Ensure Closure) from the per-utterance LLM output.
# - illustrative quotes for the paper (Part 2 design motivation).
# TODO
