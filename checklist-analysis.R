
# set working directory
# setwd()
setwd("~/BBN-PC-local")
# 1. Install the package if you haven't already
# install.packages("readxl")

# 2. Load the library
library(readxl)
library(tidyverse)

# 3. Read the file
df_bbn <- read_excel("1x_BBN IPE scores 2016-2019.xlsx")
df_bbn %>% filter(`Student Type` == "MED" & `Source` == "Self") -> df_med_self
df_bbn %>% filter(`Student Type` == "MED" & `Source` == "Graded") -> df_med_graded


# Cleaning 
df_med_self <- df_med_self %>%
  mutate(`Question Text` = str_replace_all(`Question Text`, "[\r\n]+", " "), # Replace line breaks with a space
         `Question Text` = str_squish(`Question Text`))                   # Remove double spaces and trim ends

df_med_graded <- df_med_graded %>%
  mutate(`Question Text` = str_replace_all(`Question Text`, "[\r\n]+", " "), # Replace line breaks with a space
         `Question Text` = str_squish(`Question Text`))                   # Remove double spaces and trim ends


# Reference table 
# Create the reference dictionary
unique_questions_self <- unique(df_med_self$`Question Text`)
unique_questions_graded <- unique(df_med_graded$`Question Text`)

# Create a clean data frame with those questions and an empty "Goal" column
goal_mapping_self <- data.frame(
  `Question Text` = unique_questions_self,
  Goal = NA_character_, # Fills the column with NA (character type)
  Include = TRUE,
  check.names = FALSE, 
  stringsAsFactors = FALSE
)

goal_mapping_graded <- data.frame(
  `Question Text` = unique_questions_graded,
  Goal = NA_character_, # Fills the column with NA (character type)
  Include = TRUE,
  check.names = FALSE, 
  stringsAsFactors = FALSE
)

# Check the new mapping shell
head(goal_mapping_self) 
# Export the empty mapping shell to the current working directory
# ONLY COMMENT IN BELOW TWO LINE FOR NEW 
# write.csv(goal_mapping_self, "goal_mapping_self_shell.csv", row.names = FALSE)
# write.csv(goal_mapping_graded, "goal_mapping_graded_shell.csv", row.names = FALSE)

# Import the completed mapping table
mapping_self <- read.csv("goal_mapping_self_shell.csv", check.names = FALSE, stringsAsFactors = FALSE)
mapping_graded <- read.csv("goal_mapping_graded_shell.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Merge the goals back into medical dataset
med_self_final <- left_join(df_med_self, mapping_self, by = "Question Text")
med_graded_final <- left_join(df_med_graded, mapping_graded, by = "Question Text")

# Check your work to make sure the goals attached correctly
head(med_self_final)
head(med_graded_final)

##-----------------------------------------##
# look at how well each person performs in terms of a goal 
# $$\text{Performance \%} = \left( \frac{\text{Sum of Item Scores}}{\text{Total Items} \times 2} \right) \times 100$$
##-----------------------------------------##

results_self <- med_self_final %>%
  mutate(score_numeric = as.numeric(Score)) %>%
  filter(Include == TRUE) %>% 
  # 1. Group by Team and Goal
  group_by(`Team ID`, Goal) %>%
  
  # 2. Calculate the metrics
  summarise(
    Total_Score = sum(score_numeric, na.rm = TRUE),    # Sum of points received
    Item_Count = n(),                          # Count of items in this goal
    Max_Possible = Item_Count * 2,             # Calculate max possible points
    Percentage = (Total_Score / Max_Possible) * 100, # Option A Calculation
    .groups = "drop"                           # Ungroup after calculating
  )

# View the results
print(results_self)

gp1 <- ggplot(results_self, aes(x = str_wrap(Goal, width = 15), y = Percentage, fill = Goal)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1.5, lwd = 0.7) + # Thicker lines for readability
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.9) + # Colorblind friendly & high contrast
  theme_bw(base_size = 14) + # Use theme_bw for a clean border, larger base font
  labs(
    y = "Score Percentage (%)",
    x = NULL # Remove redundant "Goal" label as categories are self-explanatory
  ) +
  theme(
    legend.position = "none",
    # Specific font adjustments for publication
    text = element_text(family = "sans"), # Arial/Helvetica is standard for ACM plots
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(color = "black", size = 11, lineheight = 0.9),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(), # Remove vertical lines to reduce clutter
    panel.grid.minor = element_blank(),
    plot.title = element_blank() # Titles are usually handled by LaTeX \caption
  ) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25))
gp1
# Saves the last plot displayed in your 'Plots' pane
ggsave("BBN_Performance_Distribution.png", width = 7, height = 5, units = "in", dpi = 300)

library(rstatix)
# Prevents scientific notation for the rest of the session
options(scipen = 999)
# Pairwise comparisons with Holm correction (better than Bonferroni for power)
pwc <- results_self %>%
  wilcox_test(
    Percentage ~ Goal, 
    paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

print(pwc)

##-----------------------------------------##
# Items within goals 
##-----------------------------------------##

# 1. Calculate the average score (Difficulty) for every item
item_analysis <- med_self_final %>%
  # Ensure score is numeric first
  mutate(score_numeric = as.numeric(Score)) %>%
  filter(Include == TRUE) %>% 
  group_by(Goal, `Question Text`) %>% # Group by specific item within goal
  summarise(
    Avg_Item_Score = mean(score_numeric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Sort from lowest score to highest to spot the hard ones easily
  arrange(Avg_Item_Score)

# Print the bottom 5 items (The hardest ones)
print(head(item_analysis, 5))

gp2 <- ggplot(item_analysis, aes(x = Avg_Item_Score, y = reorder(str_trunc(`Question Text`, 50, side = "right"), Avg_Item_Score), color = Goal)) +
  geom_point(size = 4) +
  
  # Add a vertical line at 1.0 (Middle score) for reference
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  
  # Split the chart by Goal so you can compare easily
  facet_grid(Goal ~ ., scales = "free_y", space = "free_y") +
  
  theme_bw() +
  labs(
    title = "Which Items are Driving the Results?",
    subtitle = "Average Score (0-2) for each item per Goal",
    x = "Average Item Score (0 = Fail, 2 = Perfect)",
    y = NULL # Hide y-axis label as the text itself is the label
  ) +
  theme(legend.position = "none") # Hide legend since Facets handle the grouping
gp2

##-----------------------------------------##
# Overall performance ~ Goal regression
##-----------------------------------------##

 
# Step 1: Calculate the percentage score for the SPECIFIC goals and pivot them into columns
specific_goals <- med_self_final %>%
  filter(Include == TRUE) %>%
  mutate(score_numeric = as.numeric(Score)) %>%
  group_by(`Team ID`, Goal) %>%
  summarise(
    Total_Score = sum(score_numeric, na.rm = TRUE),
    Max_Possible = n() * 2,
    Percentage = ifelse(Max_Possible > 0, (Total_Score / Max_Possible) * 100, 0),
    .groups = "drop"
  ) %>%
  select(`Team ID`, Goal, Percentage) %>%
  pivot_wider(names_from = Goal, values_from = Percentage)

# Step 2: Extract the OVERALL "ALL" score for each team
# (Assuming the overall rating is stored in the 'Score' column for that row)
overall_scores <- med_graded_final %>%
  filter(Goal == "ALL", 
         Score != "N.A.") %>%
  # If "Percent Score" is better for "ALL", change 'Score' to `Percent Score` below
  mutate(Overall_Rating = as.numeric(`Percent Score`)) %>% 
  select(`Team ID`, Overall_Rating)  

# Step 3: Merge them together to create our regression dataset
regression_data <- inner_join(specific_goals, overall_scores, by = "Team ID")

# Step 4: Clean up column names 
# (The lm() function gets confused by spaces and special characters in column names)
names(regression_data) <- make.names(names(regression_data))

# Step 5: Run the Multiple Linear Regression
# The dot (.) means "predict Overall_Rating using every other column EXCEPT Team.ID"
model <- lm(Overall_Rating ~ . - Team.ID, data = regression_data)

# Print the results!
summary(model)

##-----------------------------------------##
# A quick check for the correlation
##-----------------------------------------##

cor_data <- regression_data %>%
  select(where(is.numeric))

# Clean up the column names for a prettier plot (replaces dots with spaces)
names(cor_data) <- gsub("\\.", " ", names(cor_data))

# 2. Compute the correlation matrix
# Using Spearman correlation again, as it's robust for scores and ratings
cor_matrix <- cor(cor_data, 
                  use = "pairwise.complete.obs", 
                  method = "spearman")

# 3. Plot the correlation heatmap
library(ggcorrplot)
p_mat <- cor_pmat(cor_data, method = "spearman")

# 4. Plot the matrix with significance thresholds applied
cor_plot_sig <- ggcorrplot(cor_matrix, 
                           hc.order = TRUE,       
                           type = "lower",        
                           lab = TRUE,            
                           lab_size = 4,          
                           p.mat = p_mat,          # <-- Feeds the p-values into the plot
                           sig.level = 0.05,       # <-- Sets the significance threshold at p < 0.05
                           insig = "blank",        # <-- Hides any square that is NOT significant! 
                           #     (Change to "pch" if you prefer them crossed out with an X)
                           outline.color = "white",
                           colors = c("#6D9EC1", "white", "#E46726"), 
                           title = "Significant Correlations: Goals vs. Overall Rating") +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

# Display the plot
print(cor_plot_sig)

##-----------------------------------------##
# FDR 
##-----------------------------------------##

library(dplyr)
library(tidyr)
library(purrr)

# 1. Pivot the 27 items into a wide format
items_wide <- med_self_final %>%
  filter(Goal != "ALL", Include == TRUE) %>%
  mutate(score_numeric = as.numeric(Score)) %>%
  # Ensure we have one row per team
  group_by(`Team ID`, `Question Text`) %>%
  summarise(score_numeric = mean(score_numeric, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = `Question Text`, values_from = score_numeric)

# 2. Join with the Overall Ratings
# (Using the 89-team 'overall_scores' dataframe from your previous join)
analysis_df <- inner_join(items_wide, overall_scores, by = "Team ID")

# 3. Clean up column names for R stability
item_names <- setdiff(names(analysis_df), c("Team ID", "Overall_Rating"))

# 4. Run the correlation tests
item_correlations <- map_df(item_names, function(item_col) {
  
  item_scores <- analysis_df[[item_col]]
  overall_scores <- analysis_df$Overall_Rating
  
  sd_val <- sd(item_scores, na.rm = TRUE)
  print(sd_val)
  
  # Check if the item actually has variance
  if (!is.na(sd_val) && sd_val > 0) {
    
    test <- cor.test(item_scores, overall_scores, method = "spearman", exact = FALSE)
    
    return(data.frame(
      Item = item_col,
      Correlation = test$estimate,
      P_Value = test$p.value,
      stringsAsFactors = FALSE
    ))
    
  } else {
    
    # If it is NA or 0 variance, skip it peacefully
    return(data.frame(
      Item = item_col,
      Correlation = NA,
      P_Value = NA,
      stringsAsFactors = FALSE
    ))
  }
})

# 4. Apply FDR correction to the valid tests
final_item_results <- item_correlations %>%
  filter(!is.na(P_Value)) %>% 
  mutate(Adj_P_Value = p.adjust(P_Value, method = "fdr")) %>%
  arrange(Adj_P_Value) # Sort most significant to the top

print(final_item_results)

##-----------------------------------------##
# A bias check 
##-----------------------------------------##

# 1. Aggregate Self Scores per Goal
# We take the mean of the 0-2 scores and multiply by 50 to convert to a 0-100% scale
self_agg <- med_self_final %>%
  filter(Goal != "ALL", Include == TRUE, !is.na(Score)) %>%
  mutate(score_num = as.numeric(Score)) %>%
  group_by(`Team ID`, Goal) %>%
  summarise(Self_Score = mean(score_num, na.rm = TRUE) * 50, .groups = "drop") 

# 2. Aggregate Grader Scores per Goal
grader_agg <- med_graded_final %>%
  filter(Goal != "ALL", Include == TRUE, !is.na(Score)) %>%
  mutate(score_num = as.numeric(Score)) %>%
  group_by(`Team ID`, Goal) %>%
  summarise(Grader_Score = mean(score_num, na.rm = TRUE) * 50, .groups = "drop")

# 3. Join the datasets and calculate the Bias
bias_df <- inner_join(self_agg, grader_agg, by = c("Team ID", "Goal")) %>%
  mutate(Bias = Self_Score - Grader_Score)

# 4. Run the Statistical Tests (Paired Wilcoxon for non-normal survey data)
bias_stats <- bias_df %>%
  group_by(Goal) %>%
  summarise(
    Mean_Self = mean(Self_Score, na.rm = TRUE),
    Mean_Grader = mean(Grader_Score, na.rm = TRUE),
    Mean_Bias = mean(Bias, na.rm = TRUE),
    P_Value = wilcox.test(Self_Score, Grader_Score, paired = TRUE, exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  # Apply FDR correction for running 5 separate tests
  mutate(
    Adj_P_Value = p.adjust(P_Value, method = "fdr"),
    Significance = ifelse(Adj_P_Value < 0.05, "Significant", "Not Significant")
  ) %>%
  arrange(desc(Mean_Bias)) # Sort from most overconfident to most underconfident

# 5. Print the results
print(bias_stats)

##-----------------------------------------##
## Check if graded goals predict performance 
##-----------------------------------------##

library(dplyr)
library(tidyr)
library(broom)

# 1. Calculate Goal Percentages for the Graded Data
graded_goal_pct <- med_graded_final %>%
  filter(Goal != "ALL", Include == TRUE, !is.na(Score)) %>%
  mutate(score_numeric = as.numeric(Score)) %>%
  group_by(`Team ID`, Goal) %>%
  summarise(Pct_Score = (sum(score_numeric, na.rm=TRUE) / (n()*2)) * 100, .groups = "drop")

# 2. Pivot to Wide Format
graded_wide <- graded_goal_pct %>%
  pivot_wider(names_from = Goal, values_from = Pct_Score)

# 3. Join with the SP's Overall Ratings
# (Using the 'overall_scores' dataframe you made earlier)
graded_regression_data <- inner_join(graded_wide, overall_scores, by = "Team ID")

# Clean column names so the lm() function doesn't crash on spaces
names(graded_regression_data) <- make.names(names(graded_regression_data))

# 4. Run the Multiple Regression
# We predict the Overall_Rating using everything EXCEPT the Team.ID
graded_model <- lm(Overall_Rating ~ . - Team.ID, data = graded_regression_data)

# 5. The Moment of Truth
summary(graded_model)


##-----------------------------------------##
# Lasso regression to check the items 
##-----------------------------------------##


# Step 1: Clean the Overall dataframe
# We only want the ID and the Score (renamed to avoid confusion)
overall_clean <- overall %>%
  select(`Team ID`, Overall_Score = Score) %>% 
  # Note: If 'overall' has multiple questions, filter for the specific one you want first:
  # filter(`Question Text` == "Global Rating") %>%
  distinct() # Removes duplicates if any

# Step 2: Merge it into your main dataframe
analysis_df <- df_clean %>%
  left_join(overall_clean, by = "Team ID")

# Now every row in 'analysis_df' has the specific item score AND the team's overall score
head(analysis_df)

library(janitor) # Helps clean column names

# 1. Prepare Data (Convert to Numeric FIRST)
lasso_data <- analysis_df %>%
  # Select columns
  select(`Team ID`, `Question Text`, Score, Overall_Score) %>%
  
  # --- THE FIX IS HERE ---
  # Force Score to be a number. 
  # If you have text like "N/A" or "-", this turns them into NA (which we fix later)
  mutate(Score = as.numeric(Score)) %>%
  
  # Pivot to Wide
  pivot_wider(
    names_from = `Question Text`, 
    values_from = Score,
    values_fill = 0  # Now this works because 0 matches the numeric column
  ) %>%
  
  # Clean names for safe math
  clean_names() %>%
  
  # Remove rows missing the outcome variable
  drop_na(overall_score)

library(glmnet)

# 1. Create the Predictor Matrix (X)
# Exclude ID and Outcome columns. 
# data.matrix() automatically handles the conversion to a numeric matrix.
x <- data.matrix(lasso_data %>% select(-team_id, -overall_score))

# 2. Create the Outcome Vector (Y)
y <- lasso_data$overall_score
y <- as.numeric(as.character(y))
set.seed(123) # For reproducible results

# Run Lasso (alpha = 1) with Cross-Validation
cv_fit <- cv.glmnet(x, y, alpha = 1)

# Plot the error curve
plot(cv_fit)

# 1. Get coefficients for the "1 Standard Error" model (Conservative/Stricter)
coef_obj <- coef(cv_fit, s = "lambda.1se") 
coef_obj <- coef(cv_fit, s = "lambda.min")
# 2. Turn into a clean table of survivors
active_items <- data.frame(
  Item = rownames(coef_obj), 
  Coefficient = as.numeric(coef_obj)
) %>%
  filter(Coefficient != 0) %>%       # Remove the deleted items
  filter(Item != "(Intercept)") %>%  # Remove intercept
  arrange(desc(abs(Coefficient)))    # Sort by impact

# View the critical checklist items
print(active_items)

# 1. Get the names of the 9 items from your active_items list
selected_vars <- active_items$Item

# 2. Create the formula using the CORRECT column name (likely lowercase)
# CHANGE "Overall_Score" to "overall_score"
formula_str <- paste("overall_score ~", paste(selected_vars, collapse = " + "))

# 3. Run the model again
final_model <- lm(as.formula(formula_str), data = lasso_data)
summary(final_model)


