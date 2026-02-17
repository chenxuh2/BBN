
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
df_bbn %>% filter(`Student Type` == "MED" & `Category Name` == "BBN IPE MED student Self-assessment") -> df_med

# Check the data
head(df_med)

item_full <- unique(df_med$`Question Text`)
item_noCollab <- item_full[1:27]
goal <- c( rep("Establish a Supportive and Professional Environment",5), 
         "Assess and Align Expectations", 
         rep("Deliver the News", 4),
         "Manage the Emotional Response", 
         rep("Ensure Understanding and Facilitate Closure", 2),
         "Establish a Supportive and Professional Environment", 
         "Manage the Emotional Response", 
         "Establish a Supportive and Professional Environment", 
         "Manage the Emotional Response", 
         "Assess and Align Expectations",
         rep("Manage the Emotional Response", 2),
         "Ensure Understanding and Facilitate Closure",
         "Assess and Align Expectations",
         "Establish a Supportive and Professional Environment",
         rep("Ensure Understanding and Facilitate Closure", 4)
         )

# only rows with these items 
df_clean <- df_med[df_med$`Question Text` %in% item_noCollab, ]
df_clean$goal <- goal[match(df_clean$`Question Text`, item_noCollab)]

# look at how well each person performs in terms of a goal 
# $$\text{Performance \%} = \left( \frac{\text{Sum of Item Scores}}{\text{Total Items} \times 2} \right) \times 100$$

results_self <- df_clean %>%
  mutate(score_numeric = as.numeric(Score)) %>%
  # 1. Group by Team and Goal
  group_by(`Team ID`, goal) %>%
  
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

gp1 <- ggplot(results_self, aes(x = stringr::str_wrap(goal, width = 10), y = Percentage, fill = goal)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Performance by Goal",
    y = "Score Percentage (%)",
    x = "Goal"
  ) +
  theme(legend.position = "none") # Hide legend if x-axis labels are enough

library(rstatix)
# Prevents scientific notation for the rest of the session
options(scipen = 999)
# Pairwise comparisons with Holm correction (better than Bonferroni for power)
pwc <- results_self %>%
  wilcox_test(
    Percentage ~ goal, 
    paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

print(pwc)

# Look at ensure understanding 

# 1. Calculate the average score (Difficulty) for every item
item_analysis <- df_clean %>%
  # Ensure score is numeric first
  mutate(score_numeric = as.numeric(Score)) %>%
  group_by(goal, `Question Text`) %>% # Group by specific item within goal
  summarise(
    Avg_Item_Score = mean(score_numeric, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Sort from lowest score to highest to spot the hard ones easily
  arrange(Avg_Item_Score)

# Print the bottom 5 items (The hardest ones)
print(head(item_analysis, 5))

gp2 <- ggplot(item_analysis, aes(x = Avg_Item_Score, y = reorder(str_trunc(`Question Text`, 50, side = "right"), Avg_Item_Score), color = goal)) +
  geom_point(size = 4) +
  
  # Add a vertical line at 1.0 (Middle score) for reference
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  
  # Split the chart by Goal so you can compare easily
  facet_grid(goal ~ ., scales = "free_y", space = "free_y") +
  
  theme_bw() +
  labs(
    title = "Which Items are Driving the Results?",
    subtitle = "Average Score (0-2) for each item per Goal",
    x = "Average Item Score (0 = Fail, 2 = Perfect)",
    y = NULL # Hide y-axis label as the text itself is the label
  ) +
  theme(legend.position = "none") # Hide legend since Facets handle the grouping

# Overall performance 
df_bbn %>% filter(`Student Type` == "MED" & `Category Name` == "BBN Overall Perform") -> overall

# 1. Reshape the Goal Data (Long -> Wide)
# This takes the "goal" column and turns it into multiple columns (e.g., goal_Empathy, goal_Clarity)
goals_wide <- results_self %>%
  select(`Team ID`, goal, Percentage) %>%
  pivot_wider(
    names_from = goal, 
    values_from = Percentage,
    names_prefix = "Goal_" # Adds a prefix to avoid confusion
  )

# 2. Merge with Satisfaction Data
satisfaction_df <- goals_wide %>%
  left_join(overall %>% select(`Team ID`, Score, `Percent Score`, `Max Score`, Response), by = "Team ID")

head(satisfaction_df)

# 1. Convert the column to numeric
satisfaction_df <- satisfaction_df %>%
  mutate(`Percent Score` = as.numeric(`Percent Score`))

# Check if it worked (it should now say <dbl>)
glimpse(satisfaction_df)

# Calculate correlation matrix
library(tidyverse)

library(tidyverse)
library(Hmisc) # Required for rcorr()

# 1. Prepare Data
# Select only numeric columns
matrix_data <- satisfaction_df %>%
  select(starts_with("Goal_"), `Percent Score`) %>%
  as.matrix()

# 2. Run Correlation + P-values
# rcorr returns a list with $r (correlation) and $P (p-values)
res <- rcorr(matrix_data)

# 3. Helper Function to Flatten the Matrices
# This turns the square matrices into a Long Dataframe with Cor and P-value columns
flatten_corr <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    Var1 = rownames(cormat)[row(cormat)[ut]],
    Var2 = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut],
    p    = pmat[ut]
  )
}

# 4. Create the Plotting Dataframe
plot_data <- flatten_corr(res$r, res$P) %>%
  mutate(
    # Create a label: Check if p < 0.05 to add a star
    sig_label = ifelse(p < 0.05, paste0(round(cor, 2), "*"), round(cor, 2)),
    
    # Clean up names for the plot
    Var1 = str_replace(Var1, "Goal_", "") %>% str_wrap(15),
    Var2 = str_replace(Var2, "Goal_", "") %>% str_wrap(15)
  )

ggplot(plot_data, aes(x = Var1, y = Var2, fill = cor)) +
  geom_tile(color = "white") +
  
  # Use the label with the asterisk (*)
  geom_text(aes(label = sig_label), color = "black", size = 3) +
  
  scale_fill_gradient2(
    low = "#4575b4", mid = "white", high = "#d73027", 
    midpoint = 0, limit = c(-1, 1), name = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_fixed()

# The asterisk (*) tests for interaction between two variables
# Linear Regression with all 5 goals
full_model <- lm(`Percent Score` ~ `Goal_Assess and Align Expectations` + 
                   `Goal_Deliver the News` + 
                   `Goal_Ensure Understanding and Facilitate Closure` + 
                   `Goal_Establish a Supportive and Professional Environment` + 
                   `Goal_Manage the Emotional Response`, 
                 data = satisfaction_df)

summary(full_model)

# Lasso regression to check the items 

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
