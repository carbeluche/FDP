
# STEP 2: Statistical assumptions checks (normality, homogeneity)
install.packages("performance")
install.packages("patchwork")
library(performance)
library(patchwork)
# SUS Score
plot_qq_sus <- ggplot(data, aes(sample = SUS_Score)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Q-Q Plot: SUS Score") +
  theme_minimal()
plot_density_sus <- ggplot(data, aes(x = SUS_Score)) +
  geom_density(fill = "#c2edec", alpha = 0.6) +
  labs(title = "Density: SUS Score") +
  theme_minimal()
# Task Completion Time
plot_qq_time <- ggplot(data, aes(sample = Task_Completion_Time)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Q-Q Plot: Task Completion Time") +
  theme_minimal()
plot_density_time <- ggplot(data, aes(x = Task_Completion_Time)) +
  geom_density(fill = "#f2e7bd", alpha = 0.6) +
  labs(title = "Density: Task Completion Time") +
  theme_minimal()
# Likert Score
plot_qq_likert <- ggplot(data, aes(sample = Likert_Score)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Q-Q Plot: Likert Score") +
  theme_minimal()
plot_density_likert <- ggplot(data, aes(x = Likert_Score)) +
  geom_density(fill = "#e6c3e8", alpha = 0.6) +
  labs(title = "Density: Likert Score") +
  theme_minimal()
figure_6 <- (plot_qq_sus | plot_density_sus) /
            (plot_qq_time | plot_density_time) /
            (plot_qq_likert | plot_density_likert) +
  plot_annotation(title = "Figure 6: Complementary Graphs of the Shapiro-Wilk Test")
figure_6

# Fit linear models
lm_sus <- lm(SUS_Score ~ DisabilityType, data = data)
lm_time <- lm(Task_Completion_Time ~ DisabilityType, data = data)
lm_likert <- lm(Likert_Score ~ DisabilityType, data = data)
# Function to create residuals vs fitted plot
resid_plot <- function(model, title) {
  df <- data.frame(
    Fitted = fitted(model),
    Residuals = resid(model)
  )
  ggplot(df, aes(x = Fitted, y = Residuals)) +
    geom_point(color = "#2c3e50", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = title, x = "Fitted Values", y = "Residuals") +
    theme_minimal()
}
# Generate plots
resid_sus_plot <- resid_plot(lm_sus, "SUS Score: Residuals vs Fitted")
resid_time_plot <- resid_plot(lm_time, "Task Completion Time: Residuals vs Fitted")
resid_likert_plot <- resid_plot(lm_likert, "Likert Score: Residuals vs Fitted")
# Combine them into one figure (Figure 7)
figure_7 <- resid_sus_plot / resid_time_plot / resid_likert_plot +
  plot_annotation(title = "Figure 7: Residuals vs Fitted - Homogeneity of Variance")
# Display
figure_7


# STEP 3: ANOVA models with aov_ez()
install.packages("afex")
install.packages("emmeans")
library(afex)
library(emmeans)
anova_sus <- aov_ez(
  id = "Student",
  dv = "SUS_Score",
  within = "DisabilityType",
  between = "Gender",
  data = data
)
anova_time <- aov_ez(
  id = "Student",
  dv = "Task_Completion_Time",
  within = "DisabilityType",
  between = "Gender",
  data = data
)
anova_likert <- aov_ez(
  id = "Student",
  dv = "Likert_Score",
  within = "DisabilityType",
  between = "Gender",
  data = data
)
summary(anova_sus)
summary(anova_time)
summary(anova_likert)


# STEP 4: Post hoc comparisons
emm_sus <- emmeans(anova_sus, pairwise ~ DisabilityType, adjust = "tukey")
emm_sus$contrasts  # Table 7
emm_time <- emmeans(anova_time, pairwise ~ DisabilityType, adjust = "tukey")
emm_time$contrasts  # Table 8
emm_likert <- emmeans(anova_likert, pairwise ~ DisabilityType, adjust = "tukey")
emm_likert$contrasts  # Table 9


# ADDITIONAL SCATTERPLOTS

# Add Number_of_Errors
data$Number_of_Errors <- c(
  0, 0, 1, 2, 1, 0, 2, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0,
  0, 1, 1, 0, 1, 1, 2, 2, 0, 1, 0, 1, 0, 2, 1, 0, 1, 2, 1, 0,
  0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 2, 0, 0, 1, 0, 2, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 2, 1, 0, 2, 0, 0, 0, 1, 0, 1, 0, 2, 0, 0,
  2, 0, 1, 0, 0, 0, 1, 0, 0,0
)
# Verification
stopifnot(nrow(data) == length(data$Number_of_Errors))
# SUS Score vs Task Completion Time
plot1 <- ggplot(data, aes(x = SUS_Score, y = Task_Completion_Time)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "SUS Score vs. Task Completion Time",
    x = "SUS Score",
    y = "Task Completion Time (sec)"
  ) +
  theme_minimal()
# SUS Score vs Number of Errors
plot2 <- ggplot(data, aes(x = SUS_Score, y = Number_of_Errors)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "SUS Score vs. Number of Errors",
    x = "SUS Score",
    y = "Number of Errors"
  ) +
  theme_minimal()
# Likert Score vs Task Completion Time
plot3 <- ggplot(data, aes(x = Likert_Score, y = Task_Completion_Time)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(
    title = "Likert Score vs. Task Completion Time",
    x = "Likert Satisfaction Score",
    y = "Task Completion Time (sec)"
  ) +
  theme_minimal()
# Figura 9 
figure_9 <- (plot1 | plot2) / plot3 + plot_annotation(title = "Figure 9: Scatterplots Illustrating Trends")
figure_9


# STEP 5: Correlation analysis
cor_sus_time <- cor.test(data$SUS_Score, data$Task_Completion_Time, method = "pearson")
cor_sus_errors <- cor.test(data$SUS_Score, data$Number_of_Errors, method = "pearson")
cor_likert_time <- cor.test(data$Likert_Score, data$Task_Completion_Time, method = "pearson")
cat("SUS_Score vs Task_Completion_Time:\n")
print(cor_sus_time)
cat("\nSUS_Score vs Number_of_Errors:\n")
print(cor_sus_errors)
cat("\nLikert_Score vs Task_Completion_Time:\n")
print(cor_likert_time)


# STEP 6: Categorical Analysis (contingency table + Fisher's test)
# DisabilityType is within-subject (each subject does 3 simulations) and Gender is between-subject
data$AssistanceNeeded <- factor(data$AssistanceNeeded, levels = c("NO", "YES"))
data$DisabilityType <- factor(data$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
table_11 <- table(data$DisabilityType, data$AssistanceNeeded)
print(table_11)  

# Check expected counts
expected_counts <- chisq.test(table_11)$expected
print(expected_counts)
# If any expected count is < 5, use Fisher's test, else use Chi-squared
if (any(expected_counts < 5)) {
  test_result <- fisher.test(table_11)
  test_used <- "Fisher’s Exact Test"
} else {
  test_result <- chisq.test(table_11)
  test_used <- "Chi-Squared Test"
}
cat("Test Used:", test_used, "\n")
cat("p-value:", round(test_result$p.value, 3), "\n")
