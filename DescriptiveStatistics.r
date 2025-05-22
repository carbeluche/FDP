# BOXPLOTS BY DISABILITY TYPE

# Load the required packages
install.packages("ggplot2")
install.packages("readr")
install.packages("gridExtra")
library(ggplot2)
library(readr)
library(gridExtra)
# Manually enter the data from the Excel (Student ID repeated for each simulation) # nolint: line_length_linter.
data <- data.frame(
    Student = c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10,11,11,11,12,12,12,13,13,13,14,14,14,15,15,15,16,16,16,17,17,17,18,18,18,19,19,19,20,20,20,21,21,21,22,22,22,23,23,23,24,24,24,25,25,25,26,26,26,27,27,27,28,28,28,29,29,29,30,30,30), # nolint
    Gender = c(
        "MALE", "MALE", "MALE",
        "FEMALE", "FEMALE", "FEMALE",
        "FEMALE", "FEMALE", "FEMALE",
        "FEMALE", "FEMALE", "FEMALE",
        "MALE", "MALE", "MALE",
        "FEMALE", "FEMALE", "FEMALE",
        "FEMALE", "FEMALE", "FEMALE",
        "MALE", "MALE", "MALE",
        "FEMALE", "FEMALE", "FEMALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "FEMALE", "FEMALE", "FEMALE",
        "FEMALE", "FEMALE", "FEMALE",
        "FEMALE", "FEMALE", "FEMALE",
        "FEMALE", "FEMALE", "FEMALE",
        "FEMALE", "FEMALE", "FEMALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "MALE", "MALE", "MALE",
        "FEMALE", "FEMALE", "FEMALE"),
    DisabilityType = c(
        "Motor", "Auditory", "Visual",
        "Motor", "Visual", "Auditory",
        "Visual", "Auditory", "Motor",
        "Auditory", "Visual", "Motor",
        "Motor", "Auditory", "Visual",
        "Visual", "Auditory", "Motor",
        "Auditory", "Motor", "Visual",
        "Visual", "Auditory", "Motor",
        "Auditory", "Visual", "Motor",
        "Visual", "Auditory", "Motor",
        "Auditory", "Motor", "Visual",
        "Auditory", "Visual", "Motor",
        "Visual", "Auditory", "Motor",
        "Auditory", "Motor", "Visual",
        "Visual", "Auditory", "Motor",
        "Visual", "Auditory", "Motor",
        "Motor", "Visual", "Auditory",
        "Visual", "Motor", "Auditory",
        "Auditory", "Motor", "Visual",
        "Visual", "Auditory", "Motor",
        "Auditory", "Visual", "Motor",
        "Visual", "Motor", "Auditory",
        "Visual", "Auditory", "Motor",
        "Motor", "Auditory", "Visual",
        "Auditory", "Visual", "Motor",
        "Visual", "Motor", "Auditory",
        "Motor", "Auditory", "Visual",
        "Motor", "Visual", "Auditory",
        "Visual", "Motor", "Auditory",
        "Visual", "Auditory", "Motor"),
    AssistanceNeeded = c("NO","YES","NO","NO","YES","YES","NO","YES","NO","NO","NO","YES","NO","YES","YES","YES","NO","NO","NO","NO","NO","NO","NO","NO","NO","YES","NO","YES","YES","NO","NO","NO","YES","NO","NO","YES","NO","NO","NO","NO","YES","YES","NO","NO","YES","NO","NO","NO","NO","YES","NO","NO","YES","YES","YES","NO","NO","YES","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","YES","YES","YES","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","YES","NO","YES","NO","YES","NO","NO","NO","YES"),
    SUS_Score = c(80, 62.5, 70, 82.5, 85, 50, 87.5, 90, 90, 90, 52.5, 65, 27.5, 62.5, 40,
               85, 67.5, 72.5, 75, 95, 90, 55, 55, 77.5, 47.5, 47.5, 35, 55, 77.5, 55,
               62.5, 45, 65, 70, 60, 77.5, 67.5, 67.5, 67.5, 57.5, 50, 75, 72.5, 65,
               72.5, 72.5, 72.5, 65, 60, 72.5, 62.5, 57.5, 62.5, 47.5, 50, 50, 42.5,
               65, 55, 85, 67.5, 62.5, 85, 75, 82.5, 77.5, 65, 65, 57.5, 67.5, 65,
               55, 67.5, 67.5, 87.5, 62.5, 85, 65, 77.5, 60, 70, 65, 40, 65, 47.5,
               77.5, 65, 82.5, 57.5, 77.5),
    Likert_Score = c(3.6, 3.8, 3.2, 4.4, 4, 3.8, 3.8, 3.8, 3.8, 3.6, 4.4, 4.4, 3.4, 3.4,
                  3.4, 4, 4.2, 4.2, 4.2, 4.2, 4.2, 3, 3.4, 4.2, 3.2, 3.4, 3.2, 3.2,
                  4.2, 3.4, 4.4, 2.6, 3.4, 4, 3, 4.8, 3.4, 4.2, 3.4, 3.4, 3.2, 4.2,
                  4.2, 3.2, 4.2, 4.2, 4.2, 3.6, 3.2, 4.2, 3.4, 2.8, 3.6, 2.6, 4,
                  3.6, 3.6, 3.2, 2.4, 4.2, 3.8, 3.6, 4.2, 3.4, 3.2, 2.8, 4, 3.8,
                  2.6, 3.8, 3.2, 2.2, 3.4, 3.4, 4.6, 3.2, 4.2, 2.8, 4.4, 2.2, 3.4,
                  4.4, 3.8, 4, 2.4, 4.4, 3.8, 3.6, 3.4, 4.2),
    Task_Completion_Time = c(31.29, 27.56, 22.86, 45.24, 34.16, 23.27, 32.66, 21.25, 25.46,
                          24.12, 26.34, 28.93, 42.35, 26.67, 55.23, 26.9, 57.61, 25.23,
                          30.25, 21.67, 25.92, 55.64, 26.78, 40.11, 36.97, 35.67, 42.38,
                          30.25, 21.67, 25.92, 23.45, 22.32, 28.39, 34.57, 22.45, 28.91,
                          30.25, 20.54, 25.92, 32.66, 22.34, 28.81, 31.78, 21.25, 25.46,
                          38.68, 34.56, 34.22, 37.89, 21.26, 38.43, 35.61, 29.66, 24.35,
                          34.78, 38.94, 45.57, 32.66, 41.67, 21.32, 36.97, 35.67, 20.36,
                          27.88, 21.43, 38.67, 32.66, 21.25, 45.68, 42.35, 26.67, 55.23,
                          37.11, 35.67, 39.78, 34.56, 43.26, 28.76, 26.67, 42.35, 25.23,
                          45.24, 34.16, 23.27, 42.26, 27.34, 26.57, 21.45, 32.65, 25.46)

)
# DisabilityType must be have this defined order in the plots
data$DisabilityType <- factor(data$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
# Color palette for better visuals
palette <- c("Motor" = "#7b519c", "Visual" = "#38899c", "Auditory" = "#d64db2")  
# Boxplot: SUS Score
plot_sus <- ggplot(data, aes(x = DisabilityType, y = SUS_Score, fill = DisabilityType)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) +
  labs(title = "SUS Score by Disability Type", x = "Disability Type", y = "SUS Score") +
  theme_minimal() +
  theme(legend.position = "none")
# Boxplot: Task Completion Time 
plot_time <- ggplot(data, aes(x = DisabilityType, y = Task_Completion_Time, fill = DisabilityType)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) +
  labs(title = "Task Completion Time by Disability Type", x = "Disability Type", y = "Time (seconds)") +
  theme_minimal() +
  theme(legend.position = "none")
# Boxplot: Likert Score
plot_likert <- ggplot(data, aes(x = DisabilityType, y = Likert_Score, fill = DisabilityType)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) +
  labs(title = "Likert Score by Disability Type", x = "Disability Type", y = "Likert Score") +
  theme_minimal() +
  theme(legend.position = "none")
# Display all plots together 
grid.arrange(plot_sus, plot_time, plot_likert, ncol = 3, top = "Figure 3: Boxplots of Usability Metrics by Disability Type")

# MEAN VALUES PER DISABILITY TYPE (BAR PLOTS)

install.packages("dplyr")
library(dplyr)
# Compute the group means
mean_data <- data %>%
  group_by(DisabilityType) %>%
  summarise(
    Mean_SUS = mean(SUS_Score, na.rm = TRUE),
    Mean_Likert = mean(Likert_Score, na.rm = TRUE),
    Mean_Time = mean(Task_Completion_Time, na.rm = TRUE)
  )
# SUS mean bar plot
bar_sus <- ggplot(mean_data, aes(x = DisabilityType, y = Mean_SUS, fill = DisabilityType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette) +
  labs(title = "Mean SUS Score", y = "Mean SUS", x = "Disability Type") +
  theme_minimal()
# Time mean bar plot
bar_time <- ggplot(mean_data, aes(x = DisabilityType, y = Mean_Time, fill = DisabilityType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette) +
  labs(title = "Mean Task Completion Time", y = "Mean Time (s)", x = "Disability Type") +
  theme_minimal()
# Likert mean bar plot
bar_likert <- ggplot(mean_data, aes(x = DisabilityType, y = Mean_Likert, fill = DisabilityType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette) +
  labs(title = "Mean Likert Score", y = "Mean Likert", x = "Disability Type") +
  theme_minimal()
# Display Figure 4
grid.arrange(bar_sus, bar_time, bar_likert, ncol = 3, top = "Figure 4: Group-Level Means by Disability Type")

# CATEGORICAL DISTRIBUTION COUNTS

# Plot of Gender vs Disability Type
bar_gender <- ggplot(data, aes(x = DisabilityType, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Participants by Gender and Disability Type", x = "Disability Type", y = "Count") +
  scale_fill_manual(values = c("MALE" = "#293aab", "FEMALE" = "#de7880")) +
  theme_minimal()
# Plot of Assistance vs Disability Type
bar_assist <- ggplot(data, aes(x = DisabilityType, fill = AssistanceNeeded)) +
  geom_bar(position = "dodge") +
  labs(title = "Participants by Assistance Needed and Disability Type", x = "Disability Type", y = "Count") +
  scale_fill_manual(values = c("YES" = "#a1c9a5", "NO" = "#e89d1c")) +
  theme_minimal()
# Plot of Gender vs Assistance
bar_gender_assist <- ggplot(data, aes(x = AssistanceNeeded, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Participants by Gender and Assistance Needed", x = "Assistance Needed", y = "Count") +
  scale_fill_manual(values = c("MALE" = "#293aab", "FEMALE" = "#de7880")) +
  theme_minimal()
# Display Figure 5
grid.arrange(bar_gender, bar_assist, bar_gender_assist, ncol = 3, top = "Figure 5: Participant Distributions")


# SHAPIRO-WILK TEST
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

# HOMOGENEITY OF VARIANCE

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

# QUANTITATIVE COMPARISONS

# Librerías necesarias
install.packages("afex")
install.packages("emmeans")
library(afex)
library(emmeans)
# ANOVA para SUS Score
anova_sus <- aov_ez(
  id = "Student",
  dv = "SUS_Score",
  within = "DisabilityType",
  between = "Gender",
  data = data
)
# ANOVA para Task Completion Time
anova_time <- aov_ez(
  id = "Student",
  dv = "Task_Completion_Time",
  within = "DisabilityType",
  between = "Gender",
  data = data
)
# ANOVA para Likert Score
anova_likert <- aov_ez(
  id = "Student",
  dv = "Likert_Score",
  within = "DisabilityType",
  between = "Gender",
  data = data
)
# Table 4
summary(anova_sus)
# Table 5
summary(anova_time)
# Table 6
summary(anova_likert)
# Contrasts post hoc: SUS Score
emm_sus <- emmeans(anova_sus, pairwise ~ DisabilityType, adjust = "tukey")
emm_sus$contrasts  # Table 7
# Contrasts post hoc: Task Completion Time
emm_time <- emmeans(anova_time, pairwise ~ DisabilityType, adjust = "tukey")
emm_time$contrasts  # Table 8
# Contrasts post hoc: Likert Score
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

# PEARSON CORRELATION TESTS
cor_sus_time <- cor.test(data$SUS_Score, data$Task_Completion_Time, method = "pearson")
cor_sus_errors <- cor.test(data$SUS_Score, data$Number_of_Errors, method = "pearson")
cor_likert_time <- cor.test(data$Likert_Score, data$Task_Completion_Time, method = "pearson")
# Display results (Table 10 values)
cat("SUS_Score vs Task_Completion_Time:\n")
print(cor_sus_time)
cat("\nSUS_Score vs Number_of_Errors:\n")
print(cor_sus_errors)
cat("\nLikert_Score vs Task_Completion_Time:\n")
print(cor_likert_time)


# CONTINGENCY TABLE

# Make AssistanceNeeded a factor
data$AssistanceNeeded <- factor(data$AssistanceNeeded, levels = c("NO", "YES"))
data$DisabilityType <- factor(data$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
table_11 <- table(data$DisabilityType, data$AssistanceNeeded)
print(table_11)  # Table 11

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
