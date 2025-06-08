# STEP 1: Initial visual exploration - Boxplots and bar plots
install.packages("ggplot2")
install.packages("readr")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("patchwork")
install.packages("dplyr")
library(ggplot2)
library(readr)
library(gridExtra)
library(cowplot)
library(patchwork)
library(dplyr)
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
# DisabilityType is within-subject (each participant does 3 simulations)
data$DisabilityType <- factor(data$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
palette <- c("Motor" = "#e7d0f5", "Visual" = "#dcf0f7", "Auditory" = "#ffebfd")  
custom_theme <- theme_minimal(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
# Boxplot: SUS Score
box_sus <- ggplot(data, aes(x = DisabilityType, y = SUS_Score, fill = DisabilityType)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) +
  labs(title = "SUS by Disability", x = "Disability Type", y = "SUS Score") +
  custom_theme
# Boxplot: Task Completion Time 
box_time <- ggplot(data, aes(x = DisabilityType, y = Task_Completion_Time, fill = DisabilityType)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) +
  labs(title = "Time by Disability", x = "Disability Type", y = "Time (seconds)") +
  custom_theme
# Boxplot: Likert Score
box_likert <- ggplot(data, aes(x = DisabilityType, y = Likert_Score, fill = DisabilityType)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) +
  labs(title = "Likert by Disability", x = "Disability Type", y = "Likert Score") +
  custom_theme
summary_data <- data %>%
  group_by(DisabilityType) %>%
  summarise(
    Mean_SUS = mean(SUS_Score, na.rm = TRUE),
    SD_SUS = sd(SUS_Score, na.rm = TRUE),
    Mean_Likert = mean(Likert_Score, na.rm = TRUE),
    SD_Likert = sd(Likert_Score, na.rm = TRUE),
    Mean_Time = mean(Task_Completion_Time, na.rm = TRUE),
    SD_Time = sd(Task_Completion_Time, na.rm = TRUE)
  )
# Bar chart: SUS
bar_sus <- ggplot(summary_data, aes(x = DisabilityType, y = Mean_SUS, fill = DisabilityType)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_SUS - SD_SUS, ymax = Mean_SUS + SD_SUS), width = 0.2) +
  scale_fill_manual(values = palette) +
  labs(title = "Mean SUS ± SD", y = "Mean SUS", x = "Disability Type") +
  custom_theme
# Bar chart: Time
bar_time <- ggplot(summary_data, aes(x = DisabilityType, y = Mean_Time, fill = DisabilityType)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Time - SD_Time, ymax = Mean_Time + SD_Time), width = 0.2) +
  scale_fill_manual(values = palette) +
  labs(title = "Mean Time ± SD", y = "Mean Time (s)", x = "Disability Type") +
  custom_theme
# Bar chart: Likert
bar_likert <- ggplot(summary_data, aes(x = DisabilityType, y = Mean_Likert, fill = DisabilityType)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Mean_Likert - SD_Likert, ymax = Mean_Likert + SD_Likert), width = 0.2) +
  scale_fill_manual(values = palette) +
  labs(title = "Mean Likert ± SD", y = "Mean Likert", x = "Disability Type") +
  custom_theme
combined_figure <- (
    (box_sus | box_time | box_likert) /
    (bar_sus | bar_time | bar_likert)
) + 
    plot_layout (guides="collect") & theme(legend.position = "bottom")
print(combined_figure)

# Plot 1: Gender vs Disability Type
bar_gender <- ggplot(data, aes(x = DisabilityType, fill = Gender)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("MALE" = "#dee6fc", "FEMALE" = "#edfcee")) +
  labs(title = "Gender by Disability", x = "Disability Type", y = "Count", fill = "Gender") +
  custom_theme
# Plot 2: Assistance Needed vs Disability Type
bar_assist <- ggplot(data, aes(x = DisabilityType, fill = AssistanceNeeded)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("YES" = "#fcf7d2", "NO" = "#f7d2d5")) +
  labs(title = "Assistance by Disability", x = "Disability Type", y = "Count", fill = "Assistance") +
  custom_theme
# Plot 3: Gender vs Assistance
bar_gender_assist <- ggplot(data, aes(x = AssistanceNeeded, fill = Gender)) +
  geom_bar(stat = "count", position = "dodge") +
  scale_fill_manual(values = c("MALE" = "#dee6fc", "FEMALE" = "#edfcee")) +
  labs(title = "Gender by Assistance", x = "Assistance Needed", y = "Count", fill = "Gender") +
  custom_theme
participant_distribution_plot <- (bar_gender | bar_assist | bar_gender_assist) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
print(participant_distribution_plot)



# STEP 2: Inference Analysis
# STEP 2.1: Model Assumptions
install.packages("performance")
library(performance)
check_model(lm(SUS_Score ~ DisabilityType * Gender, data = data))
check_model(lm(Task_Completion_Time ~ DisabilityType * Gender, data = data))
check_model(lm(Likert_Score ~ DisabilityType * Gender, data = data))
library(ggplot2)
library(patchwork)
theme_large <- theme_minimal(base_size = 18) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.margin = margin(10, 10, 10, 10)
  )
plot_qq_sus <- ggplot(data, aes(sample = SUS_Score)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Q-Q: SUS") + theme_large
plot_density_sus <- ggplot(data, aes(x = SUS_Score)) +
  geom_density(fill = "#c2edec", alpha = 0.6) +
  labs(title = "Density: SUS") + theme_large
plot_qq_likert <- ggplot(data, aes(sample = Likert_Score)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Q-Q: Likert") + theme_large
plot_density_likert <- ggplot(data, aes(x = Likert_Score)) +
  geom_density(fill = "#e6c3e8", alpha = 0.6) +
  labs(title = "Density: Likert") + theme_large
plot_qq_time <- ggplot(data, aes(sample = Task_Completion_Time)) +
  stat_qq() + stat_qq_line() +
  labs(title = "Q-Q: Time") + theme_large
plot_density_time <- ggplot(data, aes(x = Task_Completion_Time)) +
  geom_density(fill = "#f2e7bd", alpha = 0.6) +
  labs(title = "Density: Time") + theme_large
figure_normality <- (plot_qq_sus | plot_density_sus) /
                    (plot_qq_likert | plot_density_likert) /
                    (plot_qq_time | plot_density_time) +
  plot_annotation(title = "Normality Check - Q-Q Plots and Densities", theme = theme_large)
print(figure_normality)
lm_sus <- lm(SUS_Score ~ DisabilityType * Gender, data = data)
lm_time <- lm(Task_Completion_Time ~ DisabilityType * Gender, data = data)
lm_likert <- lm(Likert_Score ~ DisabilityType * Gender, data = data)
resid_plot <- function(model, title) {
  df <- data.frame(Fitted = fitted(model), Residuals = resid(model))
  ggplot(df, aes(x = Fitted, y = Residuals)) +
    geom_point(color = "#2c3e50", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = title, x = "Fitted", y = "Residuals") +
    theme_large
}
figure_homoscedasticity <- resid_plot(lm_sus, "SUS Residuals vs Fitted") /
                           resid_plot(lm_likert, "Likert Residuals vs Fitted") /
                           resid_plot(lm_time, "Time Residuals vs Fitted") +
  plot_annotation(title = "Homogeneity of Variance - Residuals vs Fitted", theme = theme_large)
print(figure_homoscedasticity)

# STEP 2.2: ANOVA Results
install.packages("afex")
library(afex)
anova_sus <- aov_ez(id = "Student", dv = "SUS_Score", within = "DisabilityType", between = "Gender", data = data)
anova_time <- aov_ez(id = "Student", dv = "Task_Completion_Time", within = "DisabilityType", between = "Gender", data = data)
anova_likert <- aov_ez(id = "Student", dv = "Likert_Score", within = "DisabilityType", between = "Gender", data = data)
print(summary(anova_sus))
print(summary(anova_time))
print(summary(anova_likert))

# STEP 2.3 ANOVAs by Gender on SUS
data_female <- subset(data, Gender == "FEMALE")
data_male <- subset(data, Gender == "MALE")
aov_female_sus <- aov_ez(id = "Student", dv = "SUS_Score", within = "DisabilityType", data = data_female)
aov_male_sus <- aov_ez(id = "Student", dv = "SUS_Score", within = "DisabilityType", data = data_male)
print(summary(aov_female_sus))
print(summary(aov_male_sus))
# Gender in Disability (t-test)
motor_data <- subset(data, DisabilityType == "Motor")
t_motor <- t.test(SUS_Score ~ Gender, data = motor_data, var.equal = FALSE)
print(t_motor)
visual_data <- subset(data, DisabilityType == "Visual")
t_visual <- t.test(SUS_Score ~ Gender, data = visual_data, var.equal = FALSE)
print(t_visual)
auditory_data <- subset(data, DisabilityType == "Auditory")
t_audit <- t.test(SUS_Score ~ Gender, data = auditory_data, var.equal = FALSE)
print(t_audit)
interaction_plot <- ggplot(data, aes(x = DisabilityType, y = SUS_Score, group = Gender, color = Gender)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_color_manual(values = c("MALE" = "#2852c7", "FEMALE" = "#2abf34")) +
  labs(
    title = "Interaction Plot: SUS Score by Disability Type and Gender",
    x = "Disability Type",
    y = "Mean SUS Score",
    color = "Gender"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
print(interaction_plot)
# STEP 2.3.1: ANOVAs by Gender on Likert
data_female <- subset(data, Gender == "FEMALE")
data_male <- subset(data, Gender == "MALE")
aov_female_likert <- aov_ez(id = "Student", dv = "Likert_Score", within = "DisabilityType", data = data_female)
aov_male_likert <- aov_ez(id = "Student", dv = "Likert_Score", within = "DisabilityType", data = data_male)
print(summary(aov_female_likert))
print(summary(aov_male_likert))
# T-tests for Likert Score by DisabilityType
t_motor_likert <- t.test(Likert_Score ~ Gender, data = subset(data, DisabilityType == "Motor"), var.equal = FALSE)
print(t_motor_likert)
t_visual_likert <- t.test(Likert_Score ~ Gender, data = subset(data, DisabilityType == "Visual"), var.equal = FALSE)
print(t_visual_likert)
t_auditory_likert <- t.test(Likert_Score ~ Gender, data = subset(data, DisabilityType == "Auditory"), var.equal = FALSE)
print(t_auditory_likert)
# Interaction Plot for Likert Score
interaction_plot_likert <- ggplot(data, aes(x = DisabilityType, y = Likert_Score, group = Gender, color = Gender)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_color_manual(values = c("MALE" = "#2852c7", "FEMALE" = "#2abf34")) +
  labs(
    title = "Interaction Plot: Likert Score by Disability Type and Gender",
    x = "Disability Type",
    y = "Mean Likert Score",
    color = "Gender"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
print(interaction_plot_likert)


# STEP 2.3.1.1: Effect Size
install.packages("effectsize")
library(effectsize)
# Partial eta squared for SUS and Likert ANOVAs
eta_sus <- eta_squared(anova_sus, partial = TRUE)
eta_likert <- eta_squared(anova_likert, partial = TRUE)
print(eta_sus)
print(eta_likert)
# Cohen's d para t-tests entre géneros (por tipo de discapacidad)
d_motor_sus <- cohens_d(SUS_Score ~ Gender, data = subset(data, DisabilityType == "Motor"))
d_visual_sus <- cohens_d(SUS_Score ~ Gender, data = subset(data, DisabilityType == "Visual"))
d_auditory_sus <- cohens_d(SUS_Score ~ Gender, data = subset(data, DisabilityType == "Auditory"))
d_motor_likert <- cohens_d(Likert_Score ~ Gender, data = subset(data, DisabilityType == "Motor"))
d_visual_likert <- cohens_d(Likert_Score ~ Gender, data = subset(data, DisabilityType == "Visual"))
d_auditory_likert <- cohens_d(Likert_Score ~ Gender, data = subset(data, DisabilityType == "Auditory"))
print(d_motor_sus)
print(d_visual_sus)
print(d_auditory_sus)
print(d_motor_likert)
print(d_visual_likert)
print(d_auditory_likert)

# STEP 2.4: CORRELATION
# Add Number_of_Errors
data$Number_of_Errors <- c(
  0, 0, 1, 2, 1, 0, 2, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0,
  0, 1, 1, 0, 1, 1, 2, 2, 0, 1, 0, 1, 0, 2, 1, 0, 1, 2, 1, 0,
  0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 2, 0, 0, 1, 0, 2, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 2, 1, 0, 2, 0, 0, 0, 1, 0, 1, 0, 2, 0, 0,
  2, 0, 1, 0, 0, 0, 1, 0, 0,0
)
stopifnot(nrow(data) == length(data$Number_of_Errors))

cor <- cor.test(data$SUS_Score, data$Task_Completion_Time, method = "pearson")
cor2 <- cor.test(data$SUS_Score, data$Number_of_Errors, method = "pearson")
cor3 <- cor.test(data$Likert_Score, data$Task_Completion_Time, method = "pearson")
print(cor)
print(cor2)
print(cor3)

library(ggplot2)
library(patchwork)
data$Number_of_Errors <- as.numeric(data$Number_of_Errors)
theme_scatter <- theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  )
# 1. SUS vs Task Completion Time
plot1 <- ggplot(data, aes(x = SUS_Score, y = Task_Completion_Time)) +
  geom_point(alpha = 0.6, color = "#5B9BD5") +
  geom_smooth(method = "lm", color = "#1F4E79", se = TRUE) +
  labs(
    title = "SUS vs Task Completion Time",
    x = "SUS Score",
    y = "Time (sec)"
  ) +
  theme_scatter
# 2. SUS vs Number of Errors
plot2 <- ggplot(data, aes(x = SUS_Score, y = Number_of_Errors)) +
  geom_jitter(width = 0.5, alpha = 0.6, color = "#ED7D31") +
  geom_smooth(method = "lm", color = "#C55A11", se = TRUE) +
  labs(
    title = "SUS vs Number of Errors",
    x = "SUS Score",
    y = "Number of Errors"
  ) +
  theme_scatter
# 3. Likert vs Task Completion Time
plot3 <- ggplot(data, aes(x = Likert_Score, y = Task_Completion_Time)) +
  geom_point(alpha = 0.6, color = "#70AD47") +
  geom_smooth(method = "lm", color = "#447239", se = TRUE) +
  labs(
    title = "Likert vs Task Completion Time",
    x = "Satisfaction (Likert)",
    y = "Time (sec)"
  ) +
  theme_scatter
figure_scatter <- (plot1 | plot2) / plot3 +
  plot_annotation(title = "Scatterplots Illustrating Relationships Between Perception and Performance")
print(figure_scatter)


# STEP 2.5: CONTINGENCY
table_con <- table(data$DisabilityType, data$AssistanceNeeded)
print(table_con)
expected_counts <- chisq.test(table_con)$expected
if (any(expected_counts < 5)) {
  print(fisher.test(table_con))
} else {
  print(chisq.test(table_con))
}
