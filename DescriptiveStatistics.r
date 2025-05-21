# BOXPLOTS BY DISABILITY TYPE

# Load the required packages
install.packages("ggplot2")
install.packages("readr")
library(ggplot2)
library(readr)
# Manually enter the data from the Excel (Student ID repeated for each simulation)
data <- data.frame(
    Student = c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10,11,11,11,12,12,12,13,13,13,14,14,14,15,15,15,16,16,16,17,17,17,18,18,18,19,19,19,20,20,20,21,21,21,22,22,22,23,23,23,24,24,24,25,25,25,26,26,26,27,27,27,28,28,28,29,29,29,30,30,30),
    Gender = c("MALE", "FEMALE","FEMALE","FEMALE","MALE","FEMALE","FEMALE","MALE","FEMALE","MALE","MALE","MALE","FEMALE","FEMALE","FEMALE","FEMALE","FEMALE","FEMALE","MALE","MALE","MALE","MALE","MALE","MALE","MALE","MALE","MALE","MALE","MALE","MALE","FEMALE"),
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
        "Visual", "Auditory", "Motor",

    ),
    AssistanceNeeded = c("NO","YES","NO","NO","YES","YES","NO","YES","NO","NO","NO","YES","NO","YES","YES","YES","NO","NO","NO","NO","NO","NO","NO","NO","NO","YES","NO","YES","YES","NO","NO","NO","YES","NO","NO","YES","NO","NO","NO","NO","YES","YES","NO","NO","YES","NO","NO","NO","NO","YES","NO","NO","YES","YES","YES","NO","NO","YES","NO","NO","NO","NO","YES","NO","NO","NO","NO","NO","YES","YES","YES","NO","NO","NO","NO","NO","YES","NO","NO","NO","NO","YES","NO","YES","NO","YES","NO","NO","NO","YES","NO",),
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
grid.arrange(plot_sus, plot_time, plot_likert, ncol = 3, top = "Figure 4: Boxplots of Usability Metrics by Disability Type")
