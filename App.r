# DASHBOARD WITH SHINY

# Required libraries
install.packages(c("shiny", "ggplot2", "dplyr", "gridExtra", "patchwork", "afex", "emmeans"))
library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
library(afex)
library(emmeans)
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

# UI
ui <- fluidPage(
  titlePanel("Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("disability", "Select Simulation Type:",
                  choices = c("All", "Motor", "Visual", "Auditory"),
                  selected = "All"),
      selectInput("gender", "Filter by Gender:",
                  choices = c("All", "MALE", "FEMALE"), selected = "All"),
      selectInput("assistance", "Filter by Assistance:",
                  choices = c("All", "YES", "NO"), selected = "All"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("SUS Score Distribution", plotOutput("susPlot")),
        tabPanel("Average Errors", plotOutput("errorPlot")),
        tabPanel("Task Time", plotOutput("timePlot")),
        tabPanel("Boxplots", plotOutput("boxPlots")),
        tabPanel("Mean Plots", plotOutput("meanPlots")),
        tabPanel("Distributions", plotOutput("distributionPlots")),
        tabPanel("Scatterplots", plotOutput("scatterPlots")),
        tabPanel("Diagnostics", plotOutput("diagnostics")),
        tabPanel("Statistical Tests", verbatimTextOutput("statsSummary"))
      )
    )
  )
)
# Server
server <- function(input, output) {
  # Datos filtrados según los selectInput
  filtered_data <- reactive({
    df <- data
      if (input$disability != "All") {
        df <- df %>% filter(DisabilityType == input$disability)
      }
      if (input$gender != "All") {
        df <- df %>% filter(Gender == input$gender)
      }
      if (input$assistance != "All") {
        df <- df %>% filter(AssistanceNeeded == input$assistance)
      }
    return(df)
  })
  # PLOT 1: SUS Score boxplot
  output$susPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = DisabilityType, y = SUS_Score, fill = DisabilityType)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "SUS Score by Simulation Type", x = "Disability Type", y = "SUS Score")
  })
  # PLOT 2: Average Number of Errors barplot
  output$errorPlot <- renderPlot({
    filtered_data() %>%
      group_by(DisabilityType) %>%
      summarise(Avg_Errors = mean(Number_of_Errors, na.rm = TRUE)) %>%
      ggplot(aes(x = DisabilityType, y = Avg_Errors, fill = DisabilityType)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Average Number of Errors by Simulation Type", x = "Disability Type", y = "Average Errors")
  })
  # PLOT 3: Task Time boxplot
  output$timePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = DisabilityType, y = Task_Completion_Time, fill = DisabilityType)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Task Completion Time by Simulation Type", x = "Disability Type", y = "Time (seconds)")
  })
  # PLOT 4: Boxplots of SUS, Time, Likert Score (combinado con gridExtra)
  output$boxPlots <- renderPlot({
    df <- filtered_data()
    df$DisabilityType <- factor(df$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
    palette <- c("Motor" = "#7b519c", "Visual" = "#38899c", "Auditory" = "#d64db2")
    plot_sus <- ggplot(df, aes(x = DisabilityType, y = SUS_Score, fill = DisabilityType)) +
      geom_boxplot() +
      scale_fill_manual(values = palette) +
      labs(title = "SUS Score", x = "Disability Type", y = "SUS Score") +
      theme_minimal() + theme(legend.position = "none")
    plot_time <- ggplot(df, aes(x = DisabilityType, y = Task_Completion_Time, fill = DisabilityType)) +
      geom_boxplot() +
      scale_fill_manual(values = palette) +
      labs(title = "Task Completion Time", x = "Disability Type", y = "Time (s)") +
      theme_minimal() + theme(legend.position = "none")
    plot_likert <- ggplot(df, aes(x = DisabilityType, y = Likert_Score, fill = DisabilityType)) +
      geom_boxplot() +
      scale_fill_manual(values = palette) +
      labs(title = "Likert Score", x = "Disability Type", y = "Likert Score") +
      theme_minimal() + theme(legend.position = "none")
    gridExtra::grid.arrange(plot_sus, plot_time, plot_likert, ncol = 3,
                            top = "Figure: Boxplots of Usability Metrics by Disability Type")
  })
  # MEAN PLOTS
  output$meanPlots <- renderPlot({
    df <- filtered_data()
    df$DisabilityType <- factor(df$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
    palette <- c("Motor" = "#7b519c", "Visual" = "#38899c", "Auditory" = "#d64db2")
    mean_df <- df %>%
      group_by(DisabilityType) %>%
      summarise(
        Mean_SUS = mean(SUS_Score, na.rm = TRUE),
        Mean_Likert = mean(Likert_Score, na.rm = TRUE),
        Mean_Time = mean(Task_Completion_Time, na.rm = TRUE)
      )
    bar_sus <- ggplot(mean_df, aes(x = DisabilityType, y = Mean_SUS, fill = DisabilityType)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = palette) +
      labs(title = "Mean SUS Score", y = "Mean SUS", x = "Disability Type") +
      theme_minimal() + theme(legend.position = "none")
    bar_time <- ggplot(mean_df, aes(x = DisabilityType, y = Mean_Time, fill = DisabilityType)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = palette) +
      labs(title = "Mean Task Completion Time", y = "Mean Time (s)", x = "Disability Type") +
      theme_minimal() + theme(legend.position = "none")
    bar_likert <- ggplot(mean_df, aes(x = DisabilityType, y = Mean_Likert, fill = DisabilityType)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = palette) +
      labs(title = "Mean Likert Score", y = "Mean Likert", x = "Disability Type") +
      theme_minimal() + theme(legend.position = "none")
    gridExtra::grid.arrange(bar_sus, bar_time, bar_likert, ncol = 3,
                            top = "Figure: Group-Level Means by Disability Type")
  })
  # DISTRIBUTIONS
  output$distributionPlots <- renderPlot({
    df <- filtered_data()
    df$DisabilityType <- factor(df$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
    df$AssistanceNeeded <- factor(df$AssistanceNeeded, levels = c("NO", "YES"))
    gender_palette <- c("MALE" = "#293aab", "FEMALE" = "#de7880")
    assist_palette <- c("NO" = "#e89d1c", "YES" = "#a1c9a5")
    plot_gender <- ggplot(df, aes(x = DisabilityType, fill = Gender)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = gender_palette) +
      labs(title = "Participants by Gender and Disability Type", x = "Disability Type", y = "Count") +
      theme_minimal()
    plot_assist <- ggplot(df, aes(x = DisabilityType, fill = AssistanceNeeded)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = assist_palette) +
      labs(title = "Participants by Assistance Needed and Disability Type", x = "Disability Type", y = "Count") +
      theme_minimal()
    plot_gender_assist <- ggplot(df, aes(x = AssistanceNeeded, fill = Gender)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = gender_palette) +
      labs(title = "Participants by Gender and Assistance Needed", x = "Assistance Needed", y = "Count") +
      theme_minimal()
    gridExtra::grid.arrange(plot_gender, plot_assist, plot_gender_assist, ncol = 3,
                            top = "Figure: Participant Distributions by Category")
  })
  output$scatterPlots <- renderPlot({
    df <- filtered_data()
    plot1 <- ggplot(df, aes(x = SUS_Score, y = Task_Completion_Time)) +
      geom_point() +
      geom_smooth(method = "lm", color = "blue", se = TRUE) +
      labs(title = "SUS Score vs Task Completion Time", x = "SUS Score", y = "Time (sec)") +
      theme_minimal()
    plot2 <- ggplot(df, aes(x = SUS_Score, y = Number_of_Errors)) +
      geom_point() +
      geom_smooth(method = "lm", color = "blue", se = TRUE) +
      labs(title = "SUS Score vs Number of Errors", x = "SUS Score", y = "Errors") +
      theme_minimal()
    plot3 <- ggplot(df, aes(x = Likert_Score, y = Task_Completion_Time)) +
      geom_point() +
      geom_smooth(method = "lm", color = "darkgreen", se = TRUE) +
      labs(title = "Likert Score vs Task Completion Time", x = "Likert Score", y = "Time (sec)") +
      theme_minimal()
    (plot1 | plot2) / plot3 + patchwork::plot_annotation(title = "Figure: Scatterplots with Linear Regression")
  })
  output$diagnostics <- renderPlot({
    df <- filtered_data()
    lm_sus <- lm(SUS_Score ~ DisabilityType, data = df)
    lm_time <- lm(Task_Completion_Time ~ DisabilityType, data = df)
    lm_likert <- lm(Likert_Score ~ DisabilityType, data = df)
    resid_plot <- function(model, title) {
      ggplot(data.frame(Fitted = fitted(model), Residuals = resid(model)),
            aes(x = Fitted, y = Residuals)) +
        geom_point(color = "#2c3e50", alpha = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = title, x = "Fitted Values", y = "Residuals") +
        theme_minimal()
    }
    p1 <- resid_plot(lm_sus, "SUS Score: Residuals vs Fitted")
    p2 <- resid_plot(lm_time, "Task Time: Residuals vs Fitted")
    p3 <- resid_plot(lm_likert, "Likert Score: Residuals vs Fitted")
    p1 / p2 / p3 + patchwork::plot_annotation(title = "Figure: Residuals vs Fitted - Homogeneity of Variance")
  })
  # CORRELATIONS
  output$statsSummary <- renderPrint({
    df <- filtered_data()
    cor_sus_time <- cor.test(df$SUS_Score, df$Task_Completion_Time, method = "pearson")
    cor_sus_errors <- cor.test(df$SUS_Score, df$Number_of_Errors, method = "pearson")
    cor_likert_time <- cor.test(df$Likert_Score, df$Task_Completion_Time, method = "pearson")
    # ANOVA (discapacidad como within, género como between)
    suppressMessages(library(afex))
    suppressMessages(library(emmeans))
    aov_sus <- aov_ez(id = "Student", dv = "SUS_Score", within = "DisabilityType", between = "Gender", data = df)
    aov_time <- aov_ez(id = "Student", dv = "Task_Completion_Time", within = "DisabilityType", between = "Gender", data = df)
    aov_likert <- aov_ez(id = "Student", dv = "Likert_Score", within = "DisabilityType", between = "Gender", data = df)
    # Post-hoc Tukey
    tukey_sus <- emmeans(aov_sus, pairwise ~ DisabilityType, adjust = "tukey")
    tukey_time <- emmeans(aov_time, pairwise ~ DisabilityType, adjust = "tukey")
    tukey_likert <- emmeans(aov_likert, pairwise ~ DisabilityType, adjust = "tukey")
    cat("=== Pearson Correlations ===\n")
    cat("SUS Score vs Task Completion Time:\n"); print(cor_sus_time)
    cat("\nSUS Score vs Number of Errors:\n"); print(cor_sus_errors)
    cat("\nLikert Score vs Task Completion Time:\n"); print(cor_likert_time)
    cat("\n\n=== ANOVA: SUS Score ===\n")
    print(summary(aov_sus))
    cat("\nPost-hoc Tukey:\n"); print(tukey_sus$contrasts)
    cat("\n\n=== ANOVA: Task Completion Time ===\n")
    print(summary(aov_time))
    cat("\nPost-hoc Tukey:\n"); print(tukey_time$contrasts)
    cat("\n\n=== ANOVA: Likert Score ===\n")
    print(summary(aov_likert))
    cat("\nPost-hoc Tukey:\n"); print(tukey_likert$contrasts)
  })
}
# Ejecutar
shiny::runApp("App.R")
