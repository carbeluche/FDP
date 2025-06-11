library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
library(afex)
library(emmeans)
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
palette <- c("Motor" = "#e7d0f5", "Visual" = "#dcf0f7", "Auditory" = "#ffebfd")  
gender_colors <- c("MALE" = "#dee6fc", "FEMALE" = "#edfcee")
assist_colors <- c("YES" = "#fcf7d2", "NO" = "#f7d2d5")
custom_theme <- theme_minimal(base_size = 16) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
data$DisabilityType <- factor(data$DisabilityType, levels = c("Motor", "Visual", "Auditory"))
# Add Number_of_Errors
data$Number_of_Errors <- c(
  0, 0, 1, 2, 1, 0, 2, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0,
  0, 1, 1, 0, 1, 1, 2, 2, 0, 1, 0, 1, 0, 2, 1, 0, 1, 2, 1, 0,
  0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 2, 0, 0, 1, 0, 2, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 2, 1, 0, 2, 0, 0, 0, 1, 0, 1, 0, 2, 0, 0,
  2, 0, 1, 0, 0, 0, 1, 0, 0,0
)
ui <- fluidPage(
  titlePanel("Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("disability", "Select Simulation Type:",
                  choices = c("All", levels(data$DisabilityType)),
                  selected = "All"),
      selectInput("gender", "Filter by Gender:",
                  choices = c("All", unique(data$Gender)), selected = "All"),
      selectInput("assistance", "Filter by Assistance:",
                  choices = c("All", unique(data$AssistanceNeeded)), selected = "All")
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

server <- function(input, output) {
  filtered_data <- reactive({
    df <- data
    if (input$disability != "All") df <- df %>% filter(DisabilityType == input$disability)
    if (input$gender != "All") df <- df %>% filter(Gender == input$gender)
    if (input$assistance != "All") df <- df %>% filter(AssistanceNeeded == input$assistance)
    df
  })

  output$susPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = DisabilityType, y = SUS_Score, fill = DisabilityType)) +
      geom_boxplot() +
      scale_fill_manual(values = palette) +
      theme_minimal() +
      labs(title = "SUS Score by Simulation Type", x = "Disability Type", y = "SUS Score")
  })

  output$errorPlot <- renderPlot({
    filtered_data() %>%
      group_by(DisabilityType) %>%
      summarise(Avg_Errors = mean(Number_of_Errors, na.rm = TRUE)) %>%
      ggplot(aes(x = DisabilityType, y = Avg_Errors, fill = DisabilityType)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = palette) +
      theme_minimal() +
      labs(title = "Average Number of Errors", x = "Disability Type", y = "Avg Errors")
  })

  output$timePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = DisabilityType, y = Task_Completion_Time, fill = DisabilityType)) +
      geom_boxplot() +
      scale_fill_manual(values = palette) +
      theme_minimal() +
      labs(title = "Task Completion Time", x = "Disability Type", y = "Time (s)")
  })

  output$boxPlots <- renderPlot({
    df <- filtered_data()
    p1 <- ggplot(df, aes(x = DisabilityType, y = SUS_Score, fill = DisabilityType)) +
      geom_boxplot() + scale_fill_manual(values = palette) + theme_minimal() +
      labs(title = "SUS") + theme(legend.position = "none")
    p2 <- ggplot(df, aes(x = DisabilityType, y = Task_Completion_Time, fill = DisabilityType)) +
      geom_boxplot() + scale_fill_manual(values = palette) + theme_minimal() +
      labs(title = "Time") + theme(legend.position = "none")
    p3 <- ggplot(df, aes(x = DisabilityType, y = Likert_Score, fill = DisabilityType)) +
      geom_boxplot() + scale_fill_manual(values = palette) + theme_minimal() +
      labs(title = "Likert") + theme(legend.position = "none")
    grid.arrange(p1, p2, p3, ncol = 3)
  })

  output$meanPlots <- renderPlot({
    df <- filtered_data() %>% group_by(DisabilityType) %>%
      summarise(Mean_SUS = mean(SUS_Score), Mean_Likert = mean(Likert_Score), Mean_Time = mean(Task_Completion_Time))
    p1 <- ggplot(df, aes(x = DisabilityType, y = Mean_SUS, fill = DisabilityType)) +
      geom_bar(stat = "identity") + scale_fill_manual(values = palette) + theme_minimal() + labs(title = "Mean SUS")
    p2 <- ggplot(df, aes(x = DisabilityType, y = Mean_Likert, fill = DisabilityType)) +
      geom_bar(stat = "identity") + scale_fill_manual(values = palette) + theme_minimal() + labs(title = "Mean Likert")
    p3 <- ggplot(df, aes(x = DisabilityType, y = Mean_Time, fill = DisabilityType)) +
      geom_bar(stat = "identity") + scale_fill_manual(values = palette) + theme_minimal() + labs(title = "Mean Time")
    grid.arrange(p1, p2, p3, ncol = 3)
  })

  output$distributionPlots <- renderPlot({
    df <- filtered_data()
    p1 <- ggplot(df, aes(x = DisabilityType, fill = Gender)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = gender_colors) +
      theme_minimal() +
      labs(title = "Gender by Disability")

    p2 <- ggplot(df, aes(x = DisabilityType, fill = AssistanceNeeded)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = assist_colors) +
      theme_minimal() +
      labs(title = "Assistance by Disability")

    p3 <- ggplot(df, aes(x = AssistanceNeeded, fill = Gender)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = gender_colors) +
      theme_minimal() +
      labs(title = "Gender by Assistance")

    grid.arrange(p1, p2, p3, ncol = 3)
  })


  output$scatterPlots <- renderPlot({
    df <- filtered_data()
    p1 <- ggplot(df, aes(x = SUS_Score, y = Task_Completion_Time)) + geom_point() + geom_smooth(method = "lm") + theme_minimal()
    p2 <- ggplot(df, aes(x = SUS_Score, y = Number_of_Errors)) + geom_point() + geom_smooth(method = "lm") + theme_minimal()
    p3 <- ggplot(df, aes(x = Likert_Score, y = Task_Completion_Time)) + geom_point() + geom_smooth(method = "lm") + theme_minimal()
    (p1 | p2) / p3
  })

  output$diagnostics <- renderPlot({
    df <- filtered_data()
    p <- function(model, title) {
      ggplot(data.frame(Fitted = fitted(model), Residuals = resid(model)), aes(Fitted, Residuals)) +
        geom_point() + geom_hline(yintercept = 0, linetype = "dashed") + labs(title = title) + theme_minimal()
    }
    m1 <- lm(SUS_Score ~ DisabilityType, data = df)
    m2 <- lm(Task_Completion_Time ~ DisabilityType, data = df)
    m3 <- lm(Likert_Score ~ DisabilityType, data = df)
    p(m1, "SUS") / p(m2, "Time") / p(m3, "Likert")
  })

  output$statsSummary <- renderPrint({
    df <- filtered_data()
    cat("=== Correlations ===\n")
    print(cor.test(df$SUS_Score, df$Task_Completion_Time))
    print(cor.test(df$SUS_Score, df$Number_of_Errors))
    print(cor.test(df$Likert_Score, df$Task_Completion_Time))
    cat("\n=== ANOVAs ===\n")
    a1 <- aov_ez(id = "Student", dv = "SUS_Score", within = "DisabilityType", between = "Gender", data = df)
    a2 <- aov_ez(id = "Student", dv = "Task_Completion_Time", within = "DisabilityType", between = "Gender", data = df)
    a3 <- aov_ez(id = "Student", dv = "Likert_Score", within = "DisabilityType", between = "Gender", data = df)
    print(summary(a1))
    print(summary(a2))
    print(summary(a3))
  })
}

shinyApp(ui, server)
# To run the app:  shiny::runApp("/Users/carbeluche/Desktop/TFG/FDP")



