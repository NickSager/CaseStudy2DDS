library(shiny)
library(tidyverse)

df <- read.csv("CaseStudy2-data.csv")


# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Employment Case Study Explorer"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select the metric of interest ----
      radioButtons("metric", "Metric of Interest:",
                   c("Attrition" = "Attrition",
                     "Salary" = "MonthlyIncome")),

      # br() element to introduce extra vertical spacing ----
      br(),

      # Input: Select the desired plot ----
      radioButtons("plot", "Distribution type:",
                   c("Histogram" = "geom_histogram",
                     "Boxplot" = "geom_boxplot")),

      # br() element to introduce extra vertical spacing ----
      br(),

      # Input: Show Linear Regression ----
      radioButtons("group", "Group By:",
                   c("Job Role" = "JobRole",
                     "Department" = "Department")),

      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Predictor variable to compare to ----
      selectInput("predictor", "Variable:",
                c("Job Role" = "JobRole",
                  "Monthly Salary" = "MonthlyIncome",
                  "Job Satisfaction" = "JobSatisfaction")),

      #Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Grouped Summary", plotOutput("scatter")),
                  tabPanel("Individual Distribution", plotOutput("plot"))
      )

    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    # if (input$groupJobRole == TRUE) {
    df2 <- df %>%
      mutate(df, Attrition = ifelse(Attrition == "Yes", 1, 0))
        # group_by(!!sym(input$group)) %>%
        # summarise(
        #   Attrition = mean(Attrition),
        #   MonthlyIncome = mean(MonthlyIncome)
        # )
    # } else {
    #   df2 <- df %>%
    #     group_by(Department) %>%
    #     summarise(
    #       Attrition = mean(Attrition),
    #       MonthlyIncome = mean(MonthlyIncome)
    #     )
    # }
    if(input$plot == "geom_histogram") {
      df2 %>%
        ggplot(aes(x = !!sym(input$metric), fill = !!sym(input$group))) +
        geom_histogram(bins = input$bins) +
        labs(
            title = paste("Distribution of ", input$metric, sep = ""),
            x = paste(input$metric, sep = ""),
            y = "Count"
        )
    } else {
      df2 %>%
        # group_by(!!sym(input$group)) %>%
        # summarise(
        #   Attrition = mean(Attrition == "Yes"),
        #   MonthlyIncome = mean(MonthlyIncome)
        # ) %>%
        ggplot(aes(x = !!sym(input$metric), fill = !!sym(input$group))) +
        geom_boxplot()+
        labs(
            title = paste("Distribution of ", input$metric, sep = ""),
            x = paste(input$metric, sep = ""),
            y = "Count"
        )
    }   
  })

  # Generate a scatterplot of ABV vs IBU ----
  output$scatter <- renderPlot({
    df2 <- df %>%
      group_by(!!sym(input$group)) %>%
      summarise(
        Attrition = mean(Attrition == "Yes"),
        MonthlyIncome = mean(MonthlyIncome),
        JobSatisfaction = mean(JobSatisfaction)
      )
    # }
    df2 %>%
      ggplot(aes(x = !!sym(input$predictor), y = !!sym(input$metric), color = !!sym(input$group))) +
      geom_point() +
      labs(
        x = paste(input$Metric),
        y = "Count",
        title = paste(input$metric, input$predictor, sep = " vs. ")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Create Shiny app ----
shinyApp(ui, server)

# rsconnect::deployApp(appName = "CaseStudy2", appTitle = "MSDS 6306 Case Study 2")
