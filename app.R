library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

# Load the master dataset
data <- read.csv("master_health_demographics_dataset.csv")

# UI Definition
ui <- dashboardPage(
  skin = "blue", 
  dashboardHeader(title = "HealthPulse Sri Lanka"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "summary", icon = icon("th")),
      menuItem("Demographics & Pyramid", tabName = "demo", icon = icon("users")),
      menuItem("Disease Trends", tabName = "disease", icon = icon("virus")),
      menuItem("Healthcare & Indicators", tabName = "healthcare", icon = icon("hospital")),
      menuItem("Correlation Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Data Explorer", tabName = "rawdata", icon = icon("database"))
    ),
    hr(),
    selectInput("year_select", "Select Focus Year:", 
                choices = sort(unique(data$Year), decreasing = FALSE), 
                selected = min(data$Year))
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "summary",
              fluidRow(
                valueBoxOutput("pop_vbox", width = 3),
                valueBoxOutput("life_vbox", width = 3),
                valueBoxOutput("mort_vbox", width = 3),
                valueBoxOutput("exp_vbox", width = 3)
              ),
              fluidRow(
                box(width = 8, title = "National Gender Distribution", status = "primary", solidHeader = TRUE,
                    helpText("Proportion of Male vs. Female population for the selected year."),
                    plotlyOutput("genderPlot")),
                box(width = 4, title = "Project Information", status = "info", solidHeader = TRUE,
                    h4("Group Members:"),
                    tags$ul(
                      tags$li(strong("L. A. R. P. S. Samarasinghe")),
                      tags$li(strong("K. A. T. Hansinee"))
                    ),
                    hr(),
                    p("HealthPulse Sri Lanka: Monitoring gender-specific demographic shifts and healthcare outcomes."),
                    p(em("Data: World Bank (2000-2024)"))
                )
              )
      ),
      
      tabItem(tabName = "demo",
              fluidRow(
                box(width = 6, 
                    title = "Population Structure (Pyramid)", 
                    status = "primary", 
                    solidHeader = TRUE,
                    plotlyOutput("pyramidPlot")),
                
                box(width = 6, 
                    title = "Demographic Factors Explorer", 
                    status = "primary", 
                    solidHeader = TRUE,
                    selectInput("demo_ind", "Select Demographic Metric:", 
                                choices = c(
                                  "Population, total", "Population, male", "Population, female",
                                  "Birth rate, crude (per 1,000 people)", 
                                  "Fertility rate, total (births per woman)", 
                                  "Sex ratio at birth (male births per female births)",
                                  "Population ages 0-14 (% of total population)",
                                  "Population ages 15-64 (% of total population)",
                                  "Population ages 65 and above (% of total population)",
                                  "Age dependency ratio (% of working-age population)",
                                  "Age dependency ratio, old (% of working-age population)",
                                  "Age dependency ratio, young (% of working-age population)",
                                  "Death rate, crude (per 1,000 people)",
                                  "Mortality rate, infant (per 1,000 live births)",
                                  "Mortality rate, adult, male (per 1,000 male adults)",
                                  "Mortality rate, adult, female (per 1,000 female adults)",
                                  "Number of maternal deaths",
                                  "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
                                  "Suicide mortality rate (per 100,000 population)",
                                  "Survival to age 65, male (% of cohort)",
                                  "Survival to age 65, female (% of cohort)"
                                )),
                    plotlyOutput("demoTrend"))
              )
      ),
      
      tabItem(tabName = "disease",
              fluidRow(
                box(width = 12, title = "Disease Incidence & Prevalence", status = "warning", solidHeader = TRUE,
                    selectInput("dis_ind", "Choose Disease Metric:", 
                                choices = unique(data$Indicators[grep("Incidence|Prevalence", data$Indicators)])),
                    plotlyOutput("diseasePlot"))
              )
      ),
      
      tabItem(tabName = "healthcare",
              fluidRow(
                box(width = 12, title = "Healthcare Infrastructure & Investment", status = "success", solidHeader = TRUE,
                    selectInput("health_ind", "Select Healthcare Metric:", 
                                choices = c("Current health expenditure (% of GDP)", 
                                            "Hospital beds (per 1,000 people)", 
                                            "Physicians (per 1,000 people)", 
                                            "Nurses and midwives (per 1,000 people)")),
                    plotlyOutput("healthPlot"))
              )
      ),
      
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 16, title = "Statistical Correlation Explorer", status = "info", solidHeader = TRUE,
                    fluidRow(
                      column(6, selectInput("corr_x", "X-Axis (Predictor):", choices = unique(data$Indicators))),
                      column(6, selectInput("corr_y", "Y-Axis (Outcome):", choices = unique(data$Indicators), selected = "Life expectancy at birth, total (years)"))
                    ),
                    plotlyOutput("corrPlot"))
              )
      ),
      
      tabItem(tabName = "rawdata",
              box(width = 12, title = "Master Data Table", status = "info",
                  DTOutput("dataTable"))
      )
    )
  )
)

server <- function(input, output) {
  
  summary_data <- reactive({ data %>% filter(Year == as.numeric(input$year_select)) })
  
  output$pop_vbox <- renderValueBox({
    val <- summary_data() %>% filter(Indicators == "Population, total") %>% pull(Value)
    valueBox(formatC(ifelse(length(val)==0, 0, val), format="d", big.mark=","), "Total Population", icon = icon("users"), color = "aqua")
  })
  output$life_vbox <- renderValueBox({
    val <- summary_data() %>% filter(Indicators == "Life expectancy at birth, total (years)") %>% pull(Value)
    valueBox(round(ifelse(length(val)==0, 0, val), 1), "Life Expectancy", icon = icon("heartbeat"), color = "purple")
  })
  output$mort_vbox <- renderValueBox({
    val <- summary_data() %>% filter(Indicators == "Mortality rate, infant (per 1,000 live births)") %>% pull(Value)
    valueBox(ifelse(length(val)==0, 0, val), "Infant Mortality", icon = icon("skull"), color = "yellow")
  })
  output$exp_vbox <- renderValueBox({
    val <- summary_data() %>% filter(Indicators == "Current health expenditure (% of GDP)") %>% pull(Value)
    valueBox(paste0(ifelse(length(val)==0, 0, val), "%"), "Health Exp (% GDP)", icon = icon("dollar-sign"), color = "green")
  })
  
  output$genderPlot <- renderPlotly({
    male_pop <- summary_data() %>% filter(Indicators == "Population, male") %>% pull(Value)
    female_pop <- summary_data() %>% filter(Indicators == "Population, female") %>% pull(Value)
    if(length(male_pop) == 0) male_pop <- 0
    if(length(female_pop) == 0) female_pop <- 0
    gender_df <- data.frame(Gender = c("Male", "Female"), Count = c(male_pop, female_pop))
    plot_ly(gender_df, labels = ~Gender, values = ~Count, type = 'pie', hole = 0.5,
            marker = list(colors = c('#0984e3', '#e84393'))) %>%
      layout(title = paste("Population Breakdown by Gender (", input$year_select, ")", sep=""),
             showlegend = TRUE, margin = list(l=20, r=20, b=20, t=50))
  })
  
  # POPULATION PYRAMID — FIXED: ordered factor for correct age group sorting
  output$pyramidPlot <- renderPlotly({
    
    # Define the correct age order explicitly
    age_order <- c("0-4","5-9","10-14","15-19","20-24","25-29",
                   "30-34","35-39","40-44","45-49","50-54",
                   "55-59","60-64","65-69","70-74","75-79","80+")
    
    py_data <- data %>%
      filter(Year == as.numeric(input$year_select),
             Indicators == "Population by Age and Gender",
             Age_Group != "Total") %>%
      mutate(
        # Convert to ordered factor so ggplot sorts bars correctly
        Age_Group = factor(Age_Group, levels = age_order, ordered = TRUE),
        Value = ifelse(Gender == "Male", -Value, Value)
      )
    
    if(nrow(py_data) == 0) return(NULL)
    
    p <- ggplot(py_data, aes(x = Age_Group, y = Value, fill = Gender)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(labels = abs) +
      scale_fill_manual(values = c("Female" = "red", "Male" = "#0984e3")) +
      theme_minimal() +
      labs(x = "Age Group", y = "Count (thousands)")
    
    ggplotly(p)
  })
  
  output$demoTrend <- renderPlotly({ 
    req(input$demo_ind)
    p <- ggplot(data %>% filter(Indicators == input$demo_ind), aes(Year, Value)) + 
      geom_line(color="#0984e3", size=1) + geom_point() + theme_minimal()
    ggplotly(p)
  })
  
  output$epiTrend <- renderPlotly({ 
    req(input$epi_ind)
    p <- ggplot(data %>% filter(Indicators == input$epi_ind), aes(Year, Value)) + 
      geom_line(color="#d63031", size=1) + geom_point() + theme_minimal()
    ggplotly(p)
  })
  
  output$diseasePlot <- renderPlotly({ 
    req(input$dis_ind)
    p <- ggplot(data %>% filter(Indicators == input$dis_ind), aes(Year, Value)) + 
      geom_area(fill="pink", alpha=0.4) + 
      geom_line(color = "darkorange", linewidth = 1) + 
      theme_minimal()
    ggplotly(p)
  })
  
  output$healthPlot <- renderPlotly({ 
    req(input$health_ind)
    p <- ggplot(data %>% filter(Indicators == input$health_ind), aes(Year, Value)) + 
      geom_line(color="darkgreen", size=1) + geom_point() + theme_minimal()
    ggplotly(p)
  })
  
  output$corrPlot <- renderPlotly({
    req(input$corr_x, input$corr_y)
    x_df <- data %>% filter(Indicators == input$corr_x, Age_Group == "Total") %>% select(Year, X = Value)
    y_df <- data %>% filter(Indicators == input$corr_y, Age_Group == "Total") %>% select(Year, Y = Value)
    corr_df <- inner_join(x_df, y_df, by = "Year")
    p <- ggplot(corr_df, aes(X, Y)) + geom_point(color="purple") + 
      geom_smooth(method="lm", se=F, color="grey") + theme_minimal() + 
      labs(x = input$corr_x, y = input$corr_y)
    ggplotly(p)
  })
  
  output$dataTable <- renderDT({ 
    datatable(data, options = list(pageLength = 10, scrollX = TRUE)) 
  })
}

shinyApp(ui, server)

