library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(tidyr)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(plotly)
library(DT)

# -----------------------------
# Load & Prepare Data
# -----------------------------

wines_raw <- read_csv("AustralianWines.csv")

wines_ts <- wines_raw |>
  mutate(Month = yearmonth(Month)) |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(index = Month, key = Varietal)

# -----------------------------
# UI
# -----------------------------

ui <- navbarPage(
  title = "Australian Wines Forecast Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  # --------------------------------------------------------------------------------
  # TAB 1 — EXPLORE
  # --------------------------------------------------------------------------------
  tabPanel("Explore",
           
           sidebarLayout(
             sidebarPanel(
               selectInput("var1", "Choose Varietal:",
                           choices = unique(wines_ts$Varietal),
                           selected = "Fortified"),
               
               dateRangeInput(
                 "dates1", "Date Range:",
                 start = min(wines_ts$Month),
                 end   = max(wines_ts$Month),
                 format = "yyyy-mm"
               )
             ),
             
             mainPanel(
               h3("Overview Plot"),
               plotlyOutput("overview1"),
               br(),
               h3("Decomposition"),
               plotOutput("decomp1")
             )
           )
  ),
  
  # --------------------------------------------------------------------------------
  # TAB 2 — TRAIN & DIAGNOSE
  # --------------------------------------------------------------------------------
  tabPanel("Train & Diagnose",
           
           sidebarLayout(
             sidebarPanel(
               selectInput("var2", "Choose Varietal:",
                           choices = unique(wines_ts$Varietal),
                           selected = "Fortified"),
               
               dateRangeInput(
                 "dates2", "Training Date Range:",
                 start = min(wines_ts$Month),
                 end   = max(wines_ts$Month),
                 format = "yyyy-mm"
               ),
               
               selectInput("model_type", "Choose Model:",
                           choices = c("TSLM", "ETS", "ARIMA")),
               
               sliderInput("h2", "Forecast Horizon (Months):",
                           min = 6, max = 60, value = 24),
               
               actionButton("run2", "Run Model", class = "btn-primary")
             ),
             
             mainPanel(
               h3("Model Specification"),
               verbatimTextOutput("specs2"),
               br(),
               h3("Training Accuracy"),
               DTOutput("train_acc2"),
               br(),
               h3("Validation Accuracy"),
               DTOutput("test_acc2")
             )
           )
  ),
  
  # --------------------------------------------------------------------------------
  # TAB 3 — FORECAST
  # --------------------------------------------------------------------------------
  tabPanel("Forecast",
           
           sidebarLayout(
             sidebarPanel(
               
               selectInput("var3", "Choose Varietal:",
                           choices = unique(wines_ts$Varietal),
                           selected = "Fortified"),
               
               sliderInput("h3", "Forecast Horizon:",
                           min = 6, max = 60, value = 24),
               
               actionButton("run3", "Generate Forecast", class = "btn-success")
             ),
             
             mainPanel(
               h3("Forecast Plot"),
               plotlyOutput("forecast3")
             )
           )
  )
  
)

# -----------------------------
# SERVER
# -----------------------------

server <- function(input, output, session) {
  
  # ============================================================================
  # TAB 1 — EXPLORE
  # ============================================================================
  
  explore_data <- reactive({
    wines_ts |>
      filter(
        Varietal == input$var1,
        Month >= input$dates1[1],
        Month <= input$dates1[2]
      )
  })
  
  output$overview1 <- renderPlotly({
    p <- explore_data() |>
      autoplot(Sales) +
      labs(title = paste(input$var1, "Sales Over Time"))
    ggplotly(p)
  })
  
  output$decomp1 <- renderPlot({
    explore_data() |>
      model(STL = STL(Sales ~ season(window = "periodic"))) |>
      components() |>
      autoplot()
  })
  
  # ============================================================================
  # TAB 2 — TRAIN & DIAGNOSE
  # ============================================================================
  
  train_data <- eventReactive(input$run2, {
    wines_ts |>
      filter(
        Varietal == input$var2,
        Month >= input$dates2[1],
        Month <= input$dates2[2]
      )
  })
  
  fit_models <- eventReactive(input$run2, {
    train_data() |>
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })
  
  output$specs2 <- renderPrint({
    req(input$run2)
    mod <- fit_models()
    report(mod[[input$model_type]])
  })
  
  # Train/validation split
  validation_split <- eventReactive(input$run2, {
    fd <- train_data()
    n <- nrow(fd)
    cutoff <- n - input$h2
    
    list(
      train = fd[1:cutoff, ],
      test  = fd[(cutoff+1):n, ]
    )
  })
  
  output$train_acc2 <- renderDT({
    split <- validation_split()
    train <- split$train
    
    mods <- train |>
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
    
    datatable(accuracy(mods))
  })
  
  output$test_acc2 <- renderDT({
    split <- validation_split()
    train <- split$train
    test  <- split$test
    
    mods <- train |>
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
    
    datatable(accuracy(mods, test))
  })
  
  # ============================================================================
  # TAB 3 — FORECAST
  # ============================================================================
  
  forecast_data <- eventReactive(input$run3, {
    wines_ts |>
      filter(Varietal == input$var3)
  })
  
  forecast_models <- eventReactive(input$run3, {
    forecast_data() |>
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })
  
  output$forecast3 <- renderPlotly({
    req(input$run3)
    fc <- forecast_models() |> forecast(h = input$h3)
    
    p <- fc |>
      autoplot(forecast_data()) +
      labs(title = paste("Forecast for", input$var3))
    
    ggplotly(p)
  })
}

shinyApp(ui, server)
