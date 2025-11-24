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

#Data

wines_raw <- read_csv("AustralianWines.csv")

wines_ts <- wines_raw |>
  mutate(Month = yearmonth(Month)) |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(index = Month, key = Varietal)

# UI

ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel("Australian Wines Forecasting Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("var", "Choose Varietal:",
                  choices = unique(wines_ts$Varietal),
                  selected = "Fortified"),
      
      dateRangeInput(
        "dates", "Date Range:",
        start = min(wines_ts$Month),
        end   = max(wines_ts$Month),
        format = "yyyy-mm"
      ),
      
      sliderInput("h", "Forecast Horizon (Months):",
                  min = 6, max = 60, value = 24),
      
      actionButton("run", "Run Models", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Overview Plot",
                 plotlyOutput("overview")
        ),
        
        tabPanel("Decomposition",
                 plotOutput("decomp")
        ),
        
        tabPanel("Model Specs",
                 verbatimTextOutput("specs")
        ),
        
        tabPanel("Training Accuracy",
                 DTOutput("train_acc")
        ),
        
        tabPanel("Validation Accuracy",
                 DTOutput("test_acc")
        ),
        
        tabPanel("Forecast Plot",
                 plotlyOutput("forecast")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  filtered <- eventReactive(input$run, {
    wines_ts |>
      filter(
        Varietal == input$var,
        Month >= input$dates[1],
        Month <= input$dates[2]
      )
  })
  
  #Plot overview
  output$overview <- renderPlotly({
    req(filtered())
    p <- filtered() |>
      autoplot(Sales) +
      labs(title = paste(input$var, "Sales Over Time"))
    ggplotly(p)
  })
  
  #Decomp
  output$decomp <- renderPlot({
    req(filtered)
    filtered() |>
      model(STL = STL(Sales ~ season(window = "periodic"))) |>
      components() |>
      autoplot()
  })
  
  #models
  models <- eventReactive(input$run, {
    filtered() |>
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })
  
  output$specs <- renderPrint({
    req(models())
    report(models())
  })
  
  #Train/test split
  split_data <- eventReactive(input$run, {
    fd <- filtered()
    n <- nrow(fd)
    cutoff <- n - input$h
    
    list(
      train = fd[1:cutoff, ],
      test  = fd[(cutoff+1):n, ]
    )
  })
  
  #T. Accuracy 
  output$train_acc <- renderDT({
    req(split_data())
    train <- split_data()$train
    
    mod_train <- train |>
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
    
    datatable(accuracy(mod_train))
  })
  
  #Val. Accu
  output$test_acc <- renderDT({
    req(split_data())
    train <- split_data()$train
    test  <- split_data()$test
    
    mod_train <- train |>
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
    
    datatable(accuracy(mod_train, test))
  })
  
  output$forecast <- renderPlotly({
    req(models())
    fc <- models() |> forecast(h = input$h)
    
    p <- fc |>
      autoplot(filtered()) +
      labs(title = paste("Forecast for", input$var))
    
    ggplotly(p)
  })
  
}

shinyApp(ui, server)
