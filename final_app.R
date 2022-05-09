library(shiny)
library(fpp3)
library(readr)
library(dtplyr)
library(ggplot2)
library(ggpubr)
library(seasonal)
library(plotly)
library(zoo)
library(shinythemes)
library(rsconnect)

#Simple Models: (1) Naive, (2) Seasonal Naive, (3) Mean, & (4) Drift
#Exponential smoothing: (1) Holts & (2) Holts/Winters
#ARIMA: (1) Manually selected parameters & (2) Auto selected parameters


mytimeseries <- aus_arrivals

x11_dcmp <- mytimeseries %>%
  model(x11 = X_13ARIMA_SEATS(Arrivals ~ x11())) %>%
  components()

ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  h2("Arrivals to Australia from the US, UK, Japan, and New Zealand"),
  
  h3("Visualization Options and Interpretations"),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Welcome to my app!",
               textOutput("intro_direct")),
      
      tabPanel("Time Series Plot",
               checkboxGroupInput(inputId = "select_ts",
                                  label = "Select a Country",
                                  choices = c("Japan", "NZ", "UK", "US"),
                                  selected = "Japan"),
               checkboxInput(inputId = "trendline",
                             label = "Would you like to add a linear trend line?",
                             value = TRUE),
               plotlyOutput("ts_plot"),
               textOutput("ts_interpretation")),
      
      tabPanel("Seasonality Plot", 
               radioButtons(inputId = "select_seas",
                                  label = "Select a Country",
                                  choices = c("Japan", "NZ", "UK", "US"),
                                  selected = "Japan"),
               plotlyOutput("seasonal_plot"), 
               textOutput("seasonal_interpretation")),
      
      tabPanel("AutoCorrelation Plot",
               radioButtons(inputId = "select_ACF",
                                  label = "Select a Country",
                                  choices = c("Japan", "NZ", "UK", "US"),
                                  selected = "Japan"),
               plotOutput("ACF_plot"), 
               textOutput("ACF_interpretation")),
      
      tabPanel("Decompostion Plot", 
               plotlyOutput("decomp_plot"), 
               textOutput("decomp_interpretation")),
      
      tabPanel("Subseries Plot",
               radioButtons(inputId = "select_sub",
                            label = "Select a Country",
                            choices = c("Japan", "NZ", "UK", "US"),
                            selected = "Japan"),
               plotlyOutput("sub_plot"), 
               textOutput("sub_interpretation")),
      
      tabPanel("Naive Forecasting Model",
               radioButtons(inputId = "select_naive",
                            label = "Select a Country",
                            choices = c("Japan", "NZ", "UK", "US"),
                            selected = "Japan"),
               plotOutput("naive_model")),
      
      tabPanel("Mean Forecasting Model",
               radioButtons(inputId = "select_mean",
                            label = "Select a Country",
                            choices = c("Japan", "NZ", "UK", "US"),
                            selected = "Japan"),
               plotOutput("mean_model")),
      
      tabPanel("Seasonal Naive Forecasting Model",
               radioButtons(inputId = "select_season",
                            label = "Select a Country",
                            choices = c("Japan", "NZ", "UK", "US"),
                            selected = "Japan"),
               plotOutput("season_model")),
      
      tabPanel("Drift Forecasting Model",
               radioButtons(inputId = "select_drift",
                            label = "Select a Country",
                            choices = c("Japan", "NZ", "UK", "US"),
                            selected = "Japan"),
               plotOutput("drift_model"))
    )
  ),
)

server <- function(input, output){
  
  output$intro_direct <- renderText({
    "This app uses the Australian Arrivals timeseries dataset for Japan, New Zealand, the UK, and the US. The first tab will show the full series plotted. The second tab will show the timeseries seasonal plot. The third tab will show the autocorrelation of the timeseries. The fourth tab will show the X11 decomposition of the timerseries. The fifth tab will show the subseries plots for the timeseries. The timeseries, seasonality, and subseries plots allow you to select which countries you would like to include in the visualization. The rest of the tabs display different types of forecasting models. The country used in each model can be changed on the same page."
  })
  
  output$ts_plot <- renderPlotly({
    p1 <- mytimeseries %>%
      filter(Origin == input$select_ts) %>%
      ggplot(aes(x = Quarter, y = Arrivals, color = factor(Origin))) +
      geom_line()
    
    if(input$trendline == TRUE) {
      p1 <- p1 + geom_smooth(method = lm)
    }
    p1
  })
  
  output$ts_interpretation <- renderText({
    "For a large majority of the dataset, New Zealand has the largest amound of arrivals to Australia. Around the start of the first quarter in 1990 Japan had a large spike in arrivals and then steadily declined after 2000. US arrivals have slowly increased from 1980 to 2015."
  })
  
  output$seasonal_plot <- renderPlotly({
    mytimeseries %>% 
      filter(Origin == input$select_seas) %>%
      gg_season(Arrivals) +
      labs(title = "Aus Arrivals Seasonality Graph")
    }) 
  
  output$seasonal_interpretation <- renderText({
    "Japan is showing a lot of seasonality with the minimum arrivals being at the start of quarter two and the maximum arrivals being at the beginning of quarter three. The UK also shows strong seasonality with a steady decrease during quarter one, consistency during quarter two and then a sharp increase beginning in quarter three. New Zealand and the US show little seasonality."
  })
  
  output$ACF_plot <- renderPlot({
    mytimeseries %>%
      filter(Origin == input$select_ACF) %>%
      ACF(Arrivals) %>%
      autoplot() +
      labs(title = "Autocorrelation Plot of Arrivals to Australia")
    })
  
  output$ACF_interpretation <- renderText({
    "If substantially more than 5% of spikes are outside of the bounds, then the series is probably not white noise. In these plots, we have 21 lags and 5% of that is about 1.05. The only country with any spikes out of bounds is the UK having one spike shorter than the cutoff. Therefore the changes in arrivals per quarter for every country are not white noise. Additionally, the US, Japan, and New Zealand show scallops in their ACF plots indicating seasonality. The UK has a very patterned trend in the plot indicating cyclicity."
  })
  
  output$decomp_plot <- renderPlotly({
    autoplot(x11_dcmp) +
      labs(title =  "Arrivals to Australia X11 Decomposition")
  })
  
  output$decomp_interpretation <- renderText({
    " New Zealand, the UK, and the US show increasing trend-cycles. Japan shows a increasing trend-cycle and then a decreasing trend beginning at aroung 1998. New Zealand has the strongest upward trend-cycle. New Zealand and the US show strong signs of seasonality in the decomposition."
  })
  
  output$sub_plot <- renderPlotly({
    mytimeseries %>%
      filter(Origin == input$select_sub) %>%
      gg_subseries() +
      labs(title = "Subseries Plot of Arrivals to Australia")
  })
  
  output$sub_interpretation <- renderText({
    "Japan shows increasing trends for all quarters until the late 1990s. New Zealand shows a strong upwards trend for all quarters with the highest amount of arrivals in quarter 3 and quarter 4. The US shows consistent upwards trends with about the same number of arrivals for each quarter. The UK shows alot more arrivals during the first and fourth quarters and low numbers during the second and third quarters."
  })
  
  output$naive_model <- renderPlot({
    
    naive_data <- mytimeseries %>%
      filter(Origin == input$select_naive)
    
    train_naive <- naive_data %>%
      filter_index("1992 Q1" ~ "2006 Q4")
    
    naive_fit <- naive_data %>%
      model(NAIVE(Arrivals)) %>%
      forecast(h = 12)
    
    naive_fit %>%
      autoplot(train_naive, level = NULL) + 
      autolayer(
        filter_index(naive_data, "2006 Q1" ~ .),
        colour = "black") +
      labs(title = "Naive Forecasting Model")
  })
    
    output$mean_model <- renderPlot({
      
      mean_data <- mytimeseries %>%
        filter(Origin == input$select_mean)
      
      train_mean <- mean_data %>%
        filter_index("1992 Q1" ~ "2006 Q4")
      
      mean_fit <- mean_data %>%
        model(MEAN(Arrivals)) %>%
        forecast(h = 12)
      
      mean_fit %>%
        autoplot(train_mean, level = NULL) + 
        autolayer(
          filter_index(mean_data, "2006 Q1" ~ .),
          colour = "black") +
        labs(title = "Mean Forecasting Model")
    })
    
    output$season_model <- renderPlot({
      
      season_data <- mytimeseries %>%
        filter(Origin == input$select_season)
      
      train_season <- season_data %>%
        filter_index("1992 Q1" ~ "2006 Q4")
      
      season_fit <- season_data %>%
        model(SNAIVE(Arrivals ~lag("year"))) %>%
        forecast(h = 12)
      
      season_fit %>%
        autoplot(train_season, level = NULL) + 
        autolayer(
          filter_index(season_data, "2006 Q1" ~ .),
          colour = "black") +
        labs(title = "Seasonal Naive Forecasting Model")
    })
    
    output$drift_model <- renderPlot({
      
      drift_data <- mytimeseries %>%
        filter(Origin == input$select_drift)
      
      train_drift <- drift_data %>%
        filter_index("1992 Q1" ~ "2006 Q4")
      
      drift_fit <- drift_data %>%
        model(RW(Arrivals ~ drift())) %>%
        forecast(h = 12)
      
      drift_fit %>%
        autoplot(train_drift, level = NULL) + 
        autolayer(
          filter_index(drift_data, "2006 Q1" ~ .),
          colour = "black") +
        labs(title = "Drift Forecasting Model")
    })
}

shinyApp(ui = ui, server = server)
