library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)

### Load your CSV

forecast <- read.csv(
  "https://raw.githubusercontent.com/SamInnes26/electricity-forecast/main/data/latest_forecast.csv"
)

forecast$date_ymd <- as.Date(forecast$date_ymd)

forecast <- forecast %>%
  
  mutate(
    
    outlook_7 = case_when(
      is.na(rank_7_day) ~ NA_character_,
      rank_7_day <= 2 ~ "🟢",
      rank_7_day <= 5 ~ "🟠",
      TRUE ~ "🔴"
    ),
    
    outlook_14 = case_when(
      rank_14_day <= 4 ~ "🟢",
      rank_14_day <= 10 ~ "🟠",
      TRUE ~ "🔴"
    )
    
  )

forecast <- forecast %>%
  
  mutate(
    relative_residual =
      avg_residual -
      mean(avg_residual, na.rm = TRUE)
  )

### Tracker Price Data

tracker <- read.csv(
  "https://raw.githubusercontent.com/SamInnes26/electricity-forecast/main/data/tracker_price_history.csv"
)

tracker <- tracker %>%
  mutate(
    date = as.Date(date)
  ) %>%
  filter(date >= as.Date("2026-08-01")) %>%
  arrange(date)

# Set benchmark rates (FUSE ENERGY 15m Fixed)
best_elec_rate <- 25.16
best_gas_rate <- 7.66

### Forecast History Data

forecast_history <- read.csv(
  "https://raw.githubusercontent.com/SamInnes26/electricity-forecast/main/data/forecast_history.csv",
)

performance_data <- forecast_history %>%
  mutate(date = ymd(date_ymd)) %>% 
  select(-date_ymd) %>% 
  left_join(
    tracker %>%
      select(date, elec_unit_rate),
    by = "date"
  ) %>%
  mutate(
    forecast_date = ymd(forecast_date),
    days_ahead = as.numeric(date - forecast_date)
  )

ui <- navbarPage(
  
  title = "Electricity Price Predictor",
  collapsible = TRUE,
  
  # ==========================================================
  # PAGE 1: EXISTING FORECAST
  # ==========================================================
  
  tabPanel(
    title = "Price Forecast",
    
    br(),
    
    fluidRow(
      
      column(
        6,
        wellPanel(
          h4("Best 7-Day Day"),
          textOutput("best7")
        )
      ),
      
      column(
        6,
        wellPanel(
          h4("Best 14-Day Day"),
          textOutput("best14")
        )
      )
    ),
    
    br(),
    
    DTOutput("rankTable"),
    
    br(),
    
    plotOutput(
      "residualPlot",
      height = "400px"
    )
  ),
  
  
  # ==========================================================
  # PAGE 2: TRACKER ANALYSIS
  # ==========================================================
  
  tabPanel(
    title = "Tracker Analysis",
    
    br(),
    
    h3("Tracker Analysis"),
    
    # Electricity line plot
    plotOutput(
      "elecLinePlot",
      height = "350px"
    ),
    
    br(),
    
    # Gas line plot
    plotOutput(
      "gasLinePlot",
      height = "350px"
    ),
    
    br(),
    
    h3("Distribution of Tracker Rates"),
    
    # Side-by-side histograms
    fluidRow(
      
      column(
        width = 6,
        plotOutput(
          "elecHistogram",
          height = "400px"
        )
      ),
      
      column(
        width = 6,
        plotOutput(
          "gasHistogram",
          height = "400px"
        )
      )
    )
  ),
  # ==========================================================
  # PAGE 3: PREDICTOR PERFORMANCE ANALYSIS
  # ==========================================================
  
  tabPanel(
    title = "Predictor Performance Analysis",
    
    br(),
    
    h3("Predictor Performance Analysis"),
    
    # Electricity line plot
    plotOutput(
      "trackerRatePlot",
      height = "350px"
    )
  )
)

server <- function(input, output, session) {
  
  output$best7 <- renderText({
    best <- forecast %>%
      filter(rank_7_day == min(rank_7_day, na.rm = TRUE))
    
    paste(format(best$date_ymd, "%d %b %Y"))
  })
  
  output$best14 <- renderText({
    best <- forecast %>%
      filter(rank_14_day == min(rank_14_day))
    
    paste(format(best$date_ymd, "%d %b %Y"))
  })
  
  output$lowest_residual <- renderText({
    round(min(forecast$avg_residual), 0)
  })
  
  output$residualPlot <- renderPlot({
    
    ggplot(
      forecast,
      aes(
        x = date_ymd,
        y = relative_residual
      )
    ) +
      
      geom_line(
        colour = "forestgreen",
        linewidth = 1
      ) +
      
      geom_point(
        colour = "forestgreen",
        size = 3
      ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        colour = "grey50"
      ) +
      
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%a %d %b"
      ) +
      
      theme_minimal() +
      
      
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      ) +
      
      annotate(
        "text",
        x = min(forecast$date_ymd),
        y = max(forecast$relative_residual),
        label = "↑ Dearer",
        hjust = 0
      ) +
      
      annotate(
        "text",
        x = min(forecast$date_ymd),
        y = min(forecast$relative_residual),
        label = "↓ Cheaper",
        hjust = 0
      ) +
      
      labs(
        title = "Relative Price Outlook",
        x = NULL,
        y = NULL
      ) 
    
  })
  
  output$elecLinePlot <- renderPlot({
    
    ggplot(
      tracker,
      aes(
        x = date,
        y = elec_unit_rate
      )
    ) +
      
      geom_line(
        colour = "forestgreen",
        linewidth = 1
      ) +
      
      geom_point(
        colour = "forestgreen",
        size = 2
      ) +
      
      geom_hline(
        yintercept = best_elec_rate,
        colour = "firebrick",
        linetype = "dashed",
        linewidth = 1
      ) +
      
      annotate(
        geom = "text",
        x = min(tracker$date, na.rm = TRUE),
        y = best_elec_rate,
        label = paste0(
          "Best alternative rate: ",
          best_elec_rate,
          "p/kWh"
        ),
        colour = "firebrick",
        hjust = 0,
        vjust = -0.7
      ) +
      
      scale_x_date(
        limits = c(
          as.Date("2026-08-01"),
          max(tracker$date, na.rm = TRUE)
        ),
        date_breaks = "1 week",
        date_labels = "%d %b",
        expand = expansion(mult = c(0.01, 0.02))
      ) +
      
      labs(
        title = "Tracker Electricity Unit Rate",
        subtitle = "Daily Tracker rate since 1 August 2026",
        x = NULL,
        y = "Unit rate (p/kWh)"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  output$gasLinePlot <- renderPlot({
    
    ggplot(
      tracker,
      aes(
        x = date,
        y = gas_unit_rate
      )
    ) +
      
      geom_line(
        colour = "steelblue",
        linewidth = 1
      ) +
      
      geom_point(
        colour = "steelblue",
        size = 2
      ) +
      
      geom_hline(
        yintercept = best_gas_rate,
        colour = "firebrick",
        linetype = "dashed",
        linewidth = 1
      ) +
      
      annotate(
        geom = "text",
        x = min(tracker$date, na.rm = TRUE),
        y = best_gas_rate,
        label = paste0(
          "Best alternative rate: ",
          best_gas_rate,
          "p/kWh"
        ),
        colour = "firebrick",
        hjust = 0,
        vjust = -0.7
      ) +
      
      scale_x_date(
        limits = c(
          as.Date("2026-08-01"),
          max(tracker$date, na.rm = TRUE)
        ),
        date_breaks = "1 week",
        date_labels = "%d %b",
        expand = expansion(mult = c(0.01, 0.02))
      ) +
      
      labs(
        title = "Tracker Gas Unit Rate",
        subtitle = "Daily Tracker rate since 1 August 2026",
        x = NULL,
        y = "Unit rate (p/kWh)"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  output$elecHistogram <- renderPlot({
    
    mean_elec_rate <- mean(
      tracker$elec_unit_rate,
      na.rm = TRUE
    )
    
    percentage_below <- mean(
      tracker$elec_unit_rate < best_elec_rate,
      na.rm = TRUE
    ) * 100
    
    ggplot(
      tracker,
      aes(x = elec_unit_rate)
    ) +
      
      geom_histogram(
        bins = 15,
        fill = "forestgreen",
        colour = "white"
      ) +
      
      geom_vline(
        xintercept = mean_elec_rate,
        colour = "black",
        linetype = "dotted",
        linewidth = 1
      ) +
      
      geom_vline(
        xintercept = best_elec_rate,
        colour = "firebrick",
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        title = "Electricity Rate Distribution",
        
        subtitle = paste0(
          round(percentage_below, 1),
          "% of Tracker rates were below the best alternative rate"
        ),
        
        x = "Unit rate (p/kWh)",
        y = "Number of days",
        
        caption = paste0(
          "Dotted line: mean (",
          round(mean_elec_rate, 2),
          "p) | Dashed line: best alternative rate (",
          best_elec_rate,
          "p)"
        )
      ) +
      
      theme_minimal() +
      
      theme(
        plot.caption = element_text(
          hjust = 0,
          colour = "grey40"
        )
      )
  })
  
  output$gasHistogram <- renderPlot({
    
    mean_gas_rate <- mean(
      tracker$gas_unit_rate,
      na.rm = TRUE
    )
    
    percentage_below <- mean(
      tracker$gas_unit_rate < best_gas_rate,
      na.rm = TRUE
    ) * 100
    
    ggplot(
      tracker,
      aes(x = gas_unit_rate)
    ) +
      
      geom_histogram(
        bins = 15,
        fill = "steelblue",
        colour = "white"
      ) +
      
      geom_vline(
        xintercept = mean_gas_rate,
        colour = "black",
        linetype = "dotted",
        linewidth = 1
      ) +
      
      geom_vline(
        xintercept = best_gas_rate,
        colour = "firebrick",
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        title = "Gas Rate Distribution",
        
        subtitle = paste0(
          round(percentage_below, 1),
          "% of Tracker rates were below the best alternative rate"
        ),
        
        x = "Unit rate (p/kWh)",
        y = "Number of days",
        
        caption = paste0(
          "Dotted line: mean (",
          round(mean_gas_rate, 2),
          "p) | Dashed line: best alternative rate (",
          best_gas_rate,
          "p)"
        )
      ) +
      
      theme_minimal() +
      
      theme(
        plot.caption = element_text(
          hjust = 0,
          colour = "grey40"
        )
      )
  })
  
  output$trackerRatePlot <- renderPlot({
    
    ggplot(
      performance_data,
      aes(
        x = avg_residual,
        y = elec_unit_rate,
        colour = days_ahead
      )
    ) +
      geom_point(size = 3, alpha = 1) +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = FALSE,
        colour = "black"
      ) +
      scale_colour_gradient(
        low = "lightblue",
        high = "darkblue",
        name = "Days ahead"
      ) +
      labs(
        title = "Tracker price vs residual demand",
        x = "Residual demand",
        y = "Tracker electricity price (p/kWh)"
      ) +
      theme_minimal()
    
  })
  
  
  output$rankTable <- renderDT({
    
    forecast %>%
      
      arrange(date_ymd) %>%
      
      transmute(
        Date = format(date_ymd, "%a %d %b"),
        `7 Day Outlook` = outlook_7,
        `14 Day Outlook` = outlook_14
      )
    
  },
  options = list(
    pageLength = 14,
    dom = 't'
  ))
}

shinyApp(ui, server)
