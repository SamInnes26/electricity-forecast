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
    days_ahead = as.numeric(date - forecast_date),
    da_weight = exp(-0.4 * days_ahead)
  )

mu <- mean(performance_data$avg_residual)
sigma <- sd(performance_data$avg_residual)

forecast <- forecast %>%
  
  mutate(
    avg_residual_z_score =
      (avg_residual - mu)/sigma
  )


# Fit model
model <- lm(
  elec_unit_rate ~ avg_residual,
  data = performance_data,
  weights = da_weight
)
r2 <- summary(model)$r.squared

# Coordinates for label placement
x_pos <- max(performance_data$avg_residual, na.rm = TRUE)
y_pos <- min(performance_data$elec_unit_rate, na.rm = TRUE)


# actual ranking

performance_data <- performance_data |>
  group_by(forecast_date) |>
  mutate(
    rank_14_day_actual = if(
      sum(!is.na(elec_unit_rate)) == 14
    ) {
      rank(elec_unit_rate, ties.method = "min")
    } else {
      rep(NA_integer_, n())
    },
    
    rank_7_day_actual = if(
      sum(!is.na(elec_unit_rate[days_ahead <= 7])) == 7
    ) {
      replace(
        rep(NA_integer_, n()),
        days_ahead <= 7,
        rank(
          elec_unit_rate[days_ahead <= 7],
          ties.method = "min"
        )
      )
    } else {
      rep(NA_integer_, n())
    }
  ) |>
  ungroup()


capture_rate_data <- performance_data |>
  mutate(
    rank_7_day_cheapest_hit = as.integer(rank_7_day & rank_7_day_actual == 1),
    rank_7_day_cheapest_2 = as.integer(rank_7_day == 1 & rank_7_day_actual <= 2),
    rank_7_day_cheap_range = as.integer(rank_7_day <= 2 & rank_7_day_actual <= 2),
    rank_7_day_cheap_range_relaxed = as.integer(rank_7_day <= 2 & rank_7_day_actual <= 3),
    rank_14_day_cheapest_hit = as.integer(rank_14_day == 1 & rank_14_day_actual == 1),
    rank_14_day_cheapest_4 = as.integer(rank_14_day == 1 & rank_14_day_actual <= 4),
    rank_14_day_cheap_range = as.integer(rank_14_day <= 4 & rank_14_day_actual <= 4),
    rank_14_day_cheap_range_relaxed = as.integer(rank_14_day <= 4 & rank_14_day_actual <= 6),
  ) 

rank_7_day_cheapest_hit_rate <- capture_rate_data |> 
  filter(rank_7_day == 1) |> 
  summarise(rate = 100*round(mean(rank_7_day_cheapest_hit, na.rm = T),4)) |> 
  pull()

rank_7_day_cheapest_2_rate <- capture_rate_data |> 
  filter(rank_7_day == 1) |> 
  summarise(rate = 100*round(mean(rank_7_day_cheapest_2, na.rm = T),4)) |> 
  pull()

rank_7_day_cheap_range_rate <- capture_rate_data |> 
  filter(rank_7_day <= 2) |> 
  summarise(rate = 100*round(mean(rank_7_day_cheap_range, na.rm = T),4)) |> 
  pull()

rank_7_day_cheap_range_relaxed_rate <- capture_rate_data |> 
  filter(rank_7_day <= 2) |> 
  summarise(rate = 100*round(mean(rank_7_day_cheap_range_relaxed, na.rm = T),4)) |> 
  pull()

rank_14_day_cheapest_hit_rate <- capture_rate_data |> 
  filter(rank_7_day == 1) |> 
  summarise(rate = 100*round(mean(rank_14_day_cheapest_hit, na.rm = T),4)) |> 
  pull()

rank_14_day_cheapest_4_rate <- capture_rate_data |> 
  filter(rank_7_day == 1) |> 
  summarise(rate = 100*round(mean(rank_14_day_cheapest_4, na.rm = T),4)) |> 
  pull()

rank_14_day_cheap_range_rate <- capture_rate_data |> 
  filter(rank_7_day <= 4) |> 
  summarise(rate = 100*round(mean(rank_14_day_cheap_range, na.rm = T),4)) |> 
  pull()

rank_14_day_cheap_range_relaxed_rate <- capture_rate_data |> 
  filter(rank_7_day <= 6) |> 
  summarise(rate = 100*round(mean(rank_14_day_cheap_range_relaxed, na.rm = T),4)) |> 
  pull()


prediction_arrows <- forecast_history |>
  mutate(
    forecast_date = ymd(forecast_date),
    date_ymd = ymd(date_ymd)
  ) |>
  filter(rank_7_day == 1) |>
  left_join(
    tracker |>
      select(
        forecast_date = date,
        elec_unit_rate_forecast_date = elec_unit_rate
      ),
    by = "forecast_date"
  ) |>
  left_join(
    tracker |>
      select(
        date_ymd = date,
        elec_unit_rate_fc_min_date = elec_unit_rate
      ),
    by = "date_ymd"
  ) |>
  filter(
    !is.na(elec_unit_rate_forecast_date),
    !is.na(elec_unit_rate_fc_min_date)
  ) |>
  select(
    forecast_date,
    date_ymd,
    avg_residual,
    elec_unit_rate_forecast_date,
    elec_unit_rate_fc_min_date
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
  # PAGE 2: TRACKER RATE ANALYSIS
  # ==========================================================
  
  tabPanel(
    title = "Tracker Rate Analysis",
    
    br(),
    
    h3("Tracker Rate Analysis"),
    
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
    
    br(),
    
    h4("7-Day Forecast"),
    
    br(),
    
    fluidRow(
      
      column(
        3,
        wellPanel(
          h4("Cheapest Day Accuracy"),
          h5("Percentage of forecasts where the predicted cheapest day was actually the cheapest day."),
          textOutput("rank_7_day_cheapest_hit_rate")
        )
      ),
      
      column(
        3,
        wellPanel(
          h4("Cheapest Day Top-2 Accuracy"),
          h5("Percentage of forecasts where the predicted cheapest day was among the two cheapest actual days."),
          textOutput("rank_7_day_cheapest_2_rate")
        )
      ),
      
      column(
        3,
        wellPanel(
          h4("Cheapest Pair Capture Rate"),
          h5("Percentage of the predicted cheapest two days that were actually the two cheapest days."),
          textOutput("rank_7_day_cheap_range_rate")
        )
      ),
      
      column(
        3,
        wellPanel(
          h4("Cheapest Pair Capture Rate (Relaxed)"),
          h5("Percentage of the predicted cheapest two days that were actually among the three cheapest days."),
          textOutput("rank_7_day_cheap_range_relaxed_rate")
        )
      ),
      
      
    ),
    
    br(),
    
    h4("14-Day Forecast"),
    
    br(),
    
    fluidRow(
      
      column(
        3,
        wellPanel(
          h4("Cheapest Day Accuracy"),
          h5("Percentage of forecasts where the predicted cheapest day was actually the cheapest day."),
          textOutput("rank_14_day_cheapest_hit_rate")
        )
      ),
      
      column(
        3,
        wellPanel(
          h4("Cheapest Day Top-4 Accuracy"),
          h5("Percentage of forecasts where the predicted cheapest day was among the four cheapest actual days."),
          textOutput("rank_14_day_cheapest_4_rate")
        )
      ),
      
      column(
        3,
        wellPanel(
          h4("Cheapest Period Capture Rate"),
          h5("Percentage of the predicted cheapest four days that were actually the four cheapest days."),
          textOutput("rank_14_day_cheap_range_rate")
        )
      ),
      
      column(
        3,
        wellPanel(
          h4("Cheapest Period Capture Rate (Relaxed)"),
          h5("Percentage of the predicted cheapest four days that were actually among the six cheapest days."),
          textOutput("rank_14_day_cheap_range_relaxed_rate")
        )
      ),
      
      
    ),
    
    
    br(),
    
    # Electricity line plot
    plotOutput(
      "trackerRatePlot",
      height = "350px"
    ),
    
    br(),
    
    plotOutput(
      "predictionArrowsPlot",
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
        y = avg_residual_z_score
      )
    ) +
      
      geom_line(
        data = forecast |> slice(1:7),
        colour = "forestgreen",
        linewidth = 1,
        linetype = "solid"
      ) +
      
      geom_line(
        data = forecast |> slice(7:n()),
        colour = "forestgreen",
        linewidth = 1,
        linetype = "dashed"
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
        y = max(forecast$avg_residual_z_score),
        label = "↑ Dearer",
        hjust = 0,
        size = unit(9, "pt")
      ) +
      
      annotate(
        "text",
        x = min(forecast$date_ymd),
        y = min(forecast$avg_residual_z_score),
        label = "↓ Cheaper",
        hjust = 0, 
        size = unit(9, "pt")
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
  
  output$rank_7_day_cheapest_hit_rate <- renderText({
    paste0(round(rank_7_day_cheapest_hit_rate, 4), "%")
  })
  
  output$rank_7_day_cheapest_2_rate <- renderText({
    paste0(round(rank_7_day_cheapest_2_rate, 4), "%")
  })
  
  output$rank_7_day_cheap_range_rate <- renderText({
    paste0(round(rank_7_day_cheap_range_rate, 4), "%")
  })
  
  output$rank_7_day_cheap_range_relaxed_rate <- renderText({
    paste0(round(rank_7_day_cheap_range_relaxed_rate, 4), "%")
  })
  
  output$rank_14_day_cheapest_hit_rate <- renderText({
    paste0(round(rank_14_day_cheapest_hit_rate, 4), "%")
  })
  
  output$rank_14_day_cheapest_4_rate <- renderText({
    paste0(round(rank_14_day_cheapest_4_rate, 4), "%")
  })
  
  output$rank_14_day_cheap_range_rate <- renderText({
    paste0(round(rank_14_day_cheap_range_rate, 4), "%")
  })
  
  output$rank_14_day_cheap_range_relaxed_rate <- renderText({
    paste0(round(rank_14_day_cheap_range_relaxed_rate, 4), "%")
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
        aes(weight = da_weight),
        se = FALSE,
        colour = "black"
      ) +
      annotate(
        "text",
        x = x_pos,
        y = y_pos,
        label = paste0("R² = ", round(r2, 3)),
        hjust = 1.1,
        vjust = -0.5,
        size = 5,
        colour = "black"
      ) +
      scale_colour_gradient(
        high = "lightblue",
        low = "darkblue",
        name = "Days ahead"
      ) +
      labs(
        title = "Tracker price vs residual demand",
        x = "Residual demand",
        y = "Tracker electricity price (p/kWh)"
      ) +
      theme_minimal()
    
  })
  
  output$predictionArrowsPlot <- renderPlot({
    ggplot(
      tracker,
      aes(
        x = date,
        y = elec_unit_rate
      )
    ) +
      
      geom_line(
        colour = "grey40",
        linewidth = 1
      ) +
      
      geom_point(
        colour = "grey40",
        size = 2
      ) +
      
      geom_curve(
        data = prediction_arrows,
        aes(
          x = forecast_date,
          y = elec_unit_rate_forecast_date,
          xend = date_ymd,
          yend = elec_unit_rate_fc_min_date
        ),
        inherit.aes = FALSE,
        colour = "forestgreen",
        linewidth = 0.7,
        curvature = -0.15,
        alpha = 0.65,
        arrow = arrow(
          type = "closed",
          length = unit(0.12, "inches")
        )
      ) +
      
      scale_x_date(
        date_breaks = "1 week",
        date_labels = "%d %b"
      ) +
      
      labs(
        title = "Predicted Cheapest Electricity Days",
        subtitle = paste(
          "Each arrow shows the cheapest day predicted",
          "over the following seven days"
        ),
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
