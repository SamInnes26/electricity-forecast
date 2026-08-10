library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Load your CSV
forecast <- read.csv("data/latest_forecast.csv")

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

ui <- fluidPage(
  
  titlePanel("Electricity Price Predictor"),
  
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
  
  plotOutput("residualPlot", height = "400px"),
  
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
