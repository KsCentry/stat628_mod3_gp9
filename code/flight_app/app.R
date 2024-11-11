library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

cancel_delay <- readRDS('cancel_delay.rds')

ui <- fluidPage(
  titlePanel("Flight Data Explorer, 24-25 Holiday Season"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("origin", "Select Departure:", 
                  choices = sort(unique(cancel_delay$ORIGIN)), 
                  selected = NULL,
                  multiple = FALSE),
      
      dateInput("departure_date", "Select Departure Date:", value = NULL,
                min = "2024-11-01", max = "2025-01-31", format = "yyyy-mm-dd"),
      
      uiOutput("destination_ui"),
      
      actionButton("generate", "Generate Charts"),
      
      fluidRow(
        column(6, textOutput("avg_cancellation_rate")),
        column(6, textOutput("avg_delay_time"))
      ),
      
      hr(),
      h4("Contact Information:"),
      p("For any inquiries, please contact app maintainer:"),
      p("Hengyu Yang: hyang644@wisc.edu")
    ),
    
    mainPanel(
      h4("Cancellation Rate History"),
      plotOutput("cancellation_plot"),
      
      h4("Delay Time History"),
      plotOutput("delay_plot")
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$generate, {
    origin <- input$origin
    date <- input$departure_date
    
    if (length(date) == 0) {
      monthly_daily_data <- cancel_delay %>%
        filter(ORIGIN == origin) %>%
        group_by(MONTH, DAY) %>%
        summarise(
          avg_cancellation_rate = mean(avg_cancellation_rate, na.rm = TRUE),
          avg_delay_time = mean(avg_delay_time, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          date = as.Date(paste("2024", MONTH, DAY, sep = "-"), format = "%Y-%m-%d"),
          date_label = format(date, "%m-%d")
        ) %>%
        arrange(date)
      
      date_labels <- monthly_daily_data$date_label
      date_labels <- date_labels[seq(1, length(date_labels), by = 7)]
      
      output$cancellation_plot <- renderPlot({
        ggplot(monthly_daily_data, aes(x = date_label)) +
          geom_line(aes(y = avg_cancellation_rate), color = "blue", group = 1) +
          labs(x = "Month-Day", y = "Average Cancellation Rate",
               title = paste("Daily Average Cancellation Rate for", origin, "in Month-Day View")) +
          scale_x_discrete(breaks = date_labels) +
          theme(
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      output$delay_plot <- renderPlot({
        ggplot(monthly_daily_data, aes(x = date_label)) +
          geom_line(aes(y = avg_delay_time / 60), color = "red", group = 1) +
          labs(x = "Month-Day", y = "Average Delay Time (hours)",
               title = paste("Daily Average Delay Time for", origin, "in Month-Day View")) +
          scale_x_discrete(breaks = date_labels) +
          theme(
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            axis.text.x = element_text(angle = 45, hjust = 1))
      })
    } else {
      selected_data <- cancel_delay %>%
        filter(ORIGIN == origin, MONTH == month(date), DAY == day(date))
      
      output$cancellation_plot <- renderPlot({
        ggplot(selected_data, aes(x = HOUR)) +
          geom_line(aes(y = avg_cancellation_rate), color = "blue") +
          labs(y = "Average Cancellation Rate", title = paste("Hourly Cancellation Rate on", date, "for", origin)) +
          theme(
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            axis.text.x = element_text(angle = 0, hjust = 1))
      })
      
      output$delay_plot <- renderPlot({
        ggplot(selected_data, aes(x = HOUR)) +
          geom_line(aes(y = avg_delay_time / 60), color = "red") +
          labs(y = "Average Delay Time (hours)", title = paste("Hourly Delay Time on", date, "for", origin)) +
          theme(
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"),
            axis.text.x = element_text(angle = 0, hjust = 1))
      })
      
      avg_cancellation_rate <- mean(selected_data$avg_cancellation_rate, na.rm = TRUE)
      avg_delay_time <- mean(selected_data$avg_delay_time, na.rm = TRUE)
      
      output$avg_cancellation_rate <- renderText({
        paste("Predicted Cancellation Rate:", round(avg_cancellation_rate * 100, 2), "%")
      })
      output$avg_delay_time <- renderText({
        paste("Predicted Delay Time:", round(avg_delay_time, 2), "minutes")
      })
    }
  })
}


shinyApp(ui = ui, server = server)
