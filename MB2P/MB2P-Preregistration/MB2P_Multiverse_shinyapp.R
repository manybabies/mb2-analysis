#author: Giulia Calignano

library(shiny)
library(ggplot2)
library(purrr)
library(sjPlot)
# UI
ui <- fluidPage(
  titlePanel("MB2P - Exploring the multiverse of results"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Select Model Type:", 
                  choices = c("Linear Models" = "lm", "Mixed-Effects Models" = "lmer")),
      selectInput("metric", "Select Metric:", 
                  choices = c("BIC" = "BIC", "AIC" = "AIC", 
                              "R-Squared" = "R_squared", "Conditional R-Squared" = "conditional_R2")),
      p("Click on a point in the graph to view the interaction effect estimated by the corresponding model.")
    ),
    mainPanel(
      plotOutput("metricPlot", click = "plot_click"),
      plotOutput("interactionPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive dataset based on model type
  selected_data <- reactive({
    if (input$model_type == "lm") {
      LMmodel_metrics_df
    } else {
      LMERmodel_metrics_df
    }
  })
  
  # Reactive results based on model type
  selected_results <- reactive({
    if (input$model_type == "lm") {
      lm_results
    } else {
      lmer_results
    }
  })
  
  # Metric Plot
  output$metricPlot <- renderPlot({
    ggplot(selected_data(), aes(x = model_order, y = .data[[input$metric]], color = model_id)) +
      geom_point(size = 3) +
      labs(
        title = paste("Multiverse of", toupper(input$metric)),
        x = "Model Order",
        y = toupper(input$metric)
      ) +
      theme_minimal()
  })
  
  # Interaction Plot
  output$interactionPlot <- renderPlot({
    req(input$plot_click)
    
    # Identify the clicked model
    clicked_point <- nearPoints(
      selected_data(),
      input$plot_click,
      xvar = "model_order",           # The x-variable in the plot
      yvar = input$metric             # Dynamically set the y-variable based on user selection
    )
    req(nrow(clicked_point) == 1)    # Ensure a valid point was clicked
    
    # Get the corresponding model
    model_index <- clicked_point$model_order
    selected_model <- selected_results()[[model_index]]
    
    # Generate interaction plot
    if (input$model_type == "lm") {
      plot_model(selected_model, type = "int", terms = c("hp", "wt", "mpg"))  # Example for linear models
    } else {
      plot_model(selected_model, type = "int", terms = c("hp", "wt", "mpg"))  # Replace with mixed-model terms
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
