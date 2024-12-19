
#author: Giulia Calignano

library(shiny)
library(ggplot2)
library(purrr)
library(sjPlot)

# UI
ui <- fluidPage(
  titlePanel("MB2P - Exploring the Multiverse of Results"),
  
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
      plotOutput("specCurvePlot"),
      plotOutput("interactionPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive dataset based on model type
  selected_data <- reactive({
    if (input$model_type == "lm") {
      LMmodel_metrics_df  # This should be defined in your environment
    } else {
      LMERmodel_metrics_df  # This should be defined in your environment
    }
  })
  
  # Reactive results based on model type
  selected_results <- reactive({
    if (input$model_type == "lm") {
      lm_results  # List of linear models
    } else {
      lmer_results  # List of mixed-effects models
    }
  })
  
  # Metric Plot
  output$metricPlot <- renderPlot({
    ggplot(selected_data(), aes(x = model_id, y = .data[[input$metric]], color = model_id)) +
      geom_point(size = 3) +
      labs(
        title = paste("Multiverse of", toupper(input$metric)),
        x = "Model Order",
        y = toupper(input$metric)
      ) +
      theme_minimal()
  })
  
  # Specification Curve Plot
  output$specCurvePlot <- renderPlot({
    # Extract coefficients and terms from all models
    spec_data <- map_dfr(seq_along(selected_results()), function(i) {
      model <- selected_results()[[i]]
      coefs <- summary(model)$coefficients
      data.frame(
        term = rownames(coefs),
        estimate = coefs[, "Estimate"],
        model_id = i
      )
    })
    
    ggplot(spec_data, aes(x = term, y = estimate, color = as.factor(model_id), group = as.factor(model_id))) +
      geom_line() +
      geom_point(size = 2) +
      labs(
        title = "Specification Curve: Effects Across Models",
        x = "Terms",
        y = "Estimated Effect",
        color = "Model ID"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
