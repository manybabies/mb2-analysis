# Author: Giulia Calignano

library(shiny)
library(ggplot2)
library(purrr)
library(sjPlot)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("MB2P - Exploring the Multiverse of Results"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Select Model Type:", 
                  choices = c("Linear Models (without time)" = "lm", "Linear Models (with time)" = "lmer", "Mixed-Effects Models (with time)" = "TIMElmer")),
      selectInput("metric", "Select Metric:", 
                  choices = c("BIC" = "BIC", "AIC" = "AIC", 
                              "R-Squared" = "R_squared", "Conditional R-Squared" = "conditional_R2")),
      p("Click a point on either plot to view the model ID and its interaction plot.")
    ),
    mainPanel(
      plotOutput("metricPlot", click = "plot_click"),
      plotOutput("interactionPlot", click = "volcano_click"),
      plotOutput("modelInteractionPlot")
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  clicked_model_id <- reactiveVal(NULL)
  
  selected_data <- reactive({
    if (input$model_type == "lm") {
      LMmodel_metrics_df
    } else {
      LMERmodel_metrics_df
    }
  })
  
  selected_results <- reactive({
    if (input$model_type == "lm") {
      lm_results
    } else {
      lmer_results
    }
  })
  
  # Handle clicks from either plot
  observeEvent(input$plot_click, {
    data <- selected_data()
    clicked <- nearPoints(data, input$plot_click, xvar = "model_id", yvar = input$metric, maxpoints = 1)
    if (nrow(clicked) > 0) {
      clicked_model_id(clicked$model_id)
    }
  })
  
  observeEvent(input$volcano_click, {
    data <- selected_data()
    clicked <- nearPoints(data, input$volcano_click, xvar = "estimate", yvar = "p.value", maxpoints = 1)
    if (nrow(clicked) > 0) {
      clicked_model_id(clicked$model_id)
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
      theme(legend.position = "none")
  })
  
  # Volcano Plot (colored by significance)
  output$interactionPlot <- renderPlot({
    data <- selected_data()
    data$model_id <- as.factor(data$model_id)
    
    gg <- ggplot(data, aes(x = estimate, y = -(log10(p.value)), color = p.value < 0.05)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_vline(xintercept = c(-0.02, 0.02), linetype = "dashed", color = "grey40") +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey40") +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "grey60")) +
      labs(
        title = "Volcano Plot of Model Performance vs. Effect Size",
        x = "Effect Size: Three-way Interaction",
        y = expression(-log[10](p-value)),
        color = "Significant (p < 0.05)"
      ) +
      theme_minimal()
    
    if (!is.null(clicked_model_id())) {
      highlight <- data %>% filter(model_id == clicked_model_id())
      gg <- gg +
        geom_text(data = highlight,
                  aes(x = estimate, y = -log10(p.value), label = model_id),
                  color = "black", vjust = -1.2, size = 5, fontface = "bold",
                  inherit.aes = FALSE)
    }
    
    gg
  })
  
  # Interaction Plot
  output$modelInteractionPlot <- renderPlot({
    req(clicked_model_id())
    model_index <- as.numeric(clicked_model_id())
    model_list <- selected_results()
    req(model_index <= length(model_list))
    selected_model <- model_list[[model_index]]
    
    plot_model(selected_model, type = "int")  
  })
}

# Run the App
shinyApp(ui = ui, server = server)
