library(shiny)
library(tidyverse)
library(sjPlot)
library(performance)


# Assign model_id to data_df5 and match it to the metrics
data_df5$model_id <- as.character(seq_len(nrow(data_df5)))

# Rebind model metrics with model_id
LMmodel_metrics_df <- bind_rows(
  map(lm_results, extract_model_metrics),
  .id = "model_id"
)

LMERmodel_metrics_df <- bind_rows(
  map(lmer_results, extract_model_metrics),
  .id = "model_id"
)

timeLMERmodel_metrics_df <- bind_rows(
  map(TIMElmer_results, extract_model_metrics_timeLMER),
  .id = "model_id"
)

# Attach fork metadata
forks_df <- data_df5 %>%
  select(model_id,
         df1_extreme_values,
         df2_screen_fixation,
         df3_moving_average,
         df4_baseline_correction,
         df5_ppt_exclusion)

LMmodel_metrics_df <- left_join(LMmodel_metrics_df, forks_df, by = "model_id")
LMERmodel_metrics_df <- left_join(LMERmodel_metrics_df, forks_df, by = "model_id")
timeLMERmodel_metrics_df <- left_join(timeLMERmodel_metrics_df, forks_df, by = "model_id")


ui <- fluidPage(
  titlePanel("MB2-P Multiverse Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Model Type:", choices = c("LM", "LMER", "TimeLMER")),
      selectInput("metric", "Select Metric:",
                  choices = c("BIC", "AIC", "R_squared", "conditional_R2"), selected = "BIC"),
      tags$hr(),
      h4("Multiverse Forks"),
      selectInput("df1", "DF1: Plausibility", choices = c("all", unique(data_df5$df1_extreme_values))),
      selectInput("df2", "DF2: Screen Fixation", choices = c("all", unique(data_df5$df2_screen_fixation))),
      selectInput("df3", "DF3: Moving Average", choices = c("all", unique(data_df5$df3_moving_average))),
      selectInput("df4", "DF4: Baseline", choices = c("all", unique(data_df5$df4_baseline_correction))),
      selectInput("df5", "DF5: Participant Exclusion", choices = c("all", unique(data_df5$df5_ppt_exclusion)))
    ),
    
    mainPanel(
      plotOutput("metricPlot", click = "plot_click"),
      plotOutput("volcanoPlot", click = "volcano_click"),
      plotOutput("interactionPlot"),
      h4("Selected Model ID"),
      verbatimTextOutput("model_id_text")
    )
  )
)

server <- function(input, output, session) {
  clicked_model_id <- reactiveVal(NULL)
  
  full_data <- reactive({
    switch(input$model_type,
           "LM" = LMmodel_metrics_df,
           "LMER" = LMERmodel_metrics_df,
           "TimeLMER" = timeLMERmodel_metrics_df)
  })
  
  full_models <- reactive({
    switch(input$model_type,
           "LM" = lm_results,
           "LMER" = lmer_results,
           "TimeLMER" = TIMElmer_results)
  })
  
  selected_data <- reactive({
    df <- full_data()
    req(nrow(df) > 0)
    
    df <- df %>%
      filter(
        (df1_extreme_values == input$df1 | input$df1 == "all"),
        (df2_screen_fixation == input$df2 | input$df2 == "all"),
        (df3_moving_average == input$df3 | input$df3 == "all"),
        (df4_baseline_correction == input$df4 | input$df4 == "all"),
        (df5_ppt_exclusion == input$df5 | input$df5 == "all")
      )
    
    df$model_id <- as.factor(df$model_id)
    df
  })
  
  selected_results <- reactive({
    model_ids <- selected_data()$model_id
    full_models()[as.numeric(as.character(model_ids))]
  })
  
  observeEvent(input$plot_click, {
    data <- selected_data()
    clicked <- nearPoints(data, input$plot_click, xvar = "model_id", yvar = input$metric, maxpoints = 1)
    if (nrow(clicked) > 0) clicked_model_id(clicked$model_id)
  })
  
  observeEvent(input$volcano_click, {
    data <- selected_data()
    clicked <- nearPoints(data, input$volcano_click, xvar = "estimate", yvar = "p.value", maxpoints = 1)
    if (nrow(clicked) > 0) clicked_model_id(clicked$model_id)
  })
  
  output$model_id_text <- renderPrint({
    req(clicked_model_id())
    paste("Selected Model ID:", clicked_model_id())
  })
  
  output$metricPlot <- renderPlot({
    df <- selected_data()
    req(nrow(df) > 0)
    metric <- input$metric
    if (!metric %in% names(df)) return(NULL)
    
    ggplot(df, aes(x = model_id, y = .data[[metric]], color = model_id)) +
      geom_point(size = 3) +
      labs(title = paste("Specification Curve for", toupper(metric)),
           x = "Model ID", y = toupper(metric)) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$volcanoPlot <- renderPlot({
    df <- selected_data()
    req(nrow(df) > 0)
    
    gg <- ggplot(df, aes(x = estimate, y = -log10(p.value), color = p.value < 0.05)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_vline(xintercept = c(-0.02, 0.02), linetype = "dashed", color = "grey50") +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey50") +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "grey")) +
      labs(
        title = "Volcano Plot: Effect Size vs. Significance",
        x = "Estimate (3-way interaction)",
        y = expression(-log[10](p.value)),
        color = "p < .05"
      ) +
      theme_minimal()
    
    if (!is.null(clicked_model_id())) {
      highlight <- df %>% filter(model_id == clicked_model_id())
      gg <- gg + geom_text(data = highlight,
                           aes(label = model_id),
                           vjust = -1.2, color = "black", fontface = "bold")
    }
    
    gg
  })
  
  output$interactionPlot <- renderPlot({
    req(clicked_model_id())
    model_list <- selected_results()
    model_index <- which(names(model_list) == as.character(clicked_model_id()))
    req(length(model_index) > 0)
    selected_model <- model_list[[model_index]]
    req(!is.null(selected_model))
    plot_model(selected_model, type = "int")
  })
}

shinyApp(ui = ui, server = server)
