library(shiny)
library(tidyverse)

ui <- shiny::fluidPage(
  shiny::titlePanel("Individualizable Simulation Peters 2006"),

  shiny::actionButton("button", "Simulate"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(

      shiny::radioButtons(inputId = "bias_type", #Internal name
                          label = "Select a mechanism for the publication bias", # Character string shown to the user to let them know what they should do
                          selected = "p",
                          choiceNames = c("p-value", "Effect size"),
                          choiceValues = c("p", "es")), # User will automatically see first choice

      shiny::radioButtons(inputId = "bias_strength", #Internal name
                          label = "Select the strength of the p value based publication bias", # Character string shown to the user to let them know what they should do
                          selected = "severe",
                          choices = c("moderate", "severe")), # User will automatically see first choice

      shiny::sliderInput(inputId = "bias_percentage",
                        label = "Chose a percentage of studies that should be censored by publication bias",
                        value = 14,
                        min = 1,
                        max = 100),

      shiny::numericInput(inputId = "odds_ratio",
                          label = "Specify an Odds Ratio",
                          value = 1.5),

      shiny::numericInput(inputId = "heterogeneity",
                   label = "Specify the heterogeneity",
                   value = 0),

      shiny::numericInput(inputId = "ma_size",
                   label = "Specify the numer of studies in the meta-analysis",
                   value = 30),

      shiny::textInput(inputId = "prob_cg_distr",
                label = "Provide a distribution for the event probability in the control group.",
                value = "runif(1, min = 0.3, max = 0.7)"), #enter appropriate default

      shiny::textInput(inputId = "n_cg_distr",
                label = "Provide a distribution for the sample size of the control group.",
                value = "exp(rnorm(1, mean = 5, sd = sqrt(0.3))) %>% round()"),

      shiny::numericInput(inputId = "sim_rep",
                label = "How many repetitions should be run?",
                value = 50)),

    shiny::mainPanel(
      "Your parameter constellation had the following power:",
      shiny::tableOutput("power"),

      "The same parameter constellation in the absence of publication bias displays the following type I error rates:",
      shiny::tableOutput("type_1")
    )
  )
)


server <- function(input, output, session){

  output$power <- shiny::renderTable({
    input$button

    n_cg_distr <- shiny::isolate(input$n_cg_distr)
    prob_cg_distr <- shiny::isolate(input$prob_cg_distr)

    peters2006::run_sim(iteration_range = 1:shiny::isolate(input$sim_rep),
                        scenarios = peters2006::compile_scenarios(bias_type = shiny::isolate(input$bias_type),
                                                                  bias_percentage = shiny::isolate(input$bias_percentage)/100,
                                                                  bias_strength = shiny::isolate(input$bias_strength),
                                                                  odds_ratio = shiny::isolate(input$odds_ratio),
                                                                  heterogeneity = shiny::isolate(input$heterogeneity),
                                                                  ma_size = shiny::isolate(input$ma_size),
                                                                  prob_cg_distr = rlang::eval(prob_cg_distr),
                                                                  n_cg_distr = rlang::eval(n_cg_distr),
                                                                  bias_table = bias_table
                                                                  )) %>%
      dplyr::group_by(job_id) %>%
        dplyr::summarize(egger = egger_test(or_sim, se_lnor, sig_threshold = 0.1),
                  peters = peters_test(or_sim, n, a, b, c, d, sig_threshold = 0.1)) %>%
        dplyr::summarize(power_peters = mean(peters),
                  power_egger = mean(egger)) %>%
          rlang::set_names(c("Peters' Regression Test", "Egger's Regression Test"))
  })

    output$type_1 <- shiny::renderTable({
      input$button

      n_cg_distr <- shiny::isolate(input$n_cg_distr)
      prob_cg_distr <- shiny::isolate(input$prob_cg_distr)

      peters2006::run_sim(iteration_range = 1:shiny::isolate(input$sim_rep),
                          scenarios = peters2006::compile_scenarios(bias_type = "es",
                                                                    bias_percentage = 0,
                                                                    bias_strength = shiny::isolate(input$bias_strength),
                                                                    odds_ratio = shiny::isolate(input$odds_ratio),
                                                                    heterogeneity = shiny::isolate(input$heterogeneity),
                                                                    ma_size = shiny::isolate(input$ma_size),
                                                                    prob_cg_distr = rlang::eval(prob_cg_distr),
                                                                    n_cg_distr = rlang::eval(n_cg_distr),
                                                                    bias_table = bias_table
                                                                    )) %>%
        dplyr::group_by(job_id) %>%
          dplyr::summarize(egger = egger_test(or_sim, se_lnor, sig_threshold = 0.1),
                           peters = peters_test(or_sim, n, a, b, c, d, sig_threshold = 0.1)) %>%
          dplyr::summarize(type_1_error_peters = mean(peters),
                           type_1_error_egger = mean(egger)) %>%
            rlang::set_names(c("Peters' Regression Test", "Egger's Regression Test"))
  })
}

shiny::shinyApp(ui = ui, server = server)
