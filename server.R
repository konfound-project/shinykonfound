# server.R
# NOTE: IF UPDATING APP + REPUBLISHING, FIRST RUN THIS YOUR CONSOLE, THEN RELOAD, AND PUBLISH
## devtools::install_github("konfound-project/konfound")

library(shiny)
library(konfound)
library(shinyjs)

shinyServer(function(input, output, session) {
  
  # default outputs
  values <- reactiveValues(button = FALSE)
  values <- reactiveValues(button_nl = FALSE)
  values <- reactiveValues(button_t = FALSE)
  
  output$default_text_0 <- renderText({
    o <- if (!input$button) {
      "Text output will appear here when the the app is run"
    } else {
      NULL
    }
  })
  
  output$default_text_0b <- renderText({
    o <- if (!input$button_nl) {
      "Text output will appear here when the the app is run"
    } else {
      NULL
    }
  })
  
  output$default_text_1 <- renderText({
    o <- if (!input$button_t) {
      "Plot will appear here when the the app is run"
    } else {
      NULL
    }
  })
  
  output$default_text_2 <- renderText({
    o <- if (!input$button) {
      "Plot will appear here when the the app is run"
    } else {
      NULL
    }
  })
  
  # linear model
  df <- eventReactive(input$button, {
    
    validate(
      need(is.numeric(input$unstd_beta) &
             is.numeric(input$std_error) &
             is.numeric(input$n_obs) &
             is.numeric(input$n_covariates), "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input$n_obs > (input$n_covariates + 1), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")
    )
    
    out1 <- capture.output(pkonfound(as.numeric(input$unstd_beta), 
                                     as.numeric(input$std_error), 
                                     as.numeric(input$n_obs), 
                                     as.numeric(input$n_covariates),
                                     index = "RIR",
                                     to_return = c(c("print"))))
    
    out2 <- capture.output(pkonfound(as.numeric(input$unstd_beta),
                                    as.numeric(input$std_error),
                                    as.numeric(input$n_obs),
                                    as.numeric(input$n_covariates),
                                    index = "IT",
                                    to_return = c(c("print"))))
    
    out3 <- pkonfound(as.numeric(input$unstd_beta), 
                      as.numeric(input$std_error), 
                      as.numeric(input$n_obs), 
                      as.numeric(input$n_covariates),
                      to_return = c(c("thresh_plot", "corr_plot")))
    print(list(out1, out2, out3))
    list(out1, out2, out3)
    
  })
  
  # Linear output
  output$text1a <- renderText({
    paste0("<b>", df()[[1]][1], "</b>")
  })
  
  output$text1b <- renderText({
    df()[[1]][2:3]
  })
  
  output$text1c <- renderText({
    df()[[1]][5:6]
  })
  
  output$text1d <- renderText({
    df()[[1]][8]
  })
  
  output$text1e <- renderText({
    df()[[1]][10:13]
  })
  
  output$text2a <- renderText({
    paste0("<b>", df()[[2]][1], "</b>")
  })
  
  output$text2b <- renderText({
    df()[[2]][2:4]
  })
  
  output$text2c <- renderText({
    df()[[2]][6:7]
  })
  
  output$text2d <- renderText({
    df()[[2]][8]
  })
  
  output$text2e <- renderText({
    df()[[2]][10:12]
  })
  
  output$plot1 <- renderPlot({
    df()[[3]][[1]]
  })
  
  output$plot2 <- renderPlot({
    df()[[3]][[2]]
  })
  
  output$plot_text_1 <- renderText({
    df()
    "Right-click this plot to save it as a PNG file"}
  )
  
  output$plot_text_2 <- renderText({
    df()
    "Right-click this plot to save it as a PNG file"}
  )
  
  # non-linear model
  df_nl <- eventReactive(input$button_nl, {
    validate(
      need(is.numeric(input$unstd_beta_nl) &
             is.numeric(input$std_error_nl) &
             is.numeric(input$n_obs_nl) &
             is.numeric(input$n_covariates_nl) &
             is.numeric(input$n_trm_nl),
           "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input$n_obs_nl > (input$n_covariates_nl + 1),
           "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")
    )
    
    out <- pkonfound(input$unstd_beta_nl, 
              input$std_error_nl, 
              input$n_obs_nl, 
              input$n_covariates,
              n_treat = input$n_trm_nl, 
              model_type = "logistic",
              to_return = "raw_output")
    print(out)
    out
    
  })
  
  df_t <- eventReactive(input$button_t, {
    validate(
      need(is.numeric(input$ctrl_fail) &
             is.numeric(input$ctrl_success) &
             is.numeric(input$treat_fail) &
             is.numeric(input$treat_success),
           "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input$n_obs_nl > (input$n_covariates_nl + 1),
           "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")
    )
    
    out <- pkonfound(a = input$ctrl_fail, 
                     b = input$ctrl_success, 
                     c = input$treat_fail, 
                     d = input$treat_success,
                     to_return = "raw_output")
    print(out)
    out
    
  })
  
  # Non-linear output 
  
  observeEvent(input$button_nl, {
    updateTabsetPanel(session, 
                      "nl-output",
                      selected = "modelbased")
  })
  
  output$textnl1 <- renderText({
    paste0(df_nl()[[1]], ". ", df_nl()[[2]], " ", df_nl()[[3]])
  })
  
  output$textnl1_rir <- renderText({
    paste0(  
      "RIR: ",
      df_nl()$RIR)
  })

  output$textnl2i <- eventReactive(input$button_nl,{
    "Implied Table"
  })
  
  output$textnl2 <- renderTable({
    df_nl()$Implied_Table
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  output$textnl3i <- eventReactive(input$button_nl, {
    "Transfer Table"
  })
  
  output$textnl3 <- renderTable({
    df_nl()$Transfer_Table
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  output$textnl4 <- renderText({
    df_nl()[[7]]
  })
  
  output$textnl5 <- renderText({
    df_nl()[[8]]
  })
  
  # Non-linear output for tkonfound()
  
  observeEvent(input$button_t, {
    updateTabsetPanel(session, 
                      "nl-output",
                      selected = "twobytwo")
  })
  
  output$textt1 <- renderText({
    paste0(df_t()[[1]], " ", df_t()[[2]], ". ", df_t()[[3]], " ", df_t()[[4]], " ", df_t()[[5]])
  })
  
  output$textt1_rir <- renderText({
    paste0(  
      "RIR: ",
      df_t()$RIR)
  })
  
  output$textt4i <- eventReactive(input$button_t,{
    "Values Entered (Observed Table)"
  })

  output$textt4 <- renderTable({
    df_t()$User_enter_value
  }, digits = 0, rownames = TRUE, bordered = FALSE)

  output$textt5i <- eventReactive(input$button_t, {
    "Transfer Table"
  })

  output$textt4 <- renderTable({
    df_t()$User_enter_value
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  

  output$textt6 <- renderText({
    df_t()[[8]]
  })
  
  output$textt7 <- renderText({
    df_t()[[9]]
  })
  
})
