# server.R

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
                                     non_linear = FALSE,
                                     to_return = c(c("print"))))
    
    out2 <- pkonfound(as.numeric(input$unstd_beta), 
                      as.numeric(input$std_error), 
                      as.numeric(input$n_obs), 
                      as.numeric(input$n_covariates),
                      non_linear = FALSE,
                      to_return = c(c("thresh_plot", "corr_plot")))
    
    list(out1, out2)
    
  })
  
  # Linear output
  output$text1a <- renderText({
    paste0("<b>", df()[[1]][1], "</b>")
  })
  
  output$text1b <- renderText({
    df()[[1]][2:3]
  })
  
  output$text1c <- renderText({
    paste0("<em>", df()[[1]][4], "</em>")
  })
  
  output$text1d <- renderText({
    df()[[1]][5]
  })
  
  output$text2a <- renderText({
    paste0("<b>", df()[[1]][6], "</b>")
  })
  
  output$text2b <- renderText({
    df()[[1]][7:8]
  })
  
  output$text2c <- renderText({
    paste0("<em>", df()[[1]][9], "</em>")
  })
  
  output$text2d <- renderText({
    df()[[1]][10]
  })
  
  output$plot1 <- renderPlot({
    df()[[2]][[1]]
  })
  
  output$plot2 <- renderPlot({
    df()[[2]][[2]]
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
    
    pkonfound(input$unstd_beta_nl, 
              input$std_error_nl, 
              input$n_obs_nl, 
              input$n_covariates, 
              n_trm = input$n_trm_nl, 
              non_linear = TRUE)
    
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
    
    out <- tkonfound(input$ctrl_fail, 
              input$ctrl_success, 
              input$treat_fail, 
              input$treat_success)
    
    print(out)
    
  })
  
  # Non-linear output 
  
  observeEvent(input$button_nl, {
    updateTabsetPanel(session, 
                      "nl-output",
                      selected = "modelbased")
  })
  
  output$textnl1 <- renderText({
    df_nl()[[1]][1]
  })
  
  output$textnl2i <- eventReactive(input$button_nl,{
    "Implied Table"
  })
  
  output$textnl2 <- renderTable({
    df_nl()[[2]]
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  output$textnl3i <- eventReactive(input$button_nl, {
    "Transfer Table"
  }) 
  
  output$textnl3 <- renderTable({
    df_nl()[[4]]
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  output$textnl4 <- renderText({
    df_nl()[[5]]
  })
  
  output$textnl5 <- renderText({
    df_nl()[[6]]
  })
  
  # Non-linear output for tkonfound()
  
  observeEvent(input$button_t, {
    updateTabsetPanel(session, 
                      "nl-output",
                      selected = "twobytwo")
  })
  
  output$textt1 <- renderText({
    df_t()[[2]]
  })
  
  output$textt2i <- eventReactive(input$button_t,{
    "Values Entered (Observed Table)"
  })

  output$textt2 <- renderTable({
    df_t()$User_enter_value
  }, digits = 0, rownames = TRUE, bordered = FALSE)

  output$textt3i <- eventReactive(input$button_t, {
    "Transfer Table"
  })

  output$textt3 <- renderTable({
    df_t()$Transfer_Table
  }, digits = 0, rownames = TRUE, bordered = FALSE)

  output$textt4 <- renderText({
    df_t()[[5]]
  })
  
  output$textt5 <- renderText({
    df_t()[[6]]
  })
  
})