# server.R

library(shiny)
library(konfound)
library(shinyjs)

shinyServer(function(input, output) {
  
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
  output$text1 <- renderText({
    df()[[1]][1:3]
  })
  
  output$text2 <- renderText({
    df()[[1]][4:7]
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
  
  # Non-linear output
  # 
  output$textnl1 <- renderText({
    df_nl()[[1]][1]
  })
  
  output$textnl2i <- eventReactive(input$button_nl,{
    "Implied Table"
  })

  output$textnl2 <- renderTable({
    df_nl()[[2]]
  }, digits = 0, rownames = TRUE)

  output$textnl3i <- eventReactive(input$button_nl, {
    "Transfer Table"
  }) 
  
  output$textnl3 <- renderTable({
    df_nl()[[3]]
  }, digits = 0, rownames = TRUE)

  output$textnl4 <- renderText({
    df_nl()[[4]]
  })

  output$textnl5 <- renderText({
    df_nl()[[5]]
  })
  
})