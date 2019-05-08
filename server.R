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
             is.numeric(input$n_covariates), "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number.")
    )
    
    validate(
      need(input$n_obs > (input$n_covariates + 1), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")
    )
    capture.output(pkonfound(as.numeric(input$unstd_beta), 
                             as.numeric(input$std_error), 
                             as.numeric(input$n_obs), 
                             as.numeric(input$n_covariates),
                             non_linear = FALSE))
  })
  
  df1 <- eventReactive(input$button1, {
    
    validate(
      need(is.numeric(input$unstd_beta) &
             is.numeric(input$std_error) &
             is.numeric(input$n_obs) &
             is.numeric(input$n_covariates), "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number.")
    )
    
    validate(
      need(input$n_obs > (input$n_covariates + 1), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")
    )
  
    
    x <- pkonfound(as.numeric(input$unstd_beta1), 
                             as.numeric(input$std_error1), 
                             as.numeric(input$n_obs1), 
                             as.numeric(input$n_covariates1),
                             n_trm = as.numeric(input$n_trm),
                             non_linear = TRUE)
    
    list(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]])
  })
  
  # non-linear model
  
  # Output
  
  output$text0 <- renderText({
    all <- df1()
    "Note: This output is currently in beta and is subject to change!"
  })
  
  output$text1 <- renderText({
    all <- df()
    all[1:3]
  })
  
  output$text2 <- renderText({
    all <- df()
    all[4:7]
  })
  
  output$textnl1 <- renderText({
    all <- df1()
    all[[1]]
  })
  
  output$textnl2i <- renderText({
    all <- df1()
    "Implied Table"
  })
  
  output$textnl2 <- renderTable({
    all <- df1()
    all[[2]]
  }, digits = 0, rownames = TRUE)
  
  output$textnl3i <- renderText({
    all <- df1()
    "Transfer Table"
  })
  
  output$textnl3 <- renderTable({
    all <- df1()
    all[[3]]
  }, digits = 0, rownames = TRUE)
  
  output$textnl4 <- renderText({
    all <- df1()
    all[[4]]
  })
  
  output$textnl5 <- renderText({
    all <- df1()
    all[[5]]
  })
  
  p1 <- eventReactive(input$button, {
    pkonfound(as.numeric(input$unstd_beta), 
              as.numeric(input$std_error), 
              as.numeric(input$n_obs), 
              as.numeric(input$n_covariates),
              to_return = "thresh_plot")
  })
  
  output$plot1 <- renderPlot({
    p1()
  })
  
  p2 <- eventReactive(input$button, {
    pkonfound(as.numeric(input$unstd_beta), 
              as.numeric(input$std_error), 
              as.numeric(input$n_obs), 
              as.numeric(input$n_covariates),
              to_return = "corr_plot")
  })
  
  output$plot2 <- renderPlot({
    p2()
  })
  
})