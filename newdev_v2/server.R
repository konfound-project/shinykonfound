#New development - Conditional panel approach
#server

library(shiny)
library(tidyverse)
library(konfound)
library(shinyjs)
library(rclipboard)


server <- function(input, output, session) {
  

  
  ################################### 
  ###### GENERATE LINEAR RESULTS ###### 
  ###################################
  
  #validate user input values to make sure they are 1) numeric and 2) more than 1 covariate for all model types
  df <- eventReactive(input$results_pg_l, {
    
    #validating user input is numeric
    validate(
      need(is.numeric(input$unstd_beta) &
             is.numeric(input$std_error) &
             is.numeric(input$n_obs) &
             is.numeric(input$n_covariates), "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input$n_obs > (input$n_covariates + 1), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")
    )
    
    
    #if statements needed for linear printed output and figure (displays the correct information for RIR or ITCV)
    if(input$AnalysisL == "RIR"){
      linear_output <- capture.output(pkonfound(as.numeric(input$unstd_beta), 
                                       as.numeric(input$std_error), 
                                       as.numeric(input$n_obs), 
                                       as.numeric(input$n_covariates),
                                       index = "RIR",
                                       to_return = c(c("print", "corr_plot"))))
    }
    if(input$AnalysisL == "ITCV"){
      linear_output <- capture.output(pkonfound(as.numeric(input$unstd_beta), 
                                                as.numeric(input$std_error), 
                                                as.numeric(input$n_obs), 
                                                as.numeric(input$n_covariates),
                                                index = "IT",
                                                to_return = c(c("print", "thresh_plot"))))
    }
    if(input$AnalysisL == "RIR"){
      linear_plot <- pkonfound(as.numeric(input$unstd_beta), 
                                                as.numeric(input$std_error), 
                                                as.numeric(input$n_obs), 
                                                as.numeric(input$n_covariates),
                                                index = "RIR",
                                                to_return = "thresh_plot")
    }
    if(input$AnalysisL == "ITCV"){
      linear_plot <- pkonfound(as.numeric(input$unstd_beta), 
                                                as.numeric(input$std_error), 
                                                as.numeric(input$n_obs), 
                                                as.numeric(input$n_covariates),
                                                index = "IT",
                                                to_return = "corr_plot")
    }
    
    print(list(linear_output, linear_plot))
    list(linear_output, linear_plot)
  })
  
  # Linear printed output
    output$linear_results_print1 <- renderText({
    df()[[1]][2:4]
  })

  
    
  # Linear printed output
    output$linear_results_print2 <- renderText({
    df()[[1]][5:7]
  })
  
  
  #Linear printed plot
    output$plot1 <- renderPlot({
    df()[[2]]
  })
  
  
  
  ######################################### 
  ###### GENERATE LOGISTIC RESULTS  ###### 
  ######################################### 
  
  # non-linear model
  df_log <- eventReactive(input$results_pg_di, {
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
  
  # Logistic printed output
  output$di_results_print1 <- renderText({
    paste(df_log()[[1]][1], df_log()[[2]][1], df_log()[[3]][1])
  })
  
  # Logistic printed output
  output$di_results_print2 <- renderText({
    df_log()[[2]][1]
  })
  
  #Logistic printed implied table
  output$log_implied_title <- eventReactive(input$results_pg_di,{ #Title for Implied table
    "Implied Table"
  })
  
  output$log_transfer_title <- eventReactive(input$results_pg_di,{ #Title for Transfer table
    "Transfer Table"
  })
  
  output$di_implied_table <- renderTable({
    df_log()$Implied_Table
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  
  output$di_transfer_table <- renderTable({
    df_log()$Transfer_Table
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  
  ######################################### 
  ######### GENERATE 2x2 RESULTS  ######### 
  ######################################### 
  
  df_twobytwo <- eventReactive(input$results_pg_2x2, {
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
  # 2x2 Table printed output
  output$two_results_print1 <- renderText({
    paste(df_twobytwo()[[1]][1], df_twobytwo()[[2]][1], df_twobytwo()[[3]][1], df_twobytwo()[[4]][1], df_twobytwo()[[5]][1])
  })
  
  #Two by Two Observed Table Output
  output$two_observed_title <- eventReactive(input$results_pg_2x2,{ #Title for Observed table
    "Values Entered (Observed Table)"
  })
  
  output$two_transfer_title <- eventReactive(input$results_pg_2x2,{ #Title for Transfer table
    "Transfer Table"
  })
  
  output$two_observed_table <- renderTable({
    df_twobytwo()$User_enter_value
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  output$two_transfer_table <- renderTable({
    df_twobytwo()$Transfer_Table
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  
  
  ################################### 
  ###### GENERATE R/STATA CODE ###### 
  ###################################
  
  #generate r code for linear models using user input values
  user_est_l <- eventReactive(input$results_pg_l, {
    paste("pkonfound(", input$unstd_beta, ", ", input$std_error, ", ", input$n_obs, ", ", input$n_covariates, ", ", "index = ", input$AnalysisL, ")", sep = "")
  })
  
  #generate r code for logistic models using user input values
  user_est_di <- eventReactive(input$results_pg_di, {
    paste("pkonfound(", input$unstd_beta_nl, ", ", input$std_error_nl, ", ", input$n_obs_nl, ", ", input$n_covariates_nl, ", n_treat = ", input$n_trm_nl, ", index = ", input$Analysis, ", model_type = 'logistic')", sep = "")
  })
  
  #generate r code for 2x2 tables using user input values
  user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste("pkonfound(a = ", input$ctrl_fail, ", b = ", input$ctrl_success, ", c = ", input$treat_fail, ", d = ", input$treat_success, ")", sep = "")
  })
  
  
  #conditional statement to display the correct r code based on model type
  select_r_code <- reactive({
    #req(input$Data, input$Outcome)
    
    if(input$Outcome == "Linear"){
      r_code <- user_est_l()
    }
    
    if(input$Outcome == "Dichotomous"){
      if(input$Data == "2x2 table"){
        r_code <- user_est_2x2()
      }
      if(input$Data == "Logistic model"){
        r_code <- user_est_di()
      }
    }
    
    r_code
  })
  

  
  #Render r code in UI.R to display for user
  output$r_code_print <- renderText({
    select_r_code()
  })
  
  
  
  #Add clipboard buttons
  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy code",
      clipText = select_r_code(),
      icon = icon("clipboard"))
  })
    
  #If user hits the Run button for linear models, take to results page (linked)
  observeEvent(input$results_pg_l ,{
    updateNavbarPage(session, "mainpage", "results")
  }) 
  
  #If user hits the Run button for logistic models, take to results page (linked)
  observeEvent(input$results_pg_di ,{
    updateNavbarPage(session, "mainpage", "results")
  }) 
  
  #If user hits the Run button for 2x2 table, take to results page (linked)
  observeEvent(input$results_pg_2x2 ,{
    updateNavbarPage(session, "mainpage", "results")
  }) 
  
  
}