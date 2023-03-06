#New development - Conditional panel approach
#server

library(shiny)
library(tidyverse)
library(konfound)
library(shinyjs)
library(rclipboard)
library(fedmatch)


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
    if(input$AnalysisL == "IT"){
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
    if(input$AnalysisL == "IT"){
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

  ##################################### 
  ######### TRIGGER RESULTS PAGE ######## 
  ######################################
  
  
  #If user hits the Run button for linear models, take to results page (linked)
  observeEvent(input$results_pg_l ,{
    updateNavbarPage(session, "mainpage", "Results")
  }) 
  
  #If user hits the Run button for logistic models, take to results page (linked)
  observeEvent(input$results_pg_di ,{
    updateNavbarPage(session, "mainpage", "Results")
  }) 
  
  #If user hits the Run button for 2x2 table, take to results page (linked)
  observeEvent(input$results_pg_2x2 ,{
    updateNavbarPage(session, "mainpage", "Results")
  }) 
  
  
  
  
  ################################### 
  ###### GENERATE PRINTED OUTPUT ###### 
  ###################################
  
  r <- reactiveValues(print_results = "") #create empty reactive string for printed results.
  
  #If user presses the results button for linear models, paste the linear results
  observeEvent(input$results_pg_l, {
    r$print_results <- paste(df()[[1]][2], df()[[1]][3], df()[[1]][4], "<br>", "<br>", df()[[1]][5], df()[[1]][6], df()[[1]][7])
  })
  
  #If user presses the results button for logistic models, paste the logistic results
  observeEvent(input$results_pg_di, {
    r$print_results <- paste(df_log())
  
    
    #r$print_results <- paste(df_log()[[5]][1], df_log()[[6]][1], "<br>", "<br>", df_log()[[1]][1], df_log()[[2]][1], stringr::str_remove(df_log()[[3]][1], "\\.$"), "(Fragility).")
  })
  
  #If user presses the results button for 2x2 tables, paste the 2x2 results
  observeEvent(input$results_pg_2x2, {
    r$print_results <-paste(df_twobytwo()[[1]][1], df_twobytwo()[[2]][1], df_twobytwo()[[3]][1], df_twobytwo()[[4]][1], stringr::str_remove(df_twobytwo()[[5]][1], "\\.$"), "(Fragility).")
  })
  
  #observe event and print results for whichever button the user presses
  output$print_results <- renderText(r$print_results)
  
  
  
  
  ######################################### 
  ##### GENERATE FIGURES/PLOTS OUTPUT #####
  ################################### #####
  
  p <- reactiveValues(fig_results = "") #create empty reactive string for figure results.
  
  #If user presses the results button for linear models, show the figure results
  observeEvent(input$results_pg_l, {
    output$fig_results <- renderPlot(df()[[2]])
    p$fig_results <- plotOutput("fig_results")
  })
  
  #If user presses the results button for logistic models, show the logistic tables
  observeEvent(input$results_pg_di, {
    
    a <- df_log()$Implied_Table
    b <- df_log()$Transfer_Table
    
    p$fig_results <- c(renderUI(HTML(paste("<i>Implied Table</i>"))), renderTable(a, digits = 0, rownames = TRUE, bordered = FALSE, colnames = TRUE),
                       renderUI(HTML(paste("<i>Transfer Table</>"))), renderTable(b, digits = 0, rownames = TRUE, bordered = FALSE, colnames = TRUE))
    
  })
  
  #If user presses the results button for 2x2 tables, show the 2x2 tables
  observeEvent(input$results_pg_2x2, {
    
    a <- df_twobytwo()$User_enter_value
    b <- df_twobytwo()$Transfer_Table
    
    p$fig_results <- c(renderUI(HTML(paste("<i>Implied Table</i>"))), renderTable(a, digits = 0, rownames = TRUE, bordered = FALSE, colnames = TRUE),
                       renderUI(HTML(paste("<i>Transfer Table</>"))), renderTable(b, digits = 0, rownames = TRUE, bordered = FALSE, colnames = TRUE))

  })
  
  #observe event and show figure results for whichever button the user presses
  output$print_fig <- renderUI(p$fig_results)
  


  #Display RIR value below figure/output
  rir <- reactiveValues(print_rir = " ") #create empty reactive string for printed results.

  #If user presses the results button for linear models, paste the linear results
  observeEvent(input$results_pg_l, {
    if(req(input$AnalysisL) == "RIR"){
      e <- df()[[1]][6] #extracts the printed results with the RIR value (element 1, row 6)
      f <- clean_strings(e) #removes special characters (e.g., /.,)
      rir$print_rir <- strsplit(f, " ")[[1]][8] #extracts the 8th element which is the RIR value
      rir$print_rir <- 
        HTML(paste0("<b>Robustness of Inference to Replacement (RIR):</b>", " ", rir$print_rir))
    }
    else{
      rir$print_rir <- HTML(paste0(" "))
    }
  })
  
  observeEvent(input$results_pg_di, {
    if(req(input$Analysis) == "RIR"){
      rir$print_rir <- HTML(paste0("<b>Robustness of Inference to Replacement (RIR):</b>", " ", df_log()$RIR[1]))
    }
    else{
      rir$print_rir <- HTML(paste0(" "))
    }
  })
  
  observeEvent(input$results_pg_2x2, {
    if(req(input$Analysis) == "RIR"){
      rir$print_rir <- HTML(paste0("<b>Robustness of Inference to Replacement (RIR):</b>", " ", df_twobytwo()$RIR[1]))
    }
    else{
      rir$print_rir <- HTML(paste0(" "))
    }
  })


  #observe event and print results for whichever button the user presses
  output$print_rir <- renderPrint({HTML(rir$print_rir)})



  
  
  ################################### 
  ######### GENERATE R CODE ######## 
  ###################################
  
  #generate r code for linear models using user input values
  user_est_l <- eventReactive(input$results_pg_l, {
    paste0("#install.packages('konfound')","\n", "library(konfound)", "\n", "pkonfound(", input$unstd_beta, ", ", input$std_error, ", ", input$n_obs, ", ", input$n_covariates, ", ", "index = ", "'", input$AnalysisL, "'", ")", sep = "")
  })
  
  #generate r code for logistic models using user input values
  user_est_di <- eventReactive(input$results_pg_di, {
    paste("#install.packages('konfound')","\n", "library(konfound)", "\n","pkonfound(", input$unstd_beta_nl, ", ", input$std_error_nl, ", ", input$n_obs_nl, ", ", input$n_covariates_nl, ", n_treat = ", input$n_trm_nl, ", model_type = 'logistic')", sep = "")
  })
  
  #generate r code for 2x2 tables using user input values
  user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste("#install.packages('konfound')","\n", "library(konfound)", "\n","pkonfound(a = ", input$ctrl_fail, ", b = ", input$ctrl_success, ", c = ", input$treat_fail, ", d = ", input$treat_success, ")", sep = "")
  })
  
  
  #conditional statement to display the correct r code based on model type
  select_r_code <- reactive({
    #req(input$Data, input$Outcome)
    
    if(input$Outcome == "Continuous"){
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
      label = "Copy R code",
      clipText = select_r_code(),
      icon = icon("clipboard"))
  })
    
  
  
  ##################################### 
  ######### GENERATE STATA CODE ######## 
  ######################################
  
  #generate r code for linear models using user input values
  s_user_est_l <- eventReactive(input$results_pg_l, {
    paste0("pkonfound ", input$unstd_beta, " ", input$std_error, " ", input$n_obs, " ", input$n_covariates)
  })
  
  #generate r code for logistic models using user input values
  s_user_est_di <- eventReactive(input$results_pg_di, {
    paste("#not available")
  })
  
  #generate r code for 2x2 tables using user input values
  s_user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste("#not available")
  })
  
  
  #conditional statement to display the correct r code based on model type
  select_stata_code <- reactive({
    #req(input$Data, input$Outcome)
    
    if(input$Outcome == "Continuous"){
      stata_code <- s_user_est_l()
    }
    
    if(input$Outcome == "Dichotomous"){
      if(input$Data == "2x2 table"){
        stata_code <- s_user_est_2x2()
      }
      if(input$Data == "Logistic model"){
        stata_code <- s_user_est_di()
      }
    }
    
    stata_code
  })
  
  #Render stata code in UI.R to display for user
  output$stata_code_print <- renderText({
    select_stata_code()
  })
  
  #Add clipboard buttons
  output$clip2 <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy Stata code",
      clipText = select_stata_code(),
      icon = icon("clipboard"))
  })
  
  
  ################################### 
  ######### RESULTS RECAP  ######### 
  ###################################
  
  recap <- reactiveValues(results = "") #create empty reactive string for printed results.
  
  #If user presses the results button for linear models, paste the user-specified linear recap
  observeEvent(input$results_pg_l, {
    if(req(input$Outcome) == 'Continuous'){
      if(req(input$AnalysisL) == "IT"){
        recap$results <- paste0("<b>User-specified:</b>", " ITCV results for a linear model")
      }
      if(req(input$AnalysisL) == "RIR"){
        recap$results <- paste0("<b>User-specified:</b>", " RIR results for a linear model")
      }

    }
  })
  
  #If user presses the results button for 2x2 tables , paste the user-specified 2x2 recap
  observeEvent(input$results_pg_2x2, {
    if(req(input$Data) == '2x2 table'){
      recap$results <- paste0("<b>User-specified:</b>", " RIR/Fragility results for a 2x2 table")
    }
    else{
      recap$results <- HTML(paste0(" "))
    }
  })
  
  #If user presses the results button for logistic model , paste the user-specified logistic model recap
  observeEvent(input$results_pg_di, {
    if(req(input$Data) == 'Logistic model'){
      recap$results <- HTML(paste0("<b>User-specified:</b>", " RIR/Fragility results for a logistic model"))
    }
    else{
      recap$results <- HTML(paste0(" "))}
  })

  
  #Render user specified selections as recap on results page
  output$recap <- renderPrint({HTML(recap$results)})

  
  # Xu, R., Frank, K. A., Maroulis, S. J., & Rosenberg, J. M. 2019. konfound: Command to quantify robustness of causal inferences. The Stata Journal, 19(3), 523-550. no hyperlink
  
}