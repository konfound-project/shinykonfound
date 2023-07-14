#New development - Conditional panel approach
#server

library(shiny)
library(tidyverse)
library(konfound)
library(shinyjs)
library(rclipboard)
library(fedmatch)

jscode <- "shinyjs.refresh_page = function() { history.go(0); }" 


server <- function(input, output, session) {
  
  ############################################# 
  ###### GENERATE LINEAR RIR/ITCV RESULTS ###### 
  #############################################
  
  #validate user input values to make sure they are 1) numeric and 2) more than 1 covariate for all model types
  df <- eventReactive(input$results_pg_l, {
    
    #validating user input is numeric
    validate(
      need(is.numeric(input$unstd_beta) &
             is.numeric(input$std_error) &
             is.numeric(input$n_obs) &
             is.numeric(input$n_covariates), "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input$n_obs > (input$n_covariates + 2), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input$std_error > 0, "Did not run! Standard error needs to be greater than zero.")
    )
    
    
    #if statements needed for linear printed output and figure (displays the correct information for RIR, ITCV)
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
  
  
  
  
  
  
  
  
  
  
  
  ############################################# 
  ###### GENERATE LINEAR COP RESULTS ###### 
  #############################################
  
  #validate user input values to make sure they are 1) numeric and 2) more than 1 covariate for all model types
  df_cop <- eventReactive(input$results_pg_cop, {
    
    #validating user input is numeric
    validate(
      need(is.numeric(input$unstd_beta_cop) &
             is.numeric(input$std_err_cop) &
             is.numeric(input$n_obs_cop) &
             is.numeric(input$sdx_cop) &
             is.numeric(input$sdy_cop) &
             is.numeric(input$R2_cop) &
             is.numeric(input$eff_thr_cop) &
             is.numeric(input$FR2max_cop) &
             is.numeric(input$n_covariates_cop), "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input$n_obs_cop > (input$n_covariates_cop + 2), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input$sdx_cop > 0, "Did not run! Standard deviation of x needs to be greater than zero."),
      need(input$sdy_cop > 0, "Did not run! Standard deviation of y needs to be greater than zero."),
      need(input$std_err_cop > 0, "Did not run! Standard error needs to be greater than zero."),
      need(input$R2_cop > 0, "Did not run! R2 needs to be greater than zero."),
      need(input$R2_cop < 1, "Did not run! R2 needs to be less than one"),
      need(input$FR2max_cop < 1, "Did not run! R2 Max needs to be less than 1."),
      need(input$FR2max_cop > input$R2_cop, "Did not run! R2 Max needs to be greater than R2."),
      need(1-((input$sdy_cop^2/input$sdx_cop^2)*(1-input$R2_cop)/((input$n_obs_cop - input$n_covariates_cop - 2)*input$std_err_cop^2)) >0, "Did not run! Entered values produced Rxz^2 <0, consider adding more significant digits to your entered values")
    )
    
    #if statements needed for linear printed output and figure
    cop_output <- capture.output(pkonfound(as.numeric(input$unstd_beta_cop),
                                                as.numeric(input$std_err_cop),
                                                as.numeric(input$n_obs_cop),
                                                as.numeric(input$n_covariates_cop),
                                                sdx = as.numeric(input$sdx_cop),
                                                sdy = as.numeric(input$sdy_cop),
                                                R2 = as.numeric(input$R2_cop),
                                                eff_thr = as.numeric(input$eff_thr_cop),
                                                FR2max = as.numeric(input$FR2max_cop),
                                                index = "COP",
                                                to_return = c(c("print"))))
    
    cop_plot <- pkonfound(as.numeric(input$unstd_beta_cop),
                                                as.numeric(input$std_err_cop),
                                                as.numeric(input$n_obs_cop),
                                                as.numeric(input$n_covariates_cop),
                                                sdx = as.numeric(input$sdx_cop),
                                                sdy = as.numeric(input$sdy_cop),
                                                R2 = as.numeric(input$R2_cop),
                                                eff_thr = as.numeric(input$eff_thr_cop),
                                                FR2max = as.numeric(input$FR2max_cop),
                                                index = "COP",
                                                to_return = "raw_output")
    
    
    print(list(cop_output, cop_plot))
    list(cop_output, cop_plot)
  })
  
  ############################################# 
  ###### GENERATE LINEAR PSE RESULTS ###### 
  #############################################
  
  #validate user input values to make sure they are 1) numeric and 2) more than 1 covariate for all model types
  df_pse <- eventReactive(input$results_pg_pse, {
    
    #validating user input is numeric
    validate(
      need(is.numeric(input$unstd_beta_pse) &
             is.numeric(input$std_err_pse) &
             is.numeric(input$n_obs_pse) &
             is.numeric(input$sdx_pse) &
             is.numeric(input$sdy_pse) &
             is.numeric(input$R2_pse), "Did not run! Did you enter numbers for the estimated effect, standard error, and number of observations? Please change any of these that are not to a number."),
      need(input$std_err_pse > 0, "Did not run! Standard error needs to be greater than zero."),
      need(input$sdx_pse > 0, "Did not run! Standard deviation of x needs to be greater than zero."),
      need(input$sdy_pse > 0, "Did not run! Standard deviation of y needs to be greater than zero."),
      need(input$R2_pse > 0, "Did not run! R2 needs to be greater than zero."),
      need(input$R2_pse < 1, "Did not run! R2 needs to be less than one"),
      need(1-((input$sdy_pse^2/input$sdx_pse^2)*(1-input$R2_pse)/(input$n_obs_pse*input$std_err_pse^2)) > 0, "Did not run! Entered values produced Rxz^2 <0, consider adding more significant digits to your entered values")
      )
    
    
    #if statements needed for linear printed output and figure
    if(input$AnalysisL == "PSE"){
      linear_output <- capture.output(pkonfound(as.numeric(input$unstd_beta_pse),
                                                as.numeric(input$std_err_pse),
                                                as.numeric(input$n_obs_pse),
                                                sdx = as.numeric(input$sdx_pse),
                                                sdy = as.numeric(input$sdy_pse),
                                                R2 = as.numeric(input$R2_pse),
                                                index = "PSE",
                                                to_return = "print"))
    }
    
    if(input$AnalysisL == "PSE"){
      pse_plot <- HTML(paste("No graphical output for this analysis"))
    }
    print(list(linear_output, pse_plot))
    list(linear_output, pse_plot)
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
      need(input$n_obs_nl > (input$n_covariates_nl + 2),
           "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input$std_error_nl > 0, "Did not run! Standard error needs to be greater than zero.")
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
           "Did not run! Did you enter numbers for the input values? Please change any of these that are not to a number."),
      need(input$std_error_nl > 0, "Did not run! Standard error needs to be greater than zero."),
      need(input$ctrl_fail > 0, "Did not run! Control Condition: Result Failure needs to be greater than zero"),
      need(input$ctrl_success > 0, "Did not run! Control Condition: Result Success needs to be greater than zero"),
      need(input$treat_fail > 0, "Did not run! Treatment Condition: Result Failure needs to be greater than zero"),
      need(input$treat_success > 0, "Did not run! Treatment Condition: Result Sucesss needs to be greater than zero")
    )
    
    out <- pkonfound(a = input$ctrl_fail, 
                     b = input$ctrl_success, 
                     c = input$treat_fail, 
                     d = input$treat_success,
                     to_return = "raw_output")
    print(out)
    out

  })


  
  
  
  ################################### 
  ###### GENERATE PRINTED OUTPUT ###### 
  ###################################
  
  r <- reactiveValues(print_results = "") #create empty reactive string for printed results.
  
  #If user presses the results button for linear models, paste the linear results
  observeEvent(input$results_pg_l, {
    r$print_results <- renderText(paste0(df()[[1]][2], df()[[1]][3], df()[[1]][4], df()[[1]][5], df()[[1]][6], df()[[1]][7]))
  })
  
  #If user presses the results button for cop models, paste the cop results
  observeEvent(input$results_pg_cop, {
    r$print_results <- renderText(df_cop()[[1]])
  })
  
  #If user presses the results button for cop models, paste the cop results
  observeEvent(input$results_pg_pse, {
    r$print_results <- renderText(df_pse()[[1]])
  })
  
  
  #If user presses the results button for pse models, paste the pse results
observeEvent(input$results_pg_di, {
  r$print_results <- renderText(paste0(df_log()[[1]], df_log()[[2]], df_log()[[3]]))
})
    
  
    
  #   r$print_results <- paste(df_log()[[5]][1], df_log()[[6]][1], "<br>", "<br>", df_log()[[1]][1], df_log()[[2]][1], stringr::str_remove(df_log()[[3]][1], "\\.$"), "(Fragility).",
  #                            "<br>", df_log()[[10]][1])
  
  
  #If user presses the results button for 2x2 tables, paste the 2x2 results
  observeEvent(input$results_pg_2x2, {
    r$print_results <-renderText(paste0(df_twobytwo()[[1]][1], df_twobytwo()[[2]][1], df_twobytwo()[[3]][1], df_twobytwo()[[4]][1], stringr::str_remove(df_twobytwo()[[5]][1], "\\.$"), " (Fragility)."))
  })
  
  #observe event and print results for whichever button the user presses
  output$print_results <- renderUI(r$print_results)
  
  
  
  
  ######################################### 
  ##### GENERATE FIGURES/PLOTS OUTPUT #####
  ################################### #####
  
  p <- reactiveValues(fig_results = "") #create empty reactive string for figure results.
  
  #If user presses the results button for linear models, show the figure results
  observeEvent(input$results_pg_l, {
    output$fig_results <- renderPlot(df()[[2]])
    p$fig_results <- plotOutput("fig_results")
  })
  
  #If user presses the results button for cop models, show the figure results
  observeEvent(input$results_pg_cop, {
    output$fig_results <- renderPlot(df_cop()[[2]])
    p$fig_results <- plotOutput("fig_results")
  })
  
  #If user presses the results button for pse model, no figure is shown
  observeEvent(input$results_pg_pse, {
    p$fig_results <- renderUI(df_pse()[[2]])
  })
  
  #If user presses the results button for logistic models, show the logistic tables
  observeEvent(input$results_pg_di, {
    
    a <- renderTable(df_log()$Implied_Table, digits = 0, rownames = TRUE, bordered = FALSE, colnames = TRUE)
    b <- renderTable(df_log()$Transfer_Table, digits = 0, rownames = TRUE, bordered = FALSE, colnames = TRUE)

    
    p$fig_results <- c(renderUI(HTML(paste("<i>Implied Table</i>"))), a,  renderUI(HTML(paste("<i>Transfer Table</>"))), b)
    
  })
  
  #If user presses the results button for 2x2 tables, show the 2x2 tables
  observeEvent(input$results_pg_2x2, {
    
    a <- renderTable(df_twobytwo()$User_enter_value, digits = 0, rownames = TRUE, bordered = FALSE, colnames = TRUE)
    b <- renderTable(df_twobytwo()$Transfer_Table, digits = 0, rownames = TRUE, bordered = FALSE, colnames = TRUE)
    
    p$fig_results <- c(renderUI(HTML(paste("<i>Implied Table</i>"))), a,  renderUI(HTML(paste("<i>Transfer Table</>"))), b)

  })
  
  #observe event and show figure results for whichever button the user presses
  output$print_fig <- renderUI(p$fig_results)
  


  #Display RIR value below figure/output
  rir <- reactiveValues(print_rir = "") #create empty reactive string for printed results.

  #If user presses the results button for linear models, paste the linear results
  observeEvent(input$results_pg_l, {
    if(req(input$AnalysisL) == "RIR"){
      e <- df()[[1]][6] #extracts the printed results with the RIR value (element 1, row 6)
      f <- clean_strings(e) #removes special characters (e.g., /.,)
      g <- strsplit(f, " ")[[1]][8] #extracts the 8th element which is the RIR value
      rir$print_rir <- 
        renderUI(HTML(paste("<b>Robustness of Inference to Replacement (RIR):</b>", " ", g)))
    }
    else{
      rir$print_rir <- renderUI(paste(" "))
    }
  })
  
  observeEvent(input$results_pg_di, {
    if(req(input$Analysis) == "RIR"){
      rir$print_rir <- renderUI(HTML(paste("<b>Robustness of Inference to Replacement (RIR):</b>", " ", df_log()$RIR[1])))
    }
    else{
      rir$print_rir <- renderUI(paste(" "))
    }
  })
  
  observeEvent(input$results_pg_2x2, {
    if(req(input$Analysis) == "RIR"){
      rir$print_rir <- renderUI(HTML(paste("<b>Robustness of Inference to Replacement (RIR):</b>", " ", df_twobytwo()$RIR[1])))
    }
    else{
      rir$print_rir <- renderUI(paste(" "))
    }
  })
  
  observeEvent(input$results_pg_pse, {
    if(req(input$AnalysisL) == "PSE"){
      rir$print_rir <- renderUI(paste(" "))
    }
  })
  
  observeEvent(input$results_pg_cop, {
    if(req(input$AnalysisL) == "COP"){
      rir$print_rir <- renderUI(paste(" "))
    }
  })


  #observe event and print results for whichever button the user presses
  output$print_rir <- renderUI(
    rir$print_rir)



  
  
  ################################### 
  ######### GENERATE R CODE ######## 
  ###################################
  
  #generate r code for linear models using user input values
  user_est_l <- eventReactive(input$results_pg_l, {
    paste0("#install.packages('konfound')","\n", "library(konfound)", "\n", "pkonfound(", input$unstd_beta, ", ", input$std_error, ", ", input$n_obs, ", ", input$n_covariates, ", ", "index = ", "'", input$AnalysisL, "'", ")", sep = "")
  })
  
  #generate r code for logistic models using user input values
  user_est_di <- eventReactive(input$results_pg_di, {
    paste0("#install.packages('konfound')","\n", "library(konfound)", "\n","pkonfound(", input$unstd_beta_nl, ", ", input$std_error_nl, ", ", input$n_obs_nl, ", ", input$n_covariates_nl, ", n_treat = ", input$n_trm_nl, ", model_type = 'logistic')", sep = "")
  })
  
  #generate r code for 2x2 tables using user input va
  user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste0("#install.packages('konfound')","\n", "library(konfound)", "\n","pkonfound(a = ", input$ctrl_fail, ", b = ", input$ctrl_success, ", c = ", input$treat_fail, ", d = ", input$treat_success, ")", sep = "")
  })
  
  #generate r code for COP
  user_est_cop <- eventReactive(input$results_pg_cop, {
    paste0("#install.packages('konfound')", "\n", "library(konfound)", "\n", "pkonfound(est_eff = ", input$unstd_beta_cop, ", std_err = ", input$std_err_cop, ", n_obs = ", input$n_obs_cop, ", n_covariates = ", input$n_covariates_cop, ", sdx = ", input$sdx_cop, ", sdy = ", input$sdy_cop, ", R2 = ", input$R2_cop, ", eff_thr = ", input$eff_thr_cop, ", FR2max = ", input$FR2max_cop, ", index = 'COP')")
  })
  
  #generate r code for PSE
  user_est_pse <- eventReactive(input$results_pg_pse, {
    paste0("#install.packages('konfound')", "\n", "library(konfound)", "\n", "pkonfound(est_eff = ", input$unstd_beta_pse, ", std_err = ", input$std_err_pse, ", n_obs = ", input$n_obs_pse, ", sdx = ", input$sdx_pse, ", sdy = ", input$sdy_pse, ", R2 = ", input$R2_pse, ", index = 'PSE')")
  })
  
  
  
  #conditional statement to display the correct r code based on model type
   select_r_code <- reactive({
     req(isTruthy(input$Outcome),
         isTruthy(input$Data) || isTruthy(input$DataL)) #need or will get error: argument is of length zero
  
     if(isTruthy(input$Outcome == "Dichotomous")){
       if(isTruthy(input$Data == "2x2 table")){
         r_code <- user_est_2x2()
       }
       if(isTruthy(input$Data == "Logistic model")){
         r_code <- user_est_di()
       }
       if(is.null(input$Data)){
         r_code <- print("")
       }
     }
  
     if(isTruthy(input$Outcome == "Continuous")){
       if(isTruthy(input$DataL == "Linear model")){
         r_code <- print("")
         if(isTruthy(input$AnalysisL == "IT")){
           r_code <- user_est_l()
           }
         if(isTruthy(input$AnalysisL == "RIR")){
           r_code <- user_est_l()
           }
         if(isTruthy(input$AnalysisL == "COP")){
           r_code <- user_est_cop()
           }
         if(isTruthy(input$AnalysisL == "PSE")){
           r_code <- user_est_pse()
         }
       }
     }
     r_code
     })
   
  
  ##################################

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
    paste("pkonfound", input$unstd_beta_nl, input$std_error_nl, input$n_obs_nl, input$n_covariates_nl, input$n_trm_nl, "model_type(1)")
  })
  
  #generate r code for 2x2 tables using user input values
  s_user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste("pkonfound", input$ctrl_fail, input$ctrl_success, input$treat_fail, input$treat_success, "model_type(2)")
  })
  
  s_user_est_cop <- eventReactive(input$results_pg_cop, {
    paste0("Not available")
  })
  
  #generate r code for PSE
  s_user_est_pse <- eventReactive(input$results_pg_pse, {
    paste0("Not available")
  })
  
  
  #conditional statement to display the correct stata code based on model type
  select_stata_code <- reactive({
    req(input$Outcome) #need or will get error: argument is of length zero
    
    if(input$Outcome == "Dichotomous"){
      if(input$Data == "2x2 table"){
        stata_code <- s_user_est_2x2()
      }
      if(input$Data == "Logistic model"){
        stata_code <- s_user_est_di()
      }
    }
    
    if(input$Outcome == "Continuous"){
      if(input$AnalysisL == "IT"){
        stata_code <- s_user_est_l()
      }
      if(input$AnalysisL == "RIR"){
        stata_code <- s_user_est_l()
      }
      if(input$AnalysisL == "COP"){
        stata_code <- s_user_est_cop()
      }
      if(input$AnalysisL == "PSE"){
        stata_code <- s_user_est_pse()
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
  
  observeEvent(input$startover_button, {
    js$refresh_page();
  })
  


  
  
}