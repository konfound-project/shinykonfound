library(shiny)
library(tidyverse)
library(konfound)
library(shinyjs)
library(rclipboard)
library(fedmatch)

# install.packages("remotes")
# remotes::install_github("deepanshu88/shinyDarkmode")
library(shinyDarkmode)


################################################################################

jscode <- "shinyjs.refresh_page = function() { history.go(0); }" 


server <- function(input, output, session) {
  
  darkmode(buttonColorDark = "#7f9f3d",  # Background color of the button while in lightmode
           buttonColorLight = "#639dad",  # Background color of the button while in darkmode
           backgroundColor = "#fff",  # Background color of the page while in lightmode
           mixColor = "#fff",  # Color used to generate darkmode: lighter colors create darker darkmode
           label = "<strong>|</strong>",  # Text that shows up on the darkmode button
           bottom = "32px",
           right = "16px",
           autoMatchOsTheme = TRUE
  )
  
  ################################################################################
  
  
  
  ############################################## 
  ###### GENERATE LINEAR RIR/ITCV RESULTS ###### 
  ##############################################
  
  #validate user input values to make sure they are 1) numeric and 2) more than 3 covariate for all model types
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
                                                to_return = "print"))
    }
    if(input$AnalysisL == "IT"){
      linear_output <- capture.output(pkonfound(as.numeric(input$unstd_beta), 
                                                as.numeric(input$std_error), 
                                                as.numeric(input$n_obs), 
                                                as.numeric(input$n_covariates),
                                                index = "IT",
                                                to_return = "print"))
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
    
    # Return both text and plot separately
    list(text = linear_output, plot = linear_plot)
  })
  
  
  
  ############################################# 
  ###### GENERATE LINEAR COP RESULTS ########## 
  #############################################
  
  #validate user input values to make sure they are 1) numeric and 2) more than 3 covariate for all model types
  df_cop <- eventReactive(input$results_pg_cop, {
    
    # Validate user input
    validate(
      need(is.numeric(input$unstd_beta_cop) & 
             is.numeric(input$std_err_cop) & 
             is.numeric(input$n_obs_cop) & 
             is.numeric(input$sdx_cop) & 
             is.numeric(input$sdy_cop) & 
             is.numeric(input$R2_cop) & 
             is.numeric(input$eff_thr_cop) & 
             is.numeric(input$FR2max_cop) & 
             is.numeric(input$n_covariates_cop), 
           "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input$n_obs_cop > (input$n_covariates_cop + 2), 
           "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input$sdx_cop > 0, 
           "Did not run! Standard deviation of x needs to be greater than zero."),
      need(input$sdy_cop > 0, 
           "Did not run! Standard deviation of y needs to be greater than zero."),
      need(input$std_err_cop > 0, 
           "Did not run! Standard error needs to be greater than zero."),
      need(input$R2_cop > 0, 
           "Did not run! R2 needs to be greater than zero."),
      need(input$R2_cop < 1, 
           "Did not run! R2 needs to be less than one"),
      need(input$FR2max_cop < 1, 
           "Did not run! R2 Max needs to be less than 1."),
      need(input$FR2max_cop > input$R2_cop, 
           "Did not run! R2 Max needs to be greater than R2."),
      need(1 - ((input$sdy_cop^2 / input$sdx_cop^2) * (1 - input$R2_cop) / ((input$n_obs_cop - input$n_covariates_cop - 2) * input$std_err_cop^2)) > 0, 
           "Did not run! Entered values produced Rxz^2 < 0, consider adding more significant digits to your entered values")
    )
    
    # Generate the printed output
    cop_output <- capture.output(
      pkonfound(
        as.numeric(input$unstd_beta_cop),
        as.numeric(input$std_err_cop),
        as.numeric(input$n_obs_cop),
        as.numeric(input$n_covariates_cop),
        sdx = as.numeric(input$sdx_cop),
        sdy = as.numeric(input$sdy_cop),
        R2 = as.numeric(input$R2_cop),
        eff_thr = as.numeric(input$eff_thr_cop),
        FR2max = as.numeric(input$FR2max_cop),
        index = "COP",
        to_return = "print"
      )
    )
    
    # Generate the plot output
    cop_plot <- pkonfound(
      as.numeric(input$unstd_beta_cop),
      as.numeric(input$std_err_cop),
      as.numeric(input$n_obs_cop),
      as.numeric(input$n_covariates_cop),
      sdx = as.numeric(input$sdx_cop),
      sdy = as.numeric(input$sdy_cop),
      R2 = as.numeric(input$R2_cop),
      eff_thr = as.numeric(input$eff_thr_cop),
      FR2max = as.numeric(input$FR2max_cop),
      index = "COP",
      to_return = "raw_output"  
    )
    
    # Return both text and plot outputs as a list
    list(text = cop_output, plot = cop_plot)
  })
  
  
  
  ############################################# 
  ###### GENERATE LINEAR PSE RESULTS ###### 
  #############################################
  
  #validate user input values to make sure they are 1) numeric and 2) more than 3 covariate for all model types
  df_pse <- eventReactive(input$results_pg_pse, {
    
    #validating user input is numeric
    validate(
      need(is.numeric(input$unstd_beta_pse) &
             is.numeric(input$std_err_pse) &
             is.numeric(input$n_obs_pse) &
             is.numeric(input$n_covariates_pse) &
             is.numeric(input$eff_thr_pse) &
             is.numeric(input$sdx_pse) &
             is.numeric(input$sdy_pse) &
             is.numeric(input$R2_pse), "Did not run! Did you enter numbers for the estimated effect, standard error, and number of observations? Please change any of these that are not to a number."),
      need(input$std_err_pse > 0, "Did not run! Standard error needs to be greater than zero."),
      need(input$n_obs_pse > (input$n_covariates_pse + 2), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input$sdx_pse > 0, "Did not run! Standard deviation of x needs to be greater than zero."),
      need(input$sdy_pse > 0, "Did not run! Standard deviation of y needs to be greater than zero."),
      need(input$R2_pse > 0, "Did not run! R2 needs to be greater than zero."),
      need(input$R2_pse < 1, "Did not run! R2 needs to be less than one"),
      need(1-((input$sdy_pse^2/input$sdx_pse^2)*(1-input$R2_pse)/((input$n_obs_pse - input$n_covariates_pse - 2)*input$std_err_pse^2)) > 0, "Did not run! Entered values produced Rxz^2 <0, consider adding more significant digits to your entered values")
    )
    
    
    # Generate the printed output
    pse_output <- capture.output(
      pkonfound(
        as.numeric(input$unstd_beta_pse),
        as.numeric(input$std_err_pse),
        as.numeric(input$n_obs_pse),
        n_covariates = as.numeric(input$n_covariates_pse),
        eff_thr = as.numeric(input$eff_thr_pse),
        sdx = as.numeric(input$sdx_pse),
        sdy = as.numeric(input$sdy_pse),
        R2 = as.numeric(input$R2_pse),
        index = "PSE",
        to_return = "print"
      )
    )
    
    list(text = pse_output, plot_message = "No graphical output for this analysis.")
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
    
    # Run the logistic model function and capture both raw and print outputs
    log_output <- capture.output(
      pkonfound(input$unstd_beta_nl, 
                input$std_error_nl, 
                input$n_obs_nl, 
                input$n_covariates_nl,
                n_treat = input$n_trm_nl, 
                model_type = "logistic",
                to_return = "print")
    )
    
    # Return a list containing both the raw and print outputs
    list(text = log_output, plot_message = "No graphical output for this analysis.")
  })
  
  
  
  ######################################### 
  ######### GENERATE 2x2 RESULTS  ######### 
  ######################################### 
  
  # If user presses the results button for 2x2 tables, show the 2x2 tables
  df_twobytwo <- eventReactive(input$results_pg_2x2, {
    validate(
      need(is.numeric(input$ctrl_fail) &
             is.numeric(input$ctrl_success) &
             is.numeric(input$treat_fail) &
             is.numeric(input$treat_success),
           "Did not run! Did you enter numbers for the input values? Please change any of these that are not to a number."),
      need(input$ctrl_fail > 0, "Did not run! Control Condition: Result Failure needs to be greater than zero"),
      need(input$ctrl_success > 0, "Did not run! Control Condition: Result Success needs to be greater than zero"),
      need(input$treat_fail > 0, "Did not run! Treatment Condition: Result Failure needs to be greater than zero"),
      need(input$treat_success > 0, "Did not run! Treatment Condition: Result Success needs to be greater than zero")
    )
    
    # Capture the full output of the pkonfound function as a single string with proper line breaks
    twobytwo_output <- capture.output(
      pkonfound(a = input$ctrl_fail, 
                b = input$ctrl_success, 
                c = input$treat_fail, 
                d = input$treat_success,
                to_return = "print")
    )
    
    # Return the full output as a single concatenated string with line breaks
    list(text = twobytwo_output, plot_message = "No graphical output for this analysis.")
  })
  
  
  
  ##################################### 
  ###### GENERATE PRINTED OUTPUT ###### 
  #####################################
  
  r <- reactiveValues(print_results1 = "") # Create empty reactive string for printed results.
  r <- reactiveValues(print_results2 = "") # Create empty reactive string for printed results.
  
  
  # If user presses the results button for linear models, paste the linear results
  observeEvent(input$results_pg_l, {
    output$print_results1 <- renderText({
      df_l_text <- df()$text
      paste(df_l_text, collapse = "\n")  # Combine text output lines
    })
    output$print_results2 <- renderText({
      df_l_text <- df()$text
      paste(df_l_text, collapse = "\n")  # Combine text output lines
    })
    output$fig_results <- renderPlot({
      df()$plot  # Render the plot output 
    })
  })
  
  
  #If user presses the results button for COP models, paste the COP results
  observeEvent(input$results_pg_cop, {
    output$print_results1 <- renderText({
      df_cop_text <- df_cop()$text
      paste(df_cop_text, collapse = "\n")  # Combine text output lines
    })
    output$print_results2 <- renderText({
      df_cop_text <- df_cop()$text
      paste(df_cop_text, collapse = "\n")  # Combine text output lines
    })
    output$fig_results <- renderPlot({
      df_cop()$plot  # Render the plot output 
    })
  })
  
  
  #If user presses the results button for PSE models, paste the PSE results
  observeEvent(input$results_pg_pse, {
    output$print_results1 <- renderText({
      df_pse_text <- df_pse()$text
      paste(df_pse_text, collapse = "\n")  # Combine text output lines
    })
    output$print_results2 <- renderText({
      df_pse_text <- df_pse()$text
      paste(df_pse_text, collapse = "\n")  # Combine text output lines
    })
    
    # Render a dummy plot with text message
    output$fig_results <- renderPlot({
      message <- df_pse()$plot_message
      plot.new()
      text(0.5, 0.5, message, cex = 1.5, col = "black", font = 1.8)    
    })
  })
  
  
  #If user presses the results button for logistic models, paste the logistic results
  observeEvent(input$results_pg_di, {
    output$print_results1 <- renderText({
      df_log_text <- df_log()$text
      paste(df_log_text, collapse = "\n")  # Combine text output lines
    })
    output$print_results2 <- renderText({
      df_log_text <- df_log()$text
      paste(df_log_text, collapse = "\n")  # Combine text output lines
    })
    
    # Render a dummy plot with text message
    output$fig_results <- renderPlot({
      message <- df_log()$plot_message
      plot.new()
      text(0.5, 0.5, message, cex = 1.5, col = "black", font = 1.8)    
    })
  })
  
  
  # Generate 2x2 results when button is pressed
  observeEvent(input$results_pg_2x2, {
    output$print_results1 <- renderText({
      df_twobytwo_text <- df_twobytwo()$text
      paste(df_twobytwo_text, collapse = "\n")  # Combine text output lines
    })
    output$print_results2 <- renderText({
      df_twobytwo_text <- df_twobytwo()$text
      paste(df_twobytwo_text, collapse = "\n")  # Combine text output lines
    })
    
    # Render a dummy plot with text message
    output$fig_results <- renderPlot({
      message <- df_twobytwo()$plot_message
      plot.new()
      text(0.5, 0.5, message, cex = 1.5, col = "black", font = 1.8)    
    })
  })
  
  
  
  ################################### 
  ######### GENERATE R CODE ######## 
  ###################################
  
  # Generate R code for linear models using user input values
  user_est_l <- eventReactive(input$results_pg_l, {
    paste0("#install.packages('konfound')","\n", "library(konfound)", "\n", "pkonfound(", input$unstd_beta, ", ", input$std_error, ", ", input$n_obs, ", ", input$n_covariates, ", ", "index = ", "'", input$AnalysisL, "'", ")", sep = "")
  })
  
  
  # Generate R code for logistic models using user input values
  user_est_di <- eventReactive(input$results_pg_di, {
    paste0("#install.packages('konfound')","\n", "library(konfound)", "\n","pkonfound(", input$unstd_beta_nl, ", ", input$std_error_nl, ", ", input$n_obs_nl, ", ", input$n_covariates_nl, ", n_treat = ", input$n_trm_nl, ", model_type = 'logistic')", sep = "")
  })
  
  
  # Generate R code for 2x2 tables using user inputs
  user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste0("#install.packages('konfound')","\n", "library(konfound)", "\n","pkonfound(a = ", input$ctrl_fail, ", b = ", input$ctrl_success, ", c = ", input$treat_fail, ", d = ", input$treat_success, ")", sep = "")
  })
  
  
  # Generate R code for COP
  user_est_cop <- eventReactive(input$results_pg_cop, {
    paste0("#install.packages('konfound')", "\n", "library(konfound)", "\n", "pkonfound(est_eff = ", input$unstd_beta_cop, ", std_err = ", input$std_err_cop, ", n_obs = ", input$n_obs_cop, ", n_covariates = ", input$n_covariates_cop, ", sdx = ", input$sdx_cop, ", sdy = ", input$sdy_cop, ", R2 = ", input$R2_cop, ", eff_thr = ", input$eff_thr_cop, ", FR2max = ", input$FR2max_cop, ", index = 'COP')")
  })
  
  
  # Generate R code for PSE
  user_est_pse <- eventReactive(input$results_pg_pse, {
    paste0("#install.packages('konfound')", "\n", "library(konfound)", "\n", "pkonfound(est_eff = ", input$unstd_beta_pse, ", std_err = ", input$std_err_pse, ", n_obs = ", input$n_obs_pse, ", n_covariates = ", input$n_covariates_pse, " eff_thr = ", input$eff_thr_pse, ", sdx = ", input$sdx_pse, ", sdy = ", input$sdy_pse, ", R2 = ", input$R2_pse, ", index = 'PSE')")
  })
  
  
  # Conditional statement to display the correct R code based on model type
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
  
  
  
  ###################################### 
  ########## CREATE BUTTONS ############ 
  ######################################
  
  # Render R code in UI.R to display for user
  output$r_code_print <- renderText({
    select_r_code()
  })
  
  # Add clipboard button
  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy R code",
      clipText = select_r_code(),
      icon = icon("clipboard"))
  })
  
  
  
  ###################################### 
  ######### GENERATE STATA CODE ######## 
  ######################################
  
  # Generate Stata code for linear models using user input values
  s_user_est_l <- eventReactive(input$results_pg_l, {
    paste0(
      "ssc install konfound", "\n", 
      "ssc install indeplist", "\n", 
      "ssc install moss", "\n", 
      "ssc install matsort", "\n", 
      "pkonfound ", input$unstd_beta, " ", input$std_error, " ", 
      input$n_obs, " ", input$n_covariates, ", model_type(0) indx(", input$AnalysisL, ")"
    )  
  })
  
  
  
  # Generate Stata code for logistic models using user input values
  s_user_est_di <- eventReactive(input$results_pg_di, {
    paste0(
      "ssc install konfound", "\n", 
      "pkonfound ", input$unstd_beta_nl, " ", input$std_error_nl, " ", 
      input$n_obs_nl, " ", input$n_covariates_nl, " ", input$n_trm_nl, 
      ", model_type(1)"
    ) 
  })
  
  
  # Generate Stata code for 2x2 tables using user input values
  s_user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste0(
      "ssc install konfound", "\n", 
      "pkonfound ", input$ctrl_fail, " ", input$ctrl_success, " ", 
      input$treat_fail, " ", input$treat_success, ", model_type(2)"
    ) 
  })
  
  
  s_user_est_cop <- eventReactive(input$results_pg_cop, {
    paste0(
      "ssc install konfound", "\n", 
      "pkonfound ", input$unstd_beta_cop, " ", input$std_err_cop, " ", 
      input$n_obs_cop, " ", input$n_covariates_cop, " ", 
      input$sdx_cop, " ", input$sdy_cop, " ", input$R2_cop, 
      ", eff_thr(", input$eff_thr_cop, ") fr2max(", input$FR2max_cop, ") indx(COP)"
    )
  })
  
  
  s_user_est_pse <- eventReactive(input$results_pg_pse, {
    paste0(
      "ssc install konfound", "\n", 
      "pkonfound ", input$unstd_beta_pse, " ", input$std_err_pse, " ", 
      input$n_obs_pse, " ", input$n_covariates_pse, " ", 
      input$eff_thr_pse, " ", input$sdx_pse, " ", input$sdy_pse, 
      " ", input$R2_pse, ", eff_thr(", input$eff_thr_pse, ") indx(PSE)"
    )
  })
  
  
  # Conditional statement to display the correct Stata code based on model type
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
  
  
  # Render Stata code in UI.R to display for user
  output$stata_code_print <- renderText({
    select_stata_code()
  })
  
  
  # Add clipboard buttons
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