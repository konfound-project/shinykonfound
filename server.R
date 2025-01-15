library(shiny)
library(tidyverse)
library(shinyjs)
library(rclipboard)
library(fedmatch)

# install.packages("remotes")
# remotes::install_github("deepanshu88/shinyDarkmode")
library(shinyDarkmode)

library(konfound)

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
  
  
  
  
  
################################################################################
###### GENERATE LINEAR RIR/ITCV RESULTS ########################################
################################################################################
  
  # Validate user input values to make sure they are 1) numeric and 2) more than 3 covariates for all model types
  
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

        
################################################################################

    
    #if statements needed for linear printed output and figure (displays the correct information for RIR, ITCV)
    
    r_output <- 
      capture.output(
        pkonfound(as.numeric(input$unstd_beta), 
                  as.numeric(input$std_error), 
                  as.numeric(input$n_obs), 
                  as.numeric(input$n_covariates),
                  index = "RIR",
                  to_return = "print")
      )
    
    raw_calc <- 
      pkonfound(as.numeric(input$unstd_beta), 
                as.numeric(input$std_error), 
                as.numeric(input$n_obs), 
                as.numeric(input$n_covariates),
                index = "RIR",
                to_return = "raw_output")
    
    linear_output <-
      HTML(
        paste0(
          "<strong>Robustness of Inference to Replacement (RIR):</strong><br>",
          "RIR = ", raw_calc$RIR_primary, "<br><br>",
          "To invalidate the inference of an effect using the threshold of ", 
          round(raw_calc$beta_threshold, 3),
          " for statistical significance (with null hypothesis = 0 and alpha = 0.05), ", 
          round(raw_calc$perc_bias_to_change, 3),
          "% of the (2) estimate would have to be due to bias. This implies that to invalidate the inference one would expect to have to replace ", 
          raw_calc$RIR_primary, " (", round(raw_calc$perc_bias_to_change, 3), "%) ",
          "observations with data points for which the effect is 0 (RIR = ", 
          raw_calc$RIR_primary, ").<br><br>",
          "See Frank et al. (2013) for a description of the method.<br><br>",
          "<strong>Citation:</strong><br>",
          "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013). What would it take to change an inference? Using Rubin's causal model to interpret the robustness of causal inferences. <em>Education, Evaluation and Policy Analysis, 35</em>, 437-460.<br><br>",
          "Accuracy of results increases with the number of decimals reported.<br><br>",
          "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
        )
      )
    
    


        
######## GENERATE LINEAR ITCV RESULTS ##########################################
  
    if(input$AnalysisL == "IT"){
      
      r_output <- 
        capture.output(
          pkonfound(as.numeric(input$unstd_beta), 
                    as.numeric(input$std_error), 
                    as.numeric(input$n_obs), 
                    as.numeric(input$n_covariates),
                    index = "IT",
                    to_return = "print")
        )
      
      raw_calc <- 
        pkonfound(as.numeric(input$unstd_beta), 
                  as.numeric(input$std_error), 
                  as.numeric(input$n_obs), 
                  as.numeric(input$n_covariates),
                  index = "IT",
                  to_return = "raw_output")
      
      linear_output <-
        HTML(
          paste(
            "<strong>Impact Threshold for a Confounding Variable (ITCV):</strong><br><br>",
            "The minimum impact of an omitted variable to invalidate an inference for a null hypothesis of an effect of nu (0) is based on a correlation of", 
            round(raw_calc$rxcvGz, 3), 
            "with the outcome and", 
            round(raw_calc$rxcvGz, 3), 
            "with the predictor of interest (conditioning on all observed covariates in the model; signs are interchangeable). This is based on a threshold effect of a threshold effect of", 
            round(raw_calc$critical_r, 2),
            "for statistical significance (alpha = 0.05).<br><br>", 
            "Correspondingly, the impact of an omitted variable (as defined in Frank [2000]) must be", 
            round(raw_calc$rxcvGz, 3), "X", round(raw_calc$rxcvGz, 3), "=", 
            round(raw_calc$rxcvGz * raw_calc$rxcvGz, 3),
            "to invalidate an inference for a null hypothesis of an effect of nu (0).<br><br>",
            "For calculation of unconditional ITCV using pkonfound(), additionally include the <em>R</em><sup>2</sup>, <em>sd</em><sub>x</sub>, and <em>sd</em><sub>y</sub> as input, and request raw output.<br><br>",
            "See Frank (2000) for a description of the method.<br><br>",
            "<strong>Citation:</strong><br>",
            "Frank, K. (2000). Impact of a confounding variable on the inference of a regression coefficient. <em>Sociological Methods and Research, 29</em>(2), 147-194.<br><br>",
            "Accuracy of results increases with the number of decimals reported.<br><br>",
            "The ITCV analysis was originally derived for OLS standard errors. If the standard errors reported in the table were not based on OLS, some caution should be used to interpret the ITCV.<br><br>",
            "<em>Calculated with konfound R package version</em>", packageVersion("konfound")
          )
        )
      
      
    }

        
    
    
    
######## GENERATE LINEAR RIR RESULTS ###########################################
    
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

        
################################################################################   

    # Return both text and plot separately
    list(text = linear_output,
         plot = linear_plot,
         raw = r_output)
    
  })
  
  
  
  
  
  
  
  ################################################################################ 
  ###### GENERATE LINEAR PSE RESULTS #############################################
  ################################################################################
  
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
    
    list(text = pse_output, 
         plot_message = "No graphical output for this analysis.")
  })
  
  
  
  
  
  
  
################################################################################
###### GENERATE LINEAR COP RESULTS #############################################
################################################################################
  
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
    list(text = cop_output, 
         plot = cop_plot)
  })

  
  
  
  
  
  
################################################################################
###### GENERATE LOGISTIC RESULTS  ##############################################
################################################################################
  
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
    list(text = log_output, 
         plot_message = "No graphical output for this analysis.")
  })
  
  
  
  
  
  
################################################################################
######### GENERATE 2x2 RESULTS  ################################################
################################################################################
  
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
    twobytwo_output_raw <- 
      capture.output(
        pkonfound(a = input$ctrl_fail, 
                  b = input$ctrl_success, 
                  c = input$treat_fail, 
                  d = input$treat_success,
                  to_return = "print")
    )
    
    raw_calc <- 
      pkonfound(a = input$ctrl_fail, 
                b = input$ctrl_success, 
                c = input$treat_fail, 
                d = input$treat_success,
                to_return = "raw_output")
    
    # get odds ratio for exact fisher p test 
    fisher_oddsratio <- function(a, b, c, d){
      table <- matrix(c(a,b,c,d), byrow = TRUE, 2, 2)
      value <- suppressWarnings(fisher.test(table)$estimate)
      return(value)
    }
    
    # get p value for exact fisher p test
    fisher_p <- function(a, b, c, d){
      table <- matrix(c(a,b,c,d), byrow = TRUE, 2, 2)
      p <- suppressWarnings(fisher.test(table)$p.value)
      return(p)
    }
    
    twobytwo_output <-
      HTML(
        paste0(
          "<strong>Robustness of Inference to Replacement (RIR):</strong><br>",
          "RIR = ", raw_calc$RIR_primary, "<br>",
          "Fragility = ", raw_calc$fragility_primary, "<br><br>",
          "This function calculates the number of data points that would have to be replaced with zero effect data points (RIR) to invalidate the inference made about the association between the rows and columns in a 2x2 table.<br><br>", 
          "One can also interpret this as switches (Fragility) from one cell to another, such as from the treatment success cell to the treatment failure cell.<br><br>",
          "To sustain an inference that the effect is different from 0 (alpha = 0.05), one would need to transfer ",
          raw_calc$fragility_primary,
          " data points from treatment failure to treatment success as shown, from the User-Entered Table to the Transfer Table (Fragility = ",
          raw_calc$fragility_primary,
          ").<br><br>",
          "This is equivalent to replacing ", raw_calc$RIR_primary, " (", round(raw_calc$RIR_perc, 3), "%) treatment failure data points with data points for which the probability of success in the control group (",
          raw_calc$starting_table$Success_Rate[1],
          ") applies (RIR = ",
          raw_calc$RIR_primary, ").<br><br>",
          "RIR = Fragility / P(destination)<br>",
          "<hr>",
          "For the User-Entered Table, the estimated odds ratio is ",
          round(fisher_oddsratio(a = raw_calc$starting_table[1,1], 
                                 b = raw_calc$starting_table[1,2], 
                                 c = raw_calc$starting_table[2,1], 
                                 d = raw_calc$starting_table[2,2]), 
                3),
          ", with a <em>p</em>-value of ",
         round(fisher_p(a = raw_calc$starting_table[1,1], 
                        b = raw_calc$starting_table[1,2], 
                        c = raw_calc$starting_table[2,1], 
                        d = raw_calc$starting_table[2,2]), 
               3),
         "<br>",
         "<strong><u>User-Entered Table:</u></strong><br>",
         knitr::kable(raw_calc$starting_table, format = "html", align = "c",
                      table.attr = "style='width:100%;'",
                      col.names = c("Group", "Failures", "Successes", "Success Rate")), 
         "<hr>",
         "For the Transfer Table, the estimated odds ratio is ",
         round(fisher_oddsratio(a = raw_calc$final_table[1,1], 
                                b = raw_calc$final_table[1,2], 
                                c = raw_calc$final_table[2,1], 
                                d = raw_calc$final_table[2,2]), 
               3),
         ", with a <em>p</em>-value of ",
         round(fisher_p(a = raw_calc$final_table[1,1], 
                        b = raw_calc$final_table[1,2], 
                        c = raw_calc$final_table[2,1], 
                        d = raw_calc$final_table[2,2]), 
               3),
         "<br>",
         "<strong><u>Transfer Table:</u></strong><br>",
         knitr::kable(raw_calc$final_table, format = "html", align = "c",
                      table.attr = "style='width:100%;'",
                      col.names = c("Group", "Failures", "Successes", "Success Rate")), 
         "<hr>",
         "See Frank et al. (2021) for a description of the method.<br><br>",
         "<strong>Citation:</strong><br>",
         "*Frank, K. A., *Lin, Q., *Maroulis, S., *Mueller, A. S., Xu, R., Rosenberg, J. M., ... & Zhang, L. (2021). Hypothetical case replacement can be used to quantify the robustness of trial results. <em>Journal of Clinical Epidemiology, 134</em>, 150-159.<br>",
         "*<em>Authors are listed alphabetically.</em><br><br>",
         "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
        )
      )

        
################################################################################
    
    # Return the full output as a single concatenated string with line breaks
    list(text = twobytwo_output, 
         plot_message = "No graphical output for this analysis.",
         raw = twobytwo_output_raw)
  })
  
  
  
  
  
  
  
################################################################################
###### GENERATE PRINTED OUTPUT #################################################
################################################################################
  
  r <- reactiveValues(print_results1 = "") # Create empty reactive string for printed results.
  r <- reactiveValues(print_results2 = "") # Create empty reactive string for printed results.
  
  
  
  # If user presses the results button for linear models, paste the linear results
  observeEvent(input$results_pg_l, {
    
    output$print_results1 <- renderText({
      df()$text # Combine text output lines
    })
    output$fig_results <- renderPlot({
      df()$plot  # Render the plot output 
    })
    
    output$print_results2 <- renderText({
      paste(df()$raw , collapse = "\n")  # Combine text output lines
    })
        
  })
  
  
  
  #If user presses the results button for COP models, paste the COP results
  observeEvent(input$results_pg_cop, {
    output$print_results1 <- renderText({
      paste(df_cop()$text, collapse = "\n")  # Combine text output lines
    })
    output$print_results2 <- renderText({
      paste(df_cop()$text, collapse = "\n")  # Combine text output lines
    })
    output$fig_results <- renderPlot({
      df_cop()$plot  # Render the plot output 
    })
  })
  
  
  
  #If user presses the results button for PSE models, paste the PSE results
  observeEvent(input$results_pg_pse, {
    output$print_results1 <- renderText({
      paste(df_pse()$text, collapse = "\n")  # Combine text output lines
    })
    output$print_results2 <- renderText({
      paste(df_pse()$text, collapse = "\n")  # Combine text output lines
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
      paste(df_log()$text, collapse = "\n")  # Combine text output lines
    })
    output$print_results2 <- renderText({
      paste(df_log()$text, collapse = "\n")  # Combine text output lines
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
      df_twobytwo()$text # Combine text output lines
    })
    output$fig_results <- renderPlot({
      message <- df_twobytwo()$plot_message
      plot.new()
      text(0.5, 0.5, message, cex = 1.5, col = "black", font = 1.8)
    })
    
    output$print_results2 <- renderText({
      paste(df_twobytwo()$raw , collapse = "\n")  # Combine text output lines
    })
    
  })
  
  
  
################################################################################
######### GENERATE R CODE ######################################################
################################################################################
  
  # Generate R code for linear models using user input values
  user_est_l <- eventReactive(input$results_pg_l, {
    paste0("#install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(", input$unstd_beta, ", ", input$std_error, ", ", input$n_obs, ", ", input$n_covariates, ", ", "index = ", "'", input$AnalysisL, "'", ")"
    )
  })
  
  
  # Generate R code for logistic models using user input values
  user_est_di <- eventReactive(input$results_pg_di, {
    paste0("#install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(", input$unstd_beta_nl, ", ", input$std_error_nl, ", ", input$n_obs_nl, ", ", input$n_covariates_nl, ", n_treat = ", input$n_trm_nl, ", model_type = 'logistic')"
    )
  })
  
  
  # Generate R code for 2x2 tables using user inputs
  user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste0("#install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(a = ", input$ctrl_fail, ", b = ", input$ctrl_success, ", c = ", input$treat_fail, ", d = ", input$treat_success, ")"
    )
  })
  
  
  # Generate R code for COP
  user_est_cop <- eventReactive(input$results_pg_cop, {
    paste0("#install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(est_eff = ", input$unstd_beta_cop, ", std_err = ", input$std_err_cop, ", n_obs = ", input$n_obs_cop, ", n_covariates = ", input$n_covariates_cop, ", sdx = ", input$sdx_cop, ", sdy = ", input$sdy_cop, ", R2 = ", input$R2_cop, ", eff_thr = ", input$eff_thr_cop, ", FR2max = ", input$FR2max_cop, ", index = 'COP')"
    )
  })
  
  
  # Generate R code for PSE
  user_est_pse <- eventReactive(input$results_pg_pse, {
    paste0("#install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(est_eff = ", input$unstd_beta_pse, ", std_err = ", input$std_err_pse, ", n_obs = ", input$n_obs_pse, ", n_covariates = ", input$n_covariates_pse, " eff_thr = ", input$eff_thr_pse, ", sdx = ", input$sdx_pse, ", sdy = ", input$sdy_pse, ", R2 = ", input$R2_pse, ", index = 'PSE')"
    )
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
  
  
  
################################################################################
########## CREATE BUTTONS ######################################################
################################################################################
  
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
  
  
  
################################################################################
######### GENERATE STATA CODE ##################################################
################################################################################
  
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