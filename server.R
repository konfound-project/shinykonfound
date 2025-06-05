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

    
    

            
######## GENERATE LINEAR RIR RESULTS ##########################################
    

    
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
    
    
    if (abs(input$unstd_beta) > abs(raw_calc$beta_threshold)) {
      # Nullify scenario (like abs(est_eff) > abs(beta_threshhold))
      linear_output <- paste0(
        "<strong>Robustness of Inference to Replacement (RIR):</strong><br><br>",
        "RIR = ", round(raw_calc$RIR_primary, 3), "<br>",
        
        "To nullify the inference of an effect using the threshold of ",
        round(raw_calc$beta_threshold, 3), " for ",
        "statistical significance (with null hypothesis = 0 and alpha = 0.05), ",
        round(raw_calc$perc_bias_to_change, 3), "% ",
        "of the estimate of ", round(input$unstd_beta, 3), " would have to be due to bias. This implies that to ",
        "nullify the inference one would expect to have to replace ",
        round(raw_calc$RIR_primary, 3), " (", round(raw_calc$perc_bias_to_change, 3),
        "%) ",
        "observations with data points for which the effect is 0 (RIR = ",
        round(raw_calc$RIR_primary, 3), ").<br>"
      )
      
    } else if (abs(input$unstd_beta) < abs(raw_calc$beta_threshold)) {
      # Sustain scenario (like abs(est_eff) < abs(beta_threshhold))
      linear_output <- paste0(
        "<strong>Robustness of Inference to Replacement (RIR):</strong><br><br>",
        "RIR = ", round(raw_calc$RIR_primary, 3), "<br>",
        
        "The estimated effect is ", round(input$unstd_beta, 3), ". The threshold value for statistical significance ",
        "is ", round(raw_calc$beta_threshold, 3), " (with null hypothesis = 0 and alpha = 0.05). To reach that threshold, ",
        round(raw_calc$perc_bias_to_change, 3), "% of the estimate of ",
        round(input$unstd_beta, 3), " would have to be due to bias. This implies to ",
        "sustain an inference one would expect to have to replace ",
        round(raw_calc$RIR_primary, 3), " (", round(raw_calc$perc_bias_to_change, 3),
        "%) ",
        "observations with effect of 0 with data points with effect of ",
        round(raw_calc$beta_threshold, 3), " (RIR = ",
        round(raw_calc$RIR_primary, 3), ").<br>"
      )
      
    } else {
      # Exactly equal scenario (est_eff == beta_threshold)
      warning("The coefficient is exactly equal to the threshold.\n")
      linear_output <- paste0(
        "<strong>Robustness of Inference to Replacement (RIR):</strong><br><br>",
        "The coefficient is exactly equal to the threshold (",
        round(raw_calc$beta_threshold, 3), ").<br>"
      )
    }
    
    # Add final note/citation, as in the R code
    linear_output <- HTML(
      paste0(
      linear_output,
      "<hr>",
      "See Frank et al. (2013) for a description of the method.<br><br>",
      "<strong>Citation:</strong><br>",
      "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013).<br>",
      "What would it take to change an inference? Using Rubin's causal model to interpret the robustness of causal inferences.<br>",
      "<em>Education, Evaluation and Policy Analysis, 35</em>, 437-460.<br><br>",
      "Accuracy of results increases with the number of decimals reported.<br>",
      "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
      " of the konfound package, beginning with the advanced code provided below on this page.<br>",
      "<hr>",
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
      
      # Extract key values from raw_calc
      obs_r <- raw_calc$obs_r        
      critical_r <- raw_calc$critical_r
      beta_threshold <- raw_calc$beta_threshold
      rycvGz <- raw_calc$rycvGz       
      rxcvGz <- raw_calc$rxcvGz  
      
      # Conditional message based on obs_r and critical_r
      if((abs(obs_r) < abs(critical_r) & obs_r >= 0)|(abs(obs_r) > abs(critical_r) & obs_r < 0)){
        abs_val <- " (in absolute value) "
      } else {
        abs_val <- ""
      }
      
      if (abs(obs_r) > abs(critical_r)) {
        # Nullify Scenario
        linear_output <- HTML(
          paste0(
          "<strong>Impact Threshold for a Confounding Variable (ITCV):</strong><br><br>",
          
          "The minimum impact", abs_val, " of an omitted variable needed to nullify an inference for a null hypothesis of 0 (nu) ",
          "is based on correlations of ", 
          formatC(rycvGz, format = "f", digits = 3), " with the outcome and ",
          formatC(rxcvGz, format = "f", digits = 3), " with the predictor of interest ",
          "(conditioning on all observed covariates in the model; signs are interchangeable if they are different).<br>",
          "This is based on a threshold effect of ",
          formatC(critical_r, format = "f", digits = 3),
          " for statistical significance (alpha = 0.05).<br><br>",
          
          "Correspondingly, the conditional impact of an omitted variable (Frank 2000) must be ",
          formatC(rycvGz, format = "f", digits = 3), " × ",
          formatC(rxcvGz, format = "f", digits = 3), " = ",
          formatC(rycvGz * rxcvGz, format = "f", digits = 3),
          " to nullify the inference.<br>"
          )
        )
      } else {
        linear_output <- HTML(
          paste0(
          # Sustain Scenario
          "<strong>Impact Threshold for a Confounding Variable (ITCV):</strong><br><br>",
          
          "The maximum impact", abs_val, " of an omitted variable needed to sustain an inference for a null hypothesis of 0 (nu) ",
          "is based on correlations of ", 
          formatC(rycvGz, format = "f", digits = 3), " with the outcome and ",
          formatC(rxcvGz, format = "f", digits = 3), " with the predictor of interest ",
          "(conditioning on all observed covariates in the model; signs are interchangeable if they are different).<br>",
          "This is based on a threshold effect of ",
          formatC(beta_threshold, format = "f", digits = 3),
          " for statistical significance (alpha = 0.05).<br><br>",
          
          "Correspondingly, the maximum impact of an omitted variable (Frank 2000) is ",
          formatC(rycvGz, format = "f", digits = 3), " × ",
          formatC(rxcvGz, format = "f", digits = 3), " = ",
          formatC(rycvGz * rxcvGz, format = "f", digits = 3),
          " to sustain the inference.<br>"
          )
        )
      }
      
      # Add final disclaimers, references, etc.
      linear_output <- HTML(
        paste0(
        linear_output,
        "<hr>",
        "See Frank (2000) for a description of the method.<br><br>",
        "<strong>Citation:</strong><br>",
        "Frank, K. (2000). Impact of a confounding variable on the inference of a regression coefficient. ",
        "<em>Sociological Methods and Research, 29</em>(2), 147-194.<br><br>",
        "Accuracy of results increases with the number of decimals reported.<br>",
        "The ITCV analysis was originally derived for OLS standard errors. If your standard errors are not OLS-based, ",
        "interpret the ITCV with caution.<br>",
        "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
        " of the konfound package, beginning with the advanced code provided below on this page.<br>",
        "<hr>",
        "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
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
    
    r_output <- 
      capture.output(
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
    
    raw_calc <- 
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
        to_return = "raw_output"
      )
    
    pse_output <-
      HTML(
        paste0(
          "<strong>Preserve Standard Error (Advanced Analysis):</strong><br><br>",
          "This function calculates the correlations associated with an omitted confounding variable (CV) that generate an estimated effect that is approximately equal to the threshold while preserving the originally reported standard error.<br><br>",
          "The correlation between X and CV is ",
          sprintf("%.3f", raw_calc$`correlation between X and CV`),
          ", and the correlation between Y and CV is ",
          sprintf("%.3f", raw_calc$`correlation between Y and CV`),
          ".<br><br>",
          "Conditional on the covariates, the correlation between X and CV is ",
          sprintf("%.3f", raw_calc$`correlation between X and CV conditional on Z`),
          ", and the correlation between Y and CV is ", 
          sprintf("%.3f", raw_calc$`correlation between Y and CV conditional on Z`),
          ".<br><br>",
          "Including such a CV, the coefficient would change to ", 
          sprintf("%.3f", raw_calc$eff_M3),
          ", with standard error of ", 
          sprintf("%.3f", raw_calc$se_M3),
          ".",
          "<br><br>",
          "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
          " of the konfound package, beginning with the advanced code provided below on this page.<br>",
          "<hr>", 
          "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
        )
      )
    
    # Return both text and plot separately
    list(text = pse_output,
         plot_message = "No graphical output for this analysis.",
         raw = r_output)
    
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
    
    r_output <- 
      capture.output(
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
    cop_plot <- 
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
        to_return = "raw_output"  
    )
    
    cop_output <-
      HTML(
        paste0(
          "<strong>Coefficient of Proportionality (COP):</strong><br><br>",
          "This function calculates a correlation-based coefficient of proportionality (delta) ",
          "which is exact even in finite samples as well as Oster's delta*.<br>",
          "Using the absolute value of the estimated effect, result can be interpreted by symmetry.<br><br>",
          
          "Delta* is ", 
          sprintf("%.3f", cop_plot$`delta*`),
          " (assuming no covariates in the baseline model M1), ",
          "the correlation-based delta is ", 
          sprintf("%.3f", cop_plot$`delta_exact`),
          " with a bias of ",
          sprintf("%.3f", cop_plot$delta_pctbias),
          "%.<br>",
          
          "Note that %bias = (delta* - delta) / delta.<br><br>",
          
          "With delta*, the coefficient in the final model will be ",
          sprintf("%.3f", cop_plot$eff_x_M3_oster),
          ".<br>",
          "With the correlation-based delta, the coefficient will be ",
          sprintf("%.3f", cop_plot$eff_x_M3),
          
          "<br><br>",
          
          "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
          " of the konfound package, beginning with the advanced code provided below on this page.<br>",

          "<hr>",
          "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
        )
      )
    
    # Return both text and plot outputs as a list
    list(text = cop_output, 
         plot = cop_plot,
         raw = r_output)
  })

  
  
  
  
  
  
################################################################################
###### GENERATE LOGISTIC RESULTS  ##############################################
################################################################################
  
  # non-linear model
  df_log <- 
    eventReactive(input$results_pg_di, {
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
      
    # Generate the printed output
    
    r_output <- 
      capture.output(
        pkonfound(input$unstd_beta_nl, 
                  input$std_error_nl, 
                  input$n_obs_nl, 
                  input$n_covariates_nl,
                  n_treat = input$n_trm_nl, 
                  model_type = "logistic",
                  to_return = "print")
      )
    
    raw_calc <- 
      pkonfound(input$unstd_beta_nl, 
                input$std_error_nl, 
                input$n_obs_nl, 
                input$n_covariates_nl,
                n_treat = input$n_trm_nl, 
                model_type = "logistic",
                to_return = "raw_output"
      )

    # Base indicators/output values from raw_calc
    invalidate_ob <- raw_calc$invalidate_ob   # TRUE => "To nullify" / FALSE => "To sustain"
    needtworows <- raw_calc$needtworows     # TRUE => double-switch scenario
    
    # RIR and Fragility
    final_switch <- raw_calc$total_switch    # total fragility
    final_primary <- raw_calc$fragility_primary
    final_extra <- if (!is.null(raw_calc$fragility_supplemental)) raw_calc$fragility_supplemental else NA
    RIR_primary <- raw_calc$RIR_primary
    RIR_extra <- if (!is.null(raw_calc$RIR_supplemental)) raw_calc$RIR_supplemental else NA
    total_RIR <- raw_calc$total_RIR
    RIR_perc <- raw_calc$RIR_perc   
    RIR <- raw_calc$RIR_primary # only for RIR_pi calculation
    
    # 3x3 tables
    table_start_3x3 <- raw_calc$table_start_3x3
    table_final_3x3 <- raw_calc$table_final_3x3
    
    # Observed log-odds, final log-odds
    est_eff <- raw_calc$est_eff
    user_std_err <- raw_calc$user_std_err
    p_start <- raw_calc$p_start
    est_eff_final <- raw_calc$est_eff_final
    std_err_final <- raw_calc$std_err_final
    p_final <- raw_calc$p_final
    
    # Constructing conditional message based on intermediate values
    
    # define elements of implied table
    a <- raw_calc$starting_table[1,1]
    b <- raw_calc$starting_table[1,2]
    c <- raw_calc$starting_table[2,1]
    d <- raw_calc$starting_table[2,2]
    
    # hard coding of default alpha and tails
    alpha <- 0.05
    tails <- 2
    
    # compute thr_t 
    est_eff <- raw_calc$est_eff
    std_err <- raw_calc$user_std_err
    
    if (est_eff < 0) {
      thr_t <- qt(1 - (alpha / tails), as.numeric(input$n_obs_nl) - as.numeric(input$n_covariates_nl) - 2) * -1
    } else {
      thr_t <- qt(1 - (alpha / tails), as.numeric(input$n_obs_nl) - as.numeric(input$n_covariates_nl) - 2)
    }
    
    t_ob <- est_eff / std_err
    
    # isdcroddsratio logic
    dcroddsratio_ob <- if (thr_t < t_ob) TRUE else FALSE
    
    # For single row switch scenario, define transferway, RIRway, p_destination for flexible print output
    # ShinyApp have default switch_trm = TRUE and replace = "control"
    switch_trm <- TRUE
    replace <- "control"
    
    # single row logic
    transferway       <- ""
    RIRway            <- ""
    prob_indicator    <- ""
    p_destination_val <- NA
    
    # single row 
    if (!needtworows) {
      if (switch_trm && dcroddsratio_ob) {
        transferway <- "treatment success to treatment failure"
        RIRway <- "treatment success"
        p_destination <- p_destination <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
        RIR_pi <- RIR / d * 100
        
      } else if (switch_trm && !dcroddsratio_ob) {
        transferway <- "treatment failure to treatment success"
        RIRway <- "treatment failure"
        p_destination <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
        RIR_pi <- RIR / c * 100
        
      }
    } else if (needtworows) {
      # extract the relevant two-row fields from raw_calc
      RIR_pi <- NA
      transferway_extra    <- ""
      RIRway_extra         <- ""
      prob_indicator_extra <- ""
      p_destination_extra_val <- NA

      # define extra row logic
      if (switch_trm && dcroddsratio_ob) {
        transferway_extra <- "control failure to control success"
        RIRway_extra <- "control failure"
        RIRway_extra_start <- "control row"
        p_destination_extra <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
        
      } else if (switch_trm && !dcroddsratio_ob) {
        transferway_extra <- "control success to control failure"
        RIRway_extra <- "control success"
        RIRway_extra_start <- "control row"
        p_destination_extra <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
        
      } 
    }
    
    # Conditional Fragility calculation component 
    if (raw_calc$p_start < 0.05) {
      if (RIRway == "treatment success") {
        prob_indicator = "failure"  
      } else if (RIRway == "treatment failure") {
        prob_indicator = "success"  
      } else if (RIRway == "control success") {
        prob_indicator = "failure"  
      } else if (RIRway == "control failure") {
        prob_indicator = "success" 
      }
    } else {  # p_start > 0.05
      if (RIRway == "treatment success") {
        prob_indicator = "failure"  
      } else if (RIRway == "treatment failure") {
        prob_indicator = "success" 
      } else if (RIRway == "control success") {
        prob_indicator = "failure" 
      } else if (RIRway == "control failure") {
        prob_indicator = "success" 
      }
    }
    
    if (needtworows) {
      # Conditional Fragility calculation component 
      if (raw_calc$p_start < 0.05) {
        if (RIRway_extra == "treatment success") {
          prob_indicator_extra = "failure"  
        } else if (RIRway_extra == "treatment failure") {
          prob_indicator_extra = "success"  
        } else if (RIRway_extra == "control success") {
          prob_indicator_extra = "failure"  
        } else if (RIRway_extra == "control failure") {
          prob_indicator_extra = "success" 
        }
      } else {  # p_start > 0.05
        if (RIRway_extra == "treatment success") {
          prob_indicator_extra = "failure"  
        } else if (RIRway_extra == "treatment failure") {
          prob_indicator_extra = "success" 
        } else if (RIRway_extra == "control success") {
          prob_indicator_extra = "failure" 
        } else if (RIRway_extra == "control failure") {
          prob_indicator_extra = "success" 
        }
      }
    }
    
    # Benchmark output/plot generation
    log_output_plot <- raw_calc$benchmark_plot
    
    # Determine the message for the plot if no plot is available
    plot_message <- if (is.null(log_output_plot)) {
      "No graphical output for this analysis."
    } else {
      "Plot is displayed below."
    }
    
    # RIR benchmark description
    benchmark_section <- ""
    
    if (raw_calc$invalidate_ob) {
      # Nullify scenario: Show RIR benchmark details
      benchmark_section <- paste0(
        "<strong>Benchmarking RIR for Logistic Regression (Beta Version)</strong><br>",
        "The benchmark value helps interpret the RIR necessary to nullify an inference by comparing the change ",
        "needed to nullify the inference with the changes in the estimated effect due to observed covariates.",
        "Currently this feature is available only when the reported results are statistically significant.<br><br>",
        
        "The benchmark is used to compare the bias needed to nullify the inference / bias reduction due to ",
        "observed covariates. Specifically, change in data from implied to transfer table / change ",
        "in data from unconditional table to implied table.<br><br>",
        
        "To calculate this benchmark value, a range of treatment success values is automatically generated based on ",
        "the assumption that the marginals are constant between the implied table and the raw unadjusted table.<br>",
        "The benchmark value is visualized as a graph, allowing the user to interpret how the benchmark changes with ",
        "hypothesized treatment success values.<br><br>",
        
        "Note that switches in the control row and treatment row required to generate the implied table from the ", 
        "unadjusted table are used to define the benchmark RIR.<br>"
        
      )
    } else {
      # Sustain scenario: No meaningful benchmark
      benchmark_section <- paste0(
        "<strong>Benchmarking RIR for Logistic Regression (Beta Version)</strong><br>",
        "The treatment is not statistically significant in the implied table and would also not be statistically significant ",
        "in the raw table (before covariates were added). In this scenario, we do not yet have a clear interpretation ",
        "of the benchmark, and therefore the benchmark calculation is not reported.<br>"
      )
    }
    
################################################################################   
  
    # Text output generation
    # Decide if p_start < .05 => "nullify" or "sustain" text
    change_phrase <- if (invalidate_ob) {
      paste0("To nullify the inference that the effect is different from 0 (alpha = 0.050), one would")
    } else {
      paste0("To sustain an inference that the effect is different from 0 (alpha = 0.050), one would")
    }
    
    # For referencing partial row text, e.g. "to nullify the inference, transferring X data points..."
    change_t <- if (invalidate_ob) "to nullify the inference," else "to sustain an inference,"
    
    # 7) Build the single-row text (if needtworows == FALSE)
    single_row_text <- ""
    
    if (!needtworows) {
      single_row_text <- paste0(
        change_phrase, " ",
        "need to transfer ", raw_calc$fragility_primary, " data points from ", transferway,
        " (Fragility = ", raw_calc$fragility_primary, ").<br>",
        "This is equivalent to replacing ", raw_calc$total_RIR, " (", round(RIR_perc, 3), "%) ", RIRway, " data points with data points ",
        "for which the probability of ", prob_indicator, " in the control group (", p_destination, "%) applies (RIR = ",
        raw_calc$RIR_primary, ").<br><br>"
        
      )
    }
    
    # Build the two-row text (if needtworows == TRUE)
    two_row_text <- ""
    
    if (needtworows) {
      final_primary <- raw_calc$fragility_primary
      final_extra <- raw_calc$fragility_supplemental
      RIR_primary <- raw_calc$RIR_primary
      RIR_extra <- raw_calc$RIR_supplemental
      
      two_row_text <- paste0(
        "In terms of Fragility, ", change_t, " transferring ", final_primary, " data points from ",
        transferway, " is not enough to change the inference.<br>",
        "One would also need to transfer ", final_extra, " data points from ", transferway_extra, 
        "<br><br>",
        
        "In terms of RIR, generating the ", final_primary, " switches from ", transferway, " ",
        "is equivalent to replacing ", RIR_primary, " ", RIRway, " data points with data points for which ",
        "the probability of ", prob_indicator, " in the control group (", p_destination, "%) applies.<br><br>",
        
        "In addition, generating the ", final_extra, " switches from ", transferway_extra, " is ",
        "equivalent to replacing ", RIR_extra, " ", RIRway_extra, " data points with data points for which ",
        "the probability of ", prob_indicator_extra, " in the control group (", p_destination_extra, "%) applies.<br><br>",
        
        "Total RIR = primary RIR + supplemental RIR = (",
        final_primary, "/", sprintf("%.3f", p_destination/100), ") + (", final_extra, "/", sprintf("%.3f", p_destination_extra/100), ") = ", RIR, " + ", RIR_extra, " = ", raw_calc$total_RIR, "<br>",
        
        "Total Fragility = ", final_primary + final_extra, " = ", raw_calc$total_switch, ".<br><br>"
      )
    }
    
    changeSE_message <- if (raw_calc$user_SE != raw_calc$analysis_SE) {
      paste0(
        "The SE has been adjusted to ", round(raw_calc$analysis_SE, 3), 
        " to generate real numbers in the implied table for which the p-value would be ",
        round(p_start, 3), ".<br>",
        "Numbers in the table cells have been rounded to integers, which may slightly ",
        "alter the estimated effect from the value originally entered.<br>"
      )
    } else {
      paste0(
        "Values in the table have been rounded to the nearest integer. ",
        "This may cause a small change to the estimated effect for the table.<br>"
      )
    }
    
    if (!needtworows) {
      RIR_calc <- paste0(
        "\n\nNote that RIR = Fragility/P(destination) = ",
        raw_calc$fragility_primary, "/", sprintf("%.3f", p_destination/100), " ~ ", total_RIR, ".\n")
    } else{
      RIR_calc <- paste0(
        "\n\nNote that RIR = primary RIR + supplemental RIR = (",
        raw_calc$fragility_primary, "/", sprintf("%.3f", p_destination/100), ") + (", raw_calc$fragility_supplemental, "/", sprintf("%.3f", p_destination_extra/100), ") ~ ", total_RIR, ".\n",
        "based on the calculation RIR = Fragility/P(destination).\n"
      )
    }
    
    # Special case if RIR percentage > 100
    if (!needtworows && RIR_pi > 100) {
      conclusion_large_rir <- paste0(
        sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d data points would", raw_calc$fragility_primary),
        " require replacing more data points than are in the ", RIRway, " condition.")
    } else {
      conclusion_large_rir <- ""  # Empty string if RIR_pi <= 100
    }
    
    # Merge everything into final_output
    log_output <- HTML(
      paste0(
      "<strong>Robustness of Inference to Replacement (RIR):</strong><br>",
      "RIR = ", raw_calc$RIR_primary, "<br>",
      "Fragility = ", raw_calc$fragility_primary, "<br><br>",
      
      "You entered: log odds = ", round(est_eff, 3), ", SE = ", round(user_std_err, 3),
      ", with p-value = ", round(p_start, 3), ".<br>",
      
      "The table implied by the parameter estimates and sample sizes you entered:<br><br>",
      "<strong><u>Implied Table:</u></strong><br>",
      knitr::kable(table_start_3x3, format = "html", align = "c",
                   table.attr = "style='width:100%;'",
                   col.names = c("", "Failures", "Successes", "Success Rate")),  
      
      "<br>",
      
      changeSE_message,
      
      "<hr>",
      
      single_row_text,
      two_row_text,
      
      RIR_calc, "<br>", 
      
      "<hr>",
      
      if (!needtworows && RIR_pi > 100) {
        paste0(conclusion_large_rir, "<br><br>")
      },      
      
      # Transfer Table
      "The transfer of data points yields the following table:<br><br>",
      "<strong><u>Transfer Table:</u></strong><br>",
      knitr::kable(table_final_3x3, format = "html", align = "c",
                   table.attr = "style='width:100%;'",
                   col.names = c("", "Failures", "Successes", "Success Rate")), 
      
      "<hr>",
      
      "The log odds (estimated effect) = ", round(est_eff_final, 3), 
      ", SE = ", round(std_err_final, 3), ", p-value = ", round(p_final, 3), ".<br>",
      "This p-value is based on t = estimated effect / standard error.<br>",
      
      "<hr>",
      
      benchmark_section,
      
      "<hr>",
      
      "See Frank et al. (2021) for a description of the methods.<br><br>",
      
      "<strong>Citation:</strong><br>",
      "*Frank, K. A., *Lin, Q., *Maroulis, S., *Mueller, A. S., Xu, R., Rosenberg, J. M., ... & Zhang, L. (2021).<br>",
      "Hypothetical case replacement can be used to quantify the robustness of trial results.<br>",
      "<em>Journal of Clinical Epidemiology, 134</em>, 150-159.<br>",
      "*Authors are listed alphabetically.<br><br>",
      
      "Accuracy of results increases with the number of decimals entered.<br>",
      
      "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
      " of the konfound package, beginning with the advanced code provided below on this page.<br>",
      
      "<hr>",
      
      "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
          )
      )


################################################################################
    
    
    
    list(text = log_output, 
         raw = r_output,
         plot = log_output_plot,
         plot_message = plot_message
         )
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
    
    # Extract key input values from user-entered fields
    a = input$ctrl_fail
    b = input$ctrl_success
    c = input$treat_fail
    d = input$treat_success
    
    # Extract key fields from raw_calc
    needtworows <- isTRUE(raw_calc$needtworows)
    invalidate_ob <- isTRUE(raw_calc$invalidate_ob)  
    table_start <- raw_calc$starting_table
    table_final <- raw_calc$final_table
    
    # RIR / Fragility
    frag_primary <- raw_calc$fragility_primary
    frag_extra <- raw_calc$fragility_supplemental %||% NA
    RIR_primary <- raw_calc$RIR_primary
    RIR_extra <- raw_calc$RIR_supplemental %||% NA
    total_RIR <- RIR_primary + ifelse(is.na(RIR_extra), 0, RIR_extra)
    total_switch <- frag_primary + ifelse(is.na(frag_extra), 0, frag_extra)
    RIR_perc <- raw_calc$RIR_perc
    
    RIR <- raw_calc$RIR_primary # only for RIR_pi calculation
    
    # Odds ratio and p-values for user/transfer tables (if stored):
    fisher_ob <- raw_calc$fisher_ob %||% NA
    fisher_final <- raw_calc$fisher_final %||% NA
    p_start <- raw_calc$p_start               
    p_final <- raw_calc$p_final %||% NA
    
    alpha <- 0.05 # default
    p_ob <- raw_calc$p_start
    allnotenough <- raw_calc$needtworows
    switch_trm = TRUE # default
    test = "fisher" # default
    replace = "control" # default
    
    # Constructing conditional message based on intermediate values
    
    if (a == 0 || b == 0 || c == 0 || d == 0) {
      a_OR <- a + 0.5
      b_OR <- b + 0.5
      c_OR <- c + 0.5
      d_OR <- d + 0.5
    } else {
      a_OR <- a
      b_OR <- b
      c_OR <- c 
      d_OR <- d
    }  
    
    odds_ratio <- a_OR * d_OR / (b_OR * c_OR)
    
    if (test == "fisher"){
      solution <- konfound:::getswitch_fisher(a, b, c, d, odds_ratio, 0.05, switch_trm)
    }
    
    dcroddsratio_ob <- solution$dcroddsratio_ob
    
    if (switch_trm && dcroddsratio_ob) {
      transferway <- "treatment success to treatment failure"
      transferway_start <- "treatment row"
      
      RIRway <- "treatment success"
      RIRway_start <- "treatment row"
      RIR_pi <- RIR / d * 100
      p_destination_control <- a/(a+b)
      p_destination <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
    }
    
    if (switch_trm && !dcroddsratio_ob) {
      transferway <- "treatment failure to treatment success"
      transferway_start <- "treatment row"
      
      RIRway <- "treatment failure"
      RIRway_start <- "treatment row"
      RIR_pi <- RIR / c * 100
      p_destination_control <- b/(a+b)
      p_destination <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
    }
    
    if (allnotenough) {
      
      RIR_pi <- NA
      
      if (switch_trm && dcroddsratio_ob) {
        transferway_extra <- "control failure to control success"
        transferway_extra_start <- "control row"
        
        RIRway_extra <- "control failure"
        RIRway_extra_start <- "control row"
        p_destination_control_extra <- b/(a+b)
        p_destination_extra <- round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
      }
      if (switch_trm && !dcroddsratio_ob) {
        transferway_extra <- "control success to control failure"
        transferway_extra_start <- "control row"
        
        RIRway_extra <- "control success"
        RIRway_extra_start <- "control row"
        p_destination_control_extra <- a/(a+b)
        p_destination_extra <- round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
          round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
      }
    }
    
    ### Output language objects
    # fragility calculation component (when needtworows == F)
    if (p_ob < 0.05) {
      if (RIRway == "treatment success") {
        prob_indicator = "failure"  
      } else if (RIRway == "treatment failure") {
        prob_indicator = "success"  
      } else if (RIRway == "control success") {
        prob_indicator = "failure"  
      } else if (RIRway == "control failure") {
        prob_indicator = "success" 
      }
    } else {  
      if (RIRway == "treatment success") {
        prob_indicator = "failure"  
      } else if (RIRway == "treatment failure") {
        prob_indicator = "success" 
      } else if (RIRway == "control success") {
        prob_indicator = "failure" 
      } else if (RIRway == "control failure") {
        prob_indicator = "success" 
      }
    }
    
    if (allnotenough) {
      # fragility calculation component (when needtworows == T)
      if (p_ob < 0.05) {
        if (RIRway_extra == "treatment success") {
          prob_indicator_extra = "failure"  
        } else if (RIRway_extra == "treatment failure") {
          prob_indicator_extra = "success"  
        } else if (RIRway_extra == "control success") {
          prob_indicator_extra = "failure"  
        } else if (RIRway_extra == "control failure") {
          prob_indicator_extra = "success" 
        }
      } else {  
        if (RIRway_extra == "treatment success") {
          prob_indicator_extra = "failure"  
        } else if (RIRway_extra == "treatment failure") {
          prob_indicator_extra = "success" 
        } else if (RIRway_extra == "control success") {
          prob_indicator_extra = "failure" 
        } else if (RIRway_extra == "control failure") {
          prob_indicator_extra = "success" 
        }
      }
    }

    if (!exists("p_destination_extra") || is.na(p_destination_extra)) {
      p_destination_extra <- NA
    }
    
    # Decide if p_start < alpha => “nullify” or else => “sustain”
    change_phrase <- if (!is.na(p_start) && p_start < 0.05) {
      "To nullify the inference that the effect is different from 0 (alpha = 0.05), one would need to transfer"
    } else {
      "To sustain an inference that the effect is different from 0 (alpha = 0.05), one would need to transfer"
    }
    
    # Special case if RIR percentage > 100
    if (!needtworows && RIR_pi > 100) {
      conclusion_large_rir <- paste0(
        sprintf("\nNote the RIR exceeds 100%%. Generating the transfer of %d data points would", raw_calc$fragility_primary),
        " require replacing more data points than are in the ", RIRway, " condition.\n\n")
    } else {
      conclusion_large_rir <- ""  # Empty string if RIR_pi <= 100
    }
    
    # RIR calculation note
    if (!needtworows) {
      RIR_calc <- paste0(
        "\n\nNote that RIR = Fragility/P(destination) = ",
        raw_calc$fragility_primary, "/", sprintf("%.3f", p_destination/100), " ~ ", total_RIR, ".\n")
    } else{
      RIR_calc <- paste0(
        "\n\nNote that RIR = primary RIR + supplemental RIR = (",
        raw_calc$fragility_primary, "/", sprintf("%.3f", p_destination/100), ") + (",
        raw_calc$fragility_supplemental, "/", sprintf("%.3f", p_destination_extra/100), ") ~ ", total_RIR, "<br>",
        "based on the calculation RIR = Fragility/P(destination).\n"
      )
    }
    
    single_row_text <- ""
    double_row_text <- ""
    
    if (!needtworows) {
      ## Single-switch scenario
      single_row_text <- paste0(
        change_phrase, " ", frag_primary, " data points from ",
        transferway, " as shown, from the User-entered Table to the Transfer Table (Fragility = ", frag_primary, ").<br>",
        
        "This is equivalent to replacing ", total_RIR, " (", round(RIR_perc, 3), "%) ",
        RIRway, " data points with data points ",
        "for which the probability of ", prob_indicator, " in the control group (", p_destination, "%) applies ",
        "(RIR = ", RIR_primary, ").<br>"
        
      )
      
    } else {
      ## Double-switch scenario
      double_row_text <- paste0(
        "In terms of Fragility, to ", if (invalidate_ob) "nullify" else "sustain", 
        " an inference that the effect is different from 0 (alpha = 0.05), ",
        
        "transferring ", frag_primary, " data points from ",
        transferway, " is not enough to change the inference.<br>",
        
        "One would also need to transfer ", frag_extra, " data points from ", transferway_extra, " as shown, ",
        "from the User-Entered Table to the Transfer Table.<br><br>",
        
        "In terms of RIR, generating the ", frag_primary, " switches from ", transferway, " ",
        "is equivalent to replacing ", RIR_primary, " ", RIRway, " data points with data points for which",
        "the probability of ", prob_indicator, " in the control group (", p_destination, "%) applies.<br><br>",
        
        "In addition, generating the ", frag_extra, " switches from ", transferway_extra, " is ",
        "equivalent to replacing ", RIR_extra, " ", RIRway_extra, " data points with data points for which ",
        "the probability of ", prob_indicator_extra, " in the control group (", p_destination_extra, "%) applies.<br>"
      )
    }
    
    # construct final HTML 
    twobytwo_output <- HTML(
      paste0(
        "<strong>Robustness of Inference to Replacement (RIR):</strong><br>",
        # RIR 
        "RIR = ",
        if (!needtworows) {
          RIR_primary
        } else {
          paste0(RIR_primary, " + ", RIR_extra, " = ", total_RIR)
        },
        "<br>",
        
        # Optional total RIR line for needtworows == T
        if (needtworows) {
          paste0(
            "Total RIR = primary RIR in ", RIRway_start, " + supplemental RIR in ", RIRway_extra_start, "<br><br>"
          )
        } else {
          ""
        },
        
        # Fragility
        "Fragility = ", if (!needtworows) {
          frag_primary
        } else {
          paste0(frag_primary, " + ", frag_extra, " = ", total_switch)
        },
        "<br>",
        
        # Optional total Fragility line for needtworows == T
        if (needtworows) {
          paste0(
            "Total Fragility = primary Fragility in ", transferway_start, " + supplemental Fragility in ", transferway_extra_start, "<br>"
          )
        } else {
          ""
        },
        
        "<br>This function calculates the number of data points that would have to be replaced with zero-effect data points (RIR) ",
        "to nullify or sustain the inference made about the association between the rows and columns in a 2x2 table.<br>",
        
        "One can also interpret this as switches (Fragility) from one cell to another, such as from the treatment success cell ",
        "to the treatment failure cell.<br><br>",
        
        single_row_text,
        double_row_text,
        
        "<br>",
        
        RIR_calc, "<br><br>",
        
        if (!needtworows && RIR_pi > 100) {
          paste0(conclusion_large_rir, "<br><br>")
        },
        
        # show user odds ratio or p-value from the user table 
        if (!is.na(fisher_ob)) paste0("For the User-entered Table, the estimated odds ratio is ", round(fisher_ob,3), 
                                      ", with p-value of ", round(p_start,3),":<br><br>") else "",
        
        "<strong><u>Implied Table:</u></strong><br>",
        knitr::kable(raw_calc$starting_table, format = "html", align = "c",
                     table.attr = "style='width:100%;'",
                     col.names = c("Group", "Failures", "Successes", "Success Rate")),
        "<br>",
        
        if (!is.na(fisher_final)) paste0("For the Transfer Table, the estimated odds ratio is ", round(fisher_final,3), 
                                         ", with p-value of ", round(p_final,3),":<br><br>") else "",
        
        "<strong><u>Transfer Table:</u></strong><br>",
        knitr::kable(raw_calc$final_table, format = "html", align = "c",
                     table.attr = "style='width:100%;'",
                     col.names = c("Group", "Failures", "Successes", "Success Rate")),

        "<hr>",
        "See Frank et al. (2021) for a description of the method.<br><br>",
        "<strong>Citation:</strong><br>",
        "*Frank, K. A., *Lin, Q., *Maroulis, S., *Mueller, A. S., Xu, R., Rosenberg, J. M., ... & Zhang, L. (2021).<br>",
        "Hypothetical case replacement can be used to quantify the robustness of trial results.<br>",
        "<em>Journal of Clinical Epidemiology, 134, 150-159.</em><br>",
        "*Authors are listed alphabetically.<br><br>",
        
        "Accuracy of results increases with the number of decimals entered.<br>",
        
        "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
        " of the konfound package, beginning with the advanced code provided below on this page.<br>",
        
        "<hr>",
        
        "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
      )
    )
    

        
################################################################################
    
    
    list(text = twobytwo_output, 
         plot_message = "No graphical output for this analysis.",
         raw = twobytwo_output_raw)
  })
  
  
  
  
  
  
  
  
  
################################################################################
###### GENERATE PRINTED OUTPUT #################################################
################################################################################
  
  
  
  r <- reactiveValues(print_results1 = "") # Create empty reactive string for printed results.
  r <- reactiveValues(print_results2 = "") # Create empty reactive string for printed results.
  
  
  
  # If user presses the results button for logistic models, paste the logistic results
  observeEvent(input$results_pg_di, {
    
    output$print_results1 <- renderText({
      df_log()$text  # Combine text output lines
    })
    
    # Render a dummy plot with text message
    output$fig_results <- renderPlot({
      if (!is.null(df_log()$plot)) {
        df_log()$plot
      } else {
        plot.new()
        text(0.5, 0.5, df_log()$plot_message, cex = 1.5, col = "black", font = 1.8)
      }
    })
    
    output$print_results2 <- renderText({
      paste(df_log()$raw, collapse = "\n")  # Combine text output lines
    })
  })
  
  
  
  # Generate 2x2 results when button is pressed
  observeEvent(input$results_pg_2x2, {
    
    output$print_results1 <- renderText({
      df_twobytwo()$text # Combine text output lines
    })

    # Render a dummy plot with text message
    output$fig_results <- renderPlot({
      message <- df_twobytwo()$plot_message
      plot.new()
      text(0.5, 0.5, message, cex = 1.5, col = "black", font = 1.8)    
    })
    
    output$print_results2 <- renderText({
      paste(df_twobytwo()$raw , collapse = "\n")  # Combine text output lines
    })
  })
  
 
   
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
  
  
  
  #If user presses the results button for PSE models, paste the PSE results
  observeEvent(input$results_pg_pse, {
    
    output$print_results1 <- renderText({
      df_pse()$text  # Combine text output lines
    })
    
    # Render a dummy plot with text message
    output$fig_results <- renderPlot({
      message <- df_pse()$plot_message
      plot.new()
      text(0.5, 0.5, message, cex = 1.5, col = "black", font = 1.8)    
    })
    
    output$print_results2 <- renderText({
      paste(df_pse()$raw, collapse = "\n")  # Combine text output lines
    })
  })
  
  
  
  # If user presses the results button for COP models, paste the COP results
  observeEvent(input$results_pg_cop, {
    
    output$print_results1 <- renderText({
      df_cop()$text  # Combine text output lines
    })
    
    output$fig_results <- renderPlot({
      df_cop()$plot  # Render the plot output 
    })
    
    output$print_results2 <- renderText({
      paste(df_cop()$raw, collapse = "\n")  # Combine text output lines
    })
  })

  
  
  
  
  
  
################################################################################
######### GENERATE R CODE ######################################################
################################################################################
  
  # Generate R code for linear models using user input values
  user_est_l <- eventReactive(input$results_pg_l, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(est_eff = ", input$unstd_beta, ", std_err = ", input$std_error, ", n_obs = ", input$n_obs, ", n_covariates = ", input$n_covariates, ", ",
           "index = ", "'", input$AnalysisL, "'", ")"
    )
  })
  
  user_est_l_default <- eventReactive(input$results_pg_l, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "# help(pkonfound)  # Check this help page for more details on default arguments \n", 
           "pkonfound(est_eff = ", input$unstd_beta, ", std_err = ", input$std_error, ", n_obs = ", input$n_obs, ", n_covariates = ", input$n_covariates, ", \n\t  ",
           "sdx = NA, sdy = NA, R2 = NA, alpha = 0.05, tails = 2, nu = 0, far_bound = 0, eff_thr = NA, to_return = 'print', \n\t  ",
           "upper_bound = NULL, lower_bound = NULL, index = ", "'", input$AnalysisL, "'", ")"
    )
  })
  
  
  # Generate R code for logistic models using user input values
  user_est_di <- eventReactive(input$results_pg_di, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(est_eff = ", input$unstd_beta_nl, ", std_err = ", input$std_error_nl, ", n_obs = ", input$n_obs_nl, ", n_covariates = ", input$n_covariates_nl, ", n_treat = ", input$n_trm_nl, ", model_type = 'logistic')"
    )
  })
  
  user_est_di_default <- eventReactive(input$results_pg_di, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "# help(pkonfound)  # Check this help page for more details on default arguments \n", 
           "pkonfound(est_eff = ", input$unstd_beta_nl, ", std_err = ", input$std_error_nl, ", n_obs = ", input$n_obs_nl, ", n_covariates = ", input$n_covariates_nl, ", n_treat = ", input$n_trm_nl, ", \n\t  ",
           "alpha = 0.05, tails = 2, nu = 0, switch_trm = TRUE, replace = 'control', to_return = 'print'",
           ", model_type = 'logistic')"
    )
  })
  
  
  # Generate R code for 2x2 tables using user inputs
  user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(a = ", input$ctrl_fail, ", b = ", input$ctrl_success, ", c = ", input$treat_fail, ", d = ", input$treat_success, ")"
    )
  })
  
  user_est_2x2_default <- eventReactive(input$results_pg_2x2, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "# help(pkonfound)  # Check this help page for more details on default arguments \n", 
           "pkonfound(a = ", input$ctrl_fail, ", b = ", input$ctrl_success, ", c = ", input$treat_fail, ", d = ", input$treat_success, ", \n\t  ",
           "alpha = 0.05, switch_trm = TRUE, replace = 'control', test = 'fisher', to_return = 'print')"
    )
  })
  
  
  # Generate R code for COP
  user_est_cop <- eventReactive(input$results_pg_cop, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(est_eff = ", input$unstd_beta_cop, ", std_err = ", input$std_err_cop, ", n_obs = ", input$n_obs_cop, ", n_covariates = ", input$n_covariates_cop, ", \n\t  ",
           "sdx = ", input$sdx_cop, ", sdy = ", input$sdy_cop, ", R2 = ", input$R2_cop, ", eff_thr = ", input$eff_thr_cop, ", FR2max = ", input$FR2max_cop, ", index = 'COP')"
    )
  })
  
  user_est_cop_default <- eventReactive(input$results_pg_cop, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "# help(pkonfound)  # Check this help page for more details on default arguments \n", 
           "pkonfound(est_eff = ", input$unstd_beta_cop, ", std_err = ", input$std_err_cop, ", n_obs = ", input$n_obs_cop, ", n_covariates = ", input$n_covariates_cop, ", \n\t  ",
           "sdx = ", input$sdx_cop, ", sdy = ", input$sdy_cop, ", R2 = ", input$R2_cop, ", eff_thr = ", input$eff_thr_cop, ", FR2max = ", input$FR2max_cop, ", \n\t  ",
           "alpha = 0.05, tails = 2, to_return = 'print', index = 'COP')"
    )
  })
  
  
  # Generate R code for PSE
  user_est_pse <- eventReactive(input$results_pg_pse, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "pkonfound(est_eff = ", input$unstd_beta_pse, ", std_err = ", input$std_err_pse, ", n_obs = ", input$n_obs_pse, ", n_covariates = ", input$n_covariates_pse, ", \n\t  ", 
           "eff_thr = ", input$eff_thr_pse, ", sdx = ", input$sdx_pse, ", sdy = ", input$sdy_pse, ", R2 = ", input$R2_pse, ", index = 'PSE')"
    )
  })
  
  user_est_pse_default <- eventReactive(input$results_pg_pse, {
    paste0("# install.packages('konfound')", "\n", 
           "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
           "# help(pkonfound)  # Check this help page for more details on default arguments \n", 
           "pkonfound(est_eff = ", input$unstd_beta_pse, ", std_err = ", input$std_err_pse, ", n_obs = ", input$n_obs_pse, ", n_covariates = ", input$n_covariates_pse, ", \n\t  ", 
           "eff_thr = ", input$eff_thr_pse, ", sdx = ", input$sdx_pse, ", sdy = ", input$sdy_pse, ", R2 = ", input$R2_pse, ", \n\t  ", 
           "to_return = 'print', index = 'PSE')"
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
  
  select_r_code_default <- reactive({
    req(isTruthy(input$Outcome),
        isTruthy(input$Data) || isTruthy(input$DataL))
    
    # Dichotomous outcome
    if (isTruthy(input$Outcome == "Dichotomous")) {
      if (isTruthy(input$Data == "2x2 table")) {
        r_code_def <- user_est_2x2_default()
      }
      if (isTruthy(input$Data == "Logistic model")) {
        r_code_def <- user_est_di_default()
      }
    }
    
    # Continuous outcome
    if (isTruthy(input$Outcome == "Continuous")) {
      if (isTruthy(input$DataL == "Linear model")) {
        
        # Possibly we check input$AnalysisL to decide which snippet
        # e.g. IT, RIR, COP, PSE
        
        if (isTruthy(input$AnalysisL == "IT") || isTruthy(input$AnalysisL == "RIR")) {
          
          
          
          
          
          if (isTruthy(input$UncertaintyL == "Estimated effect")) {
            r_code_def <- user_est_l_default()
          }
          if (isTruthy(input$UncertaintyL == "Confidence interval")) {
            r_code_def <- user_est_l_default()
          }
        }
        
        
        
        
        
        
        
        if (isTruthy(input$AnalysisL == "COP")) {
          r_code_def <- user_est_cop_default()
        }
        
        if (isTruthy(input$AnalysisL == "PSE")) {
          r_code_def <- user_est_pse_default()
        }
      }
    }
    
    r_code_def
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
  
  # Render Default R code in UI.R to display for user
    output$r_code_print_default <- renderText({
    select_r_code_default()
  })
  
  # And a matching clipboard button
  output$clip_r_default <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy advanced R code",
      clipText = select_r_code_default(),
      icon = icon("clipboard")
    )
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
  
  s_user_est_l_default <- eventReactive(input$results_pg_l, {
    paste0(
      "ssc install konfound", "\n", 
      "ssc install indeplist", "\n", 
      "ssc install moss", "\n", 
      "ssc install matsort", "\n", 
      "* help pkonfound // Check this help page for more details on default arguments", "\n", 
      "pkonfound ", input$unstd_beta, " ", input$std_error, " ", 
      input$n_obs, " ", input$n_covariates, ", sig(0.05) nu(0) onetail(0) sdx(NA) sdy(NA) rs(NA) far_bound(0) eff_thr(NA) model_type(0) indx(", input$AnalysisL, ")"
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
  
  s_user_est_di_default <- eventReactive(input$results_pg_di, {
    paste0(
      "ssc install konfound", "\n", 
      "* help pkonfound // Check this help page for more details on default arguments", "\n", 
      "pkonfound ", input$unstd_beta_nl, " ", input$std_error_nl, " ", 
      input$n_obs_nl, " ", input$n_covariates_nl, " ", input$n_trm_nl, 
      ", sig(0.05) onetail(0) switch_trm(1) replace(1) model_type(1)"
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
  
  s_user_est_2x2_default <- eventReactive(input$results_pg_2x2, {
    paste0(
      "ssc install konfound", "\n", 
      "* help pkonfound // Check this help page for more details on default arguments", "\n", 
      "pkonfound ", input$ctrl_fail, " ", input$ctrl_success, " ", 
      input$treat_fail, " ", input$treat_success, 
      ", sig(0.05) onetail(0) test1(0) switch_trm(1) replace(1) model_type(2)"
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
  
  s_user_est_cop_default <- eventReactive(input$results_pg_cop, {
    paste0(
      "ssc install konfound", "\n", 
      "* help pkonfound // Check this help page for more details on default arguments", "\n", 
      "pkonfound ", input$unstd_beta_cop, " ", input$std_err_cop, " ", 
      input$n_obs_cop, " ", input$n_covariates_cop, " ", 
      input$sdx_cop, " ", input$sdy_cop, " ", input$R2_cop, 
      ", eff_thr(", input$eff_thr_cop, ") fr2max(", input$FR2max_cop, ") sig(0.05) onetail(0) indx(COP)"
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
  
  s_user_est_pse_default <- eventReactive(input$results_pg_pse, {
    paste0(
      "ssc install konfound", "\n", 
      "* help pkonfound // Check this help page for more details on default arguments", "\n",
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
  
  select_stata_code_default <- reactive({
    req(input$Outcome) #need or will get error: argument is of length zero
    
    if(input$Outcome == "Dichotomous"){
      if(input$Data == "2x2 table"){
        stata_code_def <- s_user_est_2x2_default()
      }
      if(input$Data == "Logistic model"){
        stata_code_def <- s_user_est_di_default()
      }
    }
    
    if(input$Outcome == "Continuous"){
      if(input$AnalysisL == "IT"){
        stata_code_def <- s_user_est_l_default()
      }
      if(input$AnalysisL == "RIR"){
        stata_code_def <- s_user_est_l_default()
      }
      if(input$AnalysisL == "COP"){
        stata_code_def <- s_user_est_cop_default()
      }
      if(input$AnalysisL == "PSE"){
        stata_code_def <- s_user_est_pse_default()
      }
    }
    
    stata_code_def
    
  })
  
  
  # Render Stata code in UI.R to display for user
  output$stata_code_print <- renderText({
    select_stata_code()
  })
  
  
  # Add clipboard buttons
  output$clip_stata <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy Stata code",
      clipText = select_stata_code(),
      icon = icon("clipboard"))
  })
  
  output$stata_code_print_default <- renderText({
    select_stata_code_default()
  })
  
  # Add clipboard buttons
  output$clip_stata_default <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy advanced Stata code",
      clipText = select_stata_code_default(),
      icon = icon("clipboard"))
  })
  
  observeEvent(input$startover_button, {
    js$refresh_page();
  })
  
  
}