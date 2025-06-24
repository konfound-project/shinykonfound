################################################################################
### Load Packages
################################################################################

library(shiny)
library(tidyverse)
library(konfound)



################################################################################
### LINEAR RIR + LINEAR ITCV
################################################################################

get_rir_itcv_results <- function(input_linear) {
  
  
  ############################################################################
  ### RIR
  ### Generalized Robustness of Inference to Replacement (RIR)
  ############################################################################
  
  if(input_linear$AnalysisL == "RIR"){
    
    
    ##########################################################################
    ### Estimated Effects condition for RIR
    ##########################################################################
    
    if(input_linear$Uncertainty_RIR == 'EstEff') {
      
      
      ##########################################################################
      ### Validate user input values
      ##########################################################################
      
      validate(
        need(is.numeric(input_linear$est_effect_rir_ee) & 
               is.numeric(input_linear$std_error_rir_ee) &
               is.numeric(input_linear$n_obs_rir_ee) &
               is.numeric(input_linear$n_covariates_rir_ee), 
             "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not a number."),
        need(input_linear$n_obs_rir_ee > (input_linear$n_covariates_rir_ee + 2), 
             "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
        need(input_linear$std_error_rir_ee > 0, 
             "Did not run! Standard error needs to be greater than zero.")
      )
      
      
      ##########################################################################
      ### GENERATE LINEAR RIR OUTPUT with Estimated Effects
      ##########################################################################
      
      r_output <- 
        capture.output(
          pkonfound(est_eff = as.numeric(input_linear$est_effect_rir_ee), 
                    std_err = as.numeric(input_linear$std_error_rir_ee), 
                    n_obs = as.numeric(input_linear$n_obs_rir_ee), 
                    n_covariates = as.numeric(input_linear$n_covariates_rir_ee),
                    index = "RIR",
                    to_return = "print")
        )
      
      raw_calc <- 
        pkonfound(est_eff = as.numeric(input_linear$est_effect_rir_ee), 
                  std_err = as.numeric(input_linear$std_error_rir_ee), 
                  n_obs = as.numeric(input_linear$n_obs_rir_ee), 
                  n_covariates = as.numeric(input_linear$n_covariates_rir_ee),
                  index = "RIR",
                  to_return = "raw_output"
        )
      
      
      ### Nullify scenario (like abs(est_effect_rir_ee) > abs(beta_threshhold))
      if (abs(input_linear$est_effect_rir_ee) > abs(raw_calc$beta_threshold)) {
        linear_output <- 
          paste0(
            "<strong>Robustness of Inference to Replacement (RIR):</strong><br><br>",
            "RIR = ", sprintf("%.0f", raw_calc$RIR_primary), "<br><br>",
            "To nullify the inference of an effect using the threshold of ",
            sprintf("%.3f", raw_calc$beta_threshold), " for ",
            "statistical significance (with null hypothesis = 0 and alpha = 0.05), ",
            sprintf("%.3f", raw_calc$perc_bias_to_change), "% ",
            "of the estimate of ", input_linear$est_effect_rir_ee,
            " would have to be due to bias. This implies that to ",
            "nullify the inference, one would expect to have to replace ",
            sprintf("%.0f", raw_calc$RIR_primary), 
            " (", sprintf("%.3f", raw_calc$perc_bias_to_change),
            "%) ",
            "observations with data points for which the effect is 0 (RIR = ",
            sprintf("%.0f", raw_calc$RIR_primary), ").<br>"
          )
        
        ### Sustain scenario (like abs(est_effect_rir_ee) < abs(beta_threshhold))
      } else if (abs(input_linear$est_effect_rir_ee) < abs(raw_calc$beta_threshold)) {
        linear_output <- 
          paste0(
            "<strong>Robustness of Inference to Replacement (RIR):</strong><br><br>",
            "RIR = ", sprintf("%.0f", raw_calc$RIR_primary), "<br><br>",
            "The estimated effect is ", input_linear$est_effect_rir_ee, 
            ". The threshold value for statistical significance ",
            "is ", sprintf("%.3f", raw_calc$beta_threshold), 
            " (with null hypothesis = 0 and alpha = 0.05). To reach that threshold, ",
            sprintf("%.3f", raw_calc$perc_bias_to_change), "% of the estimate of ",
            input_linear$est_effect_rir_ee, " would have to be due to bias. This implies to ",
            "sustain an inference one would expect to have to replace ",
            sprintf("%.0f", raw_calc$RIR_primary), 
            " (", sprintf("%.3f", raw_calc$perc_bias_to_change),
            "%) ",
            "observations with effect of 0 with data points with effect of ",
            sprintf("%.3f", raw_calc$beta_threshold), " (RIR = ",
            sprintf("%.0f", raw_calc$RIR_primary), ").<br>"
        )
        
      } else {
        
        ### Exactly equal scenario (est_eff == beta_threshold)
        warning("The coefficient is exactly equal to the threshold.\n")
        linear_output <- paste0(
          "<strong>Robustness of Inference to Replacement (RIR):</strong><br><br>",
          "The coefficient is exactly equal to the threshold (",
          sprintf("%.3f", raw_calc$beta_threshold), ").<br>"
        )
      }
      
      
      ##########################################################################
      ### GENERATE LINEAR RIR PLOT with Estimated Effects
      ##########################################################################
      
      linear_plot <- 
        pkonfound(est_eff = as.numeric(input_linear$est_effect_rir_ee), 
                  std_err = as.numeric(input_linear$std_error_rir_ee), 
                  n_obs = as.numeric(input_linear$n_obs_rir_ee), 
                  n_covariates = as.numeric(input_linear$n_covariates_rir_ee),
                  index = "RIR",
                  to_return = "thresh_plot"
        )
      
      

      
            
    ############################################################################
    ### Confidence Interval condition for RIR
    ############################################################################
      
    } else {
      
      ##########################################################################
      ### Validate user input values
      ##########################################################################
      
      validate(
        need(is.numeric(input_linear$lower_bnd_rir_ci) &
               is.numeric(input_linear$upper_bnd_rir_ci) &
               is.numeric(input_linear$n_obs_rir_ci) &
               is.numeric(input_linear$n_covariates_rir_ci), 
             "Did not run! Did you enter numbers for the lower bound, upper bound, number of observations, and number of covariates? Please change any of these that are not a number."),
        need(input_linear$n_obs_rir_ci > (input_linear$n_covariates_rir_ci + 2), 
             "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
        need(input_linear$upper_bnd_rir_ci > input_linear$lower_bnd_rir_ci, "Did not run! The upper bound needs to be greater than the lower bound.")
      )
      
      
      ##########################################################################
      ### GENERATE LINEAR RIR OUTPUT with Confidence Interval
      ##########################################################################
      
      r_output <- 
        capture.output(
          pkonfound(lower_bound = as.numeric(input_linear$lower_bnd_rir_ci), 
                    upper_bound = as.numeric(input_linear$upper_bnd_rir_ci),
                    n_obs = as.numeric(input_linear$n_obs_rir_ci), 
                    n_covariates = as.numeric(input_linear$n_covariates_rir_ci),
                    index = "RIR",
                    to_return = "print"
          )
        )
      
      raw_calc <- 
        pkonfound(lower_bound = as.numeric(input_linear$lower_bnd_rir_ci), 
                  upper_bound = as.numeric(input_linear$upper_bnd_rir_ci),
                  n_obs = as.numeric(input_linear$n_obs_rir_ci), 
                  n_covariates = as.numeric(input_linear$n_covariates_rir_ci),
                  index = "RIR",
                  to_return = "raw_output"
        )
      
      linear_output <- 
        paste0(
          "<strong>Robustness of Inference to Replacement (RIR):</strong><br><br>",
          "RIR = ", sprintf("%.0f", raw_calc$RIR_primary), "<br><br>",
          "To nullify the inference of an effect using the threshold of ",
          sprintf("%.3f", raw_calc$beta_threshold), " for ",
          "statistical significance (with null hypothesis = 0 and alpha = 0.05), ",
          sprintf("%.3f", raw_calc$perc_bias_to_change), "% ",
          "of the estimate of ", 
          sprintf("%.1f", mean(c(input_linear$lower_bnd_rir_ci, input_linear$upper_bnd_rir_ci))), 
          " (between the lower bound of ", input_linear$lower_bnd_rir_ci,
          " and the upper bound of ", input_linear$upper_bnd_rir_ci,
          ") would have to be due to bias. ", 
          "This implies that to nullify the inference, one would expect to have to replace ",
          sprintf("%.0f", raw_calc$RIR_primary), " (", sprintf("%.3f", raw_calc$perc_bias_to_change),
          "%) observations with data points for which the effect is 0 (RIR = ",
          sprintf("%.0f", raw_calc$RIR_primary), ").<br>"
        )
      
      
      ##########################################################################
      ### GENERATE LINEAR RIR PLOT with Confidence Interval
      ##########################################################################
      
      linear_plot <- 
        pkonfound(lower_bound = as.numeric(input_linear$lower_bnd_rir_ci), 
                  upper_bound = as.numeric(input_linear$upper_bnd_rir_ci), 
                  n_obs = as.numeric(input_linear$n_obs_rir_ci), 
                  n_covariates = as.numeric(input_linear$n_covariates_rir_ci),
                  index = "RIR",
                  to_return = "thresh_plot"
        )
      
    }

        
    
    ##########################################################################
    ### Add final note/citation, as in the R code
    ##########################################################################
    
    linear_output <- 
      paste0(
        linear_output,
        "<hr>",
        "See Frank et al. (2013) for a description of the method.<br><br>",
        "<strong>Citation:</strong><br>",
        "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013). ",
        "What would it take to change an inference? Using Rubin's causal model ",
        "to interpret the robustness of causal inferences. ",
        "<em>Education, Evaluation and Policy Analysis, 35</em>, 437-460.<br><br>",
        "Accuracy of results increases with the number of decimals reported.<br><br>",
        "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
        " of the konfound package, beginning with the advanced code provided below on this page.<br>",
        "<hr>",
        "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
      )
    
  }
    
    
    
    
    
    
    
    ############################################################################
    ### ITCV
    ### Impact Threshold for a Confounding Variable (ITCV)
    ############################################################################
    
    if(input_linear$AnalysisL == "IT"){
      
      
      ##########################################################################
      ### Estimated Effects condition for ITCV
      ##########################################################################
      
      if(input_linear$Uncertainty_RIR == 'EstEff') {
        
        
        ##########################################################################
        ### Validate user input values
        ##########################################################################
        
        validate(
          need(is.numeric(input_linear$est_effect_rir_ee) & 
                 is.numeric(input_linear$std_error_rir_ee) &
                 is.numeric(input_linear$n_obs_rir_ee) &
                 is.numeric(input_linear$n_covariates_rir_ee), 
               "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not a number."),
          need(input_linear$n_obs_rir_ee > (input_linear$n_covariates_rir_ee + 2), 
               "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
          need(input_linear$std_error_rir_ee > 0, 
               "Did not run! Standard error needs to be greater than zero.")
        )
        
        
        ##########################################################################
        ### GENERATE LINEAR ITCV OUTPUT with Estimated Effects
        ##########################################################################
        
        r_output <- 
          capture.output(
            pkonfound(est_eff = as.numeric(input_linear$est_effect_rir_ee), 
                      std_err = as.numeric(input_linear$std_error_rir_ee), 
                      n_obs = as.numeric(input_linear$n_obs_rir_ee), 
                      n_covariates = as.numeric(input_linear$n_covariates_rir_ee),
                      index = "IT",
                      to_return = "print")
          )
        
        raw_calc <- 
          pkonfound(est_eff = as.numeric(input_linear$est_effect_rir_ee), 
                    std_err = as.numeric(input_linear$std_error_rir_ee), 
                    n_obs = as.numeric(input_linear$n_obs_rir_ee), 
                    n_covariates = as.numeric(input_linear$n_covariates_rir_ee),
                    index = "IT",
                    to_return = "raw_output"
          )
        
        
        ### Conditional message based on obs_r and critical_r
        if ((abs(raw_calc$obs_r) < abs(raw_calc$critical_r) & raw_calc$obs_r >= 0) |
            (abs(raw_calc$obs_r) > abs(raw_calc$critical_r) & raw_calc$obs_r < 0)
        ) {
          abs_val <- " (in absolute value) "
        } else {
          abs_val <- " "
        }
          
        ### Nullify Scenario
        if (abs(raw_calc$obs_r) > abs(raw_calc$critical_r)
        ) {
          linear_output <- 
            paste0(
              "<strong>Impact Threshold for a Confounding Variable (ITCV):</strong><br><br>",
              "The minimum impact",
              abs_val, "of an omitted variable needed to nullify ",
              "an inference for a null hypothesis of 0 (nu) ",
              "is based on correlations of ", 
              sprintf("%.3f", raw_calc$rycvGz),
              " with the outcome and ",
              sprintf("%.3f", raw_calc$rxcvGz),
              " with the predictor of interest ",
              "(conditioning on all observed covariates in the model; ",
              "signs are interchangeable if they are different).<br><br>",
              "This is based on a threshold effect of ",
              sprintf("%.3f", raw_calc$critical_r),
              " for statistical significance (alpha = 0.05).<br><br>",
              "Correspondingly, the conditional impact of an omitted variable (Frank 2000) must be ",
              sprintf("%.3f", raw_calc$rycvGz), " * ",
              sprintf("%.3f", raw_calc$rxcvGz), " = ",
              sprintf("%.3f", raw_calc$rycvGz * raw_calc$rxcvGz), 
              " to nullify the inference.<br>"
            )
          
          ### Sustain Scenario
        } else {
          linear_output <- 
            paste0(
              "<strong>Impact Threshold for a Confounding Variable (ITCV):</strong><br><br>",
              "The maximum impact",
              abs_val, "of an omitted variable needed to sustain ",
              "an inference for a null hypothesis of 0 (nu) ",
              "is based on correlations of ", 
              sprintf("%.3f", raw_calc$rycvGz),
              " with the outcome and ",
              sprintf("%.3f", raw_calc$rxcvGz),
              " with the predictor of interest ",
              "(conditioning on all observed covariates in the model; ",
              "signs are interchangeable if they are different).<br><br>",
              "This is based on a threshold effect of ",
              sprintf("%.3f", raw_calc$beta_threshold),
              " for statistical significance (alpha = 0.05).<br><br>",
              "Correspondingly, the maximum impact of an omitted variable (Frank 2000) is ",
              sprintf("%.3f", raw_calc$rycvGz), " * ",
              sprintf("%.3f", raw_calc$rxcvGz), " = ",
              sprintf("%.3f", raw_calc$rycvGz * raw_calc$rxcvGz), 
              " to sustain the inference.<br>"
            )
        }
        
        
        ##########################################################################
        ### GENERATE LINEAR ITCV PLOT with Estimated Effects
        ##########################################################################
        
        linear_plot <- 
          pkonfound(est_eff = as.numeric(input_linear$est_effect_rir_ee), 
                    std_err = as.numeric(input_linear$std_error_rir_ee), 
                    n_obs = as.numeric(input_linear$n_obs_rir_ee), 
                    n_covariates = as.numeric(input_linear$n_covariates_rir_ee),
                    index = "IT",
                    to_return = "corr_plot"
          )
        
        
        
        
        
        ############################################################################
        ### Confidence Interval condition for ITCV
        ############################################################################
        
      } else {
        
        ##########################################################################
        ### Validate user input values
        ##########################################################################
        
        validate(
          need(is.numeric(input_linear$lower_bnd_rir_ci) &
                 is.numeric(input_linear$upper_bnd_rir_ci) &
                 is.numeric(input_linear$n_obs_rir_ci) &
                 is.numeric(input_linear$n_covariates_rir_ci), 
               "Did not run! Did you enter numbers for the lower bound, upper bound, number of observations, and number of covariates? Please change any of these that are not a number."),
          need(input_linear$n_obs_rir_ci > (input_linear$n_covariates_rir_ci + 2), 
               "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
          need(input_linear$upper_bnd_rir_ci > input_linear$lower_bnd_rir_ci, "Did not run! The upper bound needs to be greater than the lower bound.")
        )
        
        
        ##########################################################################
        ### GENERATE LINEAR ITCV OUTPUT with Confidence Interval
        ##########################################################################
        
        r_output <- 
          capture.output(
            pkonfound(lower_bound = as.numeric(input_linear$lower_bnd_rir_ci), 
                      upper_bound = as.numeric(input_linear$upper_bnd_rir_ci),
                      n_obs = as.numeric(input_linear$n_obs_rir_ci), 
                      n_covariates = as.numeric(input_linear$n_covariates_rir_ci),
                      index = "IT",
                      to_return = "print"
            )
          )
        
        raw_calc <- 
          pkonfound(lower_bound = as.numeric(input_linear$lower_bnd_rir_ci), 
                    upper_bound = as.numeric(input_linear$upper_bnd_rir_ci),
                    n_obs = as.numeric(input_linear$n_obs_rir_ci), 
                    n_covariates = as.numeric(input_linear$n_covariates_rir_ci),
                    index = "IT",
                    to_return = "raw_output"
          )
        
        
        ### Conditional message based on obs_r and critical_r
        if ((abs(raw_calc$obs_r) < abs(raw_calc$critical_r) & raw_calc$obs_r >= 0) |
            (abs(raw_calc$obs_r) > abs(raw_calc$critical_r) & raw_calc$obs_r < 0)
        ) {
          abs_val <- " (in absolute value) "
        } else {
          abs_val <- " "
        }
        
        ### Nullify Scenario
        if (abs(raw_calc$obs_r) > abs(raw_calc$critical_r)
        ) {
          linear_output <- 
            paste0(
              "<strong>Impact Threshold for a Confounding Variable (ITCV):</strong><br><br>",
              "The minimum impact",
              abs_val, "of an omitted variable needed to nullify ",
              "an inference for a null hypothesis of 0 (nu) ",
              "is based on correlations of ", 
              sprintf("%.3f", raw_calc$rycvGz),
              " with the outcome and ",
              sprintf("%.3f", raw_calc$rxcvGz),
              " with the predictor of interest ",
              "(conditioning on all observed covariates in the model; ",
              "signs are interchangeable if they are different).<br><br>",
              "This is based on a threshold effect of ",
              sprintf("%.3f", raw_calc$critical_r),
              " for statistical significance (alpha = 0.05).<br><br>",
              "Correspondingly, the conditional impact of an omitted variable (Frank 2000) must be ",
              sprintf("%.3f", raw_calc$rycvGz), " * ",
              sprintf("%.3f", raw_calc$rxcvGz), " = ",
              sprintf("%.3f", raw_calc$rycvGz * raw_calc$rxcvGz), 
              " to nullify the inference.<br>"
            )
          
          ### Sustain Scenario
        } else {
          linear_output <- 
            paste0(
              "<strong>Impact Threshold for a Confounding Variable (ITCV):</strong><br><br>",
              "The maximum impact",
              abs_val, "of an omitted variable needed to sustain ",
              "an inference for a null hypothesis of 0 (nu) ",
              "is based on correlations of ", 
              sprintf("%.3f", raw_calc$rycvGz),
              " with the outcome and ",
              sprintf("%.3f", raw_calc$rxcvGz),
              " with the predictor of interest ",
              "(conditioning on all observed covariates in the model; ",
              "signs are interchangeable if they are different).<br><br>",
              "This is based on a threshold effect of ",
              sprintf("%.3f", raw_calc$beta_threshold),
              " for statistical significance (alpha = 0.05).<br><br>",
              "Correspondingly, the maximum impact of an omitted variable (Frank 2000) is ",
              sprintf("%.3f", raw_calc$rycvGz), " * ",
              sprintf("%.3f", raw_calc$rxcvGz), " = ",
              sprintf("%.3f", raw_calc$rycvGz * raw_calc$rxcvGz), 
              " to sustain the inference.<br>"
            )
        }
        
        
        
        ##########################################################################
        ### GENERATE LINEAR ITCV PLOT with Confidence Interval
        ##########################################################################
        
        linear_plot <- 
          pkonfound(lower_bound = as.numeric(input_linear$lower_bnd_rir_ci), 
                    upper_bound = as.numeric(input_linear$upper_bnd_rir_ci),
                    n_obs = as.numeric(input_linear$n_obs_rir_ci), 
                    n_covariates = as.numeric(input_linear$n_covariates_rir_ci),
                    index = "IT",
                    to_return = "corr_plot"
          )
        
      }
      
      
    
    ##########################################################################
    ### Add final note/citation, as in the R code
    ##########################################################################
    
    linear_output <- 
      paste0(
        linear_output,
        "<hr>",
        "See Frank (2000) for a description of the method.<br><br>",
        "<strong>Citation:</strong><br>",
        "Frank, K. (2000). Impact of a confounding variable on the inference ",
        "of a regression coefficient. <em>Sociological Methods and Research, 29</em>, ",
        "147-194.<br><br>",
        "Accuracy of results increases with the number of decimals reported.<br><br>",
        "The ITCV analysis was originally derived for OLS standard errors. ",
        "If your standard errors are not OLS-based, interpret the ITCV with caution.<br><br>",
        "This analysis assumes the use of default parameters. For greater flexibility, ",
        "use the R or Stata versions of the konfound package, beginning with ",
        "the advanced code provided below on this page.",
        "<hr>",
        "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
      )
    
  }
  
  
  
  
  
  ##########################################################################
  ### Return both text and plot separately
  ##########################################################################
  
  linear_results <-
    list(
      text = linear_output,
      plot = linear_plot,
      raw = r_output
    )
  
  return(linear_results)  
  
}

