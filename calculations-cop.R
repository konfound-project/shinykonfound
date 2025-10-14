################################################################################
### Load Packages
################################################################################

library(shiny)
library(tidyverse)
library(konfound)



################################################################################
### COP
################################################################################

get_cop_results <- function(input_cop) {
  
    
  ##########################################################################
  ### Estimated Effects condition for COP
  ##########################################################################
  
  if(input_cop$Uncertainty_COP == 'EstEff') {
      
    
    ##########################################################################
    ### Validate user input values
    ##########################################################################
    
    validate(
      need(is.numeric(input_cop$est_effect_cop_ee) &
             is.numeric(input_cop$std_err_cop_ee) &
             is.numeric(input_cop$n_obs_cop_ee) &
             is.numeric(input_cop$n_covariates_cop_ee) &
             is.numeric(input_cop$sdx_cop_ee) &
             is.numeric(input_cop$sdy_cop_ee) &
             is.numeric(input_cop$R2_cop_ee) & 
             is.numeric(input_cop$eff_thr_cop_ee) &
             is.numeric(input_cop$FR2max_cop_ee), 
           "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not a number."),
      need(input_cop$n_obs_cop_ee > (input_cop$n_covariates_cop_ee + 2), 
           "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input_cop$std_err_cop_ee > 0, 
           "Did not run! Standard error needs to be greater than zero."),
      need(input_cop$sdx_cop_ee > 0, 
           "Did not run! Standard deviation of x needs to be greater than zero."),
      need(input_cop$sdy_cop_ee > 0, 
           "Did not run! Standard deviation of y needs to be greater than zero."),
      need(input_cop$R2_cop_ee > 0, 
           "Did not run! R2 needs to be greater than zero."),
      need(input_cop$R2_cop_ee < 1, 
           "Did not run! R2 needs to be less than one"),
      need(input_cop$FR2max_cop_ee < 1, 
           "Did not run! R2 Max needs to be less than 1."),
      need(input_cop$FR2max_cop_ee > input_cop$R2_cop_ee, 
           "Did not run! R2 Max needs to be greater than R2."),
      need(1 - ((input_cop$sdy_cop_ee^2 / input_cop$sdx_cop_ee^2) * (1 - input_cop$R2_cop_ee) / ((input_cop$n_obs_cop_ee - input_cop$n_covariates_cop_ee - 2)*input_cop$std_err_cop_ee^2)) > 0, "Did not run! Entered values produced Rxz^2 < 0. Consider adding more significant digits to your entered values.")
    )
    
    
    ##########################################################################
    ### GENERATE COP OUTPUT (AND PLOT) with Estimated Effects
    ##########################################################################
    
    r_output <- 
      capture.output(
        pkonfound(
          est_eff = as.numeric(input_cop$est_effect_cop_ee),
          std_err = as.numeric(input_cop$std_err_cop_ee),
          n_obs = as.numeric(input_cop$n_obs_cop_ee),
          n_covariates = as.numeric(input_cop$n_covariates_cop_ee),
          sdx = as.numeric(input_cop$sdx_cop_ee),
          sdy = as.numeric(input_cop$sdy_cop_ee),
          R2 = as.numeric(input_cop$R2_cop_ee),
          eff_thr = as.numeric(input_cop$eff_thr_cop_ee),
          FR2max = as.numeric(input_cop$FR2max_cop_ee),
          index = "COP",
          to_return = "print"
        )
      )
    
    raw_calc <- 
      pkonfound(
        est_eff = as.numeric(input_cop$est_effect_cop_ee),
        std_err = as.numeric(input_cop$std_err_cop_ee),
        n_obs = as.numeric(input_cop$n_obs_cop_ee),
        n_covariates = as.numeric(input_cop$n_covariates_cop_ee),
        sdx = as.numeric(input_cop$sdx_cop_ee),
        sdy = as.numeric(input_cop$sdy_cop_ee),
        R2 = as.numeric(input_cop$R2_cop_ee),
        eff_thr = as.numeric(input_cop$eff_thr_cop_ee),
        FR2max = as.numeric(input_cop$FR2max_cop_ee),
        index = "COP",
        to_return = "raw_output"
      )
    
    cop_plot <- raw_calc$Figure
    
    cop_output <- 
      paste0(
        "<strong>Coefficient of Proportionality (COP):</strong><br><br>",
        "This function calculates a correlation-based coefficient of proportionality (delta) ",
        "which is exact even in finite samples as well as Oster's delta*. ",
        "Using the absolute value of the estimated effect, result can be interpreted by symmetry.<br><br>",
        "Delta* is ", sprintf("%.3f", raw_calc$`delta*`),
        " (assuming no covariates in the baseline model M1), ",
        "the correlation-based delta is ", sprintf("%.3f", raw_calc$`delta_exact`),
        " with a bias of ", sprintf("%.3f", raw_calc$delta_pctbias),
        "%.<br><br>",
        "Note that %bias = (delta* - delta) / delta.<br><br>",
        "With delta*, the coefficient in the final model will be ",
        sprintf("%.3f", raw_calc$eff_x_M3_oster), ".<br>",
        "With the correlation-based delta, the coefficient will be ",
        sprintf("%.3f", raw_calc$eff_x_M3), ".<br><br>",
        "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
        " of the konfound package, beginning with the advanced code provided below on this page.<br>",
        "<hr>", 
        "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
      )
    
    
    
    
    
    
    
    ############################################################################
    ### Confidence Interval condition for COP
    ############################################################################
    
  } else {
      
      ##########################################################################
      ### Validate user input values
      ##########################################################################
      

    validate(
      need(is.numeric(input_cop$lower_bnd_cop_ci) &
             is.numeric(input_cop$upper_bnd_cop_ci) &
             is.numeric(input_cop$n_obs_cop_ci) &
             is.numeric(input_cop$n_covariates_cop_ci) &
             is.numeric(input_cop$sdx_cop_ci) &
             is.numeric(input_cop$sdy_cop_ci) &
             is.numeric(input_cop$R2_cop_ci) & 
             is.numeric(input_cop$eff_thr_cop_ci) &
             is.numeric(input_cop$FR2max_cop_ci), 
           "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not a number."),
      need(input_cop$n_obs_cop_ci > (input_cop$n_covariates_cop_ci + 2), 
           "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input_cop$sdx_cop_ci > 0, 
           "Did not run! Standard deviation of x needs to be greater than zero."),
      need(input_cop$sdy_cop_ci > 0, 
           "Did not run! Standard deviation of y needs to be greater than zero."),
      need(input_cop$R2_cop_ci > 0, 
           "Did not run! R2 needs to be greater than zero."),
      need(input_cop$R2_cop_ci < 1, 
           "Did not run! R2 needs to be less than one"),
      need(input_cop$FR2max_cop_ci < 1, 
           "Did not run! R2 Max needs to be less than 1."),
      need(input_cop$FR2max_cop_ci > input_cop$R2_cop_ci, 
           "Did not run! R2 Max needs to be greater than R2."),
      need(input_cop$upper_bnd_cop_ci > input_cop$lower_bnd_cop_ci,
           paste0("Did not run! The upper bound needs to be greater than the lower bound.",
                  " The upper bound you entered of ", input_cop$upper_bnd_cop_ci, 
                  " is less than or equal than the lower bound of ", 
                  input_cop$lower_bnd_cop_ci, "."
           )
      )
    )
    
    
    ##########################################################################
    ### GENERATE COP OUTPUT (AND PLOT) with Confidence Interval
    ##########################################################################
    
    r_output <- 
      capture.output(
        pkonfound(
          lower_bound = as.numeric(input_cop$lower_bnd_cop_ci),
          upper_bound = as.numeric(input_cop$upper_bnd_cop_ci),
          n_obs = as.numeric(input_cop$n_obs_cop_ci),
          n_covariates = as.numeric(input_cop$n_covariates_cop_ci),
          sdx = as.numeric(input_cop$sdx_cop_ci),
          sdy = as.numeric(input_cop$sdy_cop_ci),
          R2 = as.numeric(input_cop$R2_cop_ci),
          eff_thr = as.numeric(input_cop$eff_thr_cop_ci),
          FR2max = as.numeric(input_cop$FR2max_cop_ci),
          index = "COP",
          to_return = "print"
        )
      )
    
    raw_calc <- 
      pkonfound(
        lower_bound = as.numeric(input_cop$lower_bnd_cop_ci),
        upper_bound = as.numeric(input_cop$upper_bnd_cop_ci),
        n_obs = as.numeric(input_cop$n_obs_cop_ci),
        n_covariates = as.numeric(input_cop$n_covariates_cop_ci),
        sdx = as.numeric(input_cop$sdx_cop_ci),
        sdy = as.numeric(input_cop$sdy_cop_ci),
        R2 = as.numeric(input_cop$R2_cop_ci),
        eff_thr = as.numeric(input_cop$eff_thr_cop_ci),
        FR2max = as.numeric(input_cop$FR2max_cop_ci),
        index = "COP",
        to_return = "raw_output"
      )
    
    cop_output <- 
      paste0(
        "<strong>Coefficient of Proportionality (COP):</strong><br><br>",
        "This function calculates a correlation-based coefficient of proportionality (delta) ",
        "which is exact even in finite samples as well as Oster's delta*. ",
        "Using the absolute value of the estimated effect, result can be interpreted by symmetry.<br><br>",
        "Delta* is ", sprintf("%.3f", raw_calc$`delta*`),
        " (assuming no covariates in the baseline model M1), ",
        "the correlation-based delta is ", sprintf("%.3f", raw_calc$`delta_exact`),
        " with a bias of ", sprintf("%.3f", raw_calc$delta_pctbias),
        "%.<br><br>",
        "Note that %bias = (delta* - delta) / delta.<br><br>",
        "With delta*, the coefficient in the final model will be ",
        sprintf("%.3f", raw_calc$eff_x_M3_oster), ".<br>",
        "With the correlation-based delta, the coefficient will be ",
        sprintf("%.3f", raw_calc$eff_x_M3), ".<br><br>",
        "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
        " of the konfound package, beginning with the advanced code provided below on this page.<br>",
        "<hr>", 
        "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
      )
    
  }
  
  
  
  
  
  ##########################################################################
  ### Return both text and plot separately
  ##########################################################################
  
  cop_results <-
    list(
      text = cop_output,
      plot = raw_calc$Figure,
      raw = r_output
    )
  
  return(cop_results)  
  
}

