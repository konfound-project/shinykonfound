################################################################################
### Load Packages
################################################################################

library(shiny)
library(tidyverse)
library(konfound)



################################################################################
### PSE
################################################################################

get_pse_results <- function(input_pse) {
  
    
  ##########################################################################
  ### Estimated Effects condition for PSE
  ##########################################################################
  
  if(input_pse$Uncertainty_PSE == 'EstEff') {
      
    
    ##########################################################################
    ### Validate user input values
    ##########################################################################
    
    validate(
      need(is.numeric(input_pse$est_effect_pse_ee) &
             is.numeric(input_pse$std_err_pse_ee) &
             is.numeric(input_pse$n_obs_pse_ee) &
             is.numeric(input_pse$n_covariates_pse_ee) &
             is.numeric(input_pse$eff_thr_pse_ee) &
             is.numeric(input_pse$sdx_pse_ee) &
             is.numeric(input_pse$sdy_pse_ee) &
             is.numeric(input_pse$R2_pse_ee), "Did not run! Did you enter numbers for the estimated effect, standard error, and number of observations? Please change any of these that are not a number."),
      need(input_pse$std_err_pse_ee > 0, "Did not run! Standard error needs to be greater than zero."),
      need(input_pse$n_obs_pse_ee > (input_pse$n_covariates_pse_ee + 2), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input_pse$sdx_pse_ee > 0, "Did not run! Standard deviation of x needs to be greater than zero."),
      need(input_pse$sdy_pse_ee > 0, "Did not run! Standard deviation of y needs to be greater than zero."),
      need(input_pse$R2_pse_ee > 0, "Did not run! R2 needs to be greater than zero."),
      need(input_pse$R2_pse_ee < 1, "Did not run! R2 needs to be less than one"),
      need(1-((input_pse$sdy_pse_ee^2/input_pse$sdx_pse_ee^2)*(1-input_pse$R2_pse_ee)/((input_pse$n_obs_pse_ee - input_pse$n_covariates_pse_ee - 2)*input_pse$std_err_pse_ee^2)) > 0, "Did not run! Entered values produced Rxz^2 <0, consider adding more significant digits to your entered values")
    )
    
    
    ##########################################################################
    ### GENERATE PSE OUTPUT with Estimated Effects
    ##########################################################################
    
    r_output <- 
      capture.output(
        pkonfound(
          est_eff = as.numeric(input_pse$est_effect_pse_ee),
          std_err = as.numeric(input_pse$std_err_pse_ee),
          n_obs = as.numeric(input_pse$n_obs_pse_ee),
          n_covariates = as.numeric(input_pse$n_covariates_pse_ee),
          sdx = as.numeric(input_pse$sdx_pse_ee),
          sdy = as.numeric(input_pse$sdy_pse_ee),
          R2 = as.numeric(input_pse$R2_pse_ee),
          eff_thr = as.numeric(input_pse$eff_thr_pse_ee),
          index = "PSE",
          to_return = "print"
        )
      )
    
    raw_calc <- 
      pkonfound(
        est_eff = as.numeric(input_pse$est_effect_pse_ee),
        std_err = as.numeric(input_pse$std_err_pse_ee),
        n_obs = as.numeric(input_pse$n_obs_pse_ee),
        n_covariates = as.numeric(input_pse$n_covariates_pse_ee),
        sdx = as.numeric(input_pse$sdx_pse_ee),
        sdy = as.numeric(input_pse$sdy_pse_ee),
        R2 = as.numeric(input_pse$R2_pse_ee),
        eff_thr = as.numeric(input_pse$eff_thr_pse_ee),
        index = "PSE",
        to_return = "raw_output"
      )
    
    pse_output <- 
      paste0(
        "<strong>Preserve Standard Error (Advanced Analysis):</strong><br><br>",
        "This function calculates the correlations associated with an ", 
        "omitted confounding variable (CV) that generate an estimated effect ",
        "that is approximately equal to the threshold while preserving the ",
        "originally reported standard error.<br><br>",
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
    
    
    
    
    
    ############################################################################
    ### Confidence Interval condition for PSE
    ############################################################################
    
  } else {
      
      ##########################################################################
      ### Validate user input values
      ##########################################################################
      
      validate(
        need(is.numeric(input_pse$lower_bnd_pse_ci) &
               is.numeric(input_pse$upper_bnd_pse_ci) &
               is.numeric(input_pse$n_obs_pse_ci) &
               is.numeric(input_pse$n_covariates_pse_ci) &
               is.numeric(input_pse$eff_thr_pse_ci) &
               is.numeric(input_pse$sdx_pse_ci) &
               is.numeric(input_pse$sdy_pse_ci) &
               is.numeric(input_pse$R2_pse_ci), "Did not run! Did you enter numbers for the lower bound, upper bound, and number of observations? Please change any of these that are not a number."),
        need(input_pse$upper_bnd_pse_ci > input_pse$lower_bnd_pse_ci, 
             paste0("Did not run! The upper bound needs to be greater than the lower bound.",
                    " The upper bound you entered of ", input_pse$upper_bnd_pse_ci, 
                    " is less than or equal than the lower bound of ", 
                    input_pse$lower_bnd_pse_ci, "."
             )
        ),
        need(input_pse$n_obs_pse_ci > (input_pse$n_covariates_pse_ci + 2), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
        need(input_pse$sdx_pse_ci > 0, "Did not run! Standard deviation of x needs to be greater than zero."),
        need(input_pse$sdy_pse_ci > 0, "Did not run! Standard deviation of y needs to be greater than zero."),
        need(input_pse$R2_pse_ci > 0, "Did not run! R2 needs to be greater than zero."),
        need(input_pse$R2_pse_ci < 1, "Did not run! R2 needs to be less than one")
      )
      
      
      ##########################################################################
      ### GENERATE PSE OUTPUT with Confidence Interval
      ##########################################################################
      
      r_output <- 
        capture.output(
          pkonfound(
            lower_bound = as.numeric(input_pse$lower_bnd_pse_ci),
            upper_bound = as.numeric(input_pse$upper_bnd_pse_ci),
            n_obs = as.numeric(input_pse$n_obs_pse_ci),
            n_covariates = as.numeric(input_pse$n_covariates_pse_ci),
            sdx = as.numeric(input_pse$sdx_pse_ci),
            sdy = as.numeric(input_pse$sdy_pse_ci),
            R2 = as.numeric(input_pse$R2_pse_ci),
            eff_thr = as.numeric(input_pse$eff_thr_pse_ci),
            index = "PSE",
            to_return = "print"
          )
        )
      
      raw_calc <- 
        pkonfound(
          lower_bound = as.numeric(input_pse$lower_bnd_pse_ci),
          upper_bound = as.numeric(input_pse$upper_bnd_pse_ci),
          n_obs = as.numeric(input_pse$n_obs_pse_ci),
          n_covariates = as.numeric(input_pse$n_covariates_pse_ci),
          sdx = as.numeric(input_pse$sdx_pse_ci),
          sdy = as.numeric(input_pse$sdy_pse_ci),
          R2 = as.numeric(input_pse$R2_pse_ci),
          eff_thr = as.numeric(input_pse$eff_thr_pse_ci),
          index = "PSE",
          to_return = "raw_output"
        )
      
      pse_output <- 
        paste0(
          "<strong>Preserve Standard Error (Advanced Analysis):</strong><br><br>",
          "This function calculates the correlations associated with an ", 
          "omitted confounding variable (CV) that generate an estimated effect ",
          "that is approximately equal to the threshold while preserving the ",
          "originally reported standard error.<br><br>",
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
  }
  
  
  
  
  ##########################################################################
  ### Return both text and plot separately
  ##########################################################################
  
  pse_results <-
    list(
      text = pse_output,
      plot_message = "No graphical output for this analysis.",
      raw = r_output
    )
  
  return(pse_results)  
  
}

