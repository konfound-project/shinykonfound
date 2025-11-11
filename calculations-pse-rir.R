################################################################################
### Load Packages
################################################################################

library(shiny)
library(tidyverse)
library(konfound)

################################################################################
### PSE
################################################################################

# helper function 1
isinvalidate <- function(thr_t, ob_t) {
  if ((0 < thr_t && thr_t < ob_t) || (ob_t < thr_t && thr_t < 0)) {
    x <- TRUE
  } else {
    x <- FALSE
  }
  return(x)
}

# helper function 2
se_preserve_replacement <- function(est_eff, std_err, n_obs, n_covariates,
                                    sd_x, sd_y_obs,
                                    alpha = 0.05, tails = 2) {
  df <- n_obs - n_covariates - 3
  
  stopifnot(is.finite(std_err), std_err > 0,
            is.finite(sd_x), sd_x > 0,
            is.finite(df), df  > 0,
            is.finite(sd_y_obs), sd_y_obs > 0,
            is.finite(est_eff))
  
  t_hat <- est_eff / std_err
  
  # (B1) sd_{y|x} = se(delta_hat) * sd_x * sqrt(df)
  sd_yx <- std_err * sd_x * sqrt(df)
  
  # critical t (signed to match the sign of the estimate);
  # Appendix uses r# built from the critical t.
  t_crit_abs <- if (tails == 2) stats::qt(1 - alpha/2, df) else stats::qt(1 - alpha, df)
  t_crit     <- sign(est_eff) * abs(t_crit_abs)
  
  # (B3) r# = t_crit / sqrt(t_crit^2 + df)
  r_sharp <- t_crit / sqrt(t_crit^2 + df)
  
  # (B2) sd_y_combined from sd_yx and r#
  sd_y_combined <- sd_yx / sqrt(1 - r_sharp^2)
  
  # r_xy from the observed t (standard identity)
  # r_xy = t / sqrt(t^2 + df)
  r_xy <- t_hat / sqrt(t_hat^2 + df)
  
  # (B4) pi = 1 - ( r# * sd_y^{combined} / sd_y^{obs} ) / r_xy
  pi <- 1 - (r_sharp * sd_y_combined / sd_y_obs) / r_xy
  
  # Some error messages for unusual values
  if (!is.finite(pi)) stop("pi is not finite. Check inputs.")
  if (pi <= 0 || pi >= 1) warning(sprintf("pi=%.4f is outside (0,1). Interpretation may be unstable.", pi))
  
  # (B5) solve for sd_y_unobs
  # sd_y_combined^2 = (1 - pi) * s_y_obs^2 + pi * s_y_unobs^2
  # hence, sd_y_unobs = sqrt( (sd_y_combined^2 - (1 - pi) * s_y_obs^2) / pi )
  num <- sd_y_combined^2 - (1 - pi) * sd_y_obs^2
  if (num <= 0) warning("The quantity under the square root for sd_y_unobs is non-positive. Check rounding and inputs.")
  
  sd_y_unobs <- sqrt(num / pi)
  
  est_eff_new <- t_crit * std_err
  
  list(
    pi = pi,
    sd_y_unobs = sd_y_unobs,
    sd_yx = sd_yx,
    r_sharp = r_sharp,
    sd_y_combined = sd_y_combined,
    r_xy = r_xy,
    est_eff_new = est_eff_new
  )
}

get_pse_rir_results <- function(input_pse) {
  
  
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
    
    pse_rir_result <- se_preserve_replacement(
      est_eff = as.numeric(input_pse$est_effect_pse_ee),
      std_err = as.numeric(input_pse$std_err_pse_ee),
      n_obs = as.numeric(input_pse$n_obs_pse_ee),
      n_covariates = as.numeric(input_pse$n_covariates_pse_ee),
      sd_x = as.numeric(input_pse$sdx_pse_ee),
      sd_y_obs = as.numeric(input_pse$sdy_pse_ee),
      alpha = 0.05,
      tails = 2
    )
    
    # Intermediate values for language
    pi_hat <- pse_rir_result$pi
    n_obs <- as.numeric(input_pse$n_obs_pse_ee)
    n_replace <- round(n_obs * pi_hat)
    sd_y_unobs  <- pse_rir_result$sd_y_unobs
    eff_new <- pse_rir_result$est_eff_new
    std_err <-  as.numeric(input_pse$std_err_pse_ee)
    est_eff <- as.numeric(input_pse$est_effect_pse_ee)
    
    z <- qnorm(1 - 0.05/2)
    
    df <- as.numeric(input_pse$n_obs_pse_ee) - as.numeric(input_pse$n_covariates_pse_ee) - 3
    beta <- est_eff * (as.numeric(input_pse$sdx_pse_ee) / as.numeric(input_pse$sdy_pse_ee))
    SE <- std_err * (as.numeric(input_pse$sdx_pse_ee) / as.numeric(input_pse$sdy_pse_ee))
    
    ## observed regression, reg y on x given z
    tyxGz <- beta / SE  
    
    if (est_eff < 0) {
      thr_t <- qt(1 - 0.05 / 2, df) * -1
    } else {
      thr_t <- qt(1 - 0.05 / 2, df)
    }
    
    invalidate_ob <- isinvalidate(thr_t, tyxGz)
    
    # Action verb and goal sentence
    action_verb <- if (invalidate_ob) "nullify the inference" else "attain statistical significance"
    
    pse_rir_output <- 
      paste0(
        "<strong>Robustness of Inference to Replacement (fixed standard error)</strong><br>",
        "To ", action_verb, " while preserving the reported standard error, ",
        sprintf("approximately pi = %.3f (%.1f%%) of the data points would need to be replaced.<br>",
                pi_hat, 100 * pi_hat),
        sprintf("This corresponds to replacing about %.0f of %d observations.<br><br>",
                n_replace, n_obs),
        sprintf("The replacement cases would need to have a standard deviation of Y equal to %.2f ",
                sd_y_unobs),
        "in order to maintain the same standard error of the coefficient.<br><br>",
        sprintf(
          "Therefore, if %.0f%% of the cases are replaced with cases for which the effect is modified as above, ",
          100 * pi_hat
        ),
        sprintf(
          "the estimated effect will be %.4f with standard error of %.4f at alpha = 0.05.<br>",
          eff_new, std_err
        ),
        "<br>",
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

    z <- qnorm(1 - 0.05/2)

    pse_rir_result <- se_preserve_replacement(
      est_eff = (as.numeric(input_pse$lower_bnd_pse_ci) + as.numeric(input_pse$upper_bnd_pse_ci)) / 2,
      std_err = (as.numeric(input_pse$upper_bnd_pse_ci) - as.numeric(input_pse$lower_bnd_pse_ci)) / (2 * z),
      n_obs = as.numeric(input_pse$n_obs_pse_ci),
      n_covariates = as.numeric(input_pse$n_covariates_pse_ci),
      sd_x = as.numeric(input_pse$sdx_pse_ci),
      sd_y_obs = as.numeric(input_pse$sdy_pse_ci),
      alpha = 0.05,
      tails = 2
    )
    
    # Intermediate values for language
    pi_hat <- pse_rir_result$pi
    n_obs <- as.numeric(input_pse$n_obs_pse_ci)
    n_replace <- round(n_obs * pi_hat)
    sd_y_unobs <- pse_rir_result$sd_y_unobs
    eff_new <- pse_rir_result$est_eff_new
    std_err <- (as.numeric(input_pse$upper_bnd_pse_ci) - as.numeric(input_pse$lower_bnd_pse_ci)) / (2 * z)
    est_eff <- (as.numeric(input_pse$lower_bnd_pse_ci) + as.numeric(input_pse$upper_bnd_pse_ci)) / 2
    
    df <- as.numeric(input_pse$n_obs_pse_ci) - as.numeric(input_pse$n_covariates_pse_ci) - 3
    beta <- ((as.numeric(input_pse$lower_bnd_pse_ci) + as.numeric(input_pse$upper_bnd_pse_ci)) / 2) * (as.numeric(input_pse$sdx_pse_ci) / as.numeric(input_pse$sdy_pse_ci))
    SE <- ((as.numeric(input_pse$upper_bnd_pse_ci) - as.numeric(input_pse$lower_bnd_pse_ci)) / (2 * z)) * (as.numeric(input_pse$sdx_pse_ci) / as.numeric(input_pse$sdy_pse_ci))
    
    ## observed regression, reg y on x given z
    tyxGz <- beta / SE  
    
    if (est_eff < 0) {
      thr_t <- qt(1 - 0.05 / 2, df) * -1
    } else {
      thr_t <- qt(1 - 0.05 / 2, df)
    }
    
    invalidate_ob <- isinvalidate(thr_t, tyxGz)
    
    # Action verb and goal sentence
    action_verb <- if (invalidate_ob) "nullify the inference" else "attain statistical significance"
    
    pse_rir_output <- 
      paste0(
        "<strong>Robustness of Inference to Replacement (fixed standard error)</strong><br>",
        "To ", action_verb, " while preserving the reported standard error, ",
        sprintf("approximately pi = %.3f (%.1f%%) of the data points would need to be replaced.<br>",
                pi_hat, 100 * pi_hat),
        sprintf("This corresponds to replacing about %.0f of %d observations.<br><br>",
                n_replace, n_obs),
        sprintf("The replacement data points would need to have a standard deviation of Y equal to %.2f",
                sd_y_unobs),
        "in order to maintain the same standard error of the coefficient.<br><br>",
        sprintf(
          "Therefore, if %.0f%% of the data points are replaced with data points for which the effect is modified as above, ",
          100 * pi_hat
        ),
        sprintf(
          "the estimated effect will be %.4f with standard error of %.4f at alpha = 0.05.<br>",
          eff_new, std_err
        ),
        "<br>",
        "This analysis assumes the use of default parameters. For greater flexibility, use the R or Stata versions", 
        " of the konfound package, beginning with the advanced code provided below on this page.<br>",
        "<hr>", 
        "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
      )
  }
  

  ##########################################################################
  ### Return both text and plot separately
  ##########################################################################
  
  pse_rir_results <-
    list(
      text = pse_rir_output,
      plot_message = "No graphical output for this analysis.",
      raw = r_output
    )
  
  return(pse_rir_results)  
  
}

