################################################################################
### Load Packages
################################################################################

library(shiny)
library(tidyverse)
library(konfound)





################################################################################
### Logistic Non-Linear Model (LOG)
################################################################################

get_log_results <- function(input_log) {
  
  ### Set default alpha and tails
  alpha <- 0.05
  tails <- 2
  

  
  ##########################################################################
  ### GENERATE LOG OUTPUT with Estimated Effects
  ##########################################################################
  
  if(input_log$Uncertainty_log == 'EstEff') {
    
    validate(
      need(is.numeric(input_log$est_effect_log_ee) &
             is.numeric(input_log$std_error_log_ee) &
             is.numeric(input_log$n_obs_log_ee) &
             is.numeric(input_log$n_covariates_log_ee) &
             is.numeric(input_log$n_trm_log_ee),
           "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input_log$n_obs_log_ee > (input_log$n_covariates_log_ee + 2),
           "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input_log$std_error_log_ee > 0, "Did not run! Standard error needs to be greater than zero.")
    )
    
    
    ### Set clean parameters
    estimated_effect <- as.numeric(input_log$est_effect_log_ee)
    standard_error <- as.numeric(input_log$std_error_log_ee)
    n_observations <- as.numeric(input_log$n_obs_log_ee)
    n_covariates <- as.numeric(input_log$n_covariates_log_ee)
    n_treatments <- as.numeric(input_log$n_trm_log_ee)
    lower_bnd <- NULL
    upper_bnd <- NULL
    
    
    ### Run calculations
    r_output <- 
      capture.output(
        pkonfound(
          est_eff = estimated_effect,
          std_err = standard_error,
          n_obs = n_observations,
          n_covariates = n_covariates,
          n_treat = n_treatments,
          model_type = "logistic",
          to_return = "print"
        )
      )
    
    raw_calc <- 
      pkonfound(
        est_eff = estimated_effect,
        std_err = standard_error,
        n_obs = n_observations,
        n_covariates = n_covariates,
        n_treat = n_treatments,
        model_type = "logistic",
        to_return = "raw_output"
      )
    
    
    
    ##########################################################################
    ### GENERATE LOG OUTPUT with Confidence Interval
    ##########################################################################
    
  } else {
    
    validate(
      need(is.numeric(input_log$lower_bnd_log_ci) &
             is.numeric(input_log$upper_bnd_log_ci) &
             is.numeric(input_log$n_obs_log_ci) &
             is.numeric(input_log$n_covariates_log_ci) &
             is.numeric(input_log$n_trm_log_ci),
           "Did not run! Did you enter numbers for the lower bound, upper bound, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input_log$n_obs_log_ci > (input_log$n_covariates_log_ci + 2),
           "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It."),
      need(input_log$upper_bnd_log_ci > input_log$lower_bnd_log_ci, 
           paste0("Did not run! The upper bound needs to be greater than the lower bound.",
                  " The upper bound you entered of ", input_log$upper_bnd_log_ci, 
                  " is less than or equal than the lower bound of ", 
                  input_log$lower_bnd_log_ci, "."
           )
      )
    )
    
    
    ### Set clean parameters
    lower_bnd <- as.numeric(input_log$lower_bnd_log_ci)
    upper_bnd <- as.numeric(input_log$upper_bnd_log_ci)
    n_observations <- as.numeric(input_log$n_obs_log_ci)
    n_covariates <- as.numeric(input_log$n_covariates_log_ci)
    n_treatments <- as.numeric(input_log$n_trm_log_ci)
    estimated_effect <- 
      (upper_bnd + lower_bnd) / 2
    standard_error <- 
      (estimated_effect - lower_bnd) / 
      qt(alpha / tails, 
         n_observations - n_covariates, 
         lower.tail = FALSE)
    
    
    ### Run calculations
    r_output <- 
      capture.output(
        pkonfound(
          lower_bound = lower_bnd,
          upper_bound = upper_bnd,
          n_obs = n_observations,
          n_covariates = n_covariates,
          n_treat = n_treatments,
          model_type = "logistic",
          to_return = "print"
        )
      )
    
    raw_calc <- 
      pkonfound(
        lower_bound = lower_bnd,
        upper_bound = upper_bnd,
        n_obs = n_observations,
        n_covariates = n_covariates,
        n_treat = n_treatments,
        model_type = "logistic",
        to_return = "raw_output"
      )
    
  }
  
  
  
  
  
  ##########################################################################
  ### Set Parameters: RIR and Fragility
  ##########################################################################
  
  final_switch <- raw_calc$total_switch  # total fragility
  final_primary <- raw_calc$fragility_primary
  final_extra <- 
    if (!is.null(raw_calc$fragility_supplemental)) {raw_calc$fragility_supplemental} else {NA}
  RIR_primary <- raw_calc$RIR_primary
  RIR_extra <- 
    if (!is.null(raw_calc$RIR_supplemental)) {raw_calc$RIR_supplemental} else {NA}
  
  
  
  
  
  ##########################################################################
  ### Construct conditional message based on intermediate values
  ##########################################################################
  
  ### Define elements of implied table
  a <- raw_calc$starting_table[1,1]
  b <- raw_calc$starting_table[1,2]
  c <- raw_calc$starting_table[2,1]
  d <- raw_calc$starting_table[2,2]
  
  
  ### Compute thr_t 
  if (estimated_effect < 0) {
    thr_t <- 
      qt(1 - (alpha / tails), n_observations - n_covariates - 2) * -1
  } else {
    thr_t <- 
      qt(1 - (alpha / tails), n_observations - n_covariates - 2)
  }
  
  
  ### Calculate t_ob
  t_ob <- estimated_effect / standard_error
  
  
  ### isdcroddsratio logic
  dcroddsratio_ob <- 
    if (thr_t < t_ob) TRUE else FALSE
  
  
  ### Define transferway, RIRway, p_destination for flexible print output
  switch_trm <- TRUE
  replace <- "control"
  transferway <- ""
  RIRway <- ""
  prob_indicator <- ""
  p_destination_val <- NA
  invalidate_ob <- raw_calc$invalidate_ob # TRUE => "To nullify" / FALSE => "To sustain"
  needtworows <- raw_calc$needtworows # TRUE => double-switch scenario
  
  
  ### Single-Row Switch Scenario
  if (!needtworows) {
    
    if (switch_trm && dcroddsratio_ob) {
      transferway <- 
        "treatment success to treatment failure"
      RIRway <- 
        "treatment success"
      p_destination <- 
        p_destination <- 
        round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
      RIR_pi <- 
        RIR_primary / d * 100
      
    } else if (switch_trm && !dcroddsratio_ob) {
      transferway <- 
        "treatment failure to treatment success"
      RIRway <- 
        "treatment failure"
      p_destination <- 
        p_destination <- 
        round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
      RIR_pi <- 
        RIR_primary / c * 100
      
    }
    
    
    ### Double-Switch Scenario
  } else if (needtworows) {
    RIR_pi <- NA
    transferway_extra <- ""
    RIRway_extra <- ""
    prob_indicator_extra <- ""
    p_destination_extra_val <- NA
    
    
    ### Define extra row logic
    if (switch_trm && dcroddsratio_ob) {
      transferway_extra <- 
        "control failure to control success"
      RIRway_extra <- 
        "control failure"
      RIRway_extra_start <- 
        "control row"
      p_destination_extra <- 
        round((b+d)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(b/(a+b) * 100, 3) * (1 - (replace == "entire"))
      
    } else if (switch_trm && !dcroddsratio_ob) {
      transferway_extra <- 
        "control success to control failure"
      RIRway_extra <- 
        "control success"
      RIRway_extra_start <- 
        "control row"
      p_destination_extra <- 
        round((a+c)/(a+b+c+d) * 100, 3) * (replace == "entire") + 
        round(a/(a+b) * 100, 3) * (1 - (replace == "entire"))
    } 
    
  }
  
  ### Conditional Fragility calculation component 
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
    
  } else {  # raw_calc$p_start > 0.05
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
  
  ### TRUE => Double-Switch Scenario
  if (needtworows) {
    
    ### Conditional Fragility calculation component
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
      
    } else {  # raw_calc$p_start > 0.05
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
  
  
  
  
  
  ##########################################################################
  ### Generate text output
  ##########################################################################
  
  ### Decide if p_start < .05 => "nullify" or "sustain" text
  change_phrase <- 
    if (invalidate_ob) {
      paste0("To nullify the inference that the effect is different from 0 (alpha = 0.050), one would ")
    } else {
      paste0("To sustain an inference that the effect is different from 0 (alpha = 0.050), one would ")
    }
  
  
  ### Build the single-row text (if needtworows == FALSE)
  single_row_text <- ""
  
  if (!needtworows) {
    single_row_text <- 
      paste0(
        change_phrase, " ",
        "need to transfer ", raw_calc$fragility_primary, " data points from ", transferway,
        " (Fragility = ", raw_calc$fragility_primary, ").<br><br>",
        "This is equivalent to replacing ", raw_calc$total_RIR, 
        " (", sprintf("%.3f", raw_calc$RIR_perc), "%) ", RIRway, 
        " data points with data points for which the probability of ", 
        prob_indicator, " in the control group (", 
        p_destination, "%) applies (RIR = ", raw_calc$RIR_primary, ").<br><br>"
      )
  }
  
  
  ### For referencing partial row text, e.g. "to nullify the inference, transferring X data points..."
  change_t <- 
    if (invalidate_ob) "to nullify the inference," else "to sustain an inference,"
  
  
  ### Build the two-row text (if needtworows == TRUE)
  two_row_text <- ""
  
  if (needtworows == TRUE) {
    final_primary <- raw_calc$fragility_primary
    final_extra <- raw_calc$fragility_supplemental
    RIR_primary <- raw_calc$RIR_primary
    RIR_extra <- raw_calc$RIR_supplemental
    
    two_row_text <- 
      paste0(
        "In terms of Fragility, ", change_t, " transferring ", final_primary, 
        " data points from ", transferway, " is not enough to change the inference.<br><br>",
        "One would also need to transfer ", final_extra, " data points from ", 
        transferway_extra, 
        ".<br><br>",
        "In terms of RIR, generating the ", final_primary, " switches from ", transferway,
        " is equivalent to replacing ", RIR_primary, " ", RIRway, 
        " data points with data points for which the probability of ",
        prob_indicator, " in the control group (", p_destination, "%) applies.<br><br>",
        "In addition, generating the ", final_extra, " switches from ", transferway_extra,
        " is equivalent to replacing ", RIR_extra, " ", RIRway_extra, 
        " data points with data points for which the probability of ", prob_indicator_extra, 
        " in the control group (", p_destination_extra, "%) applies.<br><br>",
        "Total RIR = primary RIR + supplemental RIR = (",
        final_primary, " / ", sprintf("%.3f", p_destination/100), ") + (", 
        final_extra, " / ", sprintf("%.3f", p_destination_extra/100), ") = ", 
        RIR, " + ", RIR_extra, " = ", raw_calc$total_RIR, ".<br><br>",
        "Total Fragility = ", final_primary + final_extra, " = ", raw_calc$total_switch, 
        ".<br><br>"
      )
  }
  
  changeSE_message <- 
    if (raw_calc$user_SE != raw_calc$analysis_SE) {
      paste0(
        "The SE has been adjusted to ", sprintf("%.3f", raw_calc$analysis_SE),
        " to generate real numbers in the implied table for which the p-value would be ",
        sprintf("%.3f", raw_calc$p_start), ".<br><br>",
        "Numbers in the table cells have been rounded to integers, which may slightly ",
        "alter the estimated effect from the value originally entered.<br><br>"
      )
    } else {
      paste0(
        "Values in the table have been rounded to the nearest integer. ",
        "This may cause a small change to the estimated effect for the table.<br>"
      )
    }
  
  if (needtworows == FALSE) {
    RIR_calc <- 
      paste0(
        "Note that RIR = Fragility/P(destination) = ",
        raw_calc$fragility_primary, "/", sprintf("%.3f", p_destination/100), 
        " ~ ", raw_calc$total_RIR, ".")
  } else{
    RIR_calc <- 
      paste0(
        "Note that RIR = primary RIR + supplemental RIR = (",
        raw_calc$fragility_primary, "/", sprintf("%.3f", p_destination/100), 
        ") + (", raw_calc$fragility_supplemental, "/", 
        sprintf("%.3f", p_destination_extra/100), ") ~ ", raw_calc$total_RIR, ". ",
        "based on the calculation RIR = Fragility/P(destination)."
      )
  }
  
  ### Special case if RIR percentage > 100
  if (!needtworows && RIR_pi > 100) {
    conclusion_large_rir <- 
      paste0(
        sprintf("Note the RIR exceeds 100%. Generating the transfer of %d data points would ", 
                raw_calc$fragility_primary),
        " require replacing more data points than are in the ", RIRway, " condition. ")
  } else {
    conclusion_large_rir <- ""  # Empty string if RIR_pi <= 100
  }
  
  ### Merge everything into final_output
  log_output <-
    paste0(
      "<strong>Robustness of Inference to Replacement (RIR):</strong><br>",
      "RIR = ", raw_calc$RIR_primary, "<br>",
      "Fragility = ", raw_calc$fragility_primary, "<br><br>",
      
      "You started with:<br>",
      if (is.null(lower_bnd)) {
        paste0(
          "log odds = ", sprintf("%.3f", estimated_effect), ",<br>",
          "SE = ", sprintf("%.3f", raw_calc$user_std_err), ",<br>",
          "with p-value = ", sprintf("%.3f", raw_calc$p_start), ".<br><br>"
        )
      } else {
        paste0(
          "Lower bound = ", lower_bnd, ",<br>",
          "Upper bound = ", upper_bnd, ".<br><br>",
          "This means that the log odds are ", sprintf("%.3f", estimated_effect), 
          " and the standard error is ", sprintf("%.3f", raw_calc$user_std_err), 
          ", with p-value = ", sprintf("%.3f", raw_calc$p_start), ".<br><br>"
        )
      },
      "The table implied by the parameter estimates and sample sizes you entered:<br>",
      "<strong><u>Implied Table:</u></strong><br>",
      knitr::kable(raw_calc$table_start_3x3, format = "html", align = "c",
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
      
      ### Transfer Table
      "The transfer of data points yields the following table:<br>",
      "<strong><u>Transfer Table:</u></strong><br>",
      knitr::kable(raw_calc$table_final_3x3, format = "html", align = "c",
                   table.attr = "style='width:100%;'",
                   col.names = c("", "Failures", "Successes", "Success Rate")), 
      "<hr>",
      "The log odds (estimated effect) = ", sprintf("%.3f", raw_calc$est_eff_final),
      ", SE = ", sprintf("%.3f", raw_calc$std_err_final), 
      ", p-value = ", sprintf("%.3f", raw_calc$p_final), ".<br><br>",
      "This p-value is based on t = estimated effect / standard error.<br>"
    )
  
  
  
  
  
  ##########################################################################
  ### Generate RIR benchmark description for logistic non-linear model
  ##########################################################################
  
  benchmark_section <- ""
  
  ### Nullify Scenario: Show RIR benchmark details
  if (invalidate_ob) {
    benchmark_section <- paste0(
      "<strong>Benchmarking RIR for Logistic Regression (Beta Version)</strong><br><br>",
      "The benchmark value helps interpret the RIR necessary to nullify an inference by comparing the change ",
      "needed to nullify the inference with the changes in the estimated effect due to observed covariates. ",
      "Currently this feature is available only when the reported results are statistically significant.<br><br>",
      
      "The benchmark is used to compare the bias needed to nullify the inference / bias reduction due to ",
      "observed covariates. Specifically, change in data from implied to transfer table / change ",
      "in data from unconditional table to implied table.<br><br>",
      
      "To calculate this benchmark value, a range of treatment success values is automatically generated based on ",
      "the assumption that the marginals are constant between the implied table and the raw unadjusted table.<br><br>",
      
      "The benchmark value is visualized as a graph, allowing the user to interpret how the benchmark changes with ",
      "hypothesized treatment success values.<br><br>",
      
      "Note that switches in the control row and treatment row required to generate the implied table from the ", 
      "unadjusted table are used to define the benchmark RIR.<br>"
    )
    
    ### Sustain Scenario: No meaningful benchmark
  } else {
    
    benchmark_section <- paste0(
      "<strong>Benchmarking RIR for Logistic Regression (Beta Version)</strong><br><br>",
      "The treatment is not statistically significant in the implied table ",
      "and would also not be statistically significant in the raw table ",
      "(before covariates were added). In this scenario, we do not yet have ",
      "a clear interpretation of the benchmark, and therefore the benchmark ",
      "calculation is not reported.<br>"
    )
  }
  
  
  
  
  
  ##########################################################################
  ### Add final note/citation, as in the R code
  ##########################################################################
  
  log_output <- 
    paste0(
      log_output,
      "<hr>",
      benchmark_section,
      "<hr>",
      "See Frank et al. (2021) for a description of the method.<br><br>",
      "<strong>Citation:</strong><br>",
      "*Frank, K. A., *Lin, Q., *Maroulis, S., *Mueller, A. S., Xu, R., ",
      "Rosenberg, J. M., ... & Zhang, L. (2021). Hypothetical case replacement ",
      "can be used to quantify the robustness of trial results. ",
      "<em>Journal of Clinical Epidemiology, 134</em>, 150-159.<br>",
      "<em>*Authors are listed alphabetically.</em><br><br>",
      "Accuracy of results increases with the number of decimals reported.<br><br>",
      "This analysis assumes the use of default parameters. For greater flexibility, ",
      "use the R or Stata versions of the konfound package, beginning with ",
      "the advanced code provided below on this page.<br>",
      "<hr>",
      "<em>Calculated with konfound R package version </em>", packageVersion("konfound")
    )
  
  
  
  
  
  ##########################################################################
  ### Return both text and plot separately
  ##########################################################################
  
  # Determine the message for the plot if no plot is available:
  conditional_plot_message <-
    if (is.null(raw_calc$benchmark_plot)) {
      "No graphical output for this analysis."
    } else {
      "Plot is displayed below."
    }
  
  
  log_results <-
    list(
      text = log_output,
      plot_message = conditional_plot_message,
      plot = raw_calc$benchmark_plot,
      raw = r_output
    )
  
  return(log_results)  
  
}

