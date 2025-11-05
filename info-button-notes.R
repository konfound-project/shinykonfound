################################################################################
### Load Packages
################################################################################

library(tidyverse)
library(shinyBS)



################################################################################
### Info button notes for Shiny app
### Store all popover content in a named list
################################################################################


# Helper function to create info buttons with popovers
create_info_button <- 
  function(id, label_name) {
    tagList(
      bsButton(id, 
               label = label_name, 
               icon = icon("info", lib = "font-awesome"), 
               size = "extra-small"),
      do.call(bsPopover, c(list(id = id), info_notes[[id]]))
    )
  }

info_notes <- 
  list(
    
    ############################################################################
    step1info = list(
      title = "More Information",
      content = HTML(paste0(
        "The type of outcome you choose will affect the type of sensitivity analyses you can run.  A dichotomous outcome takes two values (e.g., 1,0) such as indicating whether a student dropped out of school (1) or not (0).  A continuous outcome can take any value across a range, such as a test score."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    step2info = list(
      title = "More Information",
      content = HTML(paste0(
        "Enter values obtained from an estimated model (e.g., estimated effect, standard error, number of covariates, sample size) that can be based on a published example or your own output.  Dichotomous outcomes also require the number in the treatment condition or the four values in a 2x2 table."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    step3info = list(
      title = "More Information",
      content = HTML(paste0(
        "Choose which type of sensitivity analysis you prefer based on the framework, constraints, and approach you wish to specify. See specific information icons for details", tags$a(href="https://www.dropbox.com/s/accoz5xu82vy27v/KonFound-it%21%20enhanced.xlsx?dl=0"), "Spreadsheet for calculating indices (KonFound-it!)"
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    fragility_info = list(
      title = "More Information",
      content = HTML(paste0(
        "This calculates Fragility – the number of cases that must be switched (e.g., from treatment success to treatment failure) to make the association between treatment and outcome have a p-value of .05.  It also calculates the Robustness of Inference to Replacement (RIR), the number of cases that must be replaced to generate the switches associated with Fragility.  See results for the 2x2 Implied Table and Transfer Table."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    itcv_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The Impact Threshold for a Confounding Variable (ITCV) reports how strongly an omitted variable would have to be correlated with both the predictor of interest and the outcome to make the estimated effect have a p-value of .05. For alternative thresholds use the  R or Stata Konfound commands or the Konfound-it spreadsheet."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    rir_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The Robustness of Inference to Replacement (RIR) quantifies what proportion of the data must be replaced (with cases with zero effect) to make the estimated effect have a p-value of .05. For alternative thresholds use the  R or Stata Konfound commands or the Konfound-it spreadsheet."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    pse_info = list(
      title = "More Information",
      content = HTML(paste0(
        "This calculates the correlation between the omitted variable and the focal predictor and between the omitted variable and the outcome necessary to make the estimated effect equal the specified threshold while the standard error remains fixed at the entered value for the original analysis. Extra inputs are required including the threshold for inference (e.g., 1.96 x standard error), standard deviation of the outcome (Y), the standard deviation of the focal predictor (X) and the observed R2."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    cop_info = list(
      title = "More Information",
      content = HTML(paste0(
        "This calculates the correlation between the omitted variable and the focal predictor and between the omitted variable and the outcome necessary to make the estimated effect of the focal predictor be zero and an R2 as specified on input. These correlations also generate the Coefficient of Proportionality (COP) , the proportion selection on unobservables (omitted covariates) relative to observables (observed covariates) necessary to reduce the effect of the focal predictor to zero for a specified R2.  COP requires extra inputs including the standard deviation of the outcome (Y), of the focal predictor (X), the observed R2, and the desired final R2 (FR2MAX)."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    step4info = list(
      title = "More Information",
      content = HTML(paste0(
        "Enter values from data or estimated model as well as specified thresholds for some analyses."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    ci_rir_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Assumes the confidence interval is symmetric."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    unstd_beta_info = list(
      title = "More Information",
      content = HTML(paste0(
        "This is the estimated coefficient for the predictor of interest in a linear model (i. e., regression) or a difference of means."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    std_error_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The standard deviation of the sampling distribution, used to calculate the statistical significance of the estimated effect."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_obs_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Sample size: For multilevel models this is the number of units at the level of the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_covariates_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Number of other variables entered into the model other than the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    l_bound_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Lower bound of the confidence interval."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    u_bound_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Upper bound of the confidence interval."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_obs_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Sample size: For multilevel models this is the number of units at the level of the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_covariates_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Number of other variables entered into the model other than the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    ci_pse_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Assumes the confidence interval is symmetric."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    unstd_beta_pse_info = list(
      title = "More Information",
      content = HTML(paste0(
        "This is the estimated coefficient for the predictor of interest in a linear model (i.e., regression) or a difference of means"
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    std_error_pse_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The standard deviation of the sampling distribution, used to calculate the statistical significance of the estimated effect."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_obs_pse_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Sample size. For multilevel models this is the number of units at the level of the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_covariates_pse_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Number of other variables entered into the model other than the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    sdx_pse_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The focal predictor is the independent variable (e.g., treatment)."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    sdy_pse_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The outcome is the dependent variable."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    R2_pse_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "R-squared is the coefficient of determination, or the proportion of variance in Y explained by the model."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ###########################################################################
    eff_thr_pse_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The Threshold for Inference is the value of the estimated effect of X on Y used for inference (e.g., 1.96 * Standard Error)."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    l_bound_pse_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Lower bound of the confidence interval."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    u_bound_pse_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Upper bound of the confidence interval."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_obs_pse_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Sample size. For multilevel models this is the number of units at the level of the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_covariates_pse_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Number of other variables entered into the model other than the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    sdx_pse_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The focal predictor is the independent variable (e.g., treatment)."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    sdy_pse_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The outcome is the dependent variable."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    R2_pse_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "R-squared is the coefficient of determination, or the proportion of variance in Y explained by the model."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ###########################################################################
    eff_thr_pse_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The Threshold for Inference is the value of the estimated effect of X on Y used for inference (e.g., 1.96 * Standard Error)."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    ci_cop_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Assumes the confidence interval is symmetric."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    unstd_beta_cop_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "This is the estimated coefficient for the predictor of interest in a linear model (i.e., regression) or a difference of means."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    std_error_cop_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The standard deviation of the sampling distribution, used to calculate the statistical significance of the estimated effect."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_obs_cop_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Sample size. For multilevel models this is the number of units at the level of the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_covariates_cop_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Number of other variables entered into the model other than the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    sdx_cop_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The focal predictor is the independent variable (e.g., treatment)."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    sdy_cop_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The outcome is the dependent variable."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    R2_cop_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "R-squared is the coefficient of determination, or the proportion of variance in Y explained by the model."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    eff_thr_cop_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The Threshold for Inference is the value of the estimated effect of X on Y used for inference (e.g., 1.96 * Standard Error)."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    FR2max_cop_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The maximum R-squared if all conceivable covariates were observed and included in the model."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    lower_bnd_cop_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Lower bound of the confidence interval."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    upper_bnd_cop_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Upper bound of the confidence interval."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_obs_cop_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Sample size. For multilevel models this is the number of units at the level of the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_covariates_cop_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Number of other variables entered into the model other than the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    sdx_cop_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The focal predictor is the independent variable (e.g., treatment)."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    sdy_cop_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The outcome is the dependent variable."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    R2_cop_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "R-squared is the coefficient of determination, or the proportion of variance in Y explained by the model."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    eff_thr_cop_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The Threshold for Inference is the value of the estimated effect of X on Y used for inference (e.g., 1.96 * Standard Error)."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    FR2max_cop_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The maximum R-squared if all conceivable covariates were observed and included in the model."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    unstd_beta_log_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "This is the estimated coefficient for the predictor of interest in a logistic model."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    std_error_log_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "The standard deviation of the sampling distribution, used to calculate the statistical significance of the estimated effect."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_obs_log_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Sample size. For multilevel models this is the number of units at the level of the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_covariates_log_ee_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Number of other variables entered into the model other than the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    lower_bnd_log_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Lower bound of the confidence interval."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    upper_bnd_log_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Upper bound of the confidence interval."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_obs_log_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Sample size. For multilevel models this is the number of units at the level of the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    n_covariates_log_ci_info = list(
      title = "More Information",
      content = HTML(paste0(
        "Number of other variables entered into the model other than the focal predictor."
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    CHANGE = list(
      title = "More Information",
      content = HTML(paste0(
        ""
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    CHANGE = list(
      title = "More Information",
      content = HTML(paste0(
        ""
      )),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    ),
    
    
    ############################################################################
    NULL
  )