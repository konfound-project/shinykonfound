################################################################################
### Load Packages
################################################################################

library(shiny)
library(tidyverse)
library(shinyjs)
library(rclipboard)
library(fedmatch)

# install.packages("remotes")
# remotes::install_github("deepanshu88/shinyDarkmode")
library(shinyDarkmode)

library(konfound)

source("calculations-rir-itcv.R")
source("calculations-pse.R")
source("calculations-cop.R")
source("calculations-log.R")


################################################################################
### Set up Shiny Server
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
           autoMatchOsTheme = FALSE
  )
  
  r <- reactiveValues(print_results1 = "") # Create empty reactive string for printed results (formatted).
  r <- reactiveValues(print_results2 = "") # Create empty reactive string for printed results (raw).
  
  
  
  
  
  ################################################################################
  ### GENERATE LINEAR RIR + LINEAR ITCV RESULTS
  ### Generalized Robustness of Inference to Replacement (RIR)
  ### Impact Threshold for a Confounding Variable (ITCV)
  ################################################################################
  
  df_linear <- 
    eventReactive(input$results_pg_l, get_rir_itcv_results(input))
  
  
  ##############################################################################
  ### GENERATE PRINTED OUTPUT AND SHOW LINEAR RIR + LINEAR ITCV RESULTS
  ##############################################################################
  
  observeEvent(input$results_pg_l, {
    
    output$print_results1 <- 
      renderText({
        df_linear()$text # Combine text output lines
      })
    
    output$fig_results <- 
      renderPlot({
        df_linear()$plot  # Render the plot output 
      })
    
    output$print_results2 <- 
      renderText({
        paste(df_linear()$raw , collapse = "\n")  # Combine text output lines
      })
  })
  
  
  ##############################################################################
  ### GENERATE LINEAR RIR + LINEAR ITCV R CODE
  ##############################################################################
  
  user_est_l <- 
    eventReactive(input$results_pg_l, {
      paste0(
        "# install.packages('konfound')", "\n",
        "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
        "pkonfound(est_eff = ", input$est_effect_rir_ee, 
        ", std_err = ", input$std_error_rir_ee, 
        ", n_obs = ", input$n_obs_rir_ee, 
        ", n_covariates = ", input$n_covariates_rir_ee,
        ", index = ", "'", input$AnalysisL, "')"
      )
    })
  
  user_est_l_default <- 
    eventReactive(input$results_pg_l, {
      paste0(
        "# install.packages('konfound')", "\n",
        "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
        "# help(pkonfound)  # Check this help page for more details on default arguments",  "\n",
        "pkonfound(est_eff = ", input$est_effect_rir_ee, 
        ", std_err = ", input$std_error_rir_ee, 
        ", n_obs = ", input$n_obs_rir_ee, 
        ", n_covariates = ", input$n_covariates_rir_ee, ",", "\n\t",
        "sdx = NA, sdy = NA, R2 = NA, alpha = 0.05, tails = 2, nu = 0,",  "\n\t",
        "far_bound = 0, eff_thr = NA,", "\n\t",
        "upper_bound = NULL, lower_bound = NULL,",
        ", to_return = 'print', index = ", "'", input$AnalysisL, "')"
      )
    })
  
  user_est_l_ci <- 
    eventReactive(input$results_pg_l, {
      paste0(
        "# install.packages('konfound')", "\n",
        "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n",
        "pkonfound(lower_bound = ", input$lower_bnd_rir_ci,
        ", upper_bound = ", input$upper_bnd_rir_ci,
        ", n_obs = ", input$n_obs_rir_ci,
        ", n_covariates = ", input$n_covariates_rir_ci, ",", "\n\t",
        "sdx = NA, sdy = NA, R2 = NA, index = '", input$AnalysisL, "')"
      )
    })
  
  user_est_l_ci_default <- 
    eventReactive(input$results_pg_l, {
      paste0(
        "# install.packages('konfound')", "\n",
        "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n",
        "# help(pkonfound)  # Check this help page for more details on default arguments", "\n",
        "pkonfound(lower_bound = ", input$lower_bnd_rir_ci,
        ", upper_bound = ", input$upper_bnd_rir_ci,
        ", n_obs = ", input$n_obs_rir_ci,
        ", n_covariates = ", input$n_covariates_rir_ci, ",", "\n\t",
        "sdx = NA, sdy = NA, R2 = NA, alpha = 0.05, tails = 2, nu = 0,", "\n\t",
        "far_bound = 0, eff_thr = NA, to_return = 'print', index = '", input$AnalysisL, "')"
      )
    })
  
  
  ##############################################################################
  ### GENERATE LINEAR RIR + LINEAR ITCV STATA CODE
  ##############################################################################  
  
  s_user_est_l <- 
    eventReactive(input$results_pg_l, {
      paste0(
        "ssc install konfound", "\n", 
        "ssc install indeplist", "\n", 
        "ssc install moss", "\n", 
        "ssc install matsort", "\n", 
        "pkonfound ", 
        input$est_effect_rir_ee, " ", 
        input$std_error_rir_ee, " ", 
        input$n_obs_rir_ee, " ", 
        input$n_covariates_rir_ee, 
        ", model_type(0) indx(", input$AnalysisL, ")"
      )  
    })
  
  s_user_est_l_default <- 
    eventReactive(input$results_pg_l, {
      paste0(
        "ssc install konfound", "\n", 
        "ssc install indeplist", "\n", 
        "ssc install moss", "\n", 
        "ssc install matsort", "\n", 
        "* help pkonfound // Check this help page for more details on default arguments", "\n", 
        "pkonfound ", 
        input$est_effect_rir_ee, " ", 
        input$std_error_rir_ee, " ", 
        input$n_obs_rir_ee, " ", 
        input$n_covariates_rir_ee, 
        ", sig(0.05) nu(0) onetail(0) sdx(NA) sdy(NA) rs(NA) far_bound(0) eff_thr(NA) model_type(0) indx(", 
        input$AnalysisL, ")"
      )  
    })
  
  
  
  
  
  ##############################################################################
  ### GENERATE PSE RESULTS
  ### Preserve Standard Error (PSE)
  ##############################################################################
  
  df_pse <- 
    eventReactive(input$results_pg_pse, get_pse_results(input))
  
  
  ##############################################################################
  ### GENERATE PRINTED OUTPUT AND SHOW PSE RESULTS
  ##############################################################################
  
  observeEvent(input$results_pg_pse, {
    
    output$print_results1 <- 
      renderText({
        df_pse()$text  # Combine text output lines
      })
    
    # Render a dummy plot with text message
    output$fig_results <- 
      renderPlot({
        message <- df_pse()$plot_message
        plot.new()
        text(0.5, 0.5, message, cex = 1.5, col = "black", font = 1.8)    
      })
    
    output$print_results2 <- 
      renderText({
        paste(df_pse()$raw, collapse = "\n")  # Combine text output lines
      })
  })
  
  
  ##############################################################################
  ### GENERATE PSE R CODE
  ##############################################################################
  
  user_est_pse <- 
    eventReactive(input$results_pg_pse, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", 
        packageVersion("konfound"), "\n", 
        "pkonfound(est_eff = ", input$est_effect_pse_ee, 
        ", std_err = ", input$std_err_pse_ee, 
        ", n_obs = ", input$n_obs_pse_ee, 
        ", n_covariates = ", input$n_covariates_pse_ee, ",", "\n\t",
        "eff_thr = ", input$eff_thr_pse_ee, 
        ", sdx = ", input$sdx_pse_ee, 
        ", sdy = ", input$sdy_pse_ee, 
        ", R2 = ", input$R2_pse_ee, ",", "\n\t",
        "upper_bound = NULL, lower_bound = NULL,", "\n\t", 
        "index = 'PSE')"
      )
    })
  
  user_est_pse_default <- 
    eventReactive(input$results_pg_pse, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", 
        packageVersion("konfound"), "\n", 
        "# help(pkonfound)  # Check this help page for more details on default arguments \n", 
        "pkonfound(est_eff = ", input$est_effect_pse_ee, 
        ", std_err = ", input$std_err_pse_ee, 
        ", n_obs = ", input$n_obs_pse_ee, 
        ", n_covariates = ", input$n_covariates_pse_ee, ",", "\n\t", 
        "eff_thr = ", input$eff_thr_pse_ee, 
        ", sdx = ", input$sdx_pse_ee, 
        ", sdy = ", input$sdy_pse_ee, 
        ", R2 = ", input$R2_pse_ee, ",", "\n\t",
        "upper_bound = NULL, lower_bound = NULL,", "\n\t",
        "to_return = 'print', index = 'PSE')"
      )
    })
  
  user_est_pse_ci <- 
    eventReactive(input$results_pg_pse, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", 
        packageVersion("konfound"), "\n", 
        "pkonfound(lower_bound = ", input$lower_bnd_pse_ci, 
        ", upper_bound = ", input$upper_bnd_pse_ci, 
        ", n_obs = ", input$n_obs_pse_ci, 
        ", n_covariates = ", input$n_covariates_pse_ci, ",", "\n\t", 
        "eff_thr = ", input$eff_thr_pse_ci, 
        ", sdx = ", input$sdx_pse_ci, 
        ", sdy = ", input$sdy_pse_ci, 
        ", R2 = ", input$R2_pse_ci,
        ", index = 'PSE')"
      )
    })
  
  user_est_pse_ci_default <- 
    eventReactive(input$results_pg_pse, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", 
        packageVersion("konfound"), "\n", 
        "# help(pkonfound)  # Check this help page for more details on default arguments\n", 
        "pkonfound(lower_bound = ", input$lower_bnd_pse_ci, 
        ", upper_bound = ", input$upper_bnd_pse_ci, 
        ", n_obs = ", input$n_obs_pse_ci, 
        ", n_covariates = ", input$n_covariates_pse_ci, ",", "\n\t", 
        "eff_thr = ", input$eff_thr_pse_ci, 
        ", sdx = ", input$sdx_pse_ci, 
        ", sdy = ", input$sdy_pse_ci, 
        ", R2 = ", input$R2_pse_ci, ",", "\n\t", 
        "to_return = 'print', index = 'PSE')"
      )
    })
  
  
  ##############################################################################
  ### GENERATE PSE STATA CODE
  ##############################################################################
  
  s_user_est_pse <-
    eventReactive(input$results_pg_pse, {
      paste0(
        "ssc install konfound", "\n", 
        "pkonfound ", 
        input$est_effect_pse_ee, " ", 
        input$std_err_pse_ee, " ", 
        input$n_obs_pse_ee, " ", 
        input$n_covariates_pse_ee, " ", 
        input$eff_thr_pse_ee, " ", 
        input$sdx_pse_ee, " ", 
        input$sdy_pse_ee, " ", 
        input$R2_pse_ee, ", eff_thr(", 
        input$eff_thr_pse_ee, ") indx(PSE)"
      )
    })
  
  s_user_est_pse_default <- 
    eventReactive(input$results_pg_pse, {
      paste0(
        "ssc install konfound", "\n", 
        "* help pkonfound // Check this help page for more details on default arguments", "\n",
        "pkonfound ", 
        input$est_effect_pse_ee, " ", 
        input$std_err_pse_ee, " ", 
        input$n_obs_pse_ee, " ", 
        input$n_covariates_pse_ee, " ", 
        input$eff_thr_pse_ee, " ", 
        input$sdx_pse_ee, " ", 
        input$sdy_pse_ee, " ", 
        input$R2_pse_ee, ", eff_thr(", 
        input$eff_thr_pse_ee, ") indx(PSE)"
      )
    })
  
  
  
  
  
  ################################################################################
  ### GENERATE COP RESULTS
  ### Coefficient of Proportionality (COP)
  ################################################################################
  
  df_cop <- 
    eventReactive(input$results_pg_cop, get_cop_results(input))
  
  
  ##############################################################################
  ### GENERATE PRINTED OUTPUT AND SHOW COP RESULTS
  ##############################################################################
  
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
  
  
  ##############################################################################
  ### GENERATE COP R CODE
  ##############################################################################
  
  user_est_cop <- 
    eventReactive(input$results_pg_cop, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", 
        packageVersion("konfound"), "\n", 
        "pkonfound(est_eff = ", input$est_effect_cop_ee, 
        ", std_err = ", input$std_err_cop_ee, 
        ", n_obs = ", input$n_obs_cop_ee, 
        ", n_covariates = ", input$n_covariates_cop_ee, ",", "\n\t",
        "sdx = ", input$sdx_cop_ee, 
        ", sdy = ", input$sdy_cop_ee, 
        ", R2 = ", input$R2_cop_ee, 
        ", eff_thr = ", input$eff_thr_cop_ee, 
        ", FR2max = ", input$FR2max_cop_ee, ",", "\n\t",
        "upper_bound = NULL, lower_bound = NULL,", "\n\t", 
        "index = 'COP')"
      )
    })
  
  user_est_cop_default <- 
    eventReactive(input$results_pg_cop, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
        "# help(pkonfound)  # Check this help page for more details on default arguments \n", 
        "pkonfound(est_eff = ", input$est_effect_cop_ee, 
        ", std_err = ", input$std_err_cop_ee, 
        ", n_obs = ", input$n_obs_cop_ee, 
        ", n_covariates = ", input$n_covariates_cop_ee, ",", "\n\t",
        "sdx = ", input$sdx_cop_ee, 
        ", sdy = ", input$sdy_cop_ee, 
        ", R2 = ", input$R2_cop_ee, 
        ", eff_thr = ", input$eff_thr_cop_ee, 
        ", FR2max = ", input$FR2max_cop_ee, ",", "\n\t",
        "alpha = 0.05, tails = 2, ",
        "upper_bound = NULL, lower_bound = NULL,", "\n\t", 
        "index = 'COP')"
      )
    })
  
  user_est_cop_ci <- 
    eventReactive(input$results_pg_cop, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
        "pkonfound(lower_bound = ", input$lower_bnd_cop_ci, 
        ", upper_bound = ", input$upper_bnd_cop_ci, 
        ", n_obs = ", input$n_obs_cop_ci, 
        ", n_covariates = ", input$n_covariates_cop_ci, ",", "\n\t",
        "sdx = ", input$sdx_cop_ci, 
        ", sdy = ", input$sdy_cop_ci, 
        ", R2 = ", input$R2_cop_ci, 
        ", eff_thr = ", input$eff_thr_cop_ci, 
        ", FR2max = ", input$FR2max_cop_ci,
        ", index = 'COP')"
      )
    })
  
  user_est_cop_ci_default <- 
    eventReactive(input$results_pg_cop, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
        "# help(pkonfound)  # Check this help page for more details on default arguments\n", 
        "pkonfound(lower_bound = ", input$lower_bnd_cop_ci, 
        ", upper_bound = ", input$upper_bnd_cop_ci, 
        ", n_obs = ", input$n_obs_cop_ci, 
        ", n_covariates = ", input$n_covariates_cop_ci, ",", "\n\t",
        "sdx= ", input$sdx_cop_ci, 
        ", sdy = ", input$sdy_cop_ci, 
        ", R2 = ", input$R2_cop_ci, 
        ", eff_thr = ", input$eff_thr_cop_ci, 
        ", FR2max = ", input$FR2max_cop_ci, ",", "\n\t",
        "alpha = 0.05, tails = 2, index = 'COP')"
      )
    })
  
  
  ##############################################################################
  ### GENERATE COP STATA CODE
  ##############################################################################  
  
  s_user_est_cop <- 
    eventReactive(input$results_pg_cop, {
      paste0(
        "ssc install konfound", "\n", 
        "pkonfound ", 
        input$est_effect_cop_ee, " ", 
        input$std_err_cop_ee, " ", 
        input$n_obs_cop_ee, " ", 
        input$n_covariates_cop_ee, " ", 
        input$sdx_cop_ee, " ", 
        input$sdy_cop_ee, " ", 
        input$R2_cop_ee, 
        ", eff_thr(", input$eff_thr_cop_ee, ") fr2max(", input$FR2max_cop_ee, 
        ") indx(COP)"
      )
    })
  
  s_user_est_cop_default <- 
    eventReactive(input$results_pg_cop, {
      paste0(
        "ssc install konfound", "\n", 
        "* help pkonfound // Check this help page for more details on default arguments", "\n", 
        "pkonfound ", 
        input$est_effect_cop_ee, " ", 
        input$std_err_cop_ee, " ", 
        input$n_obs_cop_ee, " ", 
        input$n_covariates_cop_ee, " ", 
        input$sdx_cop_ee, " ", 
        input$sdy_cop_ee, " ", 
        input$R2_cop_ee, 
        ", eff_thr(", input$eff_thr_cop_ee, ") fr2max(", input$FR2max_cop_ee, 
        ") sig(0.05) onetail(0) indx(COP)"
      )
    })
  
  
  
  
  
  ################################################################################
  ### GENERATE LOG RESULTS
  ### Coefficient of Proportionality (COP)
  ################################################################################
  
  df_log <- 
    eventReactive(input$results_pg_di, get_log_results(input))
  
  
  ##############################################################################
  ### GENERATE PRINTED OUTPUT AND SHOW LOG RESULTS
  ##############################################################################
  
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
  
  
  ##############################################################################
  ### GENERATE LOG R CODE
  ##############################################################################
  
  user_est_di <- 
    eventReactive(input$results_pg_di, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", 
        packageVersion("konfound"), "\n", 
        "pkonfound(est_eff = ", input$est_effect_log_ee, 
        ", std_err = ", input$std_error_log_ee, 
        ", n_obs = ", input$n_obs_log_ee, 
        ", n_covariates = ", input$n_covariates_log_ee, ",", "\n\t",
        "n_treat = ", input$n_trm_log_ee,
        "upper_bound = NULL, lower_bound = NULL,", "\n\t", 
        "model_type = 'logistic')"
      )
    })
  
  user_est_di_default <- 
    eventReactive(input$results_pg_di, {
    paste0(
      "# install.packages('konfound')", "\n", 
      "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
      "# help(pkonfound)  # Check this help page for more details on default arguments \n", 
      "pkonfound(est_eff = ", input$est_effect_log_ee, 
      ", std_err = ", input$std_error_log_ee, 
      ", n_obs = ", input$n_obs_log_ee, 
      ", n_covariates = ", input$n_covariates_log_ee, ",", "\n\t",
      "n_treat = ", input$n_trm_log_ee, 
      ", alpha = 0.05, tails = 2, nu = 0, switch_trm = TRUE, replace = 'control',", "\n\t",
      "upper_bound = NULL, lower_bound = NULL,", "\n\t", 
      "to_return = 'print', model_type = 'logistic')"
    )
  })
  
  user_est_di_ci <- 
    eventReactive(input$results_pg_di, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
        "pkonfound(lower_bound = ", input$lower_bnd_log_ci,
        ", upper_bound = ", input$upper_bnd_log_ci,
        ", n_obs = ", input$n_obs_log_ci,
        ", n_covariates = ",        input$n_covariates_log_ci, ",", "\n\t",
        "n_treat = ",               input$n_trm_log_ci,
        ", model_type = 'logistic')"
      )
    })
  
  user_est_di_ci_default <- 
    eventReactive(input$results_pg_di, {
      paste0(
        "# install.packages('konfound')", "\n", 
        "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
        "# help(pkonfound)  # Check this help page for more details on default arguments", "\n", 
        "pkonfound(lower_bound = ", input$lower_bnd_log_ci,
        ", upper_bound = ", input$upper_bnd_log_ci,
        ", n_obs = ", input$n_obs_log_ci,
        ", n_covariates = ", input$n_covariates_log_ci, ",", "\n\t",
        "n_treat = ", input$n_trm_log_ci,
        ", alpha = 0.05, tails = 2, nu = 0, switch_trm = TRUE, replace = 'control',", "\n\t",
        "to_return = 'print', model_type = 'logistic')"
      )
    })
  
  
  ##############################################################################
  ### GENERATE LOG STATA CODE
  ##############################################################################  
  
  s_user_est_di <- 
    eventReactive(input$results_pg_di, {
      paste0(
        "ssc install konfound", "\n", 
        "pkonfound ", 
        input$est_effect_log_ee, " ", 
        input$std_error_log_ee, " ", 
        input$n_obs_log_ee, " ", 
        input$n_covariates_log_ee, " ", 
        input$n_trm_log_ee, 
        ", model_type(1)"
      ) 
    })
  
  s_user_est_di_default <- 
    eventReactive(input$results_pg_di, {
      paste0(
        "ssc install konfound", "\n", 
        "* help pkonfound // Check this help page for more details on default arguments", "\n", 
        "pkonfound ", 
        input$est_effect_log_ee, " ", 
        input$std_error_log_ee, " ", 
        input$n_obs_log_ee, " ", 
        input$n_covariates_log_ee, " ", 
        input$n_trm_log_ee, 
        ", sig(0.05) onetail(0) switch_trm(1) replace(1) model_type(1)"
      ) 
    })
  
  
  
  
  
  ################################################################################
  ### GENERATE 2x2 RESULTS
  ################################################################################
  
  # If user presses the results button for 2x2 tables, show the 2x2 tables
  df_twobytwo <- 
    eventReactive(input$results_pg_2x2, {
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
          
          "This is equivalent to replacing ", total_RIR, " (", sprintf("%.3f", RIR_perc), "%) ",
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
          if (!is.na(fisher_ob)) paste0("For the User-entered Table, the estimated odds ratio is ", 
                                        sprintf("%.3f", fisher_ob), 
                                        ", with p-value of ", sprintf("%.3f", p_start),
                                        ":<br><br>") else "",
          
          "<strong><u>Implied Table:</u></strong><br>",
          knitr::kable(raw_calc$starting_table, format = "html", align = "c",
                       table.attr = "style='width:100%;'",
                       col.names = c("Group", "Failures", "Successes", "Success Rate")),
          "<br>",
          
          if (!is.na(fisher_final)) paste0("For the Transfer Table, the estimated odds ratio is ", 
                                           sprintf("%.3f", fisher_final), 
                                           ", with p-value of ", 
                                           sprintf("%.3f", p_final),":<br><br>") else "",
          
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
      
      
      
      ##########################################################################
      ### Return both text and plot separately
      ##########################################################################
      
      list(text = twobytwo_output, 
           plot_message = "No graphical output for this analysis.",
           raw = twobytwo_output_raw)
    })
  
  
  
  ##############################################################################
  ### GENERATE PRINTED OUTPUT AND SHOW 2X2 RESULTS
  ##############################################################################
  
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
  
  
  ################################################################################
  ### GENERATE 2X2 R CODE
  ################################################################################
  
  user_est_2x2 <- eventReactive(input$results_pg_2x2, {
    paste0(
      "# install.packages('konfound')", "\n", 
      "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
      "pkonfound(a = ", input$ctrl_fail, 
      ", b = ", input$ctrl_success, 
      ", c = ", input$treat_fail, 
      ", d = ", input$treat_success, 
      ")"
    )
  })
  
  user_est_2x2_default <- eventReactive(input$results_pg_2x2, {
    paste0(
      "# install.packages('konfound')", "\n", 
      "library(konfound)  # konfound R package version: ", packageVersion("konfound"), "\n", 
      "# help(pkonfound)  # Check this help page for more details on default arguments \n", 
      "pkonfound(a = ", input$ctrl_fail, 
      ", b = ", input$ctrl_success, 
      ", c = ", input$treat_fail, 
      ", d = ", input$treat_success, ",", "\n\t",
      "alpha = 0.05, switch_trm = TRUE, replace = 'control', test = 'fisher',", "\n\t",
      "to_return = 'print')"
    )
  })
  
  
  ################################################################################
  ### GENERATE 2X2 STATA CODE
  ################################################################################
  
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
  
  
  
  
  
  ################################################################################
  ### CHOOSE CORRECT CODE TO DISPLAY
  ################################################################################
  
  ### Conditional statement to display the correct R code based on model type
  
  select_r_code <- 
    reactive({
      req(isTruthy(input$Outcome),
          isTruthy(input$Data) || isTruthy(input$DataL)) # need or will get error: argument is of length zero
      
      if(isTruthy(input$Outcome == "Dichotomous")){
        if(isTruthy(input$DataD == "2x2 table")){
          r_code <- user_est_2x2()
        }
        if(isTruthy(input$DataD == "Logistic model")){
          if (input$Uncertainty_log == "EstEff") {
            r_code <- user_est_di()
          } else {
            # CI-based default
            r_code <- user_est_di_ci()
          }
        }
        if(is.null(input$DataD)){
          r_code <- print("")
        }
      }
      
      if(isTruthy(input$Outcome == "Continuous")){
        if(isTruthy(input$DataL == "Linear model")){
          r_code <- print("")
          if(isTruthy(input$AnalysisL == "IT")){
            if (input$Uncertainty_RIR == "EstEff") {
              r_code <- user_est_l()
            } else {
              # CI-based default
              r_code <- user_est_l_ci()
            }          
          }
          if(isTruthy(input$AnalysisL == "RIR")){
            if (input$Uncertainty_RIR == "EstEff") {
              r_code <- user_est_l()
            } else {
              # CI-based default
              r_code <- user_est_l_ci()
            }
          }
          if(isTruthy(input$AnalysisL == "COP")){
            if (input$Uncertainty_COP == "EstEff") {
              r_code <- user_est_cop()
            } else {
              # CI-based default
              r_code <- user_est_cop_ci()
            }
          }
          if(isTruthy(input$AnalysisL == "PSE")){
            if (input$Uncertainty_PSE == "EstEff") {
              r_code <- user_est_pse()
            } else {
              # CI-based default
              r_code <- user_est_pse_ci()
            }
          }
        }
      }
      
      r_code
    })
  
  
  select_r_code_default <- 
    reactive({
      req(isTruthy(input$Outcome),
          isTruthy(input$DataD) || isTruthy(input$DataL))
      
      # Dichotomous outcome
      if (isTruthy(input$Outcome == "Dichotomous")) {
        if (isTruthy(input$DataD == "2x2 table")) {
          r_code_def <- user_est_2x2_default()
        }
        if (isTruthy(input$DataD == "Logistic model")) {
          if (input$Uncertainty_log == "EstEff") {
            r_code_def <- user_est_di_default()
          } else {
            # CI-based default
            r_code_def <- user_est_di_ci_default()
          }
        }
      }
      
      # Continuous outcome
      if (isTruthy(input$Outcome == "Continuous")) {
        if (isTruthy(input$DataL == "Linear model")) {
          
          if (isTruthy(input$AnalysisL == "IT") || isTruthy(input$AnalysisL == "RIR")) {
            if (input$Uncertainty_RIR == "EstEff") {
              r_code_def <- user_est_l_default()
            } else {
              # CI-based default
              r_code_def <- user_est_l_ci_default()
            }
          }
          if (isTruthy(input$AnalysisL == "COP")) {
            if (input$Uncertainty_COP == "EstEff") {
              r_code_def <- user_est_cop_default()
            } else {
              # CI-based default
              r_code_def <- user_est_cop_ci_default()
            }          
          }
          if (isTruthy(input$AnalysisL == "PSE")) {
            if (input$Uncertainty_PSE == "EstEff") {
              r_code_def <- user_est_pse_default()
            } else {
              # CI-based default
              r_code_def <- user_est_pse_ci_default()
            }
          }
        }
      }
      
      r_code_def
    })
  
  
  ### Conditional statement to display the correct Stata code based on model type
  select_stata_code <- 
    reactive({
      req(input$Outcome) #need or will get error: argument is of length zero
      
      if(input$Outcome == "Dichotomous"){
        if(input$DataD == "2x2 table"){
          stata_code <- s_user_est_2x2()
        }
        if(input$DataD == "Logistic model"){
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
  
  
  select_stata_code_default <- 
    reactive({
      req(input$Outcome) #need or will get error: argument is of length zero
      
      if(input$Outcome == "Dichotomous"){
        if(input$DataD == "2x2 table"){
          stata_code_def <- s_user_est_2x2_default()
        }
        if(input$DataD == "Logistic model"){
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
  
  
  
  
  
  ################################################################################
  ### CREATE BUTTONS FOR COPYING CODE
  ################################################################################
  
  # Render R code in UI.R to display for user
  output$r_code_print <- 
    renderText({
      select_r_code()
    })
  
  # Add clipboard button
  output$clip <- 
    renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "Copy R code",
        clipText = select_r_code(),
        icon = icon("clipboard"))
    })
  
  # Render Default R code in UI.R to display for user
  output$r_code_print_default <- 
    renderText({
      select_r_code_default()
    })
  
  # And a matching clipboard button
  output$clip_r_default <- 
    renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "Copy advanced R code",
        clipText = select_r_code_default(),
        icon = icon("clipboard")
      )
    })
  
  
  
  # Render Stata code in UI.R to display for user
  output$stata_code_print <- 
    renderText({
      select_stata_code()
    })
  
  
  # Add clipboard buttons
  output$clip_stata <- 
    renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "Copy Stata code",
        clipText = select_stata_code(),
        icon = icon("clipboard"))
    })
  
  output$stata_code_print_default <- 
    renderText({
      select_stata_code_default()
    })
  
  # Add clipboard buttons
  output$clip_stata_default <- 
    renderUI({
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
