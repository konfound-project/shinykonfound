library(shinytest2)
#setwd("/Users/sarahnarvaiz/Dropbox/shinykonfound/newdev_v2")

#FUNCTION TO EXTRACT OUTPUT ID BUT DOESNT WORK
get_output_id <- function(print_results_output){
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  return(final_id)
}

#######################################
##TESTING INPUT VALIDATION 2X2 TABLE##
#######################################
test_that("{shinytest2} recording: newdev_v2", {
  app <- AppDriver$new(name = "newdev_v2", height = 1061, width = 1618)
  
  #set input for dichotomous outcome
  app$set_inputs(Outcome = "Dichotomous")
  
  #set input for type of data
  app$set_inputs(Data = "2x2 table")
  
  #set input for RIR analysis
  app$set_inputs(Analysis = "RIR")
  
  #set input for 0 for control condition: result failure
  app$set_inputs(ctrl_fail = 0)
  app$set_inputs(ctrl_success = 12)
  app$set_inputs(treat_fail = 12)
  app$set_inputs(treat_success = 17)
  
  #run shiny app the display results
  app$click("results_pg_2x2")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Control Condition: Result Failure needs to be greater than zero" )
  
  ####################################################
  #set input for 0 for control condition: result failure
  #####################################################
  app$set_inputs(ctrl_fail = 18)
  app$set_inputs(ctrl_success = 0)
  app$set_inputs(treat_fail = 12)
  app$set_inputs(treat_success = 17)
  
  #run shiny app the display results
  app$click("results_pg_2x2")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Control Condition: Result Success needs to be greater than zero" )
  
  
  ####################################################
  #set input for 0 for treatment condition: result failure
  #####################################################
  app$set_inputs(ctrl_fail = 18)
  app$set_inputs(ctrl_success = 12)
  app$set_inputs(treat_fail = 0)
  app$set_inputs(treat_success = 17)
  
  #run shiny app the display results
  app$click("results_pg_2x2")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Treatment Condition: Result Failure needs to be greater than zero" )
  
  ####################################################
  #set input for 0 for treatment condition: result failure
  #####################################################
  app$set_inputs(ctrl_fail = 18)
  app$set_inputs(ctrl_success = 12)
  app$set_inputs(treat_fail = 12)
  app$set_inputs(treat_success = 0)
  
  #run shiny app the display results
  app$click("results_pg_2x2")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Treatment Condition: Result Sucesss needs to be greater than zero" )
  

})

#######################################
##TESTING INPUT VALIDATION LOGISTIC##
#######################################

test_that("{shinytest2} recording: logistic-model-val-test", {
  app <- AppDriver$new(name = "logistic-model-val-test", height = 1061, width = 1400)
  
  #set input for dichotomous outcome
  app$set_inputs(Outcome = "Dichotomous")
  
  #set input for type of data
  app$set_inputs(Data = "Logistic model")
  
  #set input for RIR analysis
  app$set_inputs(Analysis = "RIR")
  
  #########################################
  #set inputs with negative standard error#
  #########################################
  
  app$set_inputs(unstd_beta_nl = -0.2)
  app$set_inputs(std_error_nl = -1)
  app$set_inputs(n_obs_nl = 20888)
  app$set_inputs(n_covariates_nl = 3)
  app$set_inputs(n_trm_nl = 17888)
  
  #run shiny app the display results
  app$click("results_pg_di")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Standard error needs to be greater than zero." )
  
  ############################################
  #set inputs with observations == covariates#
  ############################################
  app$set_inputs(unstd_beta_nl = -0.2)
  app$set_inputs(std_error_nl = 0.103)
  app$set_inputs(n_obs_nl = 20888)
  app$set_inputs(n_covariates_nl = 20888)
  app$set_inputs(n_trm_nl = 17888)
  
  #run shiny app the display results
  app$click("results_pg_di")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It." )
})


#######################################
##TESTING INPUT VALIDATION LINEAR ITCV##
#######################################

test_that("{shinytest2} recording: linear-itcv-val-test", {
  app <- AppDriver$new(name = "linear-itcv-val-test", height = 1061, width = 1400)
  app$set_inputs(Outcome = "Continuous")
  
  #set input for type of data
  app$set_inputs(DataL = "Linear model")
  
  #set input for RIR analysis
  app$set_inputs(AnalysisL = "IT")
  
  #########################################
  #set inputs with negative standard error#
  #########################################
  app$set_inputs(unstd_beta = 2)
  app$set_inputs(std_error = -1)
  app$set_inputs(n_obs = 100)
  app$set_inputs(n_covariates = 3)
  
  #run shiny app the display results
  app$click("results_pg_l")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Standard error needs to be greater than zero." )
  
  ############################################
  #set inputs with observations == covariates#
  ############################################
  app$set_inputs(unstd_beta = 2)
  app$set_inputs(std_error = 0.4)
  app$set_inputs(n_obs = 100)
  app$set_inputs(n_covariates = 100)
  
  #run shiny app the display results
  app$click("results_pg_l")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It." )
  
})


#######################################
##TESTING INPUT VALIDATION LINEAR RIR##
#######################################

test_that("{shinytest2} recording: linear-rir-val-test", {
  app <- AppDriver$new(name = "linear-rir-val-test", height = 1061, width = 1400)
  app$set_inputs(Outcome = "Continuous")
  
  #set input for type of data
  app$set_inputs(DataL = "Linear model")
  
  #set input for RIR analysis
  app$set_inputs(AnalysisL = "RIR")
  
  #########################################
  #set inputs with negative standard error#
  #########################################
  app$set_inputs(unstd_beta = 2)
  app$set_inputs(std_error = -1)
  app$set_inputs(n_obs = 100)
  app$set_inputs(n_covariates = 3)
  
  #run shiny app the display results
  app$click("results_pg_l")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Standard error needs to be greater than zero." )
  
  ############################################
  #set inputs with observations == covariates#
  ############################################
  app$set_inputs(unstd_beta = 2)
  app$set_inputs(std_error = 0.4)
  app$set_inputs(n_obs = 100)
  app$set_inputs(n_covariates = 100)
  
  #run shiny app the display results
  app$click("results_pg_l")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It." )
  
})


#######################################
##TESTING INPUT VALIDATION LINEAR PSE##
#######################################

test_that("{shinytest2} recording: linear-pse-val-test", {
  app <- AppDriver$new(name = "linear-pse-val-test", height = 1061, width = 1400)
  app$set_inputs(Outcome = "Continuous")
  
  #set input for type of data
  app$set_inputs(DataL = "Linear model")
  
  #set input for RIR analysis
  app$set_inputs(AnalysisL = "PSE")
  
  #########################################
  #set inputs with negative standard error#
  #########################################
  app$set_inputs(unstd_beta_pse = 0.5)
  app$set_inputs(std_err_pse = -1)
  app$set_inputs(n_obs_pse = 6174)
  app$set_inputs(sdx_pse = 0.22)
  app$set_inputs(sdy_pse = 1)
  app$set_inputs(R2_pse = 0.3)
  
  #run shiny app the display results
  app$click("results_pg_pse")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Standard error needs to be greater than zero." )
  
  #########################################
  ############# SET SDX AS -2 ##############
  #########################################
  app$set_inputs(unstd_beta_pse = 0.5)
  app$set_inputs(std_err_pse = 0.056)
  app$set_inputs(n_obs_pse = 6174)
  app$set_inputs(sdx_pse = -2)
  app$set_inputs(sdy_pse = 1)
  app$set_inputs(R2_pse = 0.3)
  
  #run shiny app the display results
  app$click("results_pg_pse")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Standard deviation of x needs to be greater than zero." )
  
  #########################################
  ############# SET SDY AS -2 ##############
  #########################################
  app$set_inputs(unstd_beta_pse = 0.125)
  app$set_inputs(std_err_pse = 0.05049)
  app$set_inputs(n_obs_pse = 6174)
  app$set_inputs(sdx_pse = 0.991)
  app$set_inputs(sdy_pse = -2)
  app$set_inputs(R2_pse = 0.3)
  
  #run shiny app the display results
  app$click("results_pg_pse")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Standard deviation of y needs to be greater than zero." )
  
  #########################################
  ############# SET R2 AS 0 ##############
  #########################################
  app$set_inputs(unstd_beta_pse = 0.125)
  app$set_inputs(std_err_pse = 0.05049)
  app$set_inputs(n_obs_pse = 6174)
  app$set_inputs(sdx_pse = 0.22)
  app$set_inputs(sdy_pse = .711)
  app$set_inputs(R2_pse = 0)
  
  #run shiny app the display results
  app$click("results_pg_pse")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! R2 needs to be greater than zero." )
  
  #########################################
  ############# SET R2 AS 1 ##############
  #########################################
  app$set_inputs(R2_pse = 1)
  
  #run shiny app the display results
  app$click("results_pg_pse")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! R2 needs to be less than one" )
  
  #########################################
  ############# UNDEFINED RXZ ##############
  #########################################
  
  #1-((sdy^2/sdx^2)*(1-R2)/(df*SeB^2))>0
  
  app$set_inputs(unstd_beta_pse = 0.125)
  app$set_inputs(std_err_pse = 0.050)
  app$set_inputs(n_obs_cpse = 6174)
  app$set_inputs(sdx_pse = 0.217)
  app$set_inputs(sdy_pse = 0.991)
  app$set_inputs(R2_pse = 0.251)
  
  #run shiny app the display results
  app$click("results_pg_pse")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Entered values produced Rxz^2 <0, consider adding more significant digits to your entered values")
  
  
})


#######################################
##TESTING INPUT VALIDATION LINEAR COP##
#######################################

test_that("{shinytest2} recording: linear-cop-val-test", {
  app <- AppDriver$new(name = "linear-cop-val-test", height = 1061, width = 1400)
  app$set_inputs(Outcome = "Continuous")
  
  #set input for type of data
  app$set_inputs(DataL = "Linear model")
  
  #set input for RIR analysis
  app$set_inputs(AnalysisL = "COP")
  
  #########################################
  #set inputs with negative standard error#
  #########################################
  app$set_inputs(unstd_beta_cop = -0.125)
  app$set_inputs(std_err_cop = -1)
  app$set_inputs(n_obs_cop = 6265)
  app$set_inputs(n_covariates_cop = 7)
  app$set_inputs(sdx_cop = 0.217)
  app$set_inputs(sdy_cop = 0.991)
  app$set_inputs(R2_cop = 0.251)
  app$set_inputs(eff_thr_cop = 0)
  app$set_inputs(FR2max_cop = 0.61)
  
  #run shiny app the display results
  app$click("results_pg_cop")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Standard error needs to be greater than zero." )
  
  #########################################
  ############# SET SDX AS -2 ##############
  #########################################
  app$set_inputs(unstd_beta_cop = 0.5)
  app$set_inputs(std_err_cop = 0.056)
  app$set_inputs(n_obs_cop = 6174)
  app$set_inputs(n_covariates_cop = 7)
  app$set_inputs(sdx_cop = -2)
  app$set_inputs(sdy_cop = 1)
  app$set_inputs(R2_cop = 0.251)
  app$set_inputs(eff_thr_cop = 0)
  app$set_inputs(FR2max_cop = 0.61)
  
  #run shiny app the display results
  app$click("results_pg_cop")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Standard deviation of x needs to be greater than zero." )
  
  #########################################
  ############# SET SDY AS -2 ##############
  #########################################
  app$set_inputs(unstd_beta_cop = 0.5)
  app$set_inputs(std_err_cop = 0.056)
  app$set_inputs(n_obs_cop = 6174)
  app$set_inputs(n_covariates_cop = 7)
  app$set_inputs(sdx_cop = 1)
  app$set_inputs(sdy_cop = -2)
  app$set_inputs(R2_cop = 0.251)
  app$set_inputs(eff_thr_cop = 0)
  app$set_inputs(FR2max_cop = 0.61)
  
  #run shiny app the display results
  app$click("results_pg_cop")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Standard deviation of y needs to be greater than zero." )
  
  #########################################
  ############# SET FR2Max AS 1 ##############
  #########################################
  app$set_inputs(unstd_beta_cop = 0.5)
  app$set_inputs(std_err_cop = 0.056)
  app$set_inputs(n_obs_cop = 6174)
  app$set_inputs(n_covariates_cop = 7)
  app$set_inputs(sdx_cop = 0.217)
  app$set_inputs(sdy_cop = 0.991)
  app$set_inputs(R2_cop = 0.251)
  app$set_inputs(eff_thr_cop = 0)
  app$set_inputs(FR2max_cop = 1)
  
  #run shiny app the display results
  app$click("results_pg_cop")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! R2 Max needs to be less than 1.")
  
  #########################################
  ############# SET FR2Max < R2 ##############
  #########################################
  app$set_inputs(unstd_beta_cop = 0.5)
  app$set_inputs(std_err_cop = 0.056)
  app$set_inputs(n_obs_cop = 6174)
  app$set_inputs(n_covariates_cop = 7)
  app$set_inputs(sdx_cop = 0.217)
  app$set_inputs(sdy_cop = 0.991)
  app$set_inputs(R2_cop = 0.251)
  app$set_inputs(eff_thr_cop = 0)
  app$set_inputs(FR2max_cop = 0.1)
  
  #run shiny app the display results
  app$click("results_pg_cop")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! R2 Max needs to be greater than R2." )
  
  #########################################
  ############# UNDEFINED RXZ ##############
  #########################################
  
  #1-((sdy^2/sdx^2)*(1-R2)/(df*SeB^2))>0
  
  app$set_inputs(unstd_beta_cop = 0.125)
  app$set_inputs(std_err_cop = 0.050)
  app$set_inputs(n_obs_cop = 6174)
  app$set_inputs(n_covariates_cop = 7)
  app$set_inputs(sdx_cop = 0.217)
  app$set_inputs(sdy_cop = 0.991)
  app$set_inputs(R2_cop = 0.251)
  app$set_inputs(eff_thr_cop = 0)
  app$set_inputs(FR2max_cop = 0.61)
  
  #run shiny app the display results
  app$click("results_pg_cop")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg$message[[1]], "Did not run! Entered values produced Rxz^2 <0, consider adding more significant digits to your entered values")
  
  
})


#######################################
##BASIC CHECK THE APP WORKS USING 2x2#
#######################################

# this test was requested by Josh to make sure the app works outside of validation cases :-)
test_that("{shinytest2} recording: 2x2table-general-test", {
  app <- AppDriver$new(name = "2x2table-general-test", height = 1061, width = 1618)
  
  #set input for dichotomous outcome
  app$set_inputs(Outcome = "Dichotomous")
  
  #set input for type of data
  app$set_inputs(Data = "2x2 table")
  
  #set input for RIR analysis
  app$set_inputs(Analysis = "RIR")
  
  #set input for 0 for control condition: result failure
  app$set_inputs(ctrl_fail = 18)
  app$set_inputs(ctrl_success = 12)
  app$set_inputs(treat_fail = 12)
  app$set_inputs(treat_success = 17)
  
  #run shiny app the display results
  app$click("results_pg_2x2")
  
  #strip output id to parse and compare to find output message. I made this as a function called get_output_id but test fails when using function. But this works
  validation_msg <- app$get_value(output = 'print_results')
  example <- validation_msg$html[[1]]
  res <- stringr::str_match(example, "id=(.*?)class")
  final_id <- stringi::stri_extract_all_regex(res[,1], '(?<=").*?(?=")')
  output_msg <- app$get_value(output = final_id[[1]])
  
  #check to see if the output message equals the validation error message
  expect_equal(output_msg, "This function calculates the number of cases that would have to be replacedwith no effect cases (RIR) to invalidate an inference made about the associationTo sustain an inference, you would need to replace 8 treatment failurecases for which the probability of failure in the control group applies (RIR = 8). This is equivalent to transferring 3 cases from treatment failure to treatment success (Fragility)." )
})


##################################
#### VALIDATION TESTING RULES ####
#################################

# All procedures (if these inputs are asked)
  # standard error > 0
  # observations > covariates + 3
  # sdx > 0
  # sdy > 0

# COP
  # R2 < FR2Max < 1 (FR2Max less than 1 but greater than R2)
  # 1-((sdy^2/sdx^2)*(1-R2)/(df*SeB^2))>0 

# PSE
  # 1 < R2 > 0 (R2 greater than 0 but less than 1)
  # 1-((sdy^2/sdx^2)*(1-R2)/(df*SeB^2))>0? 

# 2x2
  # all cells in 2x2 table need to be greater than 0


##################################
#HELPFUL TESTING NOTES/RESOURCES#
#################################

# https://www.youtube.com/watch?v=Gucwz865aqQ
# setwd("/Users/sarahnarvaiz/Dropbox/shinykonfound/newdev_v2")
# app <- AppDriver$new(variant = platform_variant(), name = "newdev_v2", height = 1061, width = 1618)
# app$view()
# app$get_values()

# NEW VALIDATION RULE FOR COP AND PSE
# should work .07 rxz
#pkonfound(est_eff = .125,  std_err = .05049, n_obs = 6174,  n_covariates = 7, sdx = .217,  sdy = .991, R2 = .251, eff_thr = 0, FR2max = .61, index = "COP", to_return = "raw_output")

#problem r2x less than 0and rxz undefined
#pkonfound(est_eff = .125,  std_err = .050, n_obs = 6174,  n_covariates = 7, sdx = .217,  sdy = .991, R2 = .251, eff_thr = 0, FR2max = .61, index = "COP", to_return = "raw_output")

