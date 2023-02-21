#New development - Conditional panel approach
#ui

library(shiny)
library(rclipboard)
library(tippy)

shinyUI(fluidPage(
  tags$head(tags$script(src="script.js"),
            tags$style(HTML(".tippy-tooltip {
                      font-size: 15px;
    }
                    "))),
  rclipboardSetup(),
  
  navbarPage(title = "KonFound-It! Quantify the Robustness of Causal Inferences",
             id = "mainpage",
             
             tabPanel(title = "Home",
                      h4(strong("How do I use KonFound-It?")),
                      p(style= "text-align: justify; font-size = 14px",
                        "SOME LANGUAGE ABOUT THE DECISION MAP. SOME LANGUAGE ABOUT NAVIGATING THE MENU TO CHOOSE THE CORRECT MODEL AND ANALYSIS TECHNIQUE."),
                      splitLayout(
                        wellPanel(p("Step 1"),
                                  radioButtons("Outcome", "What is your type of outcome?",
                                               choices = c("Dichotomous", "Linear"),
                                               selected = character(0))),#no default radio button selected
                        wellPanel(p("Step 2"),
                                  conditionalPanel(condition = "input.Outcome == 'Dichotomous'",
                                                   style = "display: none;",
                                                   radioButtons("Data", "Where is your data coming from?",
                                                                choices = c("2x2 table", "Logistic model"),
                                                                selected = character(0))),
                                  conditionalPanel(condition = "input.Outcome == 'Linear'",
                                                   style = "display: none;",
                                                   radioButtons("DataL", "Where is your data coming from?",
                                                                choices = ("Linear model")))), #no default radio button selected
                        wellPanel(p("Step 3"),
                                  conditionalPanel(condition = "(input.Data == 'Logistic model' || input.Data == '2x2 table') && input.Outcome == 'Dichotomous'",
                                                   style = "display: none;",
                                                   radioButtons("Analysis", "Which analysis would you like to carry out?",
                                                                choices = c("RIR", "Fragility"),
                                                                selected = character(0))),
                                  
                                  conditionalPanel(condition = "input.Outcome == 'Linear' && input.DataL == 'Linear model'",
                                                   style = "display: none;",
                                                   radioButtons("AnalysisL", "Which analysis would you like to carry out?",
                                                                choices = c("ITCV", "RIR", "Correlation: Preserve standard error",
                                                                            "Replacement: Preserve standard error", "Coefficient of proportionality",
                                                                            "Correlation: Mediation","Differential attrition"),
                                                                selected = character(0)))),
                        wellPanel(p("Step 4"),
                                  conditionalPanel(condition = "(input.AnalysisL == 'ITCV' || input.AnalysisL == 'RIR') && input.Outcome == 'Linear'",
                                                   style = "display: none;",
                                                   h5(strong("Enter these values:")),
                                                   numericInput("unstd_beta", "Estimated Effect", 2, step = .1),
                                                   numericInput("std_error", "Standard Error", .4, step = .1),
                                                   numericInput("n_obs", "Number of Observations", 100, step = 1),
                                                   numericInput("n_covariates", "Number of Covariates", 3, step = 1),
                                                   p("Note that decimals must be denoted with a period, e.g., 2.1"),
                                                   actionButton("results_pg_l", "Run")),
                                  
                                  conditionalPanel(condition = "(input.Analysis == 'RIR' || input.Analysis == 'Fragility') && 
                                                   (input.Data == 'Logistic model' && input.Outcome == 'Dichotomous')",
                                                   style = "display: none;",
                                                   numericInput("unstd_beta_nl", "Estimated Effect (Log Odds)", -.2, step = .1),
                                                   numericInput("std_error_nl", "Standard Error (of the Log Odds)", .103, step = .1),
                                                   numericInput("n_obs_nl", "Number of Observations", 20888, step = 1),
                                                   numericInput("n_covariates_nl", "Number of Covariates", 3, step = 1),
                                                   numericInput("n_trm_nl", "No. Treat. Cond. Cases", 17888, step = 1),
                                                   p("Note that decimals must be denoted with a period, e.g., 2.1"),
                                                   actionButton("results_pg_di", "Run")),
                                  
                                  conditionalPanel(condition = "(input.Analysis == 'RIR' || input.Analysis == 'Fragility') && 
                                                   (input.Data == '2x2 table' && input.Outcome == 'Dichotomous')",
                                                   style = "display: none;",
                                                   numericInput("ctrl_fail", "Control Condition: Result Failure", 18, step = 1),
                                                   numericInput("ctrl_success", "Control Condition: Result Success", 12, step = 1),
                                                   numericInput("treat_fail", "Treatment Condition: Result Failure", 12, step = 1),
                                                   numericInput("treat_success", "Treatment Condition: Result Sucesss", 17, step = 1),
                                                   actionButton("results_pg_2x2", "Run"))))),
             
             tabPanel(title = "Results",
                      p("RESULTS HERE"),
                      
                      #Figure Output panel (top left and bottom)
                      fluidRow(
                        column(7, tags$h4("Figure Output"), style = "height:600px; white; border: 2px double black;",

                               #Figure output
                               uiOutput("print_fig", width = "50%")),
                        
                        column(5,
                               #Printed Results Output (top right)
                               
                               fluidRow(tags$h4("Results (Printed)"), style = "height:300px; background-color: white; border: 2px double black; padding: 10px;",
                                        
                                        #Printed output
                                        htmlOutput("print_results")), 
                                        
                
                               fluidRow(tags$h4("Want to generate code to  use in R or Stata?"), style = "height:300px; white; border: 2px double black;",
                                        checkboxInput("gen_r_code", "Generate R Code", value = FALSE),
                                        checkboxInput("gen_stata_code", "Generate Stata Code"),
                                        conditionalPanel("input.gen_r_code == 1",
                                                         verbatimTextOutput("r_code_print"), uiOutput("clip"),
                                                         tippy_this(
                                                           "clip",
                                                           tooltip = "Copied!",
                                                           trigger = "click",
                                                           placement = "right",
                                                           arrow = "true"
                                                         ),
                                        )
                                     ))
                        
                      ))
             )))



  