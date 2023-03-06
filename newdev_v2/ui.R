#New development - Conditional panel approach
#ui
# NOTE: IF UPDATING APP + REPUBLISHING, FIRST RUN THIS YOUR CONSOLE, THEN RELOAD, AND PUBLISH
## devtools::install_github("konfound-project/konfound")

library(shiny)
library(rclipboard)
library(tippy)
library(shinythemes)
library(shinyBS)
library(fedmatch)

shinyUI(fluidPage(theme = shinythemes::shinytheme("lumen"),
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
                        "Konfound is a sensitivity analysis that quantifies the robustness of statistical inferences to concerns about bias and omitted variables."),
                      splitLayout(
                        wellPanel(p(h4(strong("Step 1"))),
                                  radioButtons("Outcome", "Type of outcome",
                                               choices = c("Dichotomous", "Continuous"),
                                               selected = character(0))),#no default radio button selected
                        wellPanel(p(h4(strong("Step 2"))),
                                  conditionalPanel(condition = "input.Outcome == 'Dichotomous'",
                                                   style = "display: none;",
                                                   radioButtons("Data", "Source of data",
                                                                choices = c("2x2 table", "Logistic model"),
                                                                selected = character(0))),
                                  conditionalPanel(condition = "input.Outcome == 'Continuous'",
                                                   style = "display: none;",
                                                   radioButtons("DataL", "Source of data",
                                                                choices = ("Linear model")))), #no default radio button selected
                        wellPanel(p(h4(strong("Step 3",
                                              bsButton("step3info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small"),
                                              ))),
                                  bsPopover(
                                    id = "step3info",
                                    title = "More information",
                                    content = HTML(paste0(
                                      "The Impact Threshold for a Confounding Variable (ITCV) reports how strongly an omitted variable would have to be correlated with both the predictor of interest and the outcome to make the estimated effect have a p-value of .05.",
                                      br(),
                                      br(),
                                      "The Robustness of Inference to Replacement (RIR) quantifies what proportion of the data must be replaced (with cases with zero effect) to make the estimated effect have a p-value of .05."
                                    )),
                                    placement = "right",
                                    trigger = "hover",
                                    options = list(container = "body")
                                  ),
                                  
                                  conditionalPanel(condition = "(input.Data == 'Logistic model' || input.Data == '2x2 table') && input.Outcome == 'Dichotomous'",
                                                   style = "display: none;",
                                                   radioButtons("Analysis", "Type of analysis",
                                                                choices = c("Robustness of Inference to Replacement (RIR)/Fragility" = "RIR"),
                                                                selected = character(0))),
                                  
                                  conditionalPanel(condition = "input.Outcome == 'Continuous' && input.DataL == 'Linear model'",
                                                   style = "display: none;",
                                                   radioButtons("AnalysisL", "Type of analysis?",
                                                                choiceNames = list("Impact Threshold for a Confounding Variable (ITCV)", 
                                                                            "Robustness of Inference to Replacement (RIR)", 
                                                                            em(HTML("<font color='gray'>Correlation: Preserve standard error (coming soon)</font>")),
                                                                            em(HTML("<font color='gray'>Replacement: Preserve standard error (coming soon)</font>")), 
                                                                            em(HTML("<font color='gray'>Coefficient of proportionality (coming soon)</font>")),
                                                                            em(HTML("<font color='gray'>Correlation: Mediation (coming soon)</font>")),
                                                                            em(HTML("<font color='gray'>Differential attrition (coming soon)</font>"))),
                                                                choiceValues = c("IT", "RIR", "copse", "rpse", "cop", "med", "da"),
    
                                                                selected = character(0)))),
                        wellPanel(p(h4(strong("Step 4"))),
                                  conditionalPanel(condition = "(input.AnalysisL == 'IT' || input.AnalysisL == 'RIR') && input.Outcome == 'Continuous'",
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
                      tags$h4(uiOutput("recap")),
                      
                      #Figure Output panel (top left and bottom)
                      fluidRow(
                        column(5, tags$h4("Results (Printed)"), style = "height:800px; background-color: white; border: 2px double black; padding: 10px;",
                               
                               #Printed output
                               htmlOutput("print_results")),
                        
                        column(7,
                               #Printed Results Output (top right)
                               
                               fluidRow(tags$h4("Figure Output"), style = "height:500px; white; border: 2px double black; padding: 10px;",
                                        
                                        #Figure output
                                        uiOutput("print_fig", width = "50%"),
                                        uiOutput("print_rir")), 
                                        
                
                               fluidRow(tags$h4("Want to generate code to  use in R or Stata?"), style = "height:300px; white; border: 2px double black; padding: 10px;",
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
                                        ),
                                        
                                        conditionalPanel("input.gen_stata_code == 1",
                                                         verbatimTextOutput("stata_code_print"), uiOutput("clip2"),
                                                         tippy_this(
                                                           "clip",
                                                           tooltip = "Copied!",
                                                           trigger = "click",
                                                           placement = "right",
                                                           arrow = "true"
                                                         ),
                                                         )
                                     ))
                        
                      )),
             tags$p("To cite this application: Rosenberg, J. M., Narvaiz, S., Xu, R., Lin, Q., & Frank, K. A. (2023). Konfound-It!: Quantify the robustness of causal inferences (v. 2.0.0)."))))



  