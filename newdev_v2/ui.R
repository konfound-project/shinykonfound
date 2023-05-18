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
library(shinyjs)

jscode <- "shinyjs.refresh_page = function() { history.go(0); }"

shinyUI(fluidPage(theme = shinythemes::shinytheme("lumen"),
  tags$head(tags$script(src="script.js"),
            tags$style(HTML(".tippy-tooltip {
                      font-size: 15px;
    }
                    "))),
  tags$style(' .well  { background-color: #fafafa !important;}'),
  tags$style(' .row  { background-color: #ffffff !important;}'),
  
  rclipboardSetup(),
  
  useShinyjs(), #for refresh button
  extendShinyjs(text = jscode, functions = "refresh_page"), #for refresh button
  
  titlePanel("KonFound-It! Quantify the Robustness of Causal Inferences"),
  p(style= "text-align: justify; font-size = 14px",
    "Sensitivity analyses that quantify the robustness of inferences to concerns about omitted variables and other sources of bias."),
  
  navbarPage("",
             tabPanel("Home",
                      sidebarLayout(
                        sidebarPanel(
                          verticalLayout(
                            fluidRow(
                              align = "center",
                              h3(strong("Specification"))),
                            
                            wellPanel(p(h4(strong("Step 1",
                                                  
                                                  bsButton("step1info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")
                                                  ))),
                                      
                                      bsPopover(
                                        id = "step1info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "The type of outcome you choose will affect the type of sensitivity analyses you can run.  A dichotomous outcome takes two values (e.g., 1,0) indicating whether a student dropped out of school (1) or not (0).  A continuous outcome can take any value across a range, such as a test score."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      
                                      
                                      radioButtons("Outcome", "Type of outcome",
                                                   choices = c("Dichotomous", "Continuous"),
                                                   selected = character(0))),#no default radio button selected
                            
                            wellPanel(p(h4(strong("Step 2",
                                                  
                                                  bsButton("step2info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")
                                                  
                                                  ))),
                                      
                                      bsPopover(
                                        id = "step2info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Enter values obtained from an estimated model (e.g., estimated effect, standard error, number of covariates, sample size) that can be based on a published example or your own output.  Dichotomous outcomes also require the number in the treatment condition or the four values in a 2x2 table."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      conditionalPanel(condition = "input.Outcome == 'Dichotomous'",
                                                       style = "display: none;",
                                                       radioButtons("Data", "Source of data",
                                                                    choices = c("2x2 table", "Logistic model"),
                                                                    selected = character(0))),
                                      
                                      conditionalPanel(condition = "input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       radioButtons("DataL", "Source of data",
                                                                    
                                                                    choiceNames = list("Estimates from a linear model",
                                                                                       em(HTML("<font color='gray'>Estimates from a Poisson model (coming soon)</font>"))),
                                                                    choiceValues = c("Linear model", "Poisson model"),
                                                                    selected = character(0)))), #no default radio button selected
                            
                            wellPanel(p(h4(strong("Step 3",
                                                  
                                                  bsButton("step3info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")
                                                  ))),
                                      
                                      bsPopover(
                                        id = "step3info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "For alternative thresholds use the  R or Stata Konfound commands or the ", tags$a(href="https://www.dropbox.com/s/accoz5xu82vy27v/KonFound-it%21%20enhanced.xlsx?dl=0", "Spreadsheet for calculating indices (KonFound-it!)")
                                        )),
                                        placement = "right",
                                        trigger = "click",
                                        options = list(container = "body")
                                      ),
                                      
                                      
                                      
                                      conditionalPanel(condition = "(input.Data == 'Logistic model' || input.Data == '2x2 table') && input.Outcome == 'Dichotomous'",
                                                       style = "display: none;",
                                                       radioButtons("Analysis", "Type of analysis",
                                                                    choiceNames = list(
                                                                      list("Generalized Robustness of Inference to Replacement (RIR)/Fragility", bsButton("fragility-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small"))),
                                                                    choiceValues = c("RIR"),
                                                                    selected = character(0))),
                                      
                                      bsPopover(
                                        id = "fragility-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "This calculates Fragility – the number of cases that must be switched (e.g., from treatment success to treatment failure) to make the association between treatment and outcome have a p-value of .05.  It also calculates the Robustness of Inference to Replacement (RIR), the number of cases that must be replaced to generate the switches associated with Fragility.  See results for the 2x2 Implied Table and Transfer Table."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      conditionalPanel(condition = "input.Outcome == 'Continuous' && input.DataL == 'Linear model'",
                                                       style = "display: none;",
                                                       radioButtons("AnalysisL", "Type of analysis?",
                                                                    
                                                                    choiceNames = list(
                                                                      list("Impact Threshold for a Confounding Variable (ITCV)", bsButton("itcv-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                                                                      list("Generalized Robustness of Inference to Replacement (RIR)", bsButton("rir-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                                                                                       em(HTML("<font color='gray'>Conditional RIR (coming soon)</font>")),
                                                                      list("Preserve standard error", bsButton("pse-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                                                                      list("Coefficient of proportionality",  bsButton("cop-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                                                                                       em(HTML("<font color='gray'>Correlation: Mediation (coming soon)</font>")),
                                                                                       em(HTML("<font color='gray'>Differential attrition (coming soon)</font>"))),
                                                                    choiceValues = c("IT", "RIR", "copse", "PSE", "COP", "med", "da"),
                                                                    selected = character(0)))),
                            
                            bsPopover(
                              id = "itcv-info",
                              title = "More information",
                              content = HTML(paste0(
                                "The Impact Threshold for a Confounding Variable (ITCV) reports how strongly an omitted variable would have to be correlated with both the predictor of interest and the outcome to make the estimated effect have a p-value of .05. For alternative thresholds use the  R or Stata Konfound commands or the Konfound-it spreadsheet."
                              )),
                              placement = "right",
                              trigger = "hover",
                              options = list(container = "body")
                            ),
                            
                            bsPopover(
                              id = "rir-info",
                              title = "More information",
                              content = HTML(paste0(
                                "The Robustness of Inference to Replacement (RIR) quantifies what proportion of the data must be replaced (with cases with zero effect) to make the estimated effect have a p-value of .05. For alternative thresholds use the  R or Stata Konfound commands or the Konfound-it spreadsheet."
                              )),
                              placement = "right",
                              trigger = "hover",
                              options = list(container = "body")
                            ),
                            
                            bsPopover(
                              id = "pse-info",
                              title = "More information",
                              content = HTML(paste0(
                                "This calculates the correlation between the omitted variable and the focal predictor and between the omitted variable and the outcome necessary to make the estimated effect of the focal predictor have a p-value of .05 while Preserving the Standard Error (PSE) of the original analysis. PSE requires extra inputs including the threshold for inference (e.g., 1.96 x standard error), standard deviation of the outcome (Y), the standard deviation of the focal predictor (X) and the observed R2."
                              )),
                              placement = "right",
                              trigger = "hover",
                              options = list(container = "body")
                            ),
                            
                            bsPopover(
                              id = "cop-info",
                              title = "More information",
                              content = HTML(paste0(
                                "This calculates the correlation between the omitted variable and the focal predictor and between the omitted variable and the outcome necessary to make the estimated effect of the focal predictor be zero and an R2 as specified on input. These correlations also generate the Coefficient of Proportionality (COP) , the proportion selection on unobservables (omitted covariates) relative to observables (observed covariates) necessary to reduce the effect of the focal predictor to zero for a specified R2.  COP requires extra inputs including the standard deviation of the outcome (Y), of the focal predictor (X), the observed R2, and the desired final R2 (FR2MAX)."
                              )),
                              placement = "right",
                              trigger = "hover",
                              options = list(container = "body")
                            ),
                            
                            
                            
                            wellPanel(p(h4(strong("Step 4",
                                                  
                                                  bsButton("step4info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")
                                                  
                                                  ))),
                                      
                                      bsPopover(
                                        id = "step4info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Enter values from data or estimated model as well as specified thresholds for some analyses."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      
                                      conditionalPanel(condition = "(input.AnalysisL == 'IT' || input.AnalysisL == 'RIR') && input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       h5(strong("Enter these values:")),
                                                       numericInput("unstd_beta", "Estimated Effect", 2, step = .1),
                                                       numericInput("std_error", "Standard Error", .4, step = .1),
                                                       numericInput("n_obs", "Number of Observations", 100, step = 1),
                                                       numericInput("n_covariates", "Number of Covariates", 3, step = 1),
                                                       p("Note that decimals must be denoted with a period, e.g., 2.1"),
                                                       actionButton("results_pg_l", "Run")),
                                      
                                      conditionalPanel(condition = "(input.AnalysisL == 'PSE') && input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       h5(strong("Enter these values:")),
                                                       numericInput("unstd_beta_pse", "Estimated Effect", .5, step = .1),
                                                       numericInput("std_err_pse", "Standard Error", .056, step = .1),
                                                       numericInput("n_obs_pse", "Number of Observations", 6174, step = 1),
                                                       numericInput("sdx_pse", "Standard Deviation of X", .22, step = .1),
                                                       numericInput("sdy_pse", "Standard Deviation of Y", 1, step = .1),
                                                       numericInput("R2_pse", HTML(paste0("R",tags$sup("2"))), .3, step = .1),
                                                       p("Note that decimals must be denoted with a period, e.g., 2.1"),
                                                       actionButton("results_pg_pse", "Run")),

                                      conditionalPanel(condition = "(input.AnalysisL == 'COP') && input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       h5(strong("Enter these values:")),
                                                       numericInput("unstd_beta_cop", "Estimated Effect", -.125, step = .1),
                                                       numericInput("std_err_cop", "Standard Error", .050, step = .1),
                                                       numericInput("n_obs_cop", "Number of Observations", 6265, step = 1),
                                                       numericInput("n_covariates_cop", "Number of Covariates", 7, step =1),
                                                       numericInput("sdx_cop", "Standard Deviation of X", .217, step = .1),
                                                       numericInput("sdy_cop", "Standard Deviation of Y", .991, step = .1),
                                                       numericInput("R2_cop", HTML(paste0("R",tags$sup("2"))), .251, step = .1),
                                                       numericInput("eff_thr_cop", "Threshold for Inference (e.g., 1.96x standard error", 0, step = 1),
                                                       numericInput("FR2max_cop", HTML(paste0("R",tags$sup("2"),"Max")), .61, step = .1),
                                                       p("Note that decimals must be denoted with a period, e.g., 2.1"),
                                                       actionButton("results_pg_cop", "Run")),
                                                       
                                                       conditionalPanel(condition = "(input.Analysis == 'RIR' || input.Analysis == 'Fragility') &&
                                                                        (input.Data == 'Logistic model' && input.Outcome == 'Dichotomous')",
                                                                        style = "display: none;",
                                                                        numericInput("unstd_beta_nl", "Estimated Effect (Log Odds)", -.2, step = .1),
                                                                        numericInput("std_error_nl", "Standard Error (of the Log Odds)", .103, step = .1),
                                                                        numericInput("n_obs_nl", "Number of Observations", 20888, step = 1),
                                                                        numericInput("n_covariates_nl", "Number of Covariates", 3, step = 1),
                                                                        numericInput("n_trm_nl", "Number of cases in treatment condition", 17888, step = 1),
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
                        
                        mainPanel(
                          fluidRow(
                            align = "center",
                            h3(strong("Results"))),
                          verticalLayout(
                            p(h5(strong("Text"))),
                            htmlOutput("print_results"),
                            hr(),
                            p(h5(strong("Graphic Output"))),
                            uiOutput("print_fig", width = "50%"),
                            uiOutput("print_rir"),
                            
                            hr(),
                            wellPanel(p(h5(strong("Want to generate R/Stata code?"))),
                                      checkboxInput("gen_r_code", "Generate R Code", value = FALSE),
                                      checkboxInput("gen_stata_code", "Generate Stata Code"),
                                      conditionalPanel("input.gen_r_code == 1",
                                                       verbatimTextOutput("r_code_print"), uiOutput("clip"),
                                                       tippy_this(
                                                         "clip",
                                                         tooltip = "Copied!",
                                                         trigger = "click",
                                                         placement = "right",
                                                         arrow = "true")),
                                      
                                      conditionalPanel("input.gen_stata_code == 1",
                                                       verbatimTextOutput("stata_code_print"), uiOutput("clip2"),
                                                       tippy_this(
                                                         "clip",
                                                         tooltip = "Copied!",
                                                         trigger = "click",
                                                         placement = "right",
                                                         arrow = "true"))),
                            
                            hr(),
                          column(12,
                                   actionButton("startover_button", "Start Over"),
                                   align = "right")
                            ))
                        )
                      ),
             
             tabPanel("Resources",
                      tags$br(),
                      tags$ul(
                        tags$b("Explanatory resources:"),
                        tags$li(tags$a(href="https://drive.google.com/file/d/1qbRx2penqzb7kEJkxJD3ARf26CjXMhzg/view", "Quick examples")), 
                        tags$li(tags$a(href="https://www.dropbox.com/s/o67e7w2sm8uww18/quantifying%20the%20robustness%20of%20causal%20inferences%20combined%20frameworks%20for%20stat%20horizons%20distribute.pptx?dl=0", "Powerpoint quantifying the robustness of causal inferences combined frameworks")),
                        tags$li(tags$a(href="https://www.dropbox.com/s/8t6x00mokkljksh/quantifying%20the%20robustness%20of%20causal%20inferences%20%20comparison%20of%20frameworks.pptx?dl=0", "Powerpoint for comparison of frameworks")),
                        p(),
                        tags$b("Additional resources for publication:"),
                        tags$li(tags$a(href="https://www.dropbox.com/s/bc4ert79kgbsac4/Examples%20of%20applications%20of%20indices%20for%20quantifying%20the%20robustness%20of%20causal%20inferences.docx?dl=0", "Published empirical examples")),
                        tags$li(tags$a(href="https://www.dropbox.com/s/accoz5xu82vy27v/KonFound-it%21%20enhanced.xlsx?dl=0", "Spreadsheet for calculating indices (KonFound-it!)")),
                        p(),
                        tags$b("Publications:"),
                        tags$li(tags$a(href="https://journals-sagepub-com.utk.idm.oclc.org/doi/pdf/10.1177/0049124100029002001?casa_token=fYuAFVObijwAAAAA:vSl2F4mcMI_SEOZwWPXVm85uWBvP6WmpX3tdzpw_PByAVuEs48uvGCBgvXmOF9V_uHxoPBMOhj-G", "Frank (2000)")),
                        tags$li(tags$a(href="https://www-journals-uchicago-edu.utk.idm.oclc.org/doi/full/10.1086/587153?casa_token=aUJoYMcWm-EAAAAA:cPXcDMrRok0eyCDiVM78F_NZnepc0TgPUCwKIviT8Hj1M_QdArU5wDrdnoxTyW0-tPU-4Tyw9qs", "Frank et al. (2008)")),
                        tags$li(tags$a(href="https://journals-sagepub-com.utk.idm.oclc.org/doi/pdf/10.3102/0162373713493129?casa_token=xqBZmXKN44sAAAAA:epm5JsJcOYKs4wFYA4yPEBQlbh9Wz5b43BE6jBKtF7Pv9uMHtFowfbLsCHaVSxMAaxnwU4AnDZkE", "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013)")),
                        tags$li(tags$a(href="https://www.dropbox.com/s/7hxtpzcpw3o6m2n/Hypothetical%20case%20replacement%20can%20be%20used%20to%20quantify%20the%20robustness%20of%20trial%20results%20as%20published.pdf?dl=0", "Frank, K. A., *Lin, Q., *Maroulis, S., *Mueller, A. S., Xu, R., Rosenberg, J. M., ... & Zhang, L. (2021)")),
                        tags$li(tags$a(href="https://www.dropbox.com/s/4vvpvgqhpe2h1rv/Quantifying%20the%20robustness%20of%20causal%20inferences%20as%20published%20sensitivity%20analysis%20for%20pragmatic%20social%20science.pdf?dl=0", "Frank, K.A., Lin, Q., Xu, R., Maroulis, S.J., Mueller, A. (2013)")),
                        p(),
                        tags$b("Contact:"),
                        tags$li(tags$a(href="https://msu.edu/~kenfrank/research.htm#causal", "Ken Frank's homepage")),
                        tags$li(tags$a(href = "mailto:kenfrank@msu.edu", "Email Ken Frank")),
                        tags$li(tags$a(href="https://groups.google.com/forum/#!forum/konfound-it", "Google Groups mailing list for Konfound-It!")),
                        p())
                      )
                    
             ),
  
  tags$p("To cite this application: Rosenberg, J. M., Narvaiz, S., Xu, R., Lin, Q., & Frank, K. A. (2023). Konfound-It!: Quantify the robustness of causal inferences (v. 2.0.0).")
  )
  )

  