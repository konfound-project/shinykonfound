# NOTE: IF UPDATING APP + REPUBLISHING, FIRST RUN THIS YOUR CONSOLE, THEN RELOAD, AND PUBLISH
## devtools::install_github("konfound-project/konfound")

# install.packages("remotes")
#remotes::install_github("deepanshu88/shinyDarkmode")

library(shiny)
library(rclipboard)
library(tippy)
library(shinythemes)
library(shinyDarkmode)
library(shinyBS)
library(fedmatch)
library(shinyjs)
library(shinyscreenshot)

jscode <- "shinyjs.refresh_page = function() { history.go(0); }" 

shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("lumen"),
    use_darkmode(),
    
    tags$head(tags$script(src="script.js"),
              tags$style(HTML("
              
              /* import Google fonts */
              @import url('https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900&family=Victor+Mono&display=swap');
              @import url('https://fonts.googleapis.com/css2?family=Raleway:ital,wght@0,100;0,200;0,300;0,400;1,100;1,200;1,300;1,400');
              @import url('https://fonts.googleapis.com/css2?family=Cardo:wght@400;700&family=Cormorant:wght@400;700&family=Nanum+Myeongjo:wght@400;700;800&family=Playfair+Display+SC:wght@400;700;900&family=Roboto+Slab:wght@100;400;700;800;900&display=swap');

  
              
              /* change page background as well as header and footer text */
              body {
                    background-color: #fff;
                    font-family: 'Roboto', sans-serif;
                    font-size: 18px;
                    font-weight: 300;
                    color: #000;
              }
  
  
              
              /* change page title */
              h2 {
                    font-family: 'Raleway', serif;
                    font-size: 76px;
                    font-weight: 200;
                    color: #639dad;
              }
              
              /* change page first-level headings (e.g., Specification, Results) */
              h3 {
                    font-family: 'Roboto', sans-serif;
                    font-size: 32px;
                    font-weight: 700;
                    color: #7f9f3d;
              }

              /* change page second-level headings */
              h4 {
                    font-family: 'Roboto', sans-serif;
                    font-size: 24px;
                    font-weight: 700;
                    color: #000;
              }
              
              /* change page third-level headings */
              h5 {
                    font-family: 'Roboto', sans-serif;
                    font-size: 18px;
                    color: #000;
              }
              
              /* change hyperlink text */
              a {
                    color: #0645ad;  /* standard blue hyperlink color */
                    font-weight: 400;
              }
              
              /* change hyperlink hover text */
              a:hover {
                    color: #e04300;
                    text-decoration: underline;
              }
              
              /* change multiple choice text */
              .shiny-input-container {
                    color: #000;
                    font-size: 16px;
              }
              
              
              
              /* change navigation bar */
              .navbar-default {
                    background-color: #639dad;
                    border-color: #999;
                    font-size: 18px;
                    font-weight: 300;
              }
              
              .navbar-nav > .active {
                    background-color: #7f9f3d;
              }
              
              /* change text in navigation bar: active tab */
              .navbar-nav > .active > a {
                    color: #fff !important;
              }
              
              /* change text in navigation bar: inactive tab */
              .navbar-nav > li > a {
                    color: #eee !important;
              }
              

              
              /* change background of area for user input */
              .well { 
                    background-color: #fff !important;
              }
              
              /* change rows in area for user input */
              .row { 
                    background-color: #fff !important;
              }
              
              
              
              /* change color of most buttons */
              #startover_button, #results_pg_l, 
              #results_pg_pse, #results_pg_cop, 
              #results_pg_di, #results_pg_2x2 {
                    background-color: #7f9f3d !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
                    font-size: 22px;
              }
              
              /* change hover color of most buttons */
              #startover_button:hover, #results_pg_l:hover, 
              #results_pg_pse:hover, #results_pg_cop:hover, 
              #results_pg_di:hover, #results_pg_2x2:hover {
                    background-color: #465723 !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
              }
              
              
              
              /* change radio buttons */
              label > input[type='radio'] {
                    opacity: 0;
                    position: absolute;
              }
              
              label > input[type='radio'] + *::before {
                    content: '';
                    margin: 4px 0 0;
                    width: 13px;
                    height: 13px;
                    position: absolute;
                    margin-left: -20px;
                    border-radius: 50%;
                    border-style: solid;
                    border-width: 0.1rem;
                    border-color: #639dad;
              }
                    
              label > input[type='radio']:checked + *::before {
                    background: radial-gradient(#7f9f3d 0%, #7f9f3d 50%, transparent 50%) !important;
                    border-color: #639dad !important;
              }
              
              
                    
              label > input[type='checkbox'] {
                    opacity: 0;
                    position: absolute;
              }
                    
              label > input[type='checkbox'] + *::before {
                    content: '';
                    position: absolute;
                    margin: 4px 0 0;
                    margin-left: -20px;
                    align: center;
                    width: 13px;
                    height: 13px;
                    margin-right: 1rem;
                    border-radius: 0%;
                    border-style: solid;
                    border-width: 0.1rem;
                    border-color: #639dad !important;
              }
                    
              label > input[type='checkbox']:checked + *::before {
                    content: '';
                    width: 13px;
                    height: 13px;
                    border-color: #639dad !important;
                    background: #7f9f3d !important;
              }



              /* change text on info button */
              [id*=info] {
                    background-color: #639dad !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
                    text-shadow: -.0px 0 #fff, 0 .0px #fff, .0px 0 #fff, 0 -.0px #fff;
              }
              
              
              
              /* change color of screenshot button */
              [id*=screenshot] {
                    background-color: #7f9f3d !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
                    font-size: 22px;
              }
              
              /* change hover color of screenshot button */
              [id*=screenshot]:hover {
                    background-color: #465723 !important;
                    color: #fff !important;
                    font-family: 'Roboto', sans-serif;
              }

              
              
              /* change color of copy source code buttons */
              #clipbtn {
                    background-color: #7f9f3d;
                    color: #fff;
                    font-family: 'Roboto', sans-serif;
                    font-size: 14px;
              }
              
              /* change hover color of copy source code buttons */
              #clipbtn:hover {
                    background-color: #698235;
                    color: #fff;
                    font-family: 'Roboto', sans-serif;
                    font-size: 14px;
              }
              
              
              
              /* change source code output */
              #r_code, #stata_code, pre {
                    background-color: #444;
                    color: #fff;
                    font-family: 'Courier', mono;
                    font-size: 14px;
                    font-weight: 300;
              }
              
    "
                              
              ))
    ),
    
    
    
    
    titlePanel(title = div(img(style = "height:0.75em; vertical-align:center; margin-bottom: 18px;",
                               src = "konfound-logo.png",
                               alt = "Konfound R package logo"), 
                           "KonFound-It!")),
    h3("Quantify the Robustness of Causal Inferences"),
    p("Sensitivity analyses that quantify the robustness of inferences to concerns about omitted variables and other sources of bias."),
  
  
  rclipboardSetup(),
  
  
  ### For refresh button: 
  useShinyjs(),
  extendShinyjs(text = jscode, functions = "refresh_page"),

  
  navbarPage("",
             tabPanel(div(icon("house", lib = "font-awesome"), " Home"),
                      sidebarLayout(
                        sidebarPanel(
                          verticalLayout(
                            fluidRow(
                              align = "center",
                              h3("Specification")),
                            
                            
                            wellPanel(p(h4("Step 1",
                                           bsButton("step1info", 
                                                    label = "", 
                                                    icon = icon("info", 
                                                                lib = "font-awesome"), 
                                                    size = "extra-small"
                                           ))),
                            
                                      bsPopover(
                                        id = "step1info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "The type of outcome you choose will affect the type of sensitivity analyses you can run.  A dichotomous outcome takes two values (e.g., 1,0) such as indicating whether a student dropped out of school (1) or not (0).  A continuous outcome can take any value across a range, such as a test score."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                            
                                      radioButtons("Outcome", 
                                                   "Type of outcome:",
                                                   choices = c("Dichotomous", "Continuous"),
                                                   selected = character(0))), # No default radio button selected
                            
                            
                            
                            wellPanel(p(h4("Step 2",
                                           bsButton("step2info", 
                                                    label = "",
                                                    icon = icon("info", 
                                                                lib = "font-awesome"), 
                                                    size = "extra-small"
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
                                                       radioButtons("Data", 
                                                                    "Source of data:",
                                                                    choices = c("2x2 table", "Logistic model"),
                                                                    selected = character(0))),
                                      
                                      conditionalPanel(condition = "input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       radioButtons("DataL", 
                                                                    "Source of data:",
                                                                    choiceNames = list("Estimates from a linear model"),
                                                                    choiceValues = c("Linear model")))), # No default radio button selected
                            
                            
                            
                            wellPanel(p(h4("Step 3",
                                           bsButton("step3info", 
                                                    label = "", 
                                                    icon = icon("info", 
                                                                lib = "font-awesome"), 
                                                    size = "extra-small"
                                           ))),
                                      
                                      bsPopover(
                                        id = "step3info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Choose which type of sensitivity analysis you prefer based on the framework, constraints, and approach you wish to specify. See specific information icons for details", tags$a(href="https://www.dropbox.com/s/accoz5xu82vy27v/KonFound-it%21%20enhanced.xlsx?dl=0"), "Spreadsheet for calculating indices (KonFound-it!)"
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      conditionalPanel(condition = "(input.Data == 'Logistic model' || input.Data == '2x2 table') && input.Outcome == 'Dichotomous'",
                                                       style = "display: none;",
                                                       radioButtons("Analysis", 
                                                                    "Type of analysis:",
                                                                    choiceNames = list(
                                                                      list("Generalized Robustness of Inference to Replacement (RIR)/Fragility", 
                                                                           bsButton("fragility-info", 
                                                                                    label = "", 
                                                                                    icon = icon("info", 
                                                                                                lib = "font-awesome"), 
                                                                                    size = "extra-small"
                                                                           ))),
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
                                                       radioButtons("AnalysisL", 
                                                                    "Type of analysis:",
                                                                    choiceNames = list(
                                                                      list("Impact Threshold for a Confounding Variable (ITCV)", 
                                                                           bsButton("itcv-info", 
                                                                                    label = "", 
                                                                                    icon = icon("info", 
                                                                                                lib = "font-awesome"), 
                                                                                    size = "extra-small"
                                                                           )),
                                                                      list("Generalized Robustness of Inference to Replacement (RIR)", 
                                                                           bsButton("rir-info", 
                                                                                    label = "", 
                                                                                    icon = icon("info", 
                                                                                                lib = "font-awesome"), 
                                                                                    size = "extra-small"
                                                                           )),
                                                                      list("Preserve standard error (beta)", 
                                                                           bsButton("pse-info", 
                                                                                    label = "", 
                                                                                    icon = icon("info", 
                                                                                                lib = "font-awesome"), 
                                                                                    size = "extra-small"
                                                                           )),
                                                                      list("Coefficient of proportionality (beta)",  
                                                                           bsButton("cop-info", 
                                                                                    label = "", 
                                                                                    icon = icon("info", 
                                                                                                lib = "font-awesome"), 
                                                                                    size = "extra-small"
                                                                           ))
                                                                    ),
                                                                    choiceValues = c("IT", "RIR","PSE", "COP"),
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
                            
                            
                            
                            wellPanel(p(h4("Step 4",
                                           bsButton("step4info", 
                                                    label = "", 
                                                    icon = icon("info", 
                                                                lib = "font-awesome"), 
                                                    size = "extra-small"
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
                                                       h5("Enter these values:"),
                                                       numericInput("unstd_beta", 
                                                                    list("Estimated Effect", 
                                                                         bsButton("unstd_beta-info", 
                                                                                  label = "", 
                                                                                  icon = icon("info", 
                                                                                              lib = "font-awesome"), 
                                                                                  size = "extra-small")), 
                                                                    2, step = .1),
                                                       numericInput("std_error", 
                                                                    list("Standard Error", 
                                                                         bsButton("std_error-info", 
                                                                                  label = "", 
                                                                                  icon = icon("info", 
                                                                                              lib = "font-awesome"), 
                                                                                  size = "extra-small")), 
                                                                    .4, step = "any"),
                                                       numericInput("n_obs", 
                                                                    list("Number of Observations", 
                                                                         bsButton("n_obs-info", 
                                                                                  label = "", 
                                                                                  icon = icon("info", 
                                                                                              lib = "font-awesome"), 
                                                                                  size = "extra-small")), 
                                                                    100, step = 1),
                                                       numericInput("n_covariates", 
                                                                    list("Number of Covariates", 
                                                                         bsButton("n_covariates-info", 
                                                                                  label = "", 
                                                                                  icon = icon("info", 
                                                                                              lib = "font-awesome"), 
                                                                                  size = "extra-small")), 
                                                                    3, step = 1),
                                                       p("Note that decimals must be denoted with a period, e.g., 2.1"),
                                                       column(12,
                                                              actionButton("results_pg_l", 
                                                                           div(icon("play", lib = "font-awesome"), 
                                                                               " Run")),
                                                              align = "center"
                                                       )
                                      ),
                                      
                                      bsPopover(
                                        id = "unstd_beta-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "This is the estimated coefficient for the predictor of interest in a linear model (i. e., regression) or a difference of means"
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "std_error-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "The standard deviation of the sampling distribution, used to calculate the statistical significance of the estimated effect."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "n_obs-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Sample size.  For multilevel models this is the number of units at the level of the focal predictor."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "n_covariates-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Number of other variables entered into the model other than the focal predictor."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      conditionalPanel(condition = "(input.AnalysisL == 'PSE') && input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       h5("Enter these values:"),
                                                       numericInput("unstd_beta_pse", list("Estimated Effect", bsButton("unstd_beta-pse-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), .5, step = .1),
                                                       numericInput("std_err_pse", list("Standard Error", bsButton("std_error-pse-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), .056, step = "any"),
                                                       numericInput("n_obs_pse", list("Number of Observations", bsButton("n_obs-pse-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), 6174, step = 1),
                                                       numericInput("n_covariates_pse", list("Number of Covariates", bsButton("n_covariates-pse-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), 2, step =1),
                                                       numericInput("sdx_pse", "Standard Deviation of X", .22, step = .1),
                                                       numericInput("sdy_pse", "Standard Deviation of Y", 1, step = .1),
                                                       numericInput("R2_pse", HTML(paste0("R",tags$sup("2"))), .3, step = .1),
                                                       numericInput("eff_thr_pse", "Threshold for Inference (e.g., 1.96x standard error)", 0, step = .1),
                                                       p("Note that decimals must be denoted with a period, e.g., 2.1"),
                                                       column(12,
                                                              actionButton("results_pg_pse", 
                                                                           div(icon("play", lib = "font-awesome"), 
                                                                               " Run")),
                                                              align = "center"
                                                       )
                                      ),
                                      
                                      bsPopover(
                                        id = "unstd_beta-pse-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "This is the estimated coefficient for the predictor of interest in a linear model (i.e., regression) or a difference of means"
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "std_error-pse-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "The standard deviation of the sampling distribution, used to calculate the statistical significance of the estimated effect."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "n_obs-pse-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Sample size.  For multilevel models this is the number of units at the level of the focal predictor."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "n_covariates-pse-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Number of other variables entered into the model other than the focal predictor."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      

                                      conditionalPanel(condition = "(input.AnalysisL == 'COP') && input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       h5("Enter these values:"),
                                                       numericInput("unstd_beta_cop", list("Estimated Effect", bsButton("unstd_beta-cop-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), -.125, step = .1),
                                                       numericInput("std_err_cop", list("Standard Error", bsButton("std_error-cop-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), .050, step = "any"),
                                                       numericInput("n_obs_cop", list("Number of Observations", bsButton("n_obs-cop-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), 6265, step = 1),
                                                       numericInput("n_covariates_cop", list("Number of Covariates", bsButton("n_covariates-cop-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), 7, step =1),
                                                       numericInput("sdx_cop", "Standard Deviation of X", .217, step = .1),
                                                       numericInput("sdy_cop", "Standard Deviation of Y", .991, step = .1),
                                                       numericInput("R2_cop", HTML(paste0("R",tags$sup("2"))), .251, step = .1),
                                                       numericInput("eff_thr_cop", "Threshold for Inference (e.g., 1.96x standard error)", 0, step = .1),
                                                       numericInput("FR2max_cop", HTML(paste0("R",tags$sup("2"),"Max")), .61, step = .1),
                                                       p("Note that decimals must be denoted with a period, e.g., 2.1"),
                                                       column(12,
                                                              actionButton("results_pg_cop", 
                                                                           div(icon("play", lib = "font-awesome"), 
                                                                               " Run")),
                                                              align = "center"
                                                       )
                                      ),
                                      
                                      bsPopover(
                                        id = "unstd_beta-cop-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "This is the estimated coefficient for the predictor of interest in a linear model (i.e., regression) or a difference of means"
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "std_error-cop-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "The standard deviation of the sampling distribution, used to calculate the statistical significance of the estimated effect."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "n_obs-cop-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Sample size.  For multilevel models this is the number of units at the level of the focal predictor."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "n_covariates-cop-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Number of other variables entered into the model other than the focal predictor."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      
                                      conditionalPanel(condition = "(input.Analysis == 'RIR' || input.Analysis == 'Fragility') &&
                                                                        (input.Data == 'Logistic model' && input.Outcome == 'Dichotomous')",
                                                       style = "display: none;",
                                                       numericInput("unstd_beta_nl", list("Estimated Effect (Log Odds)", bsButton("unstd_beta-log-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), -.2, step = .1),
                                                       numericInput("std_error_nl", list("Standard Error (of the Log Odds)", bsButton("std_error-log-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), .103, step = "any"),
                                                       numericInput("n_obs_nl", list("Number of Observations", bsButton("n_obs-log-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), 20888, step = 1),
                                                       numericInput("n_covariates_nl", list("Number of Covariates", bsButton("n_covariates-log-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")), 3, step = 1),
                                                       numericInput("n_trm_nl", "Number of cases in treatment condition", 17888, step = 1),
                                                       p("Note that decimals must be denoted with a period, e.g., 2.1"),
                                                       column(12,
                                                              actionButton("results_pg_di", 
                                                                           div(icon("play", lib = "font-awesome"), 
                                                                               " Run")),
                                                              align = "center"
                                                       )
                                      ),
                                      
                                      bsPopover(
                                        id = "unstd_beta-log-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "This is the estimated coefficient for the predictor of interest in a logistic model."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "std_error-log-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "The standard deviation of the sampling distribution, used to calculate the statistical significance of the estimated effect."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "n_obs-log-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Sample size.  For multilevel models this is the number of units at the level of the focal predictor."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      bsPopover(
                                        id = "n_covariates-log-info",
                                        title = "More information",
                                        content = HTML(paste0(
                                          "Number of other variables entered into the model other than the focal predictor."
                                        )),
                                        placement = "right",
                                        trigger = "hover",
                                        options = list(container = "body")
                                      ),
                                      
                                      
                                      conditionalPanel(condition = "(input.Analysis == 'RIR' || input.Analysis == 'Fragility') &&
                                                                                         (input.Data == '2x2 table' && input.Outcome == 'Dichotomous')",
                                                       style = "display: none;",
                                                       numericInput("ctrl_fail", "Control Condition: Result Failure", 18, step = 1),
                                                       numericInput("ctrl_success", "Control Condition: Result Success", 12, step = 1),
                                                       numericInput("treat_fail", "Treatment Condition: Result Failure", 12, step = 1),
                                                       numericInput("treat_success", "Treatment Condition: Result Sucesss", 17, step = 1),
                                                       column(12,
                                                              actionButton("results_pg_2x2", 
                                                                           div(icon("play", lib = "font-awesome"), 
                                                                               " Run")),
                                                              align = "center"
                                                       )
                                      )
                            )
                          )
                        ),
                        
                        mainPanel(
                          verticalLayout(
                            fluidRow(
                              br(.noWS = "before"),
                              align = "center",
                              h3("Results")),
                              
                              wellPanel(p(h4("Text Output")),
                                        htmlOutput("print_results")
                              ),
                              
                              wellPanel(p(h4("Graphic Output")),
                                        uiOutput("print_fig", width = "50%"),
                                        uiOutput("print_rir")
                              ),
                            
                            
                            
                            wellPanel(p(h4("Would you like to generate source code?")),
                                      checkboxInput("gen_r_code", "Generate R Code"),
                                      conditionalPanel(condition = "input.gen_r_code == 1",
                                                       id = "r_code",
                                                       verbatimTextOutput("r_code_print"), 
                                                       uiOutput("clip"),
                                                       tippy_this(
                                                         "clip",
                                                         tooltip = "<span style='font-size:14px; color:#fff;'>Copied!<span>",
                                                         trigger = "click",
                                                         placement = "right",
                                                         arrow = "true")),
                                      
                                      checkboxInput("gen_stata_code", "Generate Stata Code"),
                                      conditionalPanel(condition = "input.gen_stata_code == 1",
                                                       id = "stata_code",
                                                       verbatimTextOutput("stata_code_print"), 
                                                       uiOutput("clip2"),
                                                       tippy_this(
                                                         "clip",
                                                         tooltip = "<span style='font-size:14px; color:#fff;'>Copied!<span>",
                                                         trigger = "click",
                                                         placement = "right",
                                                         arrow = "true"))),
                         
                            
                            
                             
                            column(12,
                                   screenshotButton(inputId = "screenshot_button"),
                                   actionButton(inputId = "startover_button", 
                                                div(icon("rotate-right", lib = "font-awesome"), 
                                                    "Start Over")),
                                   align = "right"
                            )
                            
                            
                          )
                        )
                      )
             ),
             
             
             
             tabPanel(div(icon("screwdriver-wrench", lib = "font-awesome"), " Resources"),
                      
                      tags$h4("Explanatory Resources"),
                      tags$ul(
                        tags$li(tags$a(href="https://www.dropbox.com/s/9eymdekym5g50o7/frequently%20asked%20questions%20for%20application%20of%20konfound-it.docx?dl=0", "FAQ")), 
                        tags$li(tags$a(href="https://www.dropbox.com/s/33zkk861g04hocf/Overview%20of%20Konfound%20commands%20with%20inputs%20and%20outputs.docx?dl=0", "Overview of pkonfound commands")),
                        tags$li(tags$a(href="https://drive.google.com/file/d/1qbRx2penqzb7kEJkxJD3ARf26CjXMhzg/view", "Quick examples")), 
                        tags$li(tags$a(href="https://www.dropbox.com/s/o67e7w2sm8uww18/quantifying%20the%20robustness%20of%20causal%20inferences%20combined%20frameworks%20for%20stat%20horizons%20distribute.pptx?dl=0", "Powerpoint quantifying the robustness of causal inferences combined frameworks")),
                        tags$li(tags$a(href="https://www.dropbox.com/s/8t6x00mokkljksh/quantifying%20the%20robustness%20of%20causal%20inferences%20%20comparison%20of%20frameworks.pptx?dl=0", "Powerpoint for comparison of frameworks")),
                      ),
                      hr(),
                      
                      tags$h4("Additional Resources for Publication"),
                      tags$ul(
                        tags$li(tags$a(href="https://www.dropbox.com/s/bc4ert79kgbsac4/Examples%20of%20applications%20of%20indices%20for%20quantifying%20the%20robustness%20of%20causal%20inferences.docx?dl=0", "Published empirical examples")),
                        tags$li(tags$a(href="https://www.dropbox.com/s/accoz5xu82vy27v/KonFound-it%21%20enhanced.xlsx?dl=0", "Spreadsheet for calculating indices (KonFound-it!)")),
                      ),
                      hr(),
                        
                      tags$h4("Impact Threshold for a Confounding Variable Publications"),
                      tags$ul(  
                        tags$li(tags$a(href="https://drive.google.com/file/d/1F7oGYZ8SS8hnZxSI3Dch_w65Qz6KIRdI/view", "Frank, K. 2000. 'Impact of a Confounding Variable on the Inference of a Regression Coefficient.' Sociological Methods and Research, 29(2), 147-194")),
                        tags$li(tags$a(href="https://drive.google.com/file/d/1aOvAXEVnQCe9-dbWkgTqtq56Y3Z1tpkg/view", "Frank, K.A., Gary Sykes, Dorothea Anagnostopoulos, Marisa Cannata, Linda Chard, Ann Krause, Raven McCrory. 2008. Extended Influence: National Board Certified Teachers as Help Providers. Education, Evaluation, and Policy Analysis. Vol 30(1): 3-30.")),
                      ),
                      hr(),
                        
                      tags$h4("Robustness of Inference to Replacement Publications"),
                      tags$ul(  
                        tags$li(tags$a(href="https://www.dropbox.com/s/o0rmduhe8pj3khd/INDICES%20OF%20ROBUSTNESS%20FOR%20SAMPLE%20REPRESENTATION.pdf?dl=0", "*Frank, K. A. and Min, K. 2007. Indices of Robustness for Sample Representation. Sociological Methodology. Vol 37, 349-392. * co first authors.")),
                        tags$li(tags$a(href="https://drive.google.com/file/d/1aGhxGjvMvEPVAgOA8rrxvA97uUO5TTMe/view", "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. 2013. What would it take to Change an Inference?: Using Rubin’s Causal Model to Interpret the Robustness of Causal Inferences. Education, Evaluation and Policy Analysis. Vol 35: 437-460.")),
                        tags$li(tags$a(href="https://www.dropbox.com/s/2dzkvalwmgr5v5z/Hypothetical%20case%20replacement%20can%20be%20used%20to%20quantify%20the%20robustness%20of%20trial%20results%20submit.docx?dl=0", "*Frank, K. A., *Lin, Q., *Maroulis, S., *Mueller, A. S., Xu, R., Rosenberg, J. M., ... & Zhang, L. 2021. Hypothetical case replacement can be used to quantify the robustness of trial results. Journal of Clinical Epidemiology, 134, 150-159. *authors listed alphabetically.")),
                      ),                       
                      hr(),
                        
                      tags$h4("Publications for Both Frameworks"),
                      tags$ul(  
                        tags$li(tags$a(href ="https://www.researchgate.net/profile/Ran-Xu-6/publication/335956720_konfound_Command_to_quantify_robustness_of_causal_inferences/links/5e49a3d2a6fdccd965ac3564/konfound-Command-to-quantify-robustness-of-causal-inferences.pdf", "Xu, R., Frank, K. A., Maroulis, S. J., & Rosenberg, J. M. (2019). konfound: Command to quantify robustness of causal inferences. The Stata Journal, 19(3), 523-550.")),
                        tags$li(tags$a(href="https://www.dropbox.com/s/rn8a4jbxtiynefh/Quantifying%20the%20Robustness%20of%20Causal%20Inferences%20Frank%20SSR%20final.pdf?dl=0", "Frank, K.A., Lin, Q., Xu, R., Maroulis, S.J., Mueller, A. (on-line first). Quantifying the Robustness of Causal Inferences: Sensitivity Analysis for Pragmatic Social Science.  Social Science Research. 110, 102815.")),
                      ),
                      hr(),
                        
                      tags$h4("Contact"),
                      tags$p(icon("house", lib = "font-awesome"), tags$a(href="https://msu.edu/~kenfrank/research.htm#causal", "Ken Frank's homepage")),
                      tags$p(icon("envelope", lib = "font-awesome"), tags$a(href = "mailto:kenfrank@msu.edu", "Email Ken Frank")),
                      tags$p(icon("paper-plane", lib = "font-awesome"), tags$a(href="https://groups.google.com/forum/#!forum/konfound-it", "Google Groups mailing list for Konfound-It!"))
             ),            
  ),
  
  hr(),
  
  tags$p(tags$b("To cite this application: "), 
         "Rosenberg, J. M., Narvaiz, S., Xu, R., Lin, Q., Maroulis, S., & Frank, K. A. (2023).",
         tags$i("Konfound-It!: Quantify the robustness of causal inferences"), 
          "(v. 2.0.0)."
  )
  
  )
)
