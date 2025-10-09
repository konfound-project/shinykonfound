# NOTE: IF UPDATING APP + REPUBLISHING, FIRST RUN THIS YOUR CONSOLE, THEN RELOAD, AND PUBLISH



################################################################################
### Load Packages
################################################################################

# install.packages("remotes")
#remotes::install_github("deepanshu88/shinyDarkmode")

library(shiny)
library(rclipboard)
library(tippy)
library(shinythemes)
library(shinyBS)
library(fedmatch)
library(shinyjs)
library(shinyscreenshot)

#install.packages("remotes")
#remotes::install_github("deepanshu88/shinyDarkmode")
library(shinyDarkmode)

source("info-button-notes.R")



################################################################################
### Style Sheet
################################################################################

jscode <- "shinyjs.refresh_page = function() { history.go(0); }" 

shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("lumen"),
    use_darkmode(),
    
    tags$head(
      includeCSS("konfound-style.css"),
      HTML("<title>KonFound-It! Shiny App</title><link rel='icon' href='KonFoundit!-mark.png' type='image/gif/png'>"),
      tags$script(HTML("
  $(document).ready(function() {
    
    function hideSubheadingRadios() {
      console.log('Attempting to hide subheading radios...');
      
      // Target by value attribute
      var subheadingInputs = $('input[value=\"SUBHEADING1\"], input[value=\"SUBHEADING2\"]');
      console.log('Found', subheadingInputs.length, 'subheading inputs');
      
      if (subheadingInputs.length > 0) {
        subheadingInputs.each(function() {
          console.log('Hiding input with value:', $(this).val());
          
          // Hide the input itself
          $(this).hide();
          $(this).css({
            'display': 'none !important',
            'visibility': 'hidden !important',
            'width': '0',
            'height': '0'
          });
          
          // Disable it
          $(this).prop('disabled', true);
          
          // Add class to parent radio div
          $(this).closest('.radio').addClass('radio-subheading');
          
          // Also hide the label's ::before pseudo-element by modifying the label
          var label = $(this).next('span').parent('label');
          label.css('cursor', 'default');
        });
      }
    }
    
    // Run immediately
    hideSubheadingRadios();
    
    // Run again after a short delay
    setTimeout(hideSubheadingRadios, 100);
    setTimeout(hideSubheadingRadios, 500);
    setTimeout(hideSubheadingRadios, 1000);
    
    // Run when Shiny updates
    $(document).on('shiny:inputchanged', function(event) {
      setTimeout(hideSubheadingRadios, 50);
    });
    
    // Run when conditional panels change
    $(document).on('shiny:conditional', function(event) {
      setTimeout(hideSubheadingRadios, 50);
    });
    
    // Use MutationObserver to detect when elements are added to DOM
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
          setTimeout(hideSubheadingRadios, 50);
        }
      });
    });
    
    // Start observing
    observer.observe(document.body, {
      childList: true,
      subtree: true
    });
  });
"))
      
    ),
 
    
    
       
    
################################################################################
### Header
################################################################################   

    titlePanel(title = div(img(style = "height:3.0em; 
                                        vertical-align:top;
                                        margin-left: -48px;
                                        margin-top: -48px;
                                        margin-bottom: -48px;",
                               src = "KonFoundit!.png",
                               alt = "Konfound-It! logo"
                               )
                           )
               ),
    h3("Quantify the Robustness of Causal Inferences"),
    tags$p("Sensitivity analyses that quantify the robustness of inferences to concerns about omitted variables and other sources of bias."),
    tags$p(actionButton("visit_website_button",
                  icon = icon("globe", lib = "font-awesome"),
                  label = "KonFound-It website",
                  onclick = "window.open('https://konfound-it.org/', '_blank')")
           ),    
    tags$p(tags$i(paste("Powered by version", packageVersion('konfound'), "of the konfound R package."))),
  
  
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
                            
                            
                            
                            
                            
                            ################################################################################
                            ### Step 1
                            ################################################################################
                            
                            wellPanel(p(h4("Step 1", create_info_button("step1info", ""))),
                                      
                                      radioButtons("Outcome", 
                                                   div(class = "label-style", "Select type of outcome:"),
                                                   choices = c("Dichotomous", "Continuous"),
                                                   selected = character(0)  # No default radio button selected
                                      )
                            ), 
                            

                            

                                                        
                            ################################################################################
                            ### Step 2
                            ################################################################################
                            
                            wellPanel(p(h4("Step 2", create_info_button("step2info", ""))),
                                      
                                      conditionalPanel(condition = "input.Outcome == 'Dichotomous'",
                                                       style = "display: none;",
                                                       radioButtons("DataD", 
                                                                    div(class = "label-style", "Select source of data:"),
                                                                    choices = c("2x2 table", "Logistic model"),
                                                                    selected = character(0)  # No default radio button selected
                                                       )
                                      ),
                                      
                                      conditionalPanel(condition = "input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       radioButtons("DataL", 
                                                                    div(class = "label-style", "Select source of data:"),
                                                                    choiceNames = list("Estimates from a linear model"),
                                                                    choiceValues = "Linear model",
                                                                    selected = "Linear model"
                                                       )
                                      )
                            ), 
                            
                            
                            
                            
                            
                            ################################################################################
                            ### Step 3
                            ################################################################################
                            
                            wellPanel(p(h4("Step 3", create_info_button("step3info", ""))),
                                      
                                      conditionalPanel(condition = "input.Outcome == 'Dichotomous' && (input.DataD == 'Logistic model' || input.DataD == '2x2 table')",
                                                       style = "display: none;",
                                                       radioButtons("Analysis", 
                                                                    div(class = "label-style", "Select type of analysis:"),
                                                                    choiceNames = list(
                                                                      list(strong("RIR:"),
                                                                           "Generalized Robustness of Inference to Replacement/Fragility", 
                                                                           create_info_button("fragility_info", "")
                                                                      )
                                                                    ),
                                                                    choiceValues = c("RIR"),
                                                                    selected = "RIR"
                                                       )
                                      ),
                                      
                                      conditionalPanel(condition = "input.Outcome == 'Continuous' && input.DataL == 'Linear model'",
                                                       style = "display: none;",
                                                       radioButtons("AnalysisL",
                                                                    div(class = "label-style", "Select type of analysis:"),
                                                                    choiceNames = list(
                                                                      # First subheading with explicit class
                                                                      div(class = "radio-subheading", "Correlation-Based Approaches"),
                                                                      
                                                                      list(strong("ITCV:"), 
                                                                           "Impact Threshold for a Confounding Variable", 
                                                                           create_info_button("itcv_info", "")
                                                                      ),
                                                                      list("Component Correlations (With Fixed Standard Error)", 
                                                                           create_info_button("pse_info", "")
                                                                      ),
                                                                      list("Coefficient of Proportionality",
                                                                           em("(preliminary)"),
                                                                           create_info_button("cop_info", "")
                                                                      ),
                                                                      
                                                                      # Second subheading with explicit class
                                                                      div(class = "radio-subheading", "Robustness of Inference to Replacement Approach"),
                                                                      
                                                                      list(strong("RIR:"), 
                                                                           "Generalized Robustness of Inference to Replacement", 
                                                                           create_info_button("rir_info", "")
                                                                      )
                                                                    ),
                                                                    choiceValues = c("SUBHEADING1", "IT", "PSE", "COP", "SUBHEADING2", "RIR"),
                                                                    selected = character(0)  # No default radio button selected
                                                       )
                                      )
                            ),
                            
                            
                            
                            
                            
                            ################################################################################
                            ### Step 4
                            ################################################################################
                            
                            wellPanel(p(h4("Step 4", create_info_button("step4info", ""))),
                                      
                                      
                                      
                                      
                                      
                                      ################################################################################
                                      ### Conditions for ITCV and RIR
                                      ################################################################################
                                      
                                      conditionalPanel(condition = "(input.AnalysisL == 'IT' || input.AnalysisL == 'RIR') && input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       
                                                       radioButtons("Uncertainty_RIR", 
                                                                    div(class = "label-style", "Select format of input:"),
                                                                    choiceNames = 
                                                                      list("Estimated Effect", 
                                                                        list("Confidence Interval",
                                                                             create_info_button("ci_rir_info", "")
                                                                        )
                                                                      ),
                                                                    choiceValues = c("EstEff", "ConfInt"),
                                                                    selected = "EstEff"),
                                                       
                                                       
                                                       
                                                       conditionalPanel(condition = 
                                                                          "(input.AnalysisL == 'IT' || input.AnalysisL == 'RIR') && input.Outcome == 'Continuous' && input.Uncertainty_RIR == 'EstEff'",
                                                                        style = "display: none;",
                                                                        div(class = "label-style", "Enter these values (Note that decimals must be denoted with a period, e.g., 2.1):"),
                                                                        
                                                                        numericInput("est_effect_rir_ee", 
                                                                                     list("Estimated Effect", 
                                                                                          create_info_button("unstd_beta_info", "")
                                                                                     ), 
                                                                                     2, step = .1),
                                                                        numericInput("std_error_rir_ee", 
                                                                                     list("Standard Error",
                                                                                          create_info_button("std_error_info", "")
                                                                                     ),
                                                                                     .4, step = "any"),
                                                                        numericInput("n_obs_rir_ee", 
                                                                                     list("Number of Observations", 
                                                                                          create_info_button("n_obs_ee_info", "")
                                                                                     ), 
                                                                                     100, step = 1),
                                                                        numericInput("n_covariates_rir_ee", 
                                                                                     list("Number of Covariates", 
                                                                                          create_info_button("n_covariates_ee_info", "")
                                                                                     ), 
                                                                                     3, step = 1),
                                                                        
                                                                        
                                                                        column(12,
                                                                               actionButton("results_pg_l", 
                                                                                            div(icon("play", lib = "font-awesome"), 
                                                                                                " Run")),
                                                                               align = "center"),
                                                       ),
                                                       
                                                       
                                                       
                                                       conditionalPanel(condition = 
                                                                          "(input.AnalysisL == 'IT' || input.AnalysisL == 'RIR') && input.Outcome == 'Continuous' && input.Uncertainty_RIR == 'ConfInt'",
                                                                        style = "display: none;",
                                                                        div(class = "label-style", "Enter these values (Note that decimals must be denoted with a period, e.g., 2.1):"),
                                                                        
                                                                        numericInput("lower_bnd_rir_ci", 
                                                                                     list("Lower Bound",
                                                                                          create_info_button("l_bound_info", "")
                                                                                     ),
                                                                                     1, step = "any"),
                                                                        numericInput("upper_bnd_rir_ci", 
                                                                                     list("Upper Bound",
                                                                                          create_info_button("u_bound_info", "")
                                                                                     ),
                                                                                     3, step = "any"),
                                                                        numericInput("n_obs_rir_ci", 
                                                                                     list("Number of Observations", 
                                                                                          create_info_button("n_obs_ci_info", "")
                                                                                     ), 
                                                                                     100, step = 1),
                                                                        numericInput("n_covariates_rir_ci", 
                                                                                     list("Number of Covariates", 
                                                                                          create_info_button("n_covariates_ci_info", "")
                                                                                     ), 
                                                                                     3, step = 1),
                                                                        
                                                                        
                                                                        column(12,
                                                                               actionButton("results_pg_l", 
                                                                                            div(icon("play", lib = "font-awesome"), 
                                                                                                " Run")),
                                                                               align = "center")
                                                       )
                                      ),
                                      
                                      
                                      
                                      
                                      
                                      ################################################################################
                                      ### Conditions for PSE
                                      ################################################################################
                                      
                                      conditionalPanel(condition = "input.AnalysisL == 'PSE' && input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       
                                                       
                                                       radioButtons("Uncertainty_PSE", 
                                                                    div(class = "label-style", "Select format of input:"),
                                                                    choiceNames = 
                                                                      list("Estimated Effect", 
                                                                           list("Confidence Interval",
                                                                                create_info_button("ci_pse_info", "")
                                                                           )
                                                                      ),
                                                                    choiceValues = c("EstEff", "ConfInt"),
                                                                    selected = "EstEff"
                                                       ),
                                                       
                                                       conditionalPanel(condition = 
                                                                          "input.AnalysisL == 'PSE' && input.Outcome == 'Continuous' && input.Uncertainty_PSE == 'EstEff'",
                                                                        style = "display: none;",
                                                                        div(class = "label-style", "Enter these values (Note that decimals must be denoted with a period, e.g., 2.1):"),
                                                                        numericInput("est_effect_pse_ee", 
                                                                                     list("Estimated Effect",
                                                                                          create_info_button("unstd_beta_pse_info", "")
                                                                                          ), 
                                                                                     0.5, step = .1),
                                                                        numericInput("std_err_pse_ee", 
                                                                                     list("Standard Error",
                                                                                          create_info_button("std_error_pse_info", "")
                                                                                     ), 
                                                                                     0.056, step = "any"),
                                                                        numericInput("n_obs_pse_ee", 
                                                                                     list("Number of Observations",
                                                                                          create_info_button("n_obs_pse_ee_info", "")
                                                                                     ), 
                                                                                     6174, step = 1),
                                                                        numericInput("n_covariates_pse_ee", 
                                                                                     list("Number of Covariates", 
                                                                                          create_info_button("n_covariates_pse_ee_info", "")
                                                                                     ), 
                                                                                     2, step = 1),
                                                                        numericInput("sdx_pse_ee", 
                                                                                     list("Standard Deviation of the Focal Predictor",
                                                                                          create_info_button("sdx_pse_ee_info", "")
                                                                                     ), 
                                                                                     .22, step = .1),
                                                                        numericInput("sdy_pse_ee", 
                                                                                     list("Standard Deviation of the Outcome",
                                                                                          create_info_button("sdy_pse_ee_info", "")
                                                                                     ), 
                                                                                     1, step = .1),
                                                                        numericInput("R2_pse_ee",
                                                                                     list(HTML(paste0("R",tags$sup("2"))),
                                                                                          create_info_button("R2_pse_ee_info", "")
                                                                                     ), .3, step = .1),
                                                                        numericInput("eff_thr_pse_ee", 
                                                                                     list("Threshold for Inference",
                                                                                          create_info_button("eff_thr_pse_ee_info", "")
                                                                                     ), 
                                                                                     0, step = .1),
                                                                        
                                                                        column(12,
                                                                               actionButton("results_pg_pse", 
                                                                                            div(icon("play", lib = "font-awesome"), 
                                                                                                " Run")),
                                                                               align = "center"),
                                                                        
                                                       ),
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       conditionalPanel(condition = 
                                                                          "input.AnalysisL == 'PSE' && input.Outcome == 'Continuous' && input.Uncertainty_PSE == 'ConfInt'",
                                                                        style = "display: none;",
                                                                        div(class = "label-style", "Enter these values (Note that decimals must be denoted with a period, e.g., 2.1):"),
                                                                        numericInput("lower_bnd_pse_ci", 
                                                                                     list("Lower Bound", 
                                                                                          create_info_button("l_bound_pse_info", "")
                                                                                     ),
                                                                                     0.4, step = "any"),
                                                                        numericInput("upper_bnd_pse_ci", 
                                                                                     list("Upper Bound",
                                                                                          create_info_button("u_bound_pse_info", "")
                                                                                     ),
                                                                                     0.6, step = "any"),
                                                                        numericInput("n_obs_pse_ci", 
                                                                                     list("Number of Observations", 
                                                                                          create_info_button("n_obs_pse_ci_info", "")
                                                                                     ), 
                                                                                     6174, step = 1),
                                                                        numericInput("n_covariates_pse_ci", 
                                                                                     list("Number of Covariates", 
                                                                                          create_info_button("n_covariates_pse_ci_info", "")
                                                                                     ), 
                                                                                     2, step = 1),
                                                                        numericInput("sdx_pse_ci", 
                                                                                     list("Standard Deviation of the Focal Predictor",
                                                                                          create_info_button("sdx_pse_ci_info", "")
                                                                                     ), 
                                                                                     .22, step = .1),
                                                                        numericInput("sdy_pse_ci", 
                                                                                     list("Standard Deviation of the Outcome",
                                                                                          create_info_button("sdy_pse_ci_info", "")
                                                                                     ), 
                                                                                     1, step = .1),
                                                                        numericInput("R2_pse_ci", 
                                                                                     list(HTML(paste0("R",tags$sup("2"))),
                                                                                          create_info_button("R2_pse_ci_info", "")
                                                                                     ), 
                                                                                     .3, step = .1),
                                                                        numericInput("eff_thr_pse_ci", 
                                                                                     list("Threshold for Inference",
                                                                                          create_info_button("eff_thr_pse_ci_info", "")
                                                                                     ), 
                                                                                     0, step = .1),
                                                                        
                                                                        column(12,
                                                                               actionButton("results_pg_pse", 
                                                                                            div(icon("play", lib = "font-awesome"), 
                                                                                                " Run")),
                                                                               align = "center"),
                                                                        
                                                       )
                                      ),
                                                       
                                      
                                      
                                      
                                      
                                      ################################################################################
                                      ### Conditions for COP
                                      ################################################################################
                                      
                                      conditionalPanel(condition = "input.AnalysisL == 'COP' && input.Outcome == 'Continuous'",
                                                       style = "display: none;",
                                                       
                                                       radioButtons("Uncertainty_COP", 
                                                                    div(class = "label-style", "Select format of input:"),
                                                                    choiceNames = 
                                                                      list("Estimated Effect", 
                                                                           list("Confidence Interval",
                                                                                create_info_button("ci_cop_info", "")
                                                                           )
                                                                      ),
                                                                    choiceValues = c("EstEff", "ConfInt"),
                                                                    selected = "EstEff"),
                                                       
                                                       
                                                       
                                                       conditionalPanel(condition = 
                                                                          "input.AnalysisL == 'COP' && input.Outcome == 'Continuous' && input.Uncertainty_COP == 'EstEff'",
                                                                        style = "display: none;",
                                                                        div(class = "label-style", "Enter these values (Note that decimals must be denoted with a period, e.g., 2.1):"),
                                                                        
                                                                        numericInput("est_effect_cop_ee", 
                                                                                     list("Estimated Effect", 
                                                                                          create_info_button("unstd_beta_cop_ee_info", "")
                                                                                     ), 
                                                                                     -0.125),
                                                                        numericInput("std_err_cop_ee", 
                                                                                     list("Standard Error",
                                                                                          create_info_button("std_error_cop_ee_info", "")
                                                                                     ), 
                                                                                     0.050),
                                                                        numericInput("n_obs_cop_ee", 
                                                                                     list("Number of Observations", 
                                                                                          create_info_button("n_obs_cop_ee_info", "")
                                                                                     ), 
                                                                                     6265, step = 1),
                                                                        numericInput("n_covariates_cop_ee", 
                                                                                     list("Number of Covariates",
                                                                                          create_info_button("n_covariates_cop_ee_info", "")
                                                                                     ), 
                                                                                     7, step = 1),
                                                                        numericInput("sdx_cop_ee", 
                                                                                     list("Standard Deviation of the Focal Predictor",
                                                                                          create_info_button("sdx_cop_ee_info", "")
                                                                                     ), 
                                                                                     .217, step = "any"),
                                                                        numericInput("sdy_cop_ee", 
                                                                                     list("Standard Deviation of the Outcome",
                                                                                          create_info_button("sdy_cop_ee_info", "")
                                                                                     ), 
                                                                                     .991, step = "any"),
                                                                        numericInput("R2_cop_ee", 
                                                                                     list(HTML(paste0("R",tags$sup("2"))),
                                                                                          create_info_button("R2_cop_ee_info", "")
                                                                                     ), 
                                                                                     .251, step = "any"),
                                                                        numericInput("eff_thr_cop_ee", 
                                                                                     list("Threshold for Inference",
                                                                                          create_info_button("eff_thr_cop_ee_info", "")
                                                                                     ), 
                                                                                     0, step = "any"),
                                                                        numericInput("FR2max_cop_ee", 
                                                                                     list(HTML(paste0("R",tags$sup("2"),"Max")),
                                                                                          create_info_button("FR2max_cop_ee_info", "")
                                                                                     ), 
                                                                                     .61, step = "any"),
                                                                        
                                                                        column(12,
                                                                               actionButton("results_pg_cop", 
                                                                                            div(icon("play", lib = "font-awesome"), 
                                                                                                " Run")),
                                                                               align = "center"),
                                                                        
                                                       ),
                                                       
                                                       
                                                       
                                                      
                                                        
                                                       conditionalPanel(condition = 
                                                                          "input.AnalysisL == 'COP' && input.Outcome == 'Continuous' && input.Uncertainty_COP == 'ConfInt'",
                                                                        style = "display: none;",
                                                                        div(class = "label-style", "Enter these values (Note that decimals must be denoted with a period, e.g., 2.1):"),
                                                                        
                                                                        numericInput("lower_bnd_cop_ci", 
                                                                                     list("Lower Bound", 
                                                                                          create_info_button("lower_bnd_cop_ci_info", "")
                                                                                     ),
                                                                                     -0.2, step = "any"),
                                                                        numericInput("upper_bnd_cop_ci", 
                                                                                     list("Upper Bound", 
                                                                                          create_info_button("upper_bnd_cop_ci_info", "")
                                                                                     ), 
                                                                                     0, step = "any"),
                                                                        numericInput("n_obs_cop_ci", 
                                                                                     list("Number of Observations", 
                                                                                          create_info_button("n_obs_cop_ci_info", "")
                                                                                     ), 
                                                                                     6265, step = 1),
                                                                        numericInput("n_covariates_cop_ci", 
                                                                                     list("Number of Covariates",
                                                                                          create_info_button("n_covariates_cop_ci_info", "")
                                                                                     ), 
                                                                                     7, step = 1),
                                                                        numericInput("sdx_cop_ci", 
                                                                                     list("Standard Deviation of the Focal Predictor",
                                                                                          create_info_button("sdx_cop_ci_info", "")
                                                                                     ), 
                                                                                     .217, step = "any"),
                                                                        numericInput("sdy_cop_ci", 
                                                                                     list("Standard Deviation of the Outcome",
                                                                                          create_info_button("sdy_cop_ci_info", "")
                                                                                     ), 
                                                                                     .991, step = "any"),
                                                                        numericInput("R2_cop_ci", 
                                                                                     list(HTML(paste0("R",tags$sup("2"))),
                                                                                          create_info_button("R2_cop_ci_info", "")
                                                                                     ), 
                                                                                     .251, step = "any"),
                                                                        numericInput("eff_thr_cop_ci", 
                                                                                     list("Threshold for Inference (e.g., 1.96x standard error)",
                                                                                          create_info_button("eff_thr_cop_ci_info", "")
                                                                                     ), 
                                                                                     0, step = .1),
                                                                        numericInput("FR2max_cop_ci", 
                                                                                     list(HTML(paste0("R",tags$sup("2"),"Max")),
                                                                                          create_info_button("FR2max_cop_ci_info", "")
                                                                                     ), 
                                                                                     .61, step = "any"),
                                                                        
                                                                        column(12,
                                                                               actionButton("results_pg_cop", 
                                                                                            div(icon("play", lib = "font-awesome"), 
                                                                                                " Run")),
                                                                               align = "center"),
                                                                        
                                                       )
                                      ),
                                                                        
                                                                        
                                                                        
                                                                        
                                      
                                      ################################################################################
                                      ### Conditions for Logistic Model
                                      ################################################################################
                                      
                                      conditionalPanel(condition = "input.DataD == 'Logistic model' && input.Outcome == 'Dichotomous'",
                                                       style = "display: none;",
                                                       
                                                       
                                                       radioButtons("Uncertainty_log", 
                                                                    div(class = "label-style", "Select format of input:"),
                                                                    choiceNames = c("Estimated Effect", "Confidence Interval"),
                                                                    choiceValues = c("EstEff", "ConfInt"),
                                                                    selected = "EstEff"),
                                                       
                                                       
                                                       
                                                       conditionalPanel(condition = 
                                                                          "input.DataD == 'Logistic model' && input.Outcome == 'Dichotomous' && input.Uncertainty_log == 'EstEff'",
                                                                        style = "display: none;",
                                                                        div(class = "label-style", "Enter these values (Note that decimals must be denoted with a period, e.g., 2.1):"),
                                                                        
                                                                        numericInput("est_effect_log_ee", 
                                                                                     list("Estimated Effect (Log Odds)", 
                                                                                          create_info_button("unstd_beta_log_ee_info", "")
                                                                                     ), 
                                                                                     -0.2, step = .1),
                                                                        numericInput("std_error_log_ee", 
                                                                                     list("Standard Error (of the Log Odds)", 
                                                                                          create_info_button("std_error_log_ee_info", "")
                                                                                     ), 
                                                                                     0.103, step = "any"),
                                                                        numericInput("n_obs_log_ee", 
                                                                                     list("Number of Observations", 
                                                                                          create_info_button("n_obs_log_ee_info", "")
                                                                                     ), 
                                                                                     20888, step = 1),
                                                                        numericInput("n_covariates_log_ee", 
                                                                                     list("Number of Covariates", 
                                                                                          create_info_button("n_covariates_log_ee_info", "")
                                                                                     ), 
                                                                                     3, step = 1),
                                                                        numericInput("n_trm_log_ee", 
                                                                                     "Number of cases in treatment condition", 
                                                                                     17888, step = 1),
                                                                        
                                                                        column(12,
                                                                               actionButton("results_pg_di", 
                                                                                            div(icon("play", lib = "font-awesome"), 
                                                                                                " Run")),
                                                                               align = "center"
                                                                        ),
                                                                        
                                                       ),
                                                       
                                                       
                                                       
                                                       
                                                       
                                                       conditionalPanel(condition = 
                                                                          "input.DataD == 'Logistic model' && input.Outcome == 'Dichotomous' && input.Uncertainty_log == 'ConfInt'",
                                                                        style = "display: none;",
                                                                        div(class = "label-style", "Enter these values (Note that decimals must be denoted with a period, e.g., 2.1):"),
                                                                        
                                                                        numericInput("lower_bnd_log_ci", 
                                                                                     list("Lower Bound", 
                                                                                          create_info_button("lower_bnd_log_ci_info", "")
                                                                                     ), 
                                                                                     -0.4, step = "any"),
                                                                        numericInput("upper_bnd_log_ci", 
                                                                                     list("Upper Bound",
                                                                                          create_info_button("upper_bnd_log_ci_info", "")
                                                                                     ), 
                                                                                     0, step = "any"),
                                                                        numericInput("n_obs_log_ci", 
                                                                                     list("Number of Observations", 
                                                                                          create_info_button("n_obs_log_ci_info", "")
                                                                                     ), 
                                                                                     20888, step = 1),
                                                                        numericInput("n_covariates_log_ci", 
                                                                                     list("Number of Covariates", 
                                                                                          create_info_button("n_covariates_log_ci_info", "")
                                                                                     ), 
                                                                                     3, step = 1),
                                                                        numericInput("n_trm_log_ci", 
                                                                                     "Number of cases in treatment condition", 
                                                                                     17888, step = 1),
                                                                        
                                                                        column(12,
                                                                               actionButton("results_pg_di", 
                                                                                            div(icon("play", lib = "font-awesome"), 
                                                                                                " Run")),
                                                                               align = "center"),
                                                                        
                                                       )
                                      ),
                                      

                                                                            
                                      
                                      
                                      ################################################################################
                                      ### Conditions for 2x2 Table
                                      ################################################################################
                                      
                                      conditionalPanel(condition = "(input.Analysis == 'RIR' || input.Analysis == 'Fragility') &&
                                                                                         (input.DataD == '2x2 table' && input.Outcome == 'Dichotomous')",
                                                       style = "display: none;",
                                                       div(class = "label-style", "Enter these values:"),
                                                       numericInput("ctrl_fail", 
                                                                    "Control Condition: Result Failure", 
                                                                    18, step = 1),
                                                       numericInput("ctrl_success", 
                                                                    "Control Condition: Result Success", 
                                                                    12, step = 1),
                                                       numericInput("treat_fail", 
                                                                    "Treatment Condition: Result Failure", 
                                                                    12, step = 1),
                                                       numericInput("treat_success", 
                                                                    "Treatment Condition: Result Sucesss", 
                                                                    17, step = 1),
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

                        
 
                        
                                               
################################################################################
### Displaying Results
################################################################################

                        mainPanel(
                          verticalLayout(
                            fluidRow(
                              br(.noWS = "before"),
                              align = "center",
                              h3("Results")),
                              
                              wellPanel(
                                p(h4("Text Output")),
                                htmlOutput("print_results1")
                              ),
                              
                              wellPanel(
                                p(h4("Graphic Output")),
                                plotOutput("fig_results")
                              ),

                            
                            
################################################################################
                            
                            wellPanel(p(h4("Would you like to view full R output?")),
                                      checkboxInput("gen_r_output", "View Full Output from R Command"),
                                      conditionalPanel(condition = "input.gen_r_output == 1",
                                                       id = "r_output",
                                                       verbatimTextOutput("print_results2")
                                                       )),



################################################################################

                            wellPanel(
                              p(h4("Would you like to generate source code?")),
                              
                              # Row for checkboxes: 2 columns across
                              fluidRow(
                                # Left column for R checkboxes
                                column(
                                  width = 6,
                                  checkboxInput("gen_r_code", "Generate R Code"),
                                  checkboxInput("gen_r_code_default", "Generate R Code (Advanced Defaults)")
                                ),
                                # Right column for Stata checkboxes
                                column(
                                  width = 6,
                                  checkboxInput("gen_stata_code", "Generate Stata Code"),
                                  checkboxInput("gen_stata_code_default", "Generate Stata Code (Advanced Defaults)")
                                )
                              ),
                              
                              # --- SINGLE large area for R code (only if one of the R checkboxes is checked) ---
                              conditionalPanel(
                                condition = "input.gen_r_code == 1 || input.gen_r_code_default == 1",
                                fluidRow(
                                  column(
                                    width = 12,
                                    
                                    # Simple R code output (shown only if gen_r_code == 1)
                                    conditionalPanel(
                                      condition = "input.gen_r_code == 1",
                                      verbatimTextOutput("r_code_print"), 
                                      uiOutput("clip"),
                                      tippy_this(
                                        "clip",
                                        tooltip = "<span style='font-size:14px; color:#fff;'>Copied!</span>",
                                        trigger = "click",
                                        placement = "right",
                                        arrow = "true"
                                      )
                                    ),
                                    
                                    # Advanced R code output (shown only if gen_r_code_default == 1)
                                    conditionalPanel(
                                      condition = "input.gen_r_code_default == 1",
                                      verbatimTextOutput("r_code_print_default"), 
                                      uiOutput("clip_r_default"),
                                      tippy_this(
                                        "clip_r_default",
                                        tooltip = "<span style='font-size:14px; color:#fff;'>Copied!</span>",
                                        trigger = "click",
                                        placement = "right",
                                        arrow = "true"
                                      )
                                    )
                                  )
                                )
                              ),
                              
                              # --- SINGLE large area for Stata code (only if one of the Stata checkboxes is checked) ---
                              conditionalPanel(
                                condition = "input.gen_stata_code == 1 || input.gen_stata_code_default == 1",
                                fluidRow(
                                  column(
                                    width = 12,
                                    
                                    # Simple Stata code output
                                    conditionalPanel(
                                      condition = "input.gen_stata_code == 1",
                                      verbatimTextOutput("stata_code_print"), 
                                      uiOutput("clip_stata"),
                                      tippy_this(
                                        "clip_stata",
                                        tooltip = "<span style='font-size:14px; color:#fff;'>Copied!</span>",
                                        trigger = "click",
                                        placement = "right",
                                        arrow = "true"
                                      )
                                    ),
                                    
                                    # Advanced Stata code output
                                    conditionalPanel(
                                      condition = "input.gen_stata_code_default == 1",
                                      verbatimTextOutput("stata_code_print_default"), 
                                      uiOutput("clip_stata_default"),
                                      tippy_this(
                                        "clip_stata_default",
                                        tooltip = "<span style='font-size:14px; color:#fff;'>Copied!</span>",
                                        trigger = "click",
                                        placement = "right",
                                        arrow = "true"
                                      )
                                    )
                                  )
                                )
                              )
                            ),


                         
################################################################################

                            column(12,
                                   screenshotButton(inputId = "screenshot_button",
                                                    label = " Take Screenshot"),
                                   actionButton(inputId = "startover_button", 
                                                label = div(icon("rotate-right", lib = "font-awesome"), 
                                                            " Start Over")),
                                   align = "right"
                            )
                            
                            
                          )
                        )
                      )
             ),
   

 





         
################################################################################
### Second Tab
################################################################################
            
tabPanel(div(icon("screwdriver-wrench", lib = "font-awesome"), " Resources"),
         
         tags$h4("Overall"),
         tags$ul(
           tags$li(icon("globe", lib = "font-awesome", style = "color: #639dad"), 
                   "Learn more on the",
                   tags$a(href="https://konfound-it.org/", "KonFound-It website.")),
           tags$li(icon("video", lib = "font-awesome", style = "color: #639dad"), 
                   "Watch the",
                   tags$a(href="https://youtu.be/Yb3K5kJ2ftk", "Introduction to Sensitivity Analysis"),
                   "video."),
           tags$li(icon("paper-plane", lib = "font-awesome", style = "color: #639dad"), 
                   "Questions? Issues? Suggestions? Reach out through the",
                   tags$a(href="https://groups.google.com/g/konfound-it", "KounFound-It! Google Group.")
           )
         ),
         hr(),
         
         
         tags$h4("Tools"),
         tags$ul(
           tags$li(icon("r-project", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://CRAN.R-project.org/package=konfound", 
                          "R package (CRAN version)")),
           tags$li(icon("r-project", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://github.com/konfound-project/konfound", 
                          "R package (development version)")),
           tags$li(icon("calculator", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://konfound-project.shinyapps.io/konfound-it/", 
                          "R Shiny app")),
           tags$li(icon("stripe-s", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://doi.org/10.1177/1536867X19874223", 
                          "Stata package")),
           tags$li(icon("warehouse", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://konfound-project.shinyapps.io/wwc-sensitivity-benchmark/", 
                          "Benchmarks: What Works Clearinghouse"))
         ),
         hr(),
         
         
         tags$h4("Explanatory Resources"),
         tags$ul(
           tags$li(icon("globe", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://konfound-it.org/page/faq/", "FAQ")), 
           tags$li(icon("images", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://konfound-it.org/page/resources/overview-of-techniques.pptx", "Overview of KonFound techniques")),
           tags$li(icon("file-pdf", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/33zkk861g04hocf/Overview%20of%20Konfound%20commands%20with%20inputs%20and%20outputs.docx?dl=0", "Overview of KonFound commands")),
           tags$li(icon("file-pdf", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://drive.google.com/file/d/1qbRx2penqzb7kEJkxJD3ARf26CjXMhzg/view", "Quick examples")), 
           tags$li(icon("images", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/o67e7w2sm8uww18/quantifying%20the%20robustness%20of%20causal%20inferences%20combined%20frameworks%20for%20stat%20horizons%20distribute.pptx?dl=0", "Slides quantifying the robustness of causal inferences combined frameworks")),
           tags$li(icon("images", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/8t6x00mokkljksh/quantifying%20the%20robustness%20of%20causal%20inferences%20%20comparison%20of%20frameworks.pptx?dl=0", "Slides for comparison of frameworks"))
         ),
         hr(),
         
         
         tags$h4("Resources for Publication"),
         tags$ul(
           tags$li(icon("file-pdf", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/bc4ert79kgbsac4/Examples%20of%20applications%20of%20indices%20for%20quantifying%20the%20robustness%20of%20causal%20inferences.docx?dl=0", "Published empirical examples")),
           tags$li(icon("table", lib = "font-awesome", style = "color: #639dad"),
                   tags$a(href="https://www.dropbox.com/s/accoz5xu82vy27v/KonFound-it%21%20enhanced.xlsx?dl=0", "Spreadsheet for calculating indices (KonFound-it!)"))
         ),
         hr(),
         
         
         tags$h4("Publications: Impact Threshold for a Confounding Variable"),
         tags$ul(  
           tags$li("Frank, K. (2000). Impact of a confounding variable on the inference of a regression coefficient.",
                   tags$i("Sociological Methods and Research, 29"),
                   "(2), 147-194. | ",
                   tags$a(href="https://drive.google.com/file/d/1F7oGYZ8SS8hnZxSI3Dch_w65Qz6KIRdI/view", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1177/0049124100029002001", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Frank, K. A., Sykes, G., Anagnostopoulos, D., Cannata, M., Chard, L., Krause, A., & McCrory, R. (2008). Does NBPTS certification affect the number of colleagues a teacher helps with instructional matters?.",
                   tags$i("Educational Evaluation and Policy Analysis, 30"),
                   "(1), 3-30. | ",
                   tags$a(href="https://drive.google.com/file/d/1aOvAXEVnQCe9-dbWkgTqtq56Y3Z1tpkg/view", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.3102/0162373707313781", icon("globe", lib = "font-awesome"), "Web version")
           )
         ),
         hr(),
         
         
         tags$h4("Publications: Robustness of Inference to Replacement"),
         tags$ul(  
           tags$li("Frank, K. A., Lin, Q., Maroulis, S., Mueller, A. S., Xu, R., Rosenberg, J. M., Hayter, C. S., Mahmoud, R. A., Kolak, M., Dietz, T., & Zhang, L. (2021). Hypothetical case replacement can be used to quantify the robustness of trial results.",
                   tags$i("Journal of Clinical Epidemiology, 134"),
                   ", 150-159. (authors listed alphabetically.)  | ",
                   tags$a(href="https://www.dropbox.com/s/2dzkvalwmgr5v5z/Hypothetical%20case%20replacement%20can%20be%20used%20to%20quantify%20the%20robustness%20of%20trial%20results%20submit.docx?dl=0", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1016/j.jclinepi.2021.01.025", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Frank, K. A., Maroulis, S. J., Duong, M. Q., & Kelcey, B. M. (2013). What would it take to change an inference? Using Rubin’s causal model to interpret the robustness of causal inferences.",
                   tags$i("Educational Evaluation and Policy Analysis, 35"),
                   "(4), 437-460. | ",
                   tags$a(href="https://drive.google.com/file/d/1aGhxGjvMvEPVAgOA8rrxvA97uUO5TTMe/view", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.3102/0162373713493129", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Frank, K. A., & Min, K. (2007). Indices of robustness for sample representation.",
                   tags$i("Sociological Methodology. 37"),
                   "(1). 349-392. (equal first authors.) | ",
                   tags$a(href="https://www.dropbox.com/s/o0rmduhe8pj3khd/INDICES%20OF%20ROBUSTNESS%20FOR%20SAMPLE%20REPRESENTATION.pdf?dl=0", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1111/j.1467-9531.2007.00186.x", icon("globe", lib = "font-awesome"), "Web version")
           )
         ),                       
         hr(),
         
         
         tags$h4("Publications for Both Frameworks"),
         tags$ul(
           tags$li("Frank, K.A., Lin, Q., Xu, R., Maroulis, S.J., Mueller, A. (2023). Quantifying the robustness of causal inferences: Sensitivity analysis for pragmatic social science.",
                   tags$i("Social Science Research, 110"),
                   ", 102815. | ",
                   tags$a(href="https://www.dropbox.com/s/rn8a4jbxtiynefh/Quantifying%20the%20Robustness%20of%20Causal%20Inferences%20Frank%20SSR%20final.pdf?dl=0", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1016/j.ssresearch.2022.102815", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Narvaiz, S., Lin, Q., Rosenberg, J. M., Frank, K. A., Maroulis, S. J., Wang, W., & Xu, R. (2024). konfound: An R sensitivity analysis package to quantify the robustness of causal inferences.",
                   tags$i("Journal of Open Source Software, 9"),
                   "(95), 5779. | ",
                   tags$a(href="https://doi.org/10.21105/joss.05779", icon("globe", lib = "font-awesome"), "Web version")
           ),
           tags$li("Xu, R., Frank, K. A., Maroulis, S. J., & Rosenberg, J. M. (2019). konfound: Command to quantify robustness of causal inferences.",
                   tags$i("The Stata Journal, 19"),
                   "(3), 523–550. | ",
                   tags$a(href="https://www.researchgate.net/profile/Ran-Xu-6/publication/335956720_konfound_Command_to_quantify_robustness_of_causal_inferences/links/5e49a3d2a6fdccd965ac3564/konfound-Command-to-quantify-robustness-of-causal-inferences.pdf", icon("file-pdf", lib = "font-awesome"), "PDF post-print"), " | ",
                   tags$a(href="https://doi.org/10.1177/1536867X19874223", icon("globe", lib = "font-awesome"), "Web version")
           )
         ),
         hr(),
         
         tags$h4("Connect"),
         tags$ul(
           tags$li("Project Overview and Details: Peruse the", 
                   tags$a(href="https://konfound-it.org/", "KonFound-It! Website")),
           tags$li("Frequently Asked Questions: Check the", 
                   tags$a(href="https://konfound-it.org/page/faq/", "FAQ page"),
                   "|",
                   tags$a(href="https://www.dropbox.com/s/9eymdekym5g50o7/frequently%20asked%20questions%20for%20application%20of%20konfound-it.docx?dl=0", "FAQ dev version")),
           tags$li("Specific Questions: Ask in the", tags$a(href="https://groups.google.com/g/konfound-it", "KounFound-It! Google Group")),
           tags$li("Issues with the konfound R Package: Post to", tags$a(href="https://github.com/konfound-project/konfound/issues", "konfound GitHub Issues")),
           tags$li("Overall KonFound-It! Project Inquiries: Contact", tags$a(href="https://msu.edu/~kenfrank/", "Ken Frank")),
           tags$li("Sensitivity Analysis Benchmarks - What Works Clearinghouse: Contact", tags$a(href="http://www.public.asu.edu/~smarouli/Spiro_Maroulis/Home.html", "Spiro Maroulis")),
           tags$li("R Package: Contact", tags$a(href="https://www.linkedin.com/in/qinyun-lin-b72763112/", "Qinyun Lin")),
           tags$li("R Shiny App: Contact", tags$a(href="https://joshuamrosenberg.com/", "Joshua Rosenberg")),
           tags$li("Stata Package: Contact", tags$a(href="https://sites.google.com/site/ranxupersonalweb/", "Ran Xu")),
           tags$li("Practical Guide: Contact", tags$a(href="https://www.cgu.edu/people/guan-saw/", "Guan Saw")),
           tags$li("Website: Contact", tags$a(href="https://bretsw.com", "Bret Staudt Willet"))
         )
                      
             ),


################################################################################

  ),
hr(),

################################################################################


tags$p(tags$b("To cite this application: ")),
tags$p(
       "Rosenberg, J. M., Narvaiz, S., Xu, R., Lin, Q., Maroulis, S., Frank, K. A., Saw, G., & Staudt Willet, K. B. (2024).",
       tags$i("Konfound-It!: Quantify the robustness of causal inferences"), 
       "[R Shiny app built on konfound R package version 1.0.2]."
),

hr(),

tags$image(style = "height:3.5em; vertical-align:center;", src = "ies-logo.jpg", alt = "IES logo"),
tags$p("KonFound-It! is supported by", tags$b("IES Grant"),
       tags$a(href="https://ies.ed.gov/funding/grantsearch/details.asp?ID=5723", "#R305D220022"),
       "— 'Quantifying the Robustness of Causal Inferences: Extensions and Application to Existing Databases' "),

hr(),

tags$p(tags$b(paste("\u00A9", format(Sys.Date(), "%Y"))), "by KonFound-It!")

  )
)
