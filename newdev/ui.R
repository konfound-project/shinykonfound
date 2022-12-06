library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(tags$script(src="script.js")),
  navbarPage(title = "KonFound-It!",
             tabPanel(title = "Home",
                      h3("Welcome to KonFound-It!: Quantify the Robustness of Causal Inferences"),
                      br(),
                      h4(strong("What is KonFound-It?")),
                      p(style= "text-align: justify; font-size = 14px",
                        "KonFound-It! takes four values from many statistical analyses - the estimated effect (such as an unstandardized regression coefficient or the group mean difference), its standard error, the number of observations, and the number of covariates (and, for non-linear models, an additional value). KonFound-It returns output in the forms of publishable statements as well as figures to support the interpretation of the output."),
                      br(),
                      h4(strong("How do I use KonFound-It?")),
                      # p(style= "text-align: justify; font-size = 14px",
                      #   "SOME LANGUAGE ABOUT THE DECISION MAP. SOME LANGUAGE ABOUT NAVIGATING THE MENU TO CHOOSE THE CORRECT MODEL AND ANALYSIS TECHNIQUE."),
                      
                      selectInput("selected_framework", "What kind of output do you want?", 
                                  choices = list("RIR" = 1, "OV" = 2, selected = 1)),
                      selectInput("selected_model", "What type of model are you using?", 
                                  choices = list("Linear" = 1, "Other (haha Spiro)" = 2, selected = 1)),
                      textOutput("suggested_analysis"),
                      br(),
                      # imageOutput("decisionmap"),
                      br(),
                      h4(strong("More information on KonFound-It")),
                      p(style= "text-align: justify; font-size = 14px",
                        "More information can be found in the following publications",
                        tags$ul(
                          tags$li("Frank (2000)"),
                          tags$li("Frank et al.(2008)"),
                          tags$li("Frank et al. (2013)")
                        ),
                        "Please see the",
                        em("Documentation"),
                        "tab for more information"),
             ),
             
             ###### RIR PAGE######
             navbarMenu("RIR", #Menu Tab Title
                        
                        ###### RIR: LINEAR MODEL PAGE ######
                        tabPanel(title = "Linear Model", #Submenu Tab Title
                                 "content 2.a"),
                        
                        navbarMenu("Dichotomous Variables", #Submenu Tab Title
                                   
                                   #2x2 TABLE PAGE
                                   tabPanel(title = "2x2 Table", #Sub-submenu Tab Title
                                            sidebarPanel(tags$h5("Change any of the values below and then click run to see output from KonFound-It!"),
                                                         numericInput("ctrl_fail", "Control Condition: Result Failure", 35, step = 1),
                                                         numericInput("ctrl_success", "Control Condition: Result Success", 17, step = 1),
                                                         numericInput("treat_fail", "Treatment Condition: Result Failure", 17, step = 1),
                                                         numericInput("treat_success", "Treatment Condition: Result Sucesss", 38, step = 1),
                                                         actionButton("button_t", "Run"),
                                                         width = 2),
                                            
                                            mainPanel("Results (Using 2 x 2 table)",
                                                      tags$br(),
                                                      #textOutput("default_text_1"),
                                                      # span(textOutput("text0"), style="color:red"),
                                                      tags$br(),
                                                      uiOutput("textt1"),
                                                      tags$br(),
                                                      textOutput("textt1_rir"),
                                                      textOutput("textt2"),
                                                      tags$br(),
                                                      textOutput("textt3"),
                                                      tags$br(),
                                                      span(textOutput("textt4i"), style = "font-style: italic"),
                                                      tableOutput("textt4"),
                                                      tags$hr(),
                                                      span(textOutput("textt5i"), style = "font-style: italic"),
                                                      tableOutput("textt5"),
                                                      tags$br(),
                                                      textOutput("textt6"),
                                                      tags$br(),
                                                      textOutput("textt7"),
                                                      tags$br(),
                                                      tags$li(tags$a(href="https://msu.edu/~kenfrank/Examples%20of%20applications%20of%20indices%20for%20quantifying%20the%20robustness%20of%20causal%20inferences.docx", "Published empirical examples")),
                                                      tags$li(tags$a(href="https://msu.edu/~kenfrank/Quantifying%20the%20Robustness%20of%20the%20Inference%20full%20write%20up%20case%20replacement%20approach.docx", "Full publishable write-up (replacement of cases)")),
                                                      tags$li(tags$a(href="https://msu.edu/~kenfrank/Quantifying%20the%20Robustness%20of%20the%20Inference%20full%20write%20up%20correlation%20based%20approach.docx", "Full publishable write-up (correlation)"))
                                                      )
                                   ),
                                   
                                   
                                   tabPanel(title = "Logistic Regression", #Sub-submenu Tab Title
                                            "content 2.c"))),
             
             ###### OMITTED VARIABLES PAGE ######
             navbarMenu(title = "Omitted Variables",
                        tabPanel(title = "ITCV",
                                 "content 3.a"),
                        tabPanel(title = "Preserve Standard Error (PSE)",
                                 "content3.b"),
                        tabPanel(title = "Coefficient of Proportionality (COP)",
                                 "content 3.c"),
                        tabPanel(title = "Differential Attrition",
                                 "content 3.d")),
             
             ###### DOCUMENTATION PAGE ######
             tabPanel(title = "Documentation",
                      "content 4"),
             
             ###### BLOG PAGE ######
             tabPanel(title = "Blog",
                      "content 5"),
             
             ###### MORE PAGE ######
             navbarMenu(title = "More",
                        tabPanel(title = "Add to Mobile Device"),
                        tabPanel(title = "R and Stata"),
                        tabPanel(title = "More Info & Contact")))
  
  
)
)
