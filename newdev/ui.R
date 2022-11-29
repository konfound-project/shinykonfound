library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
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
                          tags$li("Frank(2000)"),
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
                                   tabPanel(title = "2x2 Table", #Sub-submenu Tab Title
                                            "content 2.b"),
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
