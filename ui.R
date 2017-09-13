# ui.R

library(shiny)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("konfound: Sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013)"),
    
    p("The goal of konfound is to carry out sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) based on Rubin's (1974) causal model."),
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("unstd_beta", "Unstandardized Beta Coefficient", "2"),
            textInput("std_error", "Standard Error", ".4"),
            textInput("n_obs", "Number of Observations", "100"),
            textInput("n_covariates", "Number of Covariates", "3"),
            actionButton("button", "Run")
        ),
        
        # Show text results
        mainPanel(
            tabsetPanel(
                tabPanel("Results (Printed)", 
                         tags$br(), 
                         verbatimTextOutput("text")), 
                tabPanel("Results (Plot)", 
                         tags$br(), 
                         plotOutput("plot", width = "50%")), 
                tabPanel("More Information", 
                         tags$br(),
                         p("For more information:"),
                         tags$ul(
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/research", "Ken Frank's homepage")), 
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/Introduction%20to%20causal%20inference.doc", "Summary statement on approach to sensitivity analysis")), 
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/What%20would%20it%20take%20to%20Change%20an%20Inference%20published.docx", "Frank, Maroulis, Duong, & Kelcey's (2013) paper in EEPA")),
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20replacement%20of%20cases%202.pptx", "Quick examples")), 
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/Introduction%20to%20causal%20inference.doc", "Powerpoint for replacement of cases")), 
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20for%20regression.pptx", "Powerpoint for correlation framework")),
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20%20comparison%20of%20frameworks.pptx", "Powerpoint for comparison of frameworks")),
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/KonFound-it!.xlsx", "Spreadsheet for calculating indices (KonFound-it!)"))
                             ),
                         tags$br()
                ),
                tabPanel("Stata and R Code",
                         tags$br(),
                         h4("STATA"),
                         p("For STATA, issue the following commands:"),
                         tags$code("ssc install konfound"),
                         tags$br(),
                         tags$code("ssc install moss"),
                         tags$br(),
                         tags$code("ssc install matsort"),
                         tags$br(),
                         tags$code("ssc install indeplist"),
                         tags$br(),
                         tags$br(),
                         tags$p("Then you can run a regression and quantify the sensitivity, i.e.:"),
                         tags$code("regress y1 x1 x4"),
                         tags$code("konfound x1"),
                         tags$br(),
                         tags$br(),
                         tags$a(href="https://msu.edu/~kenfrank/research", "More information on use in STATA can be found here."),
                         tags$br(),
                         h4("R"),
                         p("For R (presently in-development), issue the following commands:"),
                         tags$code("# install.packages('devtools')"),
                         tags$br(),
                         tags$code("devtools::install_github('jrosen48/konfound')"),
                         tags$br(),
                         tags$code("library(konfound)"),
                         tags$br(),
                         tags$br(),
                         tags$p("Then you use the following functions:"),
                         tags$code("pkonfound()"),
                         tags$br(),
                         tags$code("konfound()"),
                         tags$br(),
                         tags$code("mkonfound()"),
                         tags$br(),
                         tags$br(),
                         tags$a(href="https://jrosen48.github.io/konfound/", "More information on use in R can be found here."),
                         tags$br()
                )
            )
        )
    )
))
