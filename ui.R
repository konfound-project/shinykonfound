# ui.R

library(shiny)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("konfound: Interactive application (and Stata procedure and R package) to quantify how robust inferences are to potential sources of bias or confounding variables"),
    
    p("The goal of konfound is to carry out sensitivity analysis as described in Frank, Maroulis, Duong, and Kelcey (2013) and in Frank (2000) based on Rubin's (1974) causal model."),
    p("Use this interactive application with output from already-completed analyses. For example, you could use the unstandardized beta coefficient (for the estimated effect) and its standard error, as well as values for the number of observations and covariates, to carry out sensitivity analysis for the inference associated with the beta coefficient)."),
    p("If you are just curious about sensitivity analysis, try to change the values and see how they change the different types of output."),
    p(""),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("unstd_beta", "Estimated Effect", "2"),
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
                         tags$i("Please note that some figures (in particular Correlation Plot) may not be very clear on a mobile device."),
                         tags$br(), 
                         tags$br(),
                         htmlOutput("text1"),
                         tags$br(),
                         htmlOutput("text2")), 
                tabPanel("Results (Mobile-friendly)", 
                         tags$br(), 
                         tableOutput("brief_text"), 
                tags$em("Note."),
                tags$ul(
                    tags$li("action: what action calculated statistics apply to"),
                    tags$li("pct_bias: Percent bias to change the inference"), 
                    tags$li("omit_corr: Omitted variable correlation"), 
                    tags$li("itcv: Impact Threshold for a Confounding Variable")
                )),
                tabPanel("Threshold Plot", 
                         tags$br(), 
                         plotOutput("plot1", width = "50%")), 
                tabPanel("Correlation Plot", 
                         tags$br(), 
                         plotOutput("plot2", width = "50%")), 
                tabPanel("More Information", 
                         tags$br(),
                         p("For more information:"),
                         tags$ul(
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/research.htm#causal", "Ken Frank's homepage")), 
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/Introduction%20to%20causal%20inference.doc", "Summary statement on approach to sensitivity analysis")), 
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/What%20would%20it%20take%20to%20Change%20an%20Inference%20published.docx", "Frank and colleagues' (2013) paper in EEPA")),
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/Does%20NBPTS%20Certification%20Affect%20the%20Number%20of%20Colleagues%20a%20Teacher%20Helps%20with%20Instructional%20Matters%20acceptance%20version%202.doc", "Frank and colleagues' (2008) paper in EEPA")),
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/impact%20of%20a%20confounding%20variable.pdf", "Frank's (2000) paper in SMR")),
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/Quick%20Examples%20of%20Quantifying%20the%20Robustness%20to%20Invalidate.pptx", "Quick examples")), 
                             tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20replacement%20of%20cases%202.pptx", "Powerpoint for replacement of cases")), 
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
                         tags$a(href="https://msu.edu/~kenfrank/research.htm", "More information on use in STATA can be found here."),
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
