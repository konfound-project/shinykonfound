# ui.R

library(shiny)

shinyUI(fluidPage(theme = shinythemes::shinytheme("cerulean"),
                  
                  tags$head(includeScript("google-analytics.js")),
                  
                  # Application title
                  titlePanel("KonFound-It: Interactive application (and Stata procedure and R package) to carry out sensitivity analysis"),
                  
                  p("KonFound-It takes four values - the estimated effect (such as an unstandardized regression coefficient), its standard error, the number of observations, and the number of covariates. KonFound-It returns output in the forms of publishable statements as well as figures to support the interpretation of the output."),
                  tags$b("Change or set any of the values below and then click run to see output from KonFound-It!"),
                  tags$br(),
                  tags$br(),
                  # Sidebar with a slider input for number of bins
                  sidebarLayout(
                      sidebarPanel(
                          numericInput("unstd_beta", "Estimated Effect", "2"),
                          numericInput("std_error", "Standard Error", ".4"),
                          numericInput("n_obs", "Number of Observations", "100"),
                          numericInput("n_covariates", "Number of Covariates", "3"),
                          actionButton("button", "Run")
                      ),
                      
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Results (Printed)", 
                                       tags$br(),
                                       htmlOutput("text1"),
                                       tags$br(),
                                       htmlOutput("text2"), 
                                       tags$br()),
                              tabPanel("Threshold Plot", 
                                       tags$br(), 
                                       plotOutput("plot1", width = "50%")), 
                              tabPanel("Correlation Plot", 
                                       tags$br(), 
                                       plotOutput("plot2", width = "50%")), 
                              tabPanel("More Information", 
                                       tags$br(),
                                       tags$ul(
                                           tags$b("Resources:"),
                                           p(),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/research.htm#causal", "Ken Frank's homepage")), 
                                           tags$li(tags$a(href="https://jrosen48.github.io/konfound/", "konfound (in-development) R package")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/Introduction%20to%20causal%20inference.doc", "Summary statement on approach to sensitivity analysis")), 
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/Quick%20Examples%20of%20Quantifying%20the%20Robustness%20to%20Invalidate.pptx", "Quick examples")), 
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20replacement%20of%20cases%202.pptx", "Powerpoint for replacement of cases")), 
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20for%20regression.pptx", "Powerpoint for correlation framework")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20%20comparison%20of%20frameworks.pptx", "Powerpoint for comparison of frameworks")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/KonFound-it!.xlsx", "Spreadsheet for calculating indices (KonFound-it!)")),
                                           tags$b("Publications:"),
                                           p(),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/What%20would%20it%20take%20to%20Change%20an%20Inference%20published.docx", "Frank and colleagues' (2013) paper in EEPA")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/Does%20NBPTS%20Certification%20Affect%20the%20Number%20of%20Colleagues%20a%20Teacher%20Helps%20with%20Instructional%20Matters%20acceptance%20version%202.doc", "Frank and colleagues' (2008) paper in EEPA")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/impact%20of%20a%20confounding%20variable.pdf", "Frank's (2000) paper in SMR"))
                                       ),
                                       tags$br()
                              ),
                              tabPanel("Stata and R",
                                       tags$br(),
                                       h4("R"),
                                       tags$a(href="https://jrosen48.github.io/konfound/", "More information on use in R can be found here."),
                                       tags$br(),
                                       tags$br(),
                                       p("For R (presently in-development), issue the following commands:"),
                                       tags$code("# install.packages('devtools')"),
                                       tags$br(),
                                       tags$code("devtools::install_github('jrosen48/konfound')"),
                                       tags$br(),
                                       tags$code("library(konfound)"),
                                       tags$br(),
                                       tags$br(),
                                       tags$p("Then you use the following functions for already-published studies, models (including mixed effects models) fit in R, and meta-analyses:"),
                                       tags$code("pkonfound()"),
                                       tags$br(),
                                       tags$code("konfound()"),
                                       tags$br(),
                                       tags$code("mkonfound()"),
                                       tags$br(),
                                       tags$br(),
                                       h4("STATA"),
                                       tags$a(href="https://msu.edu/~kenfrank/research.htm", "More information on use in STATA can be found here."),
                                       tags$br(),
                                       tags$br(),
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
                                       tags$br()
                              )
                          )
                      )
                  ),
                  tags$i("To cite this KonFound-It interactive application:"),
                  p(),
                  p(" Rosenberg, J. M., Xu, R., & Frank, K. A. (2018). Konfound-It: Interactive application (and Stata procedure and R package) to carry out sensitivity analysis. http://konfound-it.com."),
                  p(),
                  tags$i("More information can be found in Frank (2000) and  Frank, Maroulis, Duong, and Kelcey (2013):"),
                  p(),
                  p("Frank, K. 2000. “Impact of a Confounding Variable on the Inference of a Regression Coefficient.” Sociological Methods and Research, 29(2), 147-194 https://msu.edu/~kenfrank/papers/impact%20of%20a%20confounding%20variable.pdf"),
                  p("Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. 2013. What would it take to change an inference?: Using Rubin’s causal model to interpret the robustness of causal inferences. Education, Evaluation and Policy Analysis. Vol 35: 437-460. https://msu.edu/~kenfrank/What%20would%20it%20take%20to%20Change%20an%20Inference%20published.docx"),
                  p(),
                  tags$i("Source code for this app is", 
                         a(href="https://github.com/jrosen48/shinykonfound", "here."))
))
