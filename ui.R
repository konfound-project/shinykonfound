# ui.R

library(shiny)

# tags$head(
#     tags$style(HTML("
#                     
#                     h4 {
#                     color: #48ca3b;
#                     }
#                     
#                     ")
#     )
# ),

# instructions here: https://dev.twitter.com/web/tweet-button/web-intent
url <- "https://twitter.com/intent/tweet?text=Check%20out%20the%20Konfound-it%20web%20application%20for%20carrying%20out%20sensitivity%20analysis!&url=http://konfound-it.com/&hashtags=konfoundit,sensitivityanalysis"

shinyUI(fluidPage(theme = shinythemes::shinytheme("cerulean"),
                  
                  tags$head(includeScript("google-analytics.js")),
                  
                  # Application title
                  titlePanel("KonFound-It!"),
                  img(src='konfoundlogo-small-new.png'),
                  h3("Quantify the Robustness of Causal Inferences"),
                  p("KonFound-It! takes four values - the estimated effect (such as an unstandardized regression coefficient or the group mean difference), its standard error, the number of observations, and the number of covariates. KonFound-It returns output in the forms of publishable statements as well as figures to support the interpretation of the output."),
                  # tags$h5(""),
                  # Sidebar with a slider input for number of bins
                  sidebarLayout(
                      sidebarPanel(tags$h5("Change or set any of the values below and then click run to see output from KonFound-It!"),
                          numericInput("unstd_beta", "Estimated Effect", 2, step = .1),
                          numericInput("std_error", "Standard Error", .4, step = .1),
                          numericInput("n_obs", "Number of Observations", 100, step = 1),
                          numericInput("n_covariates", "Number of Covariates", 3, step = 1),
                          p("Please note that decimals must be denoted with a period, i.e., 2.1"),
                          actionButton("button", "Run"),
                          width = 2
                      ),
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Results (Printed)", 
                                       tags$br(),
                                       htmlOutput("text1"),
                                       tags$br(),
                                       htmlOutput("text2"), 
                                       tags$br(),
                                       tags$li(tags$a(href="https://msu.edu/~kenfrank/Examples%20of%20applications%20of%20indices%20for%20quantifying%20the%20robustness%20of%20causal%20inferences.docx", "Published empirical examples")),
                                       tags$li(tags$a(href="https://msu.edu/~kenfrank/Quantifying%20the%20Robustness%20of%20the%20Inference%20full%20write%20up%20case%20replacement%20approach.docx", "Full publishable write-up (replacement of cases)")),
                                       tags$li(tags$a(href="https://msu.edu/~kenfrank/Quantifying%20the%20Robustness%20of%20the%20Inference%20full%20write%20up%20correlation%20based%20approach.docx", "Full publishable write-up (correlation)")),
                                       p()
                              ),
                              tabPanel("Threshold Plot", 
                                       tags$br(), 
                                       plotOutput("plot1", width = "50%"), 
                                       tags$br(),
                                       tags$p("'Right click' this plot to save it")),
                              tabPanel("Correlation Plot", 
                                       tags$br(), 
                                       plotOutput("plot2", width = "50%"),
                                       tags$br(),
                                       tags$p("'Right click' this plot to save it")),
                              tabPanel("Workshops", 
                                       tags$br(), 
                                       tags$p("Upcoming workshops and trainings:"),
                                       tags$li("None scheduled right now; check with the mailing list (see More Info. & Contact tab) for details about how to join!"),
                                       tags$br()
                              ),
                              tabPanel("Add to Mobile Device", 
                                       tags$b("To add this app to your home screen, follow these instructions based on the browser and operating system you're using"),
                                       tags$p(),
                                       tags$u("Using Chrome"),
                                       tags$ol(
                                           tags$li("Browse to http://konfound-it.com"),
                                           tags$li("Open settings"),
                                           tags$li("Tap 'Add to homescreen'"),
                                           tags$li("Tap 'Add'")),
                                       tags$p(),
                                       tags$u("Using Safari"),
                                       tags$ol(
                                           tags$li("Browse to http://konfound-it.com"),
                                           tags$li("Tap the 'Share' icon"),
                                           tags$li("Tap 'Add to Home Screen'"),
                                           tags$li("Tap 'Add'")),
                                       tags$p()
                              ),
                              tabPanel("R and Stata",
                                       tags$br(),
                                       h4("R"),
                                       tags$a(href="https://jrosen48.github.io/konfound/", "More information on the R package can be found here"),
                                       tags$br(),
                                       tags$br(),
                                       p("For R (presently in-development), issue the following commands:"),
                                       tags$code('install.packages("konfound")'),
                                       tags$br(),
                                       tags$code("library(konfound)"),
                                       tags$br(),
                                       tags$br(),
                                       tags$p("Then you use the following functions for already-published studies, models (including mixed effects models) fit in R, and meta-analyses, respectively:"),
                                       tags$code("pkonfound()"),
                                       tags$br(),
                                       tags$code("konfound()"),
                                       tags$br(),
                                       tags$code("mkonfound()"),
                                       tags$br(),
                                       tags$br(),
                                       h4("STATA"),
                                       tags$a(href="https://msu.edu/~kenfrank/research.htm", "More information on the STATA module can be found here"),
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
                              ),
                              tabPanel("More Info. & Contact", 
                                       tags$br(),
                                       tags$ul(
                                           tags$b("Explanatory resources:"),
                                           p(),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/Introduction%20to%20causal%20inference.doc", "Summary statement on approach to sensitivity analysis")), 
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/Quick%20Examples%20of%20Quantifying%20the%20Robustness%20to%20Invalidate.pptx", "Quick examples")), 
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20replacement%20of%20cases%202.pptx", "Powerpoint for replacement of cases")), 
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20for%20regression.pptx", "Powerpoint for correlation framework")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/quantifying%20the%20robustness%20of%20causal%20inferences%20%20comparison%20of%20frameworks.pptx", "Powerpoint for comparison of frameworks")),
                                           p(),
                                           tags$b("Additional resources for publication:"),
                                           p(),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/Examples%20of%20applications%20of%20indices%20for%20quantifying%20the%20robustness%20of%20causal%20inferences.docx", "Published empirical examples")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/Quantifying%20the%20Robustness%20of%20the%20Inference%20full%20write%20up%20case%20replacement%20approach.docx", "Full publishable write-up (replacement of cases)")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/Quantifying%20the%20Robustness%20of%20the%20Inference%20full%20write%20up%20correlation%20based%20approach.docx", "Full publishable write-up (correlation)")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/KonFound-it!.xlsx", "Spreadsheet for calculating indices (KonFound-it!)")),
                                           p(),
                                           tags$b("Publications:"),
                                           p(),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/impact%20of%20a%20confounding%20variable.pdf", "Frank (2000)")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/papers/Does%20NBPTS%20Certification%20Affect%20the%20Number%20of%20Colleagues%20a%20Teacher%20Helps%20with%20Instructional%20Matters%20acceptance%20version%202.doc", "Frank et al. (2008)")),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/What%20would%20it%20take%20to%20Change%20an%20Inference%20published.docx", "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013)")),
                                           p(),
                                           tags$b("Contact:"),
                                           p(),
                                           tags$li(tags$a(href="https://msu.edu/~kenfrank/research.htm#causal", "Ken Frank's homepage")), 
                                           tags$li(tags$a(href = "mailto:kenfrank@msu.edu", "Email Ken Frank")),
                                           tags$li(tags$a(href="https://groups.google.com/forum/#!forum/konfound-it", "Google Groups mailing list for Konfound-It!")),
                                           p())
                              )
                          ))),
                  tags$b("Note:"),
                  p(),
                  tags$p("This is intended for academic use. We do not make any money on this and we will not sell your data. We ask for you to join the mailing list (see link below) to build a user community. We also ask that you cite this app and the publications when you use the results."),
                  p(),
                  tags$b("Mailing list:"),
                  p(),
                  tags$a(href="https://groups.google.com/forum/#!forum/konfound-it", "Google Groups mailing list for Konfound-It!"),
                  p(),
                  tags$b("To cite this application:"),
                  p(),
                  p(" Rosenberg, J. M., Xu, R., & Frank, K. A. (2018). Konfound-It!: Quantify the Robustness of Causal Inferences. http://konfound-it.com."),
                  tags$b("More information can be found in the following three publications:"),
                  p(),
                  tags$a(href="https://msu.edu/~kenfrank/papers/impact%20of%20a%20confounding%20variable.pdf", "Frank (2000)"),
                  p(),
                  tags$a(href="https://msu.edu/~kenfrank/papers/Does%20NBPTS%20Certification%20Affect%20the%20Number%20of%20Colleagues%20a%20Teacher%20Helps%20with%20Instructional%20Matters%20acceptance%20version%202.doc", "Frank et al. (2008)"),
                  p(),
                  tags$a(href = "https://msu.edu/~kenfrank/What%20would%20it%20take%20to%20Change%20an%20Inference%20published.docx", "Frank, K.A., Maroulis, S., Duong, M., and Kelcey, B. (2013)"),
                  p(),
                  tags$hr(),
                  tags$a(href=url, "Tweet", class="twitter-share-button"),
                  includeScript("http://platform.twitter.com/widgets.js"),
                  tags$hr(),
                  tags$i("Source code for this app is", 
                         a(href="https://github.com/jrosen48/shinykonfound", "here")),
                  tags$hr()
)
)
