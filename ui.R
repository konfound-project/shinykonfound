# ui.R

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("konfound: Sensitivity analysis based on Rubin's causal model"),

  p("For more information visit ",  
    tags$a(href="https://msu.edu/~kenfrank/", "https://msu.edu/~kenfrank/"), "and",
    tags$a(href="https://jrosen48.github.io/konfound/", "https://jrosen48.github.io/konfound/")),
  
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
        verbatimTextOutput("text")
    )
  )
))
