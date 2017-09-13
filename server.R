# server.R

library(shiny)
library(konfound)

shinyServer(function(input, output) {
    
    df <- eventReactive(input$button, {
        pkonfound(as.numeric(input$unstd_beta), 
                  as.numeric(input$std_error), 
                  as.numeric(input$n_obs), 
                  as.numeric(input$n_covariates))
    })
    
    output$text <- renderPrint({
        df()
    })
    
    p <- eventReactive(input$button, {
        pkonfound(as.numeric(input$unstd_beta), 
                  as.numeric(input$std_error), 
                  as.numeric(input$n_obs), 
                  as.numeric(input$n_covariates),
                  to_return = "plot")
    })
    
    output$plot <- renderPlot({
        p()
    })
    
})
