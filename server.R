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
    
    p1 <- eventReactive(input$button, {
        pkonfound(as.numeric(input$unstd_beta), 
                  as.numeric(input$std_error), 
                  as.numeric(input$n_obs), 
                  as.numeric(input$n_covariates),
                  to_return = "thresh_plot")
    })
    
    output$plot1 <- renderPlot({
        p1()
    })
    
    p2 <- eventReactive(input$button, {
        pkonfound(as.numeric(input$unstd_beta), 
                  as.numeric(input$std_error), 
                  as.numeric(input$n_obs), 
                  as.numeric(input$n_covariates),
                  to_return = "corr_plot")
    })
    
    output$plot2 <- renderPlot({
        p2()
    })
    
})
