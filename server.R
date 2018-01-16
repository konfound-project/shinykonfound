# server.R

library(shiny)
library(konfound)

shinyServer(function(input, output) {
    
    df_brief <- eventReactive(input$button, {
        pkonfound_output <- pkonfound(as.numeric(input$unstd_beta), 
                                      as.numeric(input$std_error), 
                                      as.numeric(input$n_obs), 
                                      as.numeric(input$n_covariates),
                                      to_return = "raw_output")
        pkonfound_output <- pkonfound_output[, -c(2, 4, 5, 6)] # removing replace null cases, beta_threshold, and omitted variable corr
        
    })
    
    output$brief_text <- renderTable({
        x <- df_brief()
        names(x) <- c("action", "pct_bias", "omit_corr", "itcv")    
        x
    })
    
    df <- eventReactive(input$button, {
        capture.output(pkonfound(as.numeric(input$unstd_beta), 
                                 as.numeric(input$std_error), 
                                 as.numeric(input$n_obs), 
                                 as.numeric(input$n_covariates)))
    })
    
    output$text1 <- renderText({
        all <- df()
        all[1:3]
    })
    
    output$text2 <- renderText({
        all <- df()
        all[4:7]
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
