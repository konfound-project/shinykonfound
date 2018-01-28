# server.R

library(shiny)
library(konfound)

shinyServer(function(input, output) {
    
    # df_brief <- eventReactive(input$button, {
    #     
    #     unstd_beta <- as.numeric(input$unstd_beta)
    #     std_error <- as.numeric(input$std_error)
    #     n_obs <- as.numeric(input$n_obs)
    #     n_covariates <- as.numeric(input$n_covariates)
    #     
    #     pkonfound_output <- pkonfound(unstd-beta,
    #                                   std_error,
    #                                   n_obs,
    #                                   n_covariates,
    #                                   to_return = "raw_output")
    #     
    #     pkonfound_output <- pkonfound_output[, -c(2, 4, 5, 6)] # removing replace null cases, beta_threshold, and omitted variable corr
    #     
    # })
    # output$brief_text <- renderTable({
    #     x <- df_brief()
    #     names(x) <- c("action", "pct_bias", "omit_corr", "itcv")    
    #     x
    # })
    
    df <- eventReactive(input$button, {
        
        validate(
            need(is.numeric(input$unstd_beta) &
                     is.numeric(input$std_error) &
                     is.numeric(input$n_obs) &
                     is.numeric(input$n_covariates), "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number.")
        )

        validate(
            need(input$n_obs > (input$n_covariates + 1), "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")
        )
        
        capture.output(pkonfound(as.numeric(input$unstd_beta), 
                                 as.numeric(input$std_error), 
                                 as.numeric(input$n_obs), 
                                 as.numeric(input$n_covariates)))
    })
    
    output$text1 <- renderText({
        all <- df()
        all[1:3]
    })
    
    # df2 <- eventReactive(input$button, {
    #     
    #     unstd_beta <- as.numeric(input$unstd_beta)
    #     std_error <- as.numeric(input$std_error)
    #     n_obs <- as.numeric(input$n_obs)
    #     n_covariates <- as.numeric(input$n_covariates)
    #     
    #     validate(
    #         need(is.numeric(unstd_beta) &
    #                  is.numeric(std_error) &
    #                  is.numeric(n_obs) &
    #                  is.numeric(n_covariates), "Please select a data set")
    #     )
    #     
    #     capture.output(pkonfound(as.numeric(input$unstd_beta), 
    #                              as.numeric(input$std_error), 
    #                              as.numeric(input$n_obs), 
    #                              as.numeric(input$n_covariates)))
    # })
    
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
