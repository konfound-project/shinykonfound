#server sarah
library(shiny)
library(konfound)
library(shinyjs)

shinyServer(function(input, output, session) {
  
  #DECISION MAP PLACEHOLDER ON HOME PAGE OUTPUT
  output$decisionmap <- renderImage({
    list(src = "www/decisionmap.png",
         width = "75%")
  }, deleteFile = F)
  
  #DECISION MAP ON HOME PAGE OUTPUT
  output$suggested_analysis <- renderText( {
    "pkonfound(b = xx, se = xx, df = xx, ncov = xx)"
  })
  
  # default outputs
  values <- reactiveValues(button = FALSE)
  values <- reactiveValues(button_nl = FALSE)
  values <- reactiveValues(button_t = FALSE)
  
  #2x2 USER INPUTS
  df_t <- eventReactive(input$button_t, {
    validate(
      need(is.numeric(input$ctrl_fail) &
             is.numeric(input$ctrl_success) &
             is.numeric(input$treat_fail) &
             is.numeric(input$treat_success),
           "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
      need(input$n_obs_nl > (input$n_covariates_nl + 1),
           "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")
    )
    
    out <- pkonfound(a = input$ctrl_fail, 
                     b = input$ctrl_success, 
                     c = input$treat_fail, 
                     d = input$treat_success,
                     to_return = "raw_output")
    print(out)
    out
    
  })
  
  # Non-linear output for tkonfound()
  
  output$textt1 <- renderText({
    paste0(df_t()[[1]], " ", df_t()[[2]], ". ", df_t()[[3]], " ", df_t()[[4]], " ", df_t()[[5]])
  })
  
  output$textt1_rir <- renderText({
    paste0(  
      "RIR: ",
      df_t()$RIR)
  })
  
  output$textt4i <- eventReactive(input$button_t,{
    "Values Entered (Observed Table)"
  })
  
  output$textt4 <- renderTable({
    df_t()$User_enter_value
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  output$textt5i <- eventReactive(input$button_t, {
    "Transfer Table"
  })
  
  output$textt5 <- renderTable({
    df_t()$Transfer_Table
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  
  output$textt6 <- renderText({
    df_t()[[8]]
  })
  
  output$textt7 <- renderText({
    df_t()[[9]]
  })

})
