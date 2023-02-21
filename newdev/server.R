#server sarah
library(shiny)
library(tidyverse)
library(konfound)
library(shinyjs)

shinyServer(function(input, output, session) {
  
  #DECISION MAP PLACEHOLDER ON HOME PAGE OUTPUT
  output$decisionmap <- renderImage({
    list(src = "www/decisionmap.png",
         width = "75%")
  }, deleteFile = F)
  
  #DECISION MAP ON HOME PAGE OUTPUT
  #REACTIVE DECISION MAP ELEMENTS ON HOME PAGE
  outcome_sel <- reactive({
    map_df %>% filter(outcome %in% input$selected_outcome)
  })
  
  observeEvent(outcome_sel(),{
    
    updateSelectInput(session, 
                      "selected_data-type",
                      choices = sort(outcome_sel()$type))
    
    data_type_sel <-reactive({outcome_sel() %>% filter(type %in% input$selected_data-type)})
    
    observeEvent(data_type_sel(),{
      updateSelectInput(session, "selected_sensitivity", choices = sort(data_type_sel()$sensi))
    })
                         
    
  })
  
  output$suggested_analysis <- renderText( {
    "pkonfound(b = xx, se = xx, df = xx, ncov = xx)"
  })
  
  # default outputs
  values <- reactiveValues(button = FALSE)
  values <- reactiveValues(button_nl = FALSE)
  values <- reactiveValues(button_t = FALSE)
  
  #2x2 USER INPUTS
  df_t <- eventReactive(input$button_t, {
    # If we unncomment this code and run as is, we get the error message it produces even though the validation should work
    # so it's a bug
    
    # validate(
    #   need(is.numeric(input$ctrl_fail) &
    #          is.numeric(input$ctrl_success) &
    #          is.numeric(input$treat_fail) &
    #          is.numeric(input$treat_success),
    #        "Did not run! Did you enter numbers for the estimated effect, standard error, number of observations, and number of covariates? Please change any of these that are not to a number."),
    #   need(input$n_obs_nl > (input$n_covariates_nl + 1),
    #        "Did not run! There are too few observations relative to the number of observations and covariates. Please specify a less complex model to use KonFound-It.")
    # )

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
  #THIS GENERATES: This function calculates the number of cases that would have to be replaced with no effect cases (RIR) to 
  #invalidate an inference made about the association. To sustain an inference, you would need to replace 8 treatment failure 
  # cases with null hypothesis cases (RIR = 8). This is equivalent to transferring 3 cases from treatment failure to treatment success.
  
  output$textt1_rir <- renderText({
    paste0(  
      "RIR: ",
      df_t()$RIR)
  })
  #THIS GENERATES: RIR: 8
  
  output$textt4i <- eventReactive(input$button_t,{
    "Values Entered (Observed Table)"
  })
  #THIS GENERATES: "Values Entered (Observed Table)"
  
  output$textt4 <- renderTable({
    df_t()$User_enter_value
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  #THIS GENERATES: The Observed Table
  
  output$textt5i <- eventReactive(input$button_t, {
    "Transfer Table"
  })
  #THIS GENERATES: "Transfer Table" title
  
  output$textt5 <- renderTable({
    df_t()$Transfer_Table
  }, digits = 0, rownames = TRUE, bordered = FALSE)
  #THIS GENERATES: The Transfer Table
  
  output$textt6 <- renderText({
    df_t()[[8]]
  })
  #THIS GENERATES: "For the User-entered Table, we have an estimated odds ratio of 2.097, with p-value of 0.196:"
  
  output$textt7 <- renderText({
    df_t()[[9]]
  })
  #THIS GENERATES: "For the Transfer Table, we have an estimated odds ratio of 2.278, with p-value of 0.051:"
  
  #NEED THIS IN ORDER TO RENDER THE OUTPUT WITHOUT CLICKING ON THE TAB FOR IT TO RENDER
  outputOptions(output, "textt1", suspendWhenHidden = FALSE)
  outputOptions(output, "textt1_rir", suspendWhenHidden = FALSE)
  outputOptions(output, "textt4i", suspendWhenHidden = FALSE)
  outputOptions(output, "textt4", suspendWhenHidden = FALSE)
  outputOptions(output, "textt5i", suspendWhenHidden = FALSE)
  outputOptions(output, "textt5", suspendWhenHidden = FALSE)
  outputOptions(output, "textt6", suspendWhenHidden = FALSE)
  outputOptions(output, "textt7", suspendWhenHidden = FALSE)

})
