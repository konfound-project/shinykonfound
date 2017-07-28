# server.R

library(shiny)
library(konfound)

shinyServer(function(input, output) {
    
    output$text1 <- renderText({ 
        "You have selected this"
    })
    
}
)
