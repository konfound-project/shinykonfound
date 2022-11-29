#server
library(shiny)


shinyServer(function(input, output) {
  output$decisionmap <- renderImage({
    list(src = "www/decisionmap.png",
         width = "75%")
  }, deleteFile = F)

   

})
