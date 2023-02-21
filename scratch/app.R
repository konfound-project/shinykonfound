library(shiny)

shinyUI(navbarPage("My Application",
                   tabPanel("Component 1"),
                   tabPanel("Component 2"),
                   navbarMenu("More",
                              tabPanel("Sub-Component A"),
                              tabPanel("Sub-Component B"),
                              navbarMenu("Yet More",
                                         tabPanel("Subpart 1"),
                                         tabPanel("Subpart 2"))
                   )
)
)



server = function(input, output) {}

# Run the application 
