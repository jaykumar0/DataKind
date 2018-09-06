library(shiny)

shinyUI(fluidPage(
  titlePanel("Daksh - Legal"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a State to Analyse."), 
      
      uiOutput("stateOutput"),
      
      br(),
      br(),      
      
      radioButtons(inputId = "anaType", 
                   label = "Analysis Type",
                   choices = c("Courts", "Case Type"),
                   selected = "Case Type"),
      
      
      radioButtons(inputId = "vizClr", 
                   label = ("Visual Colour"),
                   choices = c("Oranges", "Blues","Greens","Pastel2"),
                   selected = "Blues"),
      br(),
      br(),
      
      checkboxInput("adjust", 
        "Show for All states", value = FALSE)
    ),
    
    mainPanel(plotOutput("plot"))
  )
))