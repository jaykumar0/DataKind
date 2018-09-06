library(shiny)
library(treemap)
library(dplyr)
library(ggplot2)

d <- read.csv("C:/project/Daksh/dataWrangling/dataInput/District Courts Cases by Court, District, Case Type.csv",header=T,sep=";",as.is=T)

# remove commas in the days and convert to num
d$Pendency..Days. <-  as.numeric(gsub(",","",d$Pendency..Days.))

shinyServer(function(input, output) {

  output$stateOutput <- renderUI({
    selectInput("stateInput", "State",
                sort(unique(d$State)),
                selected = "Punjab")
  })
  
  data <- reactive({
    if (is.null(input$stateInput)) {
      return(NULL)
    }    
    
    if(input$anaType == "Case Type"){
      d %>% group_by(State,Case.Type) %>%
        summarise(TotalCases = sum(Cases)) %>% filter(State == input$stateInput) %>% rename(Type = Case.Type)
    }
    else {
      d %>% group_by(State,Court.Name) %>%
        summarise(TotalCases = sum(Cases)) %>% filter(State == input$stateInput) %>% rename(Type = Court.Name)
    }
    
    
  })

    
#  finalInput <- reactive({
#    if (!input$adjust) return(dataInput())
#    adjust(dataInput())
#  })
  
  output$plot <- renderPlot({
    if (is.null(input$stateInput)) {
      return(NULL)
    } 
    
    if(input$anaType == "Case Type"){
      # filter(q,State == "Punjab" & TotalCases >= 20)
      ggplot(data(), aes(x=sort(Type), y=TotalCases)) + 
        geom_bar(stat="identity", color = "black", fill="#0072B2") +
        ggtitle("T i t l e") +
        xlab("Case Types") +
        ylab("Total Cases") +
        theme_classic() +
        geom_text(aes(label=TotalCases), vjust=1.5, colour="white", position=position_dodge(.9), size=5)  +
        theme(plot.title=element_text(size=24),
              axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      
      
    }
    else {
      treemap(data(),
              index="Type",
              vSize="TotalCases",
              vColor="TotalCases",
              type="index",palette=input$vizClr)
      
    }
    
    
    
  })
})