library(shiny)

shinyServer(function(input,output){
  
  rawData <- reactive({
    validate(
      need(input$file != "", "Please Load TopTable files")
    )
    
    read.csv(input$file$datapath)
    
  })
  
})