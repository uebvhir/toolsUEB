library(shiny)

shinyServer(function(input,output){
  
  #load TopTable Files
  dataT <- reactive({
    validate(
      need(input$file != "", "Please Load TopTable files")
    )
    
    read.csv(input$file$datapath)
    })
  
  #Load Expression Matrix Files
  genes <- reactive({
    validate(
      need(input$file2 !="", "Please load the Expression Matrix file")
    )
    
    genes1 = input$file2
    data1 = read.csv(genes1$datapath,sep = ";")
    return(data1)
  })
  
  
})