library(shiny)

shinyServer(function(input,output){
##load FILES
  #load TopTable Files
  dataT <- reactive({
    validate(
      need(input$file != "", "Please Load TopTable files")
    )
    
    read.csv(input$file$datapath,sep = ";")
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
  
  #####Custom targets information#####
  
  output$dataT <- renderDataTable({
    dataT()
  })
  
##Analysis
  ##Graphical analysis
  
  
})