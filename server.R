library(shiny)

shinyServer(function(input,output){
######load FILES
  #load TopTable Files
  topTab <- reactive({
    validate(
      need(input$file1 != "", "Please Load TopTable files")
    )
    inFile1 <- input$file1
    if (is.null(inFile1)) return(invisible(NULL))
    else {read.csv2(inFile1$datapath, sep=input$sep1, dec=input$dec1, header=TRUE)}
    })
  
  #Load Expression Matrix Files
  expMat <- reactive({
    validate(
      need(input$file2 !="", "Please load the Expression Matrix file")
    )
    inFile2 <- input$file2
    if (is.null(inFile2)) return(invisible(NULL))
    else {read.csv2(inFile2$datapath, sep=input$sep2, dec=input$dec2, header=TRUE)}
  })
  
  #####Custom topTable information#####
  
  output$topTab <- renderDataTable({
    topTab()
  })
  
##Analysis
  ##Graphical analysis
  
  #####Selected genes table######
  
  selected <- reactive({
    
    if(input$pvalue == "0.05"){
      if(input$lfc == "1"){
        sel1 <- topTab()[topTab()[, "adj.P.Val"] < 0.05, ]
        selected <- sel1[sel1[, "logFC"] > 1, ]
      }else if(input$lfc == "-1"){
        sel1 <- topTab()[topTab()[, "adj.P.Val"] < 0.05, ]
        selected <- sel1[sel1[, "logFC"] < -1, ]
      }else if(input$lfc == "no filter"){
        selected <- topTab()[topTab()[, "adj.P.Val"] < 0.05, ]
      }
   }else if(input$pvalue == "0.001"){
      if(input$lfc == "1"){
        sel1 <- topTab()[topTab()[, "adj.P.Val"] < 0.001, ]
        selected <- sel1[sel1[, "logFC"] > 1, ]
      }else if(input$lfc == "-1"){
        sel1 <- topTab()[topTab()[, "adj.P.Val"] < 0.001, ]
        selected <- sel1[sel1[, "logFC"] < -1, ]
      }else if(input$lfc == "no filter"){
        selected <- topTab()[topTab()[, "adj.P.Val"] < 0.001, ]
      }
   }else if(input$pvalue == "no filter"){
     if(input$lfc == "1"){
       selected <- topTab()[topTab()[, "logFC"] > 1, ]
     }else if(input$lfc == "-1"){
       selected <- topTab()[topTab()[, "logFC"] < -1, ]
     }else if(input$lfc == "no filter"){
       selected <- topTab()
     }
    }
    
  })
  
   #####Output selected genes table#######
  
  output$selectedtable <- renderDataTable({
    data <- selected()[,c(1:6)]
    
    datatable(data, rownames = FALSE)
  })
 
  #####Volcano Plot#######

  output$volcano <- renderPlot({
    y <- as.numeric(data()$P.Value)
    x <- data()$logFC
    with(data(), plot(x, -log10(y), pch=20, main="Differentially expressed genes", xlim=c(-2.5,2),ylim=c(-2.5,2))) 
               
    #
    abline(v=c(-1,1))
  })
})