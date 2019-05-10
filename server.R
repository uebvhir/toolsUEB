library(shiny)

shinyServer(function(input,output){
  ###############################LOAD FILES###############################
  #####FLOAD TOP TABLE FILES######
  topTab <- reactive({
    validate(
      need(input$file1 != "", "Please Load TopTable files")
    )
    inFile1 <- input$file1
    if (is.null(inFile1)) return(invisible(NULL))
    else {read.csv2(inFile1$datapath, sep=input$sep1, dec=input$dec1, header=TRUE)}
    })
  
  #####LOAD EXPRESSION MATRIX FILES######
  expMat <- reactive({
    validate(
      need(input$file2 !="", "Please load the Expression Matrix file")
    )
    inFile2 <- input$file2
    if (is.null(inFile2)) return(invisible(NULL))
    else {read.csv2(inFile2$datapath, sep=input$sep2, dec=input$dec2, header=TRUE)}
  })
  
  #####CUSTOM TOP TABLE INFORMATION######
  output$topTab <- renderDataTable({
    topTab()
  })
  
  #####CUSTOM EXPRESSION MATRIX INFORMATION######
  output$expMat <- renderDataTable({
    expMat()
  })
  
##ANALYSIS##
  ###############################GRAPHICAL ANALYSIS###############################
  
  #####FILTERING SELECTED GENES TABLE######
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
  
  #####OUTPUT SELECTED GENES TABLE#######
  output$selectedtable <- renderDataTable({
    data <- selected()[,c(1:7)]
    datatable(data, rownames = FALSE)
  })
 
  data <- reactive({
    data <- selected()[,c(1:7)]
    return(data)
  })
  #####VOLCANO PLOT#######
  output$volcano <- renderPlot({
     ex<-as.data.frame(topTab())
     y <- as.numeric(ex$P.Value)
     x <- as.numeric(ex$logFC)
   
    with(ex[c(1:input$volcano),], plot(x, -log(y), pch=20, main="Differentially expressed genes", xlab="logFC", ylab="-log(p-value)", xlim=c(-6,6),ylim=c(-2,20))) 
    
    abline(v=c(-1,1))
  })
  
  #####HEATMAP#######
  output$heatmap <- renderPlot({
    #browser()
    ex<-as.data.frame(expMat())
    rownames(ex) <- ex$X
    rownames(ex)
    selGenes <- ex[c(1:input$heatmapSlider),]
    m<-data.matrix(selGenes, rownames.force = NA)
    colours <- colorRampPalette(c(input$colNum1, "grey", input$colNum2))(n = input$colorBreaks+1)
    par(mar = rep(2,4))
    y <- scale(m)
    hr <- hclust(as.dist(1-cor(t(y), method="pearson")))
    hc <- hclust(as.dist(1-cor(y, method="pearson")), method="complete")
    mycl  <- cutree(hr, h=max(hr$height)/1.5); 
    mycolhc <-  rainbow(length(unique(mycl)), start=0.1, end=0.9);
    mycolhc <-  mycolhc[as.vector(mycl)]
    heatmap.2(y,  key=T, symkey=F, Rowv=as.dendrogram(hr), Colv=as.dendrogram(hc), col=colours, 
              scale="row", trace="none", RowSideColors=mycolhc, 
              cexRow=0.1, cexCol=0.7)
  })
  
  ###############################FUNCTIONAL ANALYSIS###############################
  
  topGOdata <- reactive({


  ttab <-as.data.frame(topTab())
  rownames(ttab) <- ttab$X
  geneID<-rownames(ttab)
  sel <- ttab[c(1:20),]
  myInterestingGenes <- rownames(sel)
  geneList <- factor(as.integer(geneID %in% myInterestingGenes))
  names(geneList) <- geneID
    
  affyLib <- paste(annotation(ALL), "db", sep = ".")
    
  topGOdata <- new("topGOdata", ontology = "BP", allGenes = geneList,
                    nodeSize = 10,
                     annot=annFUN.db,
                     affyLib = affyLib)
  return(topGOdata)
  })
  
  output$gotable <- renderDataTable({
   
    resultFisher <- runTest(topGOdata(), algorithm = "classic", statistic = "fisher")
    
    resultKS <- runTest(topGOdata(), algorithm = "classic", statistic = "ks")
    resultKS.elim <- runTest(topGOdata(), algorithm = "elim", statistic = "ks")
    
    gotable <- GenTable(topGOdata(), classicFisher = resultFisher,
                       classicKS = resultKS, elimKS = resultKS.elim,
                       orderBy = "elimKS", ranksOf = "classicFisher", topNodes = 10)
    datatable(gotable, rownames = FALSE)
  })
  
  output$goplot <- renderPlot({
    showSigOfNodes(topGOdata(),
                   score(resultKS.elim), firstSigNodes = 5,
                   useInfo = 'all')
  })
  
})


