#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input,output){
  ###############################LOAD FILES###############################
  #####FLOAD TOP TABLE FILES######
  
  #' @title topTab
  #' @description This function loads files that contain topTable and Expression Matrix
  #' @param 
  #' @details The user must load a file in .csv format and indicate the type of separator and decimal
  topTab <- reactive({
    validate(
      need(input$file1 != "", "Please Load TopTable & Expression Matrix files")
    )
    inFile1 <- input$file1
    if (is.null(inFile1)) return(invisible(NULL))
    else {read.csv2(inFile1$datapath, sep=input$sep1, dec=input$dec1, header=TRUE)}
  })
  
  
  #####CUSTOM TOP TABLE INFORMATION######
  #'@title output topTab
  #'@description This function outputs the first 9 columns of the topTable in a data table
  #'@param
  #'@details After loading files, this function selects the columns corresponding to the topTable
  #'and outputs it in a data table
  output$topTab <- renderDataTable({
    topTab()[,c(1:9)]
  })
  
  #####CUSTOM EXPRESSION MATRIX INFORMATION######
  #'@title output exMat
  #'@description This function outputs the expression matrix in a data table
  #'@param
  #'@details After loading files, this function selects the columns corresponding to the expression matrix
  #'and outputs it in a data table
  output$expMat <- renderDataTable({
    topTab()[,c(1,10:19)]
  })
  
  ##ANALYSIS##
  ###############################GRAPHICAL ANALYSIS###############################
  
  #####FILTERING SELECTED GENES TABLE######
  #'@title selected
  #'@description This function filters the topTable by Adjusted p-value and by logFC
  #'@param
  #'@details According to the the input values introduced by the user, this function filters the topTable choosing
  #'only the genes with these values
  selected <- reactive({
  
    if(input$lfc >= 1){
      sel1 <- topTab()[topTab()[, "adj.P.Val"] <= input$pvalue, ]
      selected <- sel1[sel1[, "logFC"] >= input$lfc, ]
    }else if(input$lfc <= -1){
      sel2 <- topTab()[topTab()[, "adj.P.Val"] <= input$pvalue, ]
      selected <- sel2[sel2[, "logFC"] <= input$lfc, ]
    }
    
  })
  
  #####OUTPUT SELECTED GENES TABLE#######
  #'@title output selected table
  #'@description This function outputs the first 7 columns of the filtered topTable previously in a data table
  #'@param
  #'@details After choosing the input values of logFC and Adjusted p-value, this function selects the 7 first columns 
  #'of the genes that have these values from the topTable and output these in a data table
  output$selectedtable <- renderDataTable({
    data <- selected()[,c(1:7)]
    datatable(data, rownames = FALSE)
  })
  
  data <- reactive({
    data <- selected()[,c(1:7)]
    return(data)
  })
  
  
  ####DOWNLOAD SELECTED GENES TABLE#####
  #'@title output table1
  #'@description This function allows the user to download the selected table generated previously
  #'@param
  #'@details After generating the selected table filtering by logFC and Adjusted p-value, the user can download
  #'this table after clicking a download button
  output$table1 <- downloadHandler(
    filename = function(){
      "selectedtable.csv"},
    content <- function(file){
      write.csv(selected(),file)
    }
  )
  
  #####VOLCANO PLOT
  #'@title output volcano
  #'@description This function generates a volcano Plot plotting logFC in the x axis and -log(p-value) in the y axis.
  #'The filtered gened previously will be marked with different colours.
  #'@param
  #'@details 
  
  output$volcano <- renderPlot({
    ex<-as.data.frame(topTab())
    
    with(ex, plot(logFC, -log10(P.Value), pch=20, main="Differentially expressed genes", xlim=c(-5,5)))
    with(subset(ex, adj.P.Val<input$pvalue), points(logFC, -log10(P.Value), pch=20, col="red"))
    with(subset(ex, abs(logFC)>input$lfc), points(logFC, -log10(P.Value), pch=20, col="orange"))
    with(subset(ex, adj.P.Val<input$pvalue & abs(logFC)>input$lfc), points(logFC, -log10(P.Value), pch=20, col="green"))
    #with(subset(ex, adj.P.Val<.05 & abs(logFC)>input$lfc), textxy(logFC, -log10(P.Value), labs=SymbolsA, cex=.8))
    
    abline(v=c(-1,1))
  })
  
  #####HEATMAP#######
  #'@title output heatmap
  #'@description This function generates a heatmap from the Expression Matrix according to the filters applied previously. 
  #'@param
  #'@details After applying the filters by Adjusted p-value and logFC, a heatmap will be generated from the expression
  #'matrix. The user can change the colours of the heatmap and the shadows if he/she wants.
  output$heatmap <- renderPlot({
    #browser()
    ex<-as.data.frame(selected())
    rownames(ex) <- ex$X
    rownames(ex)
    selGenes <- ex[,c(10:19)]
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
  
  
  ###DOWNLOAD HEATMAP##############
  #'@title output plot2
  #'@description This function allows the user to download the heatmap generated previously
  #'@param
  #'@details After generating the heatmap from the expression matrix 
  #'according to the genes filtered previously, the user can download
  #'this plot clicking a download button
  
  output$plot2 <- downloadHandler(
    filename = function(){
      paste("heatmap", "png", sep = ".")
    },
    content<- function(file){
      png(file)
      
      ex<-as.data.frame(selected())
      rownames(ex) <- ex$X
      rownames(ex)
      selGenes <- ex[,c(10:19)]
      m<-data.matrix(selGenes, rownames.force = NA)
      colours <- colorRampPalette(c(input$colNum1, "grey", input$colNum2))(n = input$colorBreaks+1)
     
      y <- scale(m)
      hr <- hclust(as.dist(1-cor(t(y), method="pearson")))
      hc <- hclust(as.dist(1-cor(y, method="pearson")), method="complete")
      mycl  <- cutree(hr, h=max(hr$height)/1.5); 
      mycolhc <-  rainbow(length(unique(mycl)), start=0.1, end=0.9);
      mycolhc <-  mycolhc[as.vector(mycl)]
      reg <- heatmap.2(y,  key=T, symkey=F, Rowv=as.dendrogram(hr), Colv=as.dendrogram(hc), col=colours, 
                        scale="row", trace="none", RowSideColors=mycolhc, 
                        cexRow=0.1, cexCol=0.7)
     
     print(reg)
      dev.off()
    },
    contentType = "image/png"
  )
  
  
  
  ###############################FUNCTIONAL ANALYSIS###############################
  #'@title topGOdata
  #'@description This function generates a topGO data object in order to perform the Gene Ontology analysis
  #'@param
  #'@details According to the topTable and the selected genes table, this functions prepares a topGO data object
  #'in order to allow doing the subsequent analysis 
  topGOdata <- reactive({
    
    ttab <-as.data.frame(topTab())
    rownames(ttab) <- ttab$X
    geneID<-rownames(ttab)
    sel <- selected()
    rownames(sel) <- sel$X
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
  
  #'@title output gotable
  #'@description This function outputs a table resulting from the Gene Ontology analysis
  #'@param
  #'@details After generating the topGO data object, this function performs different statistical analysis
  #'and outputs the resulting table
  output$gotable <- renderDataTable({
    
    resultFisher <- runTest(topGOdata(), algorithm = "classic", statistic = "fisher")
    
    #resultFisher <- runTest(topGOdata(), algorithm = "weight01", statistic = "fisher")
    
    resultKS <- runTest(topGOdata(), algorithm = "classic", statistic = "ks")
    resultKS.elim <- runTest(topGOdata(), algorithm = "elim", statistic = "ks")
    
    gotable <- GenTable(topGOdata(), classicFisher = resultFisher,
                        classicKS = resultKS, elimKS = resultKS.elim,
                        orderBy = "elimKS", ranksOf = "classicFisher", topNodes = 10)
    
    #gotable <- GenTable(topGOdata(), weightFisher = resultFisher,
                                        
    #                                  orderBy = "weightFisher", ranksOf = "weightFisher", 
    #                                  topNodes = sum(score(resultFisher)<.05))
                        
    
    datatable(gotable, rownames = FALSE)
  })
  
  
  #'@title output goplot
  #'@description This function generates a plot resulting of the Gene Ontology Analysis
  #'@param
  #'@details After applying different statistical methods to the topGO data object, this function generates a plot
  #'according these results.
  output$goplot <- renderPlot({
    resultKS.elim <- runTest(topGOdata(), algorithm = "elim", statistic = "ks")
    showSigOfNodes(topGOdata(),
                   score(resultKS.elim), firstSigNodes = 5,
                   useInfo = 'all')
  })
  
  
  ###Download goplot##############
  #'@title output plot3
  #'@description This function allows the user to download the GO plot generated previously
  #'@param
  #'@details After generating the GO plot from the GO analysis,
  # the user can download this plot clicking a download button
  output$plot3 <- downloadHandler(
    filename = function(){
      paste("plot3", "png", sep = ".")
    },
    content<- function(file){
      png(file)
      
      resultKS.elim <- runTest(topGOdata(), algorithm = "elim", statistic = "ks")
      reg2 <- showSigOfNodes(topGOdata(),
                     score(resultKS.elim), firstSigNodes = 5,
                     useInfo = 'all')
      
      
      print(reg2)
      dev.off()
    },
    contentType = "image/png"
  )
  
})
