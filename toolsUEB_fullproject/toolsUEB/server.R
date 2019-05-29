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
  
  
  #' @title output both
  #' @description This function loads files that contain topTable and Expression Matrix
  #' @param 
  #' @details After loading the file,this function outputs the data from the file in a data table
  output$both <- renderDataTable({
    topTab()
    
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
  
    if(input$lfc >= 0){
      sel1 <- topTab()[topTab()[, "adj.P.Val"] <= input$pvalue, ]
      selected <- sel1[sel1[, "logFC"] >= input$lfc, ]
    }else if(input$lfc < 0){
      sel2 <- topTab()[topTab()[, "adj.P.Val"] <= input$pvalue, ]
      selected <- sel2[sel2[, "logFC"] <= input$lfc, ]
    }
    
  })
  
  #####OUTPUT SELECTED GENES TABLE#######
  #'@title output selectedtable
  #'@description This function outputs the first 7 columns of the filtered topTable previously in a data table
  #'@param
  #'@details After choosing the input values of logFC and Adjusted p-value, this function selects the 7 first columns 
  #'of the genes that have these values from the topTable and output these in a data table
  output$selectedtable <- renderDataTable({
    data <- selected()[,c(1:7)]
    datatable(data, rownames = FALSE)
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
  #'An input called volcano allows choosing the number of labels with the gene names you want to appear in the plot
  #'@param
  #'@details 
    output$volcano <- renderPlot({
    ex<-as.data.frame(topTab())
   
      if(input$lfc>=0){
      ex$volcThreshold <- ifelse(ex$adj.P.Val<input$pvalue&ex$logFC>input$lfc, "TRUE", "FALSE")
      ex$filters = "others"
      ex$filters[ex$adj.P.Val < input$pvalue] = "Adj p-value"
      ex$filters[ex$logFC > input$lfc] = "logFC"
      ex$filters[ex$adj.P.Val<input$pvalue & ex$logFC>input$lfc] = "Significant"
    }else if(input$lfc<0){
      ex$volcThreshold <- ifelse(ex$adj.P.Val<input$pvalue&ex$logFC<input$lfc, "TRUE", "FALSE")
      ex$filters = "others"
      ex$filters[ex$adj.P.Val < input$pvalue] = "Adj p-value"
      ex$filters[ex$logFC < input$lfc] = "logFC"
      ex$filters[ex$adj.P.Val<input$pvalue & ex$logFC<input$lfc] = "Significant"
    }
     
    v <- ggplot(ex, aes(x=logFC, y=-log(P.Value), color=filters, label=SymbolsA)) +
      geom_point(shape=1) + geom_text_repel(data = subset(ex, volcThreshold=="TRUE")[1:input$volcano,],
                                            size = 3, vjust = -0.25, hjust = 1.1, segment.size=0.1, col="gray20")
    
    print(v)
  })
  
  ##DOWNLOAD VOLCANO PLOT##
  #'@title output plot1
  #'@description This function allows the user to download the volcano plot generated previously
  #'@param
  #'@details After generating the volcano from the top Table 
  #'according to the genes filtered previously marked with colours in the plot, the user can download
  #'this plot clicking a download button
  output$plot1 <- downloadHandler(
    filename = function(){
      paste("volcano", "png", sep = ".")
    },
    content<- function(file){
      png(file)
      
      ex<-as.data.frame(topTab())
      
      if(input$lfc>=0){
        ex$volcThreshold <- ifelse(ex$adj.P.Val<input$pvalue&ex$logFC>input$lfc, "TRUE", "FALSE")
        ex$filters = "others"
        ex$filters[ex$adj.P.Val < input$pvalue] = "Adj p-value"
        ex$filters[ex$logFC > input$lfc] = "logFC"
        ex$filters[ex$adj.P.Val<input$pvalue & ex$logFC>input$lfc] = "Significant"
      }else if(input$lfc<0){
        ex$volcThreshold <- ifelse(ex$adj.P.Val<input$pvalue&ex$logFC<input$lfc, "TRUE", "FALSE")
        ex$filters = "others"
        ex$filters[ex$adj.P.Val < input$pvalue] = "Adj p-value"
        ex$filters[ex$logFC < input$lfc] = "logFC"
        ex$filters[ex$adj.P.Val<input$pvalue & ex$logFC<input$lfc] = "Significant"
      }
      
      v <- ggplot(ex, aes(x=logFC, y=-log(P.Value), color=filters, label=SymbolsA)) +
        geom_point(shape=1) + geom_text_repel(data = subset(ex, volcThreshold=="TRUE")[1:input$volcano,],
                                              size = 3, vjust = -0.25, hjust = 1.1, segment.size=0.1, col="gray20")
      print(v)
      dev.off()
    },
    contentType = "image/png"
  )
  

  #####HEATMAP#######
  #'@title output heatmap
  #'@description This function generates a heatmap from the Expression Matrix according to the filters applied previously. 
  #'@param
  #'@details After applying the filters by Adjusted p-value and logFC, a heatmap will be generated from the expression
  #'matrix. The user can change the colours of the heatmap and the shadows if he/she wants.
  output$heatmap <- renderPlot({
    
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
              cexRow=0.7, cexCol=0.8)
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
                        cexRow=0.7, cexCol=0.8)
     
     print(reg)
      dev.off()
    },
    contentType = "image/png"
  )
  
  
  ###############################FUNCTIONAL ANALYSIS###############################
  
  #'@title geneID
  #'@description This function returns transforms the values of the second column of the table selected into a string
  #'@param
  #'@details We need the gene names of the topTable to do the GO analysis
  geneID <- reactive({
    geneid <- as.character(selected()[,2])
    return(geneid)
  })
  
  #'@title input_hyper
  #'@description This function puts the gene IDs into a data frame
  #'@param
  #'@details
  input_hyper <- reactive({
    input_hyper = data.frame(geneID(), is_candidate=1)
  })
  
  #'@title res_hyper
  #'@description This function does the GO analysis according to the data frame created before
  #'@param
  #'@details
  res_hyper <- reactive({
    res_hyper = go_enrich(input_hyper(), n_randset=100)
  })
  
  #'@title top_gos_hyper
  #'@description 
  #'@param
  #'@details
  top_gos_hyper <- reactive({
    res_hyper()[[1]][1:20,"node_id"]
  })
  
  #'@title annogenes
  #'@description This function constructs the final table with results of the GO analysis 
  #'@param
  #'@details
  annogenes <- reactive({
    gos <- as.character(top_gos_hyper())
    genes <- as.character(selected()[,2])
    anno_genes <- get_anno_genes(go_ids=gos, genes=genes)
    final <- cbind(anno_genes, get_names(anno_genes$go_id)[,2:3])
    return(final)
  })
  
  #'@title ouput gotable
  #'@description This function outputs the results of the GO analysis into a data table
  #'@param
  #'@details
  output$gotable <- renderDataTable({
    annogenes()
  })
  
  #'@title output table2
  #'@description This function allows the user to download the GO table generated previously
  #'@param
  #'@details
  output$table2 <- downloadHandler(
    filename = function(){
      "gotable.csv"},
    content <- function(file){
      write.csv(annogenes(),file)
    }
  )

  #'@title top_gos_hyperplot
  #'@description 
  #'@param
  #'@details
  top_gos_hyperplot <- reactive({
    res_hyper()[[1]][1:20,"node_id"]
  })
 
  #'@title goplot output
  #'@description this function outputs the resulting plots of the GO analysis
  #'@param
  #'@details
  output$goplot <- renderPlot({
    plot_anno_scores(res_hyper(),top_gos_hyperplot())
  })
  
  
  #'@title output plot3
  #'@description This function allows the user to download the GO plots generated previously
  #'@param
  #'@details
  output$plot3 <- downloadHandler(
    filename = function(){
      paste("goplots", "png", sep = ".")
    },
    content<- function(file){
      png(file)
      
      reg<- plot_anno_scores(res_hyper(),top_gos_hyperplot())
      print(reg)
      dev.off()
    },
    contentType = "image/png"
  )
})