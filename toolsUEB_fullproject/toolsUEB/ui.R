#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
options(spinner.color="#00ff83")
options(shiny.maxRequestSize = 70*1024^2)
options(expressions = 5000)

#libraries
source("installifnot.R")
library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library (RColorBrewer)
library(colourpicker)
library(digest)
library(gplots)
library(ggplot2)
library(ggrepel)
library(calibrate)
library(limma)
library(xtable)
library(stringi)
library(GOfuncR)
library(Homo.sapiens)



header <- dashboardHeader(
  title = "UEB Tools App",
  
  titleWidth = 250,
  tags$li(class = "dropdown",
          tags$a(href="http://ueb.vhir.org", target="_blank",
                 tags$img(height = "17px", alt="UEB logo", 
                          src="logopetit.png"))))

######################Sidebar######################
sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs", menuItem("Introduction", tabName = "intro", icon = icon("edit")),
              menuItem("Upload Files", tabName = "uploadfiles", icon = icon("server"),
                       menuSubItem("Input Data", tabName = "upfiles"),
                       menuSubItem("Tables", tabName = "tables")),
              menuItem("Analysis", tabName = "analysis", icon = icon("braille"),
                       menuItem("Graphics", tabName = "graphics",
                                menuSubItem("Selected Genes", tabName = "selectedgenes"),
                                menuSubItem("Volcano", tabName = "volcano"),
                                menuSubItem("Heatmap", tabName = "heatmap")),
                       menuItem("Functional", tabName = "functional",
                                menuSubItem("GO analysis", tabName = "goanalysis")))),
  tags$footer(style= "position:fixed; bottom:0;",
              #Inserim la imatge del logo del VHIR
              tags$a(href="http://vhir.org", target="_blank",
                     tags$img(width = "230px", alt="VHIR logo", 
                              src="VHIR.jpg")),
              br(),
              #Inserim la imatge del logo de la UEB
              tags$a(href="http://ueb.vhir.org", target="_blank",
                     tags$img(width = "230px", alt="UEB logo", 
                              src="UEBblanc.jpg")
              ),br(),
              #Inserim el mail de contacte
              div(align="center", h5(icon("envelope"),tags$a(href="mailto:ueb@vhir.org","ueb@vhir.org")))))

######################Body######################
body <- dashboardBody(
  tabItems(
    ##introduction
    tabItem(tabName = "intro",
            br(),
            h1("Introduction"),
            br(),
            tags$div(style = "text-align: justify;",
            p("The Statistics and Bioinformatics Unit (UEB) conducts studies of different types, for example,
              analysis of microarray data to select biomarkers. The results of these studies are usually given
              to the researchers in a compressed file that contains many tables (for example topTables or 
              Expression Matrix) and graphics (Heatmaps, VolcanoPlots, ...)."),
            p("After receiving their studies, researchers usually want to review them and maybe repeat some 
              analysis or specially do some some graphics."),
            p("This application allows making these changes easily, without having to redo the study from the 
              beginning."),
            br(),
            h2("How to use it?"),
            p("This section provides a short guide to run this UEB tools App."),
            br(),
            h3("Upload files"),
            h4("Input data"),
            p("The input data section, provides all the data that the app needs to run the analysis."),
            p("In the TopTable & Expression Matrix Files section, the user have to provide the path to his 
              results files. In the microarray studies, the UEB generates a results file that contains topTables
              and Expression Matrix as a result of joining the content of a topTable file and an Expression Matrix 
              file in order to facilitate the work.The resulting file have .csv format."),
            p("Below, you can choose the type of separator contained in the .csv file (tab, comma, semicolon), and
              the type of decimal (point or comma)."),
            tags$img(width = "300", alt="input data", 
                     src="inputdata.png"),
            p("The Top Table & Expression Matrix section shows the whole content of the file."),
            tags$img(width = "500", alt="express and top", 
                     src="exptop.png"),
            h4("Tables"),
            p("The topTable section shows the content of the file that corresponds to the topTable. 
              The Expression Matrix section, instead, shows the content of the file that corresponds to the 
              Expression Matrix. "),
            tags$img(width = "500", alt="top table", 
                     src="toptable.png"),
            tags$img(width = "500", alt="expression matrix", 
                     src="expressionmatrix.png"),
            
            h3("Analysis"),
            p("This section contains another two subsections: Graphics, that contains the graphical analysis, and 
              Functional, that contains the functional analysis."),
            h4("Graphics"),
            
            tags$b("Selected genes"),
            p("The Selected genes section allows the user to filter the topTable by adjusted p-value
              and by logFC."),
            p("The Selected genes from Top Table section shows the resulting table after applying the filters 
              previously commented."),
            tags$img(width = "300", alt="selected controls", 
                     src="selectedcontrols.png"),
            tags$img(width = "500", alt="selected table", 
                     src="selectedtable.png"),
            tags$div(
              br(),
            tags$b("Volcano"),
            p("This section shows a volcano plot from the topTable. It plots logFC versus -log(p-value) on the x and y
              axes, respectively. According the filters selected previously, you can see the different genes in different
              colours, respectively."),
            p("The slider below allows the user to choose how many label names wants to appear in the plot"),
            tags$img(width = "500", alt="volcano", 
                     src="volcano.png")
            ),
            tags$div(
              br(),
            tags$b("Heatmap"),
            p("The controls section has two types of inputs: the colour inputs allows the user to change the 
              heatmap colours, and the slider allows the user to change the number of colour breaks."),
            p("According to the filters selected previously, a heatmap will appear in an other box from the Expression 
              Matrix."),
            tags$img(width = "500", alt="heatmap", 
                     src="heatmap.png")
            ),
            h4("Functional"),
            p("This section contains the results of the Functional analysis."),
            tags$div(
            tags$b("Gene Ontology"),
            br(),
            p("The GO table section contains a table resulting of the study of the Gene Ontology. It shows the three types of ontology:
              Biological Process (BP), Cellular Component (CC) and Molecular Function (MF)."),
            tags$img(width = "500", alt="gotable", 
                     src="gotable.png"),
            
           
            p("The GO plot section contains a plot resulting of the study of the Gene Ontology."),
            tags$img(width = "500", alt="goplots", 
                     src="goplots.png")
            )
            )
            
    ),
    
    ##upload files
    tabItem(tabName = "upfiles", fluidRow(
      box(title = "Input Data",
          solidHeader = T, status = "success", width = 6,
           
          fluidRow(
            ####Input files####
            box(title ="TopTable & Expression Matrix FILES",
                solidHeader = T, status = "info", width = 12,
                
                fileInput("file1", "Load Express&Top files(.csv)",
                          multiple = TRUE,
                          accept = ".csv",
                          placeholder = "Please, insert the datapath of Express&Top files"),
                radioButtons('sep1', 'Separator', c('Tab' ='\t','Comma (,)' =',', 'Semicolon (;)' =';'),"\t"),
                radioButtons('dec1', 'Decimal',   c('Point (.)'='.', 'Comma (,)'=','), '.'),
                submitButton("Submit")
            )
          )
      ),
      fluidRow(
        ###Shows the data table from the input file###
        box(title = "Top Table & Expression Matrix",
            solidHeader = T, status = "info", width = 12,
            withSpinner(dataTableOutput("both")),style = "overflow-y: scroll;overflow-x:scroll")
        )
      )
    ),
    tabItem(tabName = "tables",
            fluidRow(
              ###shows the top Table###
              box(title = "Top Table",
                  solidHeader = T, status = "info", width = 12,
                  withSpinner(dataTableOutput("topTab")),style = "overflow-y: scroll;overflow-x:scroll"),
             ###shows the Expression matrix###
               box(title = "Expression matrix",
                  solidHeader = T, status = "info", width = 12,
                  withSpinner(dataTableOutput("expMat")),style = "overflow-y: scroll;overflow-x:scroll")
            )
            ),
    
    ###Analysis###
    tabItem(tabName = "analysis",
            h2("Summary of Results")
    ),
    ###Graphics###
    tabItem(tabName = "graphics",
            h2("Results of the graphical analysis")
    ),
    ###Selected genes###
    tabItem(tabName = "selectedgenes",
            
            box(title = "Toptable",
                solidHeader = T, status = "success", width = 12,
                fluidRow(
                  ###Choose filters###
                  box(title = "Selected Genes",
                      solidHeader = T, status = "info", width = 3,
                      numericInput("pvalue", "Select Adjust P Value:",value = 0.05, min = 0.001, max= 0.7),
                      numericInput("lfc", "Select logFC: ", value = -1, min = -5, max= 5), 
                      submitButton("Submit")),
                  ###Output table with selected genes###
                  box(title = "Selected Genes from Top Table",
                      solidHeader = T, status = "info", width = 9,
                      withSpinner(dataTableOutput("selectedtable")),style = "overflow-y: scroll;overflow-x:scroll",
                      downloadButton("table1", label = "Download")
                  )
                )
            )
    ),
    ###Volcano plot###
    tabItem(tabName = "volcano",
            ###Output volcano###
            box(title = "Volcano plot", 
                solidHeader = T, status="success",width = 12,
                downloadButton("plot1", label = "Download", style="float:right;"),
                withSpinner(plotOutput("volcano")),
                ###Select the number of gene names###
                sliderInput("volcano", "Number of genes:",
                            min = 1, max = 100,
                            value = 20,step = 1),
                submitButton("Submit")
                
            )),
    ###Heatmap###
    tabItem(tabName = "heatmap",
            box(title='Heatmap',
                solidHeader = T, status="success",width = 12,
                fluidRow(
                  ###Controls to apply to the heatmap###
                  box(title = 'Controls',
                      solidHeader = T, status="info",width = 4,
                      colourInput("colNum1", label = "Choose colours for heatmap", "yellow"),
                      colourInput("colNum2", label = NULL, "red"),
                      sliderInput("colorBreaks", label ="Select number of colour breaks", 
                                  min = 2, max = 128, value = 16),
                      submitButton("Submit")),
                  box(
                    ###Output heatmap###
                    solidHeader = T, status="info",width = 6,
                    withSpinner(plotOutput("heatmap"))
                    ),
                  downloadButton("plot2", label = "Download")
                ))
    ),
    ###Gene Ontology###
    tabItem(tabName = "goanalysis",
            h2("Results of the Gene Ontology analysis"),
            fluidRow(
              box(title = "GO analysis",
                  solidHeader = T, status="success",width = 12,
                  fluidRow(
                    ###Output table from the GO analysis###
                    box(title='GO table',
                        solidHeader = T, status="info",width = 12,
                        withSpinner(dataTableOutput("gotable")),
                        downloadButton("table2", label = "Download"))   
                  ),
                  ###Output plots from the GO analysis###
                  box(title='GO plots',
                      solidHeader = T, status="info",width = 12,  
                      withSpinner(plotOutput("goplot")),
                      downloadButton("plot3", label = "Download")
              )
            )
          )
        )
      )
    )

ui <- dashboardPage(header, sidebar, body, skin = "green")