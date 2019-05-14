options(spinner.color="#00ff83")
options(shiny.maxRequestSize = 70*1024^2)
options(expressions = 5000)


#libraries
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library (RColorBrewer)
library(colourpicker)
library(gplots)
library(calibrate)
library(limma)
library(xtable)
library(gplots)
library(topGO)
library(ALL)
#source("http://bioconductor.org/biocLite.R")
#biocLite("limma")
#biocLite("xtable")
#biocLite("gplots")
#biocLite("topGO")
#biocLite("ALL")

header <- dashboardHeader(
  title = "UEB Tools App"
)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs", menuItem("Introduction", tabName = "intro", icon = icon("edit")),
                         menuItem("Upload Files", tabName = "upfiles", icon = icon("server")),
                         menuItem("Analysis", tabName = "analysis", icon = icon("braille"),
                                              menuSubItem("Graphics", tabName = "graphics"),
                                              menuSubItem("Functional", tabName = "functional")
                                    ))
)

body <- dashboardBody(
  tabItems(
   
     ##introduction
    tabItem(tabName = "intro",
            tags$img("Desktop/projecte/index.png"),
            br(),
            h1("UEB Tools App"),
            br(),
            p("This app allows you to upload a file and doing modifications...")
          ),
   
     ##upload files
    tabItem(tabName = "upfiles", fluidRow(
      box(title = "Input Data",
          solidHeader = T, status = "success", width = 12,
          
          fluidRow(
            ####Input TopTable files####
            box(title ="TopTable FILES",
                solidHeader = T, status = "info", width = 6,
                
                fileInput("file1", "Load TopTable files(.csv)",
                          multiple = TRUE,
                          accept = ".csv",
                          placeholder = "Please, insert the datapath of Top Table files"),
                radioButtons('sep1', 'Separator', c('Tab' ='\t','Comma (,)' =',', 'Semicolon (;)' =';'),"\t"),
                radioButtons('dec1', 'Decimal',   c('Point (.)'='.', 'Comma (,)'=','), '.'),
                submitButton("Submit")
            ),
            ####Input Expression Matrix files####
            box(title ="Expression Matrix FILES",
                solidHeader = T, status = "info", width = 6,
                
                fileInput("file2", "Load Expression Matrix files(.csv)",
                          multiple = TRUE,
                          accept = ".csv",
                          placeholder = "Please, insert the datapath of Matrix Expression files"),
                radioButtons('sep2', 'Separator', c('Tab' ='\t','Comma (,)' =',', 'Semicolon (;)' =';'),"\t"),
                radioButtons('dec2', 'Decimal',   c('Point (.)'='.', 'Comma (,)'=','), '.'),
                submitButton("Submit")
          )
        )
      ),
      fluidRow(
        box(title = "Top Table",
            solidHeader = T, status = "info", width = 12,
            withSpinner(dataTableOutput("topTab"))),
        box(title = "Expression matrix",
            solidHeader = T, status = "info", width = 12,
            withSpinner(dataTableOutput("expMat")))
      )
)),
    tabItem(tabName = "analysis",
        h2("Summary of Results")
),
        tabItem(tabName = "graphics",
           h2("Results of the graphical analysis"),
            fluidRow(
              box(title = "Toptable",
               solidHeader = T, status = "success", width = 12,
                fluidRow(
                  box(title = "Selected Genes",
                      solidHeader = T, status = "info", width = 3,
                      numericInput("pvalue", "Select Adjust P Value:",value = 0.05, min = 0.001, max= 0.7),
                      numericInput("lfc", "Select logFC: ", value = -1, min = -5, max= 5), 
                      submitButton("Submit")),
                  box(title = "Selected Genes from Top Table",
                     solidHeader = T, status = "info", width = 9,
                     withSpinner(dataTableOutput("selectedtable"))
                     )
                 )
              )
            ),
           fluidRow(
              box(title = "Volcano plot", 
                  solidHeader = T, status="success",width = 12,
                  withSpinner(plotOutput("volcano")),
                  sliderInput("volcano", "Number of genes:",
                              min = 1, max = 7000,
                              value = 5000,step = 1),
                  submitButton("Submit")
                  
                  )),
           fluidRow(
             box(title='Heatmap',
                 solidHeader = T, status="success",width = 12,
                 fluidRow(
                   box(title = 'Controls',
                       solidHeader = T, status="info",width = 3,
                       colourInput("colNum1", label = "Choose colours for heatmap", "yellow"),
                       colourInput("colNum2", label = NULL, "red"),
                       sliderInput("colorBreaks", label ="Select number of color breaks", 
                                   min = 2, max = 128, value = 16),
                       submitButton("Submit")),
                   box(title='Heatmap',
                       solidHeader = T, status="info",width = 6,
                       withSpinner(plotOutput("heatmap")))
                 ))
           )
          ),
    tabItem(tabName = "functional",
            h2("Results of the functional analysis"),
            fluidRow(
              box(title = "GO analysis",
                  solidHeader = T, status="success",width = 12,
                  fluidRow(
                    box(title='GO table',
                        solidHeader = T, status="info",width = 12,
                        withSpinner(dataTableOutput("gotable")))   
                    ),
                    box(title = "GO plot",
                        solidHeader = T, status="info",width = 12,
                        withSpinner(plotOutput("goplot",width = 900, height = 1100)))
                  )
                )
              )
            )
          )
              
ui <- dashboardPage(header, sidebar, body, skin = "green")