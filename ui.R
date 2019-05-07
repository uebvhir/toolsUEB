options(spinner.color="#00ff83")
options(shiny.maxRequestSize = 70*1024^2)
options(expressions = 5000)
options(repos = BiocInstaller::biocinstallRepos())
getOption("repos")

#libraries
install.packages("shinycssloaders")
library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
source("http://bioconductor.org/biocLite.R")
biocLite("oligo")
biocLite("pd.mogene.1.0.st.v1")
biocLite("arrayQualityMetrics")
biocLite("genefilter")
biocLite("limma")
biocLite("xtable")
biocLite("gplots")


header <- dashboardHeader(
  title = "UEB Tools App"
)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs", menuItem("Introduction", tabName = "intro", icon = icon("edit")),
                         menuItem("Upload Files", tabName = "upfiles", icon = icon("server")),
                         menuItem("Analysis", tabName = "analysis", icon = icon("braille"),
                                              menuSubItem("Graphics", tabName = "graphics"),
                                              menuSubItem("Functional", tabName = "graphics2")
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
     )
)),
    tabItem(tabName = "analysis",
        h2("Summary of Results")
),

        tabItem(tabName = "graphics",
           h2("Results"),
            fluidRow(
              box(title = "Toptable",
               solidHeader = T, status = "success", width = 12,
                fluidRow(
                  box(title = "Top Table",
                     solidHeader = T, status = "info", width = 12,
                      dataTableOutput("topTab")),
                  
                  box(title = "Selected Genes",
                      solidHeader = T, status = "info", width = 3,
                      selectInput("pvalue","Adj.p-val",choices=list("no filter","0.001","0.05"), 
                                  selected = "no filter", multiple = FALSE,
                                  selectize = TRUE, width = NULL, size = NULL),
                      selectInput("lfc", "Select LFC: ", choices=list("no filter","1","-1"), 
                                  selected = "no filter", multiple = FALSE,
                                  selectize = TRUE, width = NULL, size = NULL),
                      submitButton("Submit")),
                  box(title = "Selected Genes from Top Table",
                     solidHeader = T, status = "info", width = 9,
                     dataTableOutput("selectedtable")
                     )
                 )
              ),
              box(title = "Volcano plot", 
                  solidHeader = T, status="success",width = 12,
                  withSpinner(plotOutput("volcano")),
                  sliderInput("volcano", "number of genes:",
                              min = 1, max = 10,
                              value = 2,step = 1),
                  submitButton("Submit")
                  
                  )
            )
        )
)
)
               


ui <- dashboardPage(header, sidebar, body, skin = "green")