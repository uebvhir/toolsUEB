options(spinner.color="#00ff83")
options(shiny.maxRequestSize = 70*1024^2)
options(expressions = 5000)
options(repos = BiocInstaller::biocinstallRepos())
getOption("repos")


library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "UEB Tools App"
)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs", menuItem("Introduction", tabName = "intro", icon = icon("edit")),
                          menuItem("Upload Files", tabName = "upfiles", icon = icon("server")),
                          menuItem("Analysis", tabName = "analysis", icon = icon("braille"),
                                              menuSubItem("Graphics", tabName = "graphics"),
                                               menuSubItem("Statistics", tabName = "graphics")
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
            ###Input TopTable files###
            box(title ="TopTable FILES",
                solidHeader = T, status = "info", width = 6,
                
                fileInput("file", "Load TopTable files(.csv)",
                          multiple = TRUE,
                          accept = ".csv",
                          placeholder = "Please, insert the datapath of Top Table files"),
                submitButton("Submit")
            ),
            ###Input Expression Matrix files###
            box(title ="Expression Matrix FILES",
                solidHeader = T, status = "info", width = 6,
                
                fileInput("file2", "Load Expression Matrix files(.csv)",
                          multiple = TRUE,
                          accept = ".csv",
                          placeholder = "Please, insert the datapath of Matrix Expression files"),
                submitButton("Submit")
          )
        )
  
     )
)
  ##filters
   #tabItem(tabName = "filters", fluidRow(
    # box(title = "filtering",
     #    solidHeader = T, status = warning, width = 12,
         
      #   fluidRow(
       #    box(title = "filter contitions",
        #       solidHeader = T, status = "info", width = 6
               
         #      )
        
         #)
         #)
  # ))

)
)
)

ui <- dashboardPage(header, sidebar, body, skin = "green")