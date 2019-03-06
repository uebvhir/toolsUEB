library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "UEB Tools App"
)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs", menuItem("Introduction", tabName = "intro", icon = icon("edit")),
                          menuItem("Upload Files", tabName = "upfiles", icon = icon("server")),
                          menuItem("Filters", tabName = "filters", icon = icon("filter")),
                          menuItem("Volcano Plot", tabName = "volcano", icon = icon("braille")),
                          menuItem("Heatmap", tabName = "heatmap", icon = icon("braille")),
                          menuItem("Output Results", tabName = "res", icon = icon("eye"))
)
)

body <- dashboardBody()