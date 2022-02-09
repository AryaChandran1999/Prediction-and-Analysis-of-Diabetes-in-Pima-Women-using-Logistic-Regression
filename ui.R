library(shiny)
library(shinydashboard)
library(pheatmap)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "",titleWidth = 550),
    dashboardSidebar(width = 220,sidebarMenu(
      menuItem("Introduction", tabName = "intro_tab", icon = icon("dashboard"), selected = TRUE),
      menuItem("Description", icon = icon("th"), tabName = "description_tab"),
      menuItem("Data Table", tabName = "datatable_tab", icon = icon("table")),
      menuItem('Univariate Plots', tabName = "univariate_tab", icon = icon('line-chart')),
      menuItem('Bivariate Plots', tabName = "bivariate_tab", icon=icon("dashboard")),
      menuItem("Heat Map", tabName = "heatmap_tab", icon = icon("line-chart")))),
    dashboardBody()
  )
)