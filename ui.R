library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(lubridate)

source("data_prep.R")
current_year = as.integer(format(Sys.Date(), "%Y"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    strong("Overview"),
    menuItem("All Employees Overview", tabName = "all_empl_view"),
    hr(),
    strong("LTI Report Views"),
    selectInput("yv_year", label = strong("Select Year"), 
                choices = c(2013 : current_year), 
                selected = current_year),
    
    menuItem("Yearly View", tabName = "yearly_view"),
    menuItem("Employee View", tabName = "empl_view"),
    menuItem("Monthly View", tabName = "monthly_view"),
    hr(),
    strong("Grant Management"),
    menuItem("Create New Grant", tabName = "empl_manage_view"),
    menuItem("Set BOD Percent Match", tabName = "bod_manage_view")
  ) # End sidebar Menue
) # End sidebar

dashboardPage(
  dashboardHeader(title = "LTI Tracker Tool"),
  sidebar,
  dashboardBody(
    
    tags$head(
      tags$style( HTML(".shiny-notification {  position:fixed; top: calc(25%); left: calc(50%); width = 50%; }") ) 
    ), # end tags,
    tabItems(
      source("views/all_empl_view.R", local = TRUE)$value,
      source("views/yearly_view.R", local = TRUE)$value,
      source("views/empl_view.R", local = TRUE)$value,
      source("views/monthly_view.R", local=TRUE)$value,
      source("views/empl_manage_view.R", local = TRUE)$value,
      source("views/bod_manage_view.R", local = TRUE)$value
      )) # End Tab Items
  ) # End
