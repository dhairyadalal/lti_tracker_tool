tabItem(
  tabName = "all_empl_view", 
  h1("Employee Overview"),
  
  actionButton("all_empl_refresh", label = "Refresh Table"),
  
  br(),br(),
  
  fluidRow(
    box( title = "", width = 12,
         collapsible = TRUE, solidHeader = TRUE, status = "primary",
         DT::dataTableOutput("all_empl_tbl") 
    )
  ) # End Fluid Row
) # End tabItem