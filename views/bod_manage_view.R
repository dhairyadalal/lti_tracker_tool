tabItem(
  
  tabName = "bod_manage_view", 
  h1("Set BOD LTI Match Percent"),
  "Warning, updating BOD percent will affect all records.",
  
  box(h3("Update Percent"), 
      selectInput("bod_year", label = "Select Year", choices = bod_match_tbl$year,
                  selected = format(Sys.Date(),"%Y") ),
      numericInput("bod_perc", label ="BOD Match (%)", min = 0, max = 50, value = 50),
      actionButton("bod_submit", label = "Update BOD %")
  ),
  box( h3("BOD Match Percent by Year"), 
       actionButton("bod_refresh", label = "Refresh Table"),
       br(),br(),
       DT::dataTableOutput("bod_mg_tbl"))
) # End tabItem
