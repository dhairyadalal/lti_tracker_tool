tabItem(
  tabName = "yearly_view", 
  h1(htmlOutput("year_title")),
  br(),
  h3("Summary"),
  
  fluidRow(
    infoBoxOutput("yv_total_lti_spend"),
    infoBoxOutput("yv_lti_grants_awarded"),
    infoBoxOutput("yv_employee_count")
  ), # End row
  
  br(),
  
  fluidRow(
    box( title = strong("Grant Details"), width = 12,
         collapsible = TRUE, solidHeader = TRUE,
         status = "primary",
         box( width = 6, title = strong("Expiring Grants"),
              solidHeader = TRUE, status = "warning",
              DT::dataTableOutput("exp_grants") 
             ), # End Box
         box(width = 6, title = strong("Newly Issued Grants"),
             solidHeader = TRUE, status = "success",
             DT::dataTableOutput("new_grants") 
         ) # End Box
    )
  ), # End Fluid Row     

  fluidRow(
    box( title = strong("Yearly Spend Breakdown"), width = 12,
         downloadButton(outputId = "yv_download_details", label = "Download", class = NULL),
         collapsible = TRUE, solidHeader = TRUE, status = "primary",
         DT::dataTableOutput("yv_grant_details") 
    )
  ) # End Fluid Row  
  
) # End tabItem
