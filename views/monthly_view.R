months = c("January", "February", "March", "April", "May", "June",
           "July", "August", "September", "October", "November", "December")
tabItem(
  tabName = "monthly_view",
  h1(htmlOutput("mv_title")),
  
  selectInput("mv_month", label = "Select Month", 
              choices = months, selected = "January"),
  
  
  fluidRow(
    # Monthly Breakdown
    box( title = strong("Monthly Summary"), width = 12,
         collapsible = TRUE, solidHeader = TRUE, status = "primary",
         infoBoxOutput("mv_total_spend"),
         
         # Breakdown table 
         box( title = strong("Monthly Breakdown"), width = 12,
           collapsible = TRUE, solidHeader = TRUE, status = "info",
           downloadButton(outputId = "mv_download", label = "Download", class = NULL),
           DT::dataTableOutput("mv_breakdown") 
         ) # 
    ) # End Monthly Breakdown Box
  )
  
 
) # End tabItem
