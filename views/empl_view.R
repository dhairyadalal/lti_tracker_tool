tabItem(
  tabName = "empl_view", 
  h1(htmlOutput("empl_title")),
  
  selectInput("ev_empl_name", label = strong("Select Employee"), 
              choices = all_empl_list, 
              selected = all_empl_list[1]),
  
  fluidRow(
    box( title = strong("Grant Summary"), width = 12,
         collapsible = TRUE, solidHeader = TRUE, status = "primary",
         DT::dataTableOutput("ev_grant_sum") 
    )
  ), # End Fluid Row    
  
  fluidRow(
    # Grant Details
    box( title = strong("Grant Details"), width = 12,
         collapsible = TRUE, solidHeader = TRUE, status = "primary",
        
          # Paid Grants
         box(
           title = strong("Grants Payments"), width = 6,
           collapsible = TRUE, solidHeader = TRUE, status = "success",
           DT::dataTableOutput("ev_grant_paid") 
          ), # End Box
         
         # Upcoming Grants
         box(
           title = strong("Potential Remaining Payments"), width = 6,
           collapsible = TRUE, solidHeader = TRUE, status = "info",
           DT::dataTableOutput("ev_grant_upcoming") 
         ) # End Box
    
    ) # End Details Box 
  ) # Fluid Row
  
) # End tabItem
