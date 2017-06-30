tabItem(
  tabName = "empl_manage_view", 
  h1("Employee Administrative View"),
  
  tabBox(
    title = "",
    id = "empl_manage_tab", 
    width = 12, 
    
    tabPanel( 
      "Create New Grant for Existing Employee",
      selectInput("emng_select", label = "Select Employee", choices = empl_list),
      
      strong("Current Grants"),
      DT::dataTableOutput("em_current_grants"),
      br(),
      strong("LTI Grant Details"),
      splitLayout(
        numericInput("emng_lti_amount", label = "LTI Grant Amount ($)", value = 0),
        dateInput("emng_lti_date", label = "Grant Start Date", value = Sys.Date()),
        br(),br(),br(),br()
      ),
      "Vesting Schedule (%)",
      splitLayout(
        numericInput("emng_vc_1", label = "", value = 21, min = 0, max = 100), 
        numericInput("emng_vc_2", label = "", value = 25, min = 0, max = 100),
        numericInput("emng_vc_3", label = "", value = 25, min = 0, max = 100), 
        numericInput("emng_vc_4", label = "", value = 29, min = 0, max = 100)
      ),
      actionButton("emng_submit", label = "Create New Grant")
      
    ), # End tab panel for creating a new grant 
    
    
    tabPanel("Add New Employee and Grant",
      splitLayout(
        textInput("em_fn", label = "First", value = "First name"),
        textInput("em_md", label = "Middle", value = "Middle Initial"),
        textInput("em_ln", label = "Last", value = "Last name"),
        dateInput("em_hd", label = "Hire Date", value = Sys.Date())  
      ),
      strong("LTI Grant Details"),
      splitLayout(
        numericInput("em_lti_amount", label = "LTI Grant Amount ($)", value = 0),
        dateInput("em_lti_date", label = "Grant Start Date", value = Sys.Date()),
        br(),br(),br(),br()
      ),
      "Vesting Schedule (%)",
      splitLayout(
          width = 4,
          numericInput("em_vc_1", label = "", value = 21, min = 0, max = 100), 
          numericInput("em_vc_2", label = "", value = 25, min = 0, max = 100),
          numericInput("em_vc_3", label = "", value = 25, min = 0, max = 100), 
          numericInput("em_vc_4", label = "", value = 29, min = 0, max = 100)
       ),
      actionButton("em_submit", label = "Add Employee and Create New Grant")
    ) # End Add Empl Tabpanel 
  ) # End Empl Admin TabBox  
  
) # End tabItem
