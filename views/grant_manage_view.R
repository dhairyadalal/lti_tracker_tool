tabItem(
  tabName = "grant_manage_view", 
  h3("Grant Administration"),
  
  tabBox(
    title = "",
    id = "gm_tab",
    width = 12,
    
    tabPanel( "Log New Payment",
              selectInput("gm_empl", label = "Select Employee", choices = empl_list),
              DT::dataTableOutput("gm_empl_paid"),
              splitLayout(
                selectInput("gm_grant_id", label = "Select Grant", choices = c("LTI-1","LTI-2") ),
                dateInput("gm_pay_date", label = "Payment Date", value = Sys.Date()),
                numericInput("gm_pay_amount", label = "Payment Amount ($)", value = 0, min = 0)
              ),
              actionButton("gm_pay_submit", label = "Log Payment"),
              hr(),
              "Grant Calculator",
              htmlOutput("gm_vc"),
              splitLayout(
                dateInput("gm_c_sdate", label = "Start Date", value = Sys.Date()),
                dateInput("gm_c_edate", label = "End Date", value = Sys.Date()),
                numericInput("gm_pay_amount", label = "LTI Grant Amount ($)", value = 0, min = 0)
              ),
              splitLayout(
                numericInput("gm_empl_perc", label = "Employee %", value = 0, min = 0, max = 100),
                numericInput("gm_comp_perc", label = "Comp Performance %", value = 0, min = 0, max = 100)
              ),
              "Expected Payment: ", htmlOutput("gm_ep"),
              actionButton("gm_calc_pay", label = "Estimate Payment")
    ) # End Log Payment Tab
  ) # End tab box
) # End tabItem
