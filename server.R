library(dplyr)
library(DT)
library(lubridate)

# Load Data and supplementary functions
source("data_prep.R")
source("controllers/empl_manage_functions.R")
source("controllers/bod_manage_functions.R")
source("controllers/all_empl_functions.R")
source("controllers/empl_view_functions.R")
source("controllers/monthly_view_functions.R")
source("controllers/yv_functions.R")
source('controllers/tbl_create_functions.R')


message_ls <- list("success" = "Successfully added employee to database and logged new LTI grant.",
                   "warning" = "An employee with this name already exists. If you hit submit, employee will be add with new empl_id.",
                   "error" = "Error: Unable to write to database.")

type_ls <- list("success" = "message", "warning" = "warning", "error" = "error")


server <- function(input, output, session) {
  
  ##########################################################################################################################################
  #                                                          All Employees Output                                                          #  
  output$all_empl_tbl <- DT::renderDataTable(
      create_all_empl_summary(),
      rownames = FALSE,
      options = list("paging"=FALSE,  ordering = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = 0:9)) )
  )
  
  
  observeEvent(   input$all_empl_refresh , {
    output$all_empl_tbl <- DT::renderDataTable(
      create_all_empl_summary(),
      rownames = FALSE,
      options = list("paging"=FALSE,  ordering = FALSE,
                     columnDefs = list(list(className = 'dt-center', targets = 0:9)) )
    )
  })

  
  ##########################################################################################################################################
  #                                                          Yearly View Output                                                            #  
  
  # Year Title
  output$year_title <- renderText( paste0("Yearly View - " , input$yv_year) )
  
  
  # Total LTI Spend
  output$yv_total_lti_spend <- renderInfoBox({
    infoBox(title = strong("Total LTI Spend"),
            width = 4,
            get_lti_spend(input$yv_year)$total,
            icon = icon("usd"), fill = FALSE
    ) # end infobox   
  }) # End  
  
  
  # Total # of Grants Awarded
  output$yv_lti_grants_awarded <- renderInfoBox({
    infoBox(title = strong("Grants Disbursed"),
            width = 4,
            get_lti_count(input$yv_year),
            icon = icon("hashtag"), fill = FALSE
    ) # end infobox   
  }) # End   
  
  # Total # of Employees
  output$yv_employee_count <- renderInfoBox({
    infoBox(title = strong("# of Employees Recieving Grants"),
            width = 4,
            get_empl_count(input$yv_year),
            icon = icon("users"), fill = FALSE
    ) # end infobox   
  }) # End  
  
  # New grants awarded
  output$new_grants <- DT::renderDataTable(
    get_new_grants(input$yv_year), 
    options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = 0:5)) ),
    class = "compact", rownames = FALSE
  ) 
  
  # Expiring grants 
  output$exp_grants <- DT::renderDataTable(
    get_exp_grants(input$yv_year),
    
    options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                                                columnDefs = list(list(className = 'dt-center', targets = 0:5)) ),
    class = "compact", rownames = FALSE
  )
  
  # Year grant breakdown 
  output$yv_grant_details <- renderDataTable(
    get_lti_spend(input$yv_year)$df,
    options = list("paging"=FALSE),
    class = "compact", rownames = FALSE
  )
  
  # year breakdown download handler
  output$yv_download_details <- downloadHandler(
    filename = function() { paste('data-', Sys.Date(), '.csv', sep='') },
    content = function(con) { write.csv( get_lti_spend(input$yv_year)$df, con) }
  )
  
  
  ##########################################################################################################################################
  #                                                          Monthly View Output                                                           #  
  
  output$year_title <- renderText( paste0("Yearly View - " , input$yv_year) )
  
  output$mv_total_spend <-  renderInfoBox({
    infoBox(title = strong("Overall Monthly LTI Spend"),
            width = 12,
            prettyNum(round(get_monthly_total(input$mv_month, input$yv_year)$month.sum), big.mark = ","), 
            icon = icon("usd"), fill = FALSE
    ) # end infobox   
  }) # End  
  
  output$mv_breakdown <- renderDataTable(
    get_monthly_total(input$mv_month, input$yv_year)$df,
    options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = 0:8)) ),
    class = "compact", rownames = FALSE
  )
  
  output$mv_download <- downloadHandler(
    filename = function() { paste('monthly_view-',input$mv_month,"-" ,input$yv_year,'.csv', sep='') },
    content = function(con) { write.csv(  get_monthly_total(input$mv_month, input$yv_year)$df , con) }
  )
  
  
  ##########################################################################################################################################
  #                                                          Employee View Output                                                          #
  
  output$mv_title <- renderText( paste0("Month View: " , input$mv_month, " ", input$yv_year) )
  
  # Output Employee's grant summary
  output$ev_grant_sum <- renderDataTable( 
    create_emp_summary( get_empl_id(input$ev_empl_name) ),
    options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = 0:5)) ),
    class = "compact", rownames = FALSE
  )
  
  output$ev_grant_paid <- renderDataTable(
    create_empl_paid_tbl( get_empl_id(input$ev_empl_name) ),
    options = list("paging"=FALSE, dom = 't', "ordering" = FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = 0:6)) ),
    class = "compact", rownames = FALSE
  )
  
  
  output$ev_grant_upcoming <- renderDataTable(
    create_upcoming_tbl( get_empl_id(input$ev_empl_name) ),
    options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = 0:4)) ),
    class = "compact", rownames = FALSE
  )
  

  ##########################################################################################################################################
  #                                                          Empl Management Ouput                                                         #  
  
  output$em_current_grants <- DT::renderDataTable( 
                                grant_tbl %>%  filter(empl_id == get_empl_id(input$emng_select) ),
                                options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                                               columnDefs = list(list(className = 'dt-left', targets = 0:5)) )
                              )
  
  # Observe event for creating new grant
  observeEvent(input$emng_submit, {
    vest_sch <- paste0( input$emng_vc_1/100,"-",input$emng_vc_2/100,"-",
                        input$emng_vc_3/100,"-",input$emng_vc_4  /100 )
    
    em_empl_id = empl_tbl[empl_tbl$full_name == input$emng_select, ]$empl_id
    
    cg_res <- create_new_grant(em_empl_id, input$emng_lti_amount, input$emng_lti_date, vest_sch)
    
    showNotification(
      ui = message_ls["success"][[1]], duration = NA, closeButton = TRUE, type = type_ls["success"][[1]]
    ) #end showNotification
    
    print(cg_res)
    
    
    if(cg_res == "success"){
      create_paid_tbl()
      create_upcoming_payments_tbl()
      
    }
  
  }) # End observeEvent 
  
  # Observe event for Adding new employee
  observeEvent(input$em_submit, {
    vest_sch <- paste0( input$em_vc_1/100,"-",input$em_vc_2/100,"-",
                        input$em_vc_3/100,"-",input$em_vc_4  /100 )
     
     creat_res <- create_new_empl(input$em_fn, input$em_md, input$em_ln, input$em_hd, 
                               input$em_lti_amount, input$em_lti_date, vest_sch)
     
     if(creat_res == "success"){
       create_paid_tbl()
       create_upcoming_payments_tbl()
       
     }
     
     showNotification(
        ui = message_ls["success"][[1]], duration = NA, closeButton = TRUE, type = type_ls["success"][[1]]
     ) #end showNotification
     
   
     
       
  }) # End observeEvent 
  
  
  
  
  ##########################################################################################################################################
  #                                                          BOD Perc Management Ouput                                                     #  
  output$bod_mg_tbl <- DT::renderDataTable( get_bod_tbl(), 
                                            rownames = FALSE,
                                            options = list("paging"=TRUE,  ordering = FALSE,
                                                           columnDefs = list(list(className = 'dt-left', targets = 0:1)) ) 
                      )
  # Update BOD table
  observeEvent( input$bod_submit, {
                bod_res <- update_bod(input$bod_year, input$bod_perc)
                
                message = paste0("Updated BOD % for year: ", input$bod_year)
                showNotification( ui = message, duration = NA, closeButton = TRUE, type = "message")
                create_paid_tbl()
                create_upcoming_payments_tbl()
  }) 
  
  # Refresh BOD table
  observeEvent( input$bod_refresh, {
    
    output$bod_mg_tbl <- DT::renderDataTable( get_bod_tbl(), 
                                              rownames = FALSE,
                                              options = list("paging"=TRUE,  ordering = FALSE,
                                                             columnDefs = list(list(className = 'dt-left', targets = 0:1)) )) 
  })
  
  
  

  } # End Server
