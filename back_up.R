dat = read.csv("data/base_data.csv", stringsAsFactors = FALSE)
names(dat) = tolower(names(dat))
dat <- dat %>% mutate(hire_date = mdy(hire_date),
                      exp_date = hire_date + years(4),
                      hire_year = year(hire_date), 
                      exp_year = year(exp_date),
                      exit.date = mdy(exit.date),
                      exit_year = year(exit.date))

# 
dat$issue_date = dat$hire_date
for( i in 1:nrow(dat) )
  dat[i,]$issue_date = get_first_month(dat[i,]$hire_date)

dat$issue_year = year(dat$issue_date)


empl_list <- dat$name %>% unique %>% sort

get_first_month <- function(date){
  next_month = date + months(1)
  new_date = mdy(paste0(month(next_month), "/01/", year(next_month)))
  return(new_date)
}


#################################################################################
#                              Yearly View Functions                            #  

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


output$exp_grants <- DT::renderDataTable(
  get_exp_grants(input$yv_year), 
  options = list("paging"=FALSE),
  rownames = FALSE,
  class = "compact"
)
output$new_grants <- DT::renderDataTable(
  get_new_grants(input$yv_year), 
  options = list("paging"=FALSE),
  class = "compact", rownames = FALSE
) 

output$yv_grant_details <- renderDataTable(
  get_lti_spend(input$yv_year)$df,
  options = list("paging"=FALSE),
  class = "compact", rownames = FALSE
)

output$yv_download_details <- downloadHandler(
  filename = function() { paste('data-', Sys.Date(), '.csv', sep='') },
  content = function(con) { write.csv( get_lti_spend(input$yv_year)$df, con) }
)

#################################################################################
#                              Employee View Functions                          #  
output$empl_title <- renderText( paste0("Employee View - " , input$ev_empl_name) )

# Output Employee's grant summary
output$ev_grant_sum <- renderDataTable( 
  create_emp_summary(get_empl_id(input$ev_empl_name)),    
  options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                 columnDefs = list(list(className = 'dt-left', targets = 0:6)) ),
  class = "compact", rownames = FALSE
)

output$ev_grant_paid <- renderDataTable(
  create_paid_tbl(dat %>% filter(name == input$ev_empl_name)),
  options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                 columnDefs = list(list(className = 'dt-left', targets = 0:5)) ),
  class = "compact", rownames = FALSE
)

output$ev_grant_upcoming <- renderDataTable(
  create_upcoming_tbl(dat %>% filter(name == input$ev_empl_name)),
  options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                 columnDefs = list(list(className = 'dt-left', targets = 0:5)) ),
  class = "compact", rownames = FALSE
)

#################################################################################
#                              Monthly View Functions                           #  
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
                 columnDefs = list(list(className = 'dt-left', targets = 0:7)) ),
  class = "compact", rownames = FALSE
)

output$mv_download <- downloadHandler(
  filename = function() { paste('monthly_view-',input$mv_month,"-" ,input$yv_year,'.csv', sep='') },
  content = function(con) { write.csv(  get_monthly_total(input$mv_month, input$yv_year)$df , con) }
)

output$all_empl_tbl <- renderDataTable(
  read.csv("data/fake_empl_view.csv"),
  options = list("paging"=FALSE, dom = 't', ordering = FALSE,
                 columnDefs = list(list(className = 'dt-left', targets = 0:8))),
  class = "compact", rownames = FALSE
)
