
create_emp_summary <- function(id){
  tbl <- grant_tbl %>% filter(empl_id == id)
  
  tbl <- tbl %>% mutate( amount = prettyNum(amount, big.mark = ",") ) %>%
         select( "Employee ID" = empl_id, "Grant ID" = grant_id, "Grant Issue" = issue_date,
                         "Grant Expiration" = expiration_date, "Vesting Schedule" = vesting_schedule,
                         "Grant Amount($)" = amount)
  
  return(tbl)
}

create_empl_paid_tbl <- function(id){
  
  tbl <- grant_paid_tbl %>% filter(empl_id == id) %>%
         mutate(unadjusted_amount = prettyNum( unadjusted_amount, big.mark = ","),
                paid_amount = prettyNum(paid_amount, big.mark = ","),
                bod_perc = bod_perc * 100) %>%
         select( "Grant ID" = grant_id, 
                 "Payment Start" = payment_start, "Payment End" = payment_end, 
                 "Captured Months"= captured_months, "Unadjusted Amount" = unadjusted_amount,
                 "BOD LTI %" = bod_perc, "Total Paid($)" = paid_amount)
  return(tbl)
}

create_upcoming_tbl <- function(id){
  
  tbl <- upcoming_tbl  %>% filter(empl_id == id) %>%
         mutate(potential_payment = prettyNum( potential_payment, big.mark = ",") ) %>%
         select( "Grant ID" = grant_id, 
                 "Payment Start" = payment_start, "Payment End" = payment_end, 
                 "Captured Months"= captured_months, "Potential Payment ($)" = potential_payment )
        
  return(tbl)
}