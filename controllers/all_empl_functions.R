
# Generate summary view for All Employee Page

create_all_empl_summary <- function(){
  
  # 1. Read in all data files
  all_grants <- read.csv("data/grant_tbl.csv",stringsAsFactors = FALSE)
  
  empl_info <- read.csv("data/empl_tbl.csv", stringsAsFactors = FALSE)
  empl_info <- empl_info %>% select(empl_id, full_name, hire_date, term_date)
  
  pg <- read.csv("data/grants_paid_tbl.csv")
  pg <- pg %>% group_by(empl_id, grant_id) %>% summarise(total_paid = sum(paid_amount))
  
  up <- read.csv("data/upcoming_grants.csv", stringsAsFactors = FALSE )
  up <- up %>% group_by(empl_id, grant_id) %>% summarise(potential_remaining = sum(potential_payment))
  
  # 2. Merge
  all_grants <- all_grants %>% left_join(empl_info, by= c("empl_id"="empl_id")     ) %>%
                left_join(pg, by = c("empl_id" = "empl_id", "grant_id"="grant_id") ) %>%
                left_join(up, by = c("empl_id" = "empl_id", "grant_id"="grant_id") )
  
  # 3. Clean up
  all_grants <- all_grants %>% 
                mutate(total_paid = ifelse(is.na(total_paid), 0, total_paid),
                       potential_remaining = ifelse(is.na(potential_remaining), 0, potential_remaining) ) %>%
                mutate( total_paid = prettyNum( round(total_paid, 2), big.mark = ","),
                        potential_remaining = prettyNum( round(potential_remaining, 2), big.mark = ","),
                        amount =  prettyNum(amount, big.mark = ",") ) %>%
                select("Employee ID" = empl_id, "Name" = full_name, "Hire Date" = hire_date, "Termination Date" = term_date,
                       "Grant ID" = grant_id, "Grant Issue" = issue_date, "Grant Expiration" = expiration_date, "Grant Amount ($)" = amount, 
                       "Vesting Schedule" = vesting_schedule, "Total Paid ($)" = total_paid, "Potential Remaining ($)" = potential_remaining) %>%
                arrange(Name)
                
  
  return(all_grants)
}