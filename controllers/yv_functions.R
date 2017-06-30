get_lti_spend <- function( cy ){
  cy = as.numeric(cy)
  
  tbl <- grant_tbl %>% left_join(empl_tbl %>% select(empl_id, full_name, Active), by = c("empl_id" = "empl_id"))
  
  year_start = mdy(paste0("01/01",cy))
  year_end   = mdy(paste0("12/31",cy))
  
  spend <- tbl %>% filter(exp_year >= cy, issue_year <= cy ) %>%
           mutate(start_month = elapsed_months(issue_date, year_start),
                  end_month   = elapsed_months(issue_date, year_end) ) %>%
           mutate(start_month = ifelse(start_month == -1, 1, start_month))
  
  spend <- spend %>% 
           group_by(grant_id, empl_id,full_name, issue_date, expiration_date, amount, vesting_schedule) %>%
           do(data.frame(month = seq(.$start_month, .$end_month, by = 1))) %>%
           mutate(year = cy) %>% ungroup
  
  vc = create_vest_cal(".21-.25-.25-.29")
  
  spend.comb <- spend %>% left_join(vc, by = c("month" = "months"))  %>% 
                left_join(bod_match_tbl, by = c("year"="year")) %>%
                mutate(gm = .5, adj_perc = gm + comp_perc) %>%
                filter(!is.na(vest_perc))
  
  spend.comb <- spend.comb %>% mutate( unadj_amount = amount * multiplier,
                                       adj_amount = unadj_amount * adj_perc) %>%
                group_by(grant_id, empl_id, full_name, issue_date, expiration_date, amount, vesting_schedule) %>%
                summarise(unadj_sum = sum(unadj_amount, na.rm = TRUE), 
                          adj_sum = sum(adj_amount, na.rm = TRUE),
                          captured_months = paste0(min(month),"-",max(month))) %>% 
                ungroup()
  
  total.spend <- sum(spend.comb$adj_sum)
  
  spend <- spend.comb %>%  mutate( amount = prettyNum(amount, big.mark = ","),
                                   adj_sum = prettyNum( round(adj_sum,2), big.mark = ","),
                                   unadj_sum = prettyNum( round(unadj_sum,2), big.mark = ",")) %>%
           select("Grant ID" = grant_id, "Employee" = full_name, 
                   "Grant Issue" = issue_date, "Expiration Date" = expiration_date,
                   "Vesting Schedule" = vesting_schedule, "Grant Amount" = amount,
                   "Captured Months" = captured_months,
                   "Unadjusted Amount ($)" = unadj_sum, "Adjusted Amount($)" = adj_sum) %>%
          arrange(Employee)
  
  return( list(total = prettyNum(total.spend , big.mark = ","),
               df = spend))
  
}

get_lti_count <- function(cy){
  cy = as.numeric(cy)
  tbl.count <- grant_tbl %>% filter( issue_year <= cy, exp_year >= cy) %>% nrow
  return(tbl.count)  
}

get_empl_count <- function(cy){
  cy = as.numeric(cy)
  tbl.count <- grant_tbl %>% filter( issue_year <= cy, exp_year >= cy) %>% 
               select(empl_id) %>% unique() %>% nrow
  return(tbl.count)  
}

get_new_grants <- function(cy){
  cy = as.numeric(cy)
  
  tbl <- grant_tbl %>% left_join(empl_tbl %>% select(empl_id, full_name), by = c("empl_id"="empl_id"))
  
  tbl <- tbl %>% filter(issue_year == cy) %>%
         mutate(amount = prettyNum(amount, big.mark = ",")) %>% 
         select( "Name" = full_name, "Grant ID" = grant_id,
                 "Grant Issue" = issue_date, "Grant Expiration" = expiration_date,
                 "Vesting Schedule" = vesting_schedule, "Grant Amount ($)" = amount) %>%
          arrange(Name)
  return(tbl)  
}

get_exp_grants <- function(cy){
  cy = as.numeric(cy)
  
  tbl <- grant_tbl %>% left_join(empl_tbl %>% select(empl_id, full_name), by = c("empl_id"="empl_id"))
  
  tbl <- tbl %>% filter(exp_year == cy) %>%
    mutate(amount = prettyNum(amount, big.mark = ",")) %>% 
    select( "Name" = full_name, "Grant ID" = grant_id,
            "Grant Issue" = issue_date, "Grant Expiration" = expiration_date,
            "Vesting Schedule" = vesting_schedule, "Grant Amount ($)" = amount) %>%
    arrange(Name)
  return(tbl)  
}