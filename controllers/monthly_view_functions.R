month_days_map <- list("January" = 31, "February" = 28, "March" = 31, "April" = 30,
                       "May" = 31, "June" = 30, "July" = 31, "August"=31, 
                       "September" = 30,"October" = 31, "November"=30, "December"=31)

get_monthly_total <- function(month, year){
  
  interval_date <- mdy( paste0(month, month_days_map[month], year) )
  
  year = as.numeric(year)
  
  vc = create_vest_cal(".21-.25-.25-.29")
  
  empl_info <- empl_tbl %>% filter(Active =="Yes") %>% select(empl_id, full_name)
  
  dat.filt <- grant_tbl %>% mutate(eval_year = year, gm = .50) %>% 
              left_join(empl_info) %>% left_join(bod_match_tbl, by = c("eval_year" = "year")) %>%
              mutate(adj_perc = gm + comp_perc) %>%
              mutate( distance = elapsed_months(issue_date, interval_date) ) %>%
              filter(distance > 0, distance <= 48) %>% 
              left_join(vc, by = c("distance" = "months")) %>%
              mutate(adjusted_amount = multiplier * amount * adj_perc)
  
  month_sum = dat.filt$adjusted_amount %>% sum()
  
  # clean up return table
  dat.filt <- dat.filt %>% mutate(lti_grant = prettyNum(amount, big.mark = ","),
                                  adjusted_amount = prettyNum(round(adjusted_amount, 2), big.mark = ",")) %>%
              arrange(full_name) %>% 
              select("Employee ID" = empl_id, "Name" = full_name, 
                     "Grant ID" = grant_id, "Grant Amount ($)" = amount, "Grant Issue Date" = issue_date, "Grant Expiration" = expiration_date,
                     "Elapsed Months"= distance, "Prorated Amount($)" = adjusted_amount, "Vesting Schedule" = vesting_schedule)
            
  
  return(list(month.sum = month_sum, df = dat.filt))
}