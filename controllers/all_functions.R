###############################################################################################
#                                       Yearly View Functions                                 #

# Total amount of money spent in a provided year on LTI grants
get_lti_spend <- function(cy){
  
  year_start = mdy(paste0("01/31",cy))
  year_end   = mdy(paste0("12/31",cy))
  
  spend <- dat %>% filter(exit_year >= cy | is.na(exit_year)) %>%
           mutate(start_month = interval(issue_date, year_start) %/% months(1),
                  end_month   = interval(issue_date, year_end) %/% months(1)) %>%
           filter(start_month > 0, start_month <= 48)
  
    
  spend <- spend %>% 
           group_by(name, hire_date, exp_date, grant_id, issue_date, lti_grant, vesting.schedule) %>%
           do(data.frame(month = seq(.$start_month, .$end_month, by = 1)))
  
  vc = create_vest_cal(".21/.25/.25/.29")
  
  spend.comb <- spend %>% left_join(vc, by = c("month" = "months")) 
  
  spend.comb <- spend.comb %>% mutate( monthly_amount = lti_grant * multiplier ) %>%
                group_by(name, hire_date,exp_date, grant_id, issue_date, lti_grant, vesting.schedule) %>%
                summarise(yearly_sum = sum(monthly_amount, na.rm = TRUE), 
                          captured_months = paste0(min(month),"-",max(month))) %>% 
                ungroup()
  
  total.spend <- sum(spend.comb$yearly_sum)
  
  spend <- spend.comb %>%  mutate(lti_grant = prettyNum(lti_grant, big.mark = ","),
                             year_total = prettyNum(yearly_sum, big.mark = ",")) %>%
            select("Grant ID" = grant_id, "Employee" = name, 
                 "Hire Date" = hire_date,
                 "Grant Issue Date" = issue_date, "Expiration Date" = exp_date,
                 "Grant Amount" = lti_grant,
                 "Captured Months" = captured_months,
                 "Vested Spend for Year" = year_total)
  
  return( list(total = prettyNum(total.spend , big.mark = ","),
               df = spend))
}




#Total LTI Grants
get_lti_count <- function(cy){
  count = dat %>% filter(exp_year >= cy) %>% nrow
  return(count)
}

# Count of employees recieving grants
get_empl_count <- function(cy){
  count = dat %>% filter(exp_year >= cy) %>% select(name) %>% unique %>% nrow
  return(count)
}

# Expiring Grants
get_exp_grants <- function(ey){
  
  rd = dat %>% 
    filter(exp_year == ey) %>%
    mutate(lti_grant = prettyNum(lti_grant, big.mark = ",")) %>%
    arrange(name) %>%
    select("Grant ID" = grant_id, "Employee" = name, 
           "Hire Date" = hire_date,
           "Grant Issue Date" = issue_date, "Expiration Date" = exp_date,
           "Grant Amount ($)" = lti_grant)
  
  return(rd)
  
}

# New Grants 
get_new_grants <- function(cy){
  rd = dat %>% 
    filter( hire_year == cy | issue_year == cy) %>%
    arrange(name) %>%
    mutate(lti_grant = prettyNum(lti_grant, big.mark = ",")) %>%
    select("Grant ID" = grant_id, "Employee" = name, 
           "Hire Date" = hire_date,
           "Grant Issue Date" = issue_date, "Expiration Date" = exp_date,
           "Grant Amount ($)" = lti_grant)
  
  return(rd)
}

###############################################################################################
#                                       Monthly View Functions                                #

month_days_map <- list("January" = 31, "February" = 28, "March" = 31, "April" = 30,
                       "May" = 31, "June" = 30, "July" = 31, "August"=31, 
                       "September" = 30,"October" = 31, "November"=30, 
                        "December"=31)

# Get total for month
get_monthly_total <- function(month, year){
  
  interval_date <- mdy( paste0(month, month_days_map[month], year) )
  
  vc = create_vest_cal(".21/.25/.25/.29")
  
  # Join on vesting schedule by elaspsed month and calculate montly prorated amount
  dat.filt <- dat %>% mutate( distance = interval(issue_date, interval_date) %/% months(1)) %>%
              mutate(distance = ifelse(!is.na(exit.date), 0, distance) ) %>%
              filter(distance > 0, distance <= 48) %>% 
              left_join(vc, by = c("distance" = "months")) %>%
              mutate(prorate = multiplier * lti_grant)
  
  month_sum = dat.filt$prorate %>% sum()

  # clean up return table
  dat.filt <- dat.filt %>% mutate(lti_grant = prettyNum(lti_grant, big.mark = ","),
                                  prorate = prettyNum(round(prorate, 2), big.mark = ",")) %>%
              arrange(name) %>% 
              select("Employee" = name, "Hire Date" = hire_date, "Grant ID" = grant_id,
                     "Grant Amount ($)" = lti_grant, "Grant Issue Date" = issue_date, 
                     "Elapsed Months"= distance, "Prorated Amount($)" = prorate, 
                     "Vesting Schedule" = vesting.schedule)
  
  
  return(list(month.sum = month_sum, df = dat.filt))
}



###############################################################################################
#                                 Employee View Functions                                     #
# Summary of Employee 
create_emp_summary <- function(empl_df) {
 disbursed = c()
 
 for(row in 1:nrow(empl_df) )
   disbursed = c(disbursed, 
                 get_lti_disbursed( empl_df[row,]$issue_date , empl_df[row,]$lti_grant,
                                    empl_df[row,]$vesting.schedule))
 
  df_sum <- empl_df %>% cbind(disbursed) %>% 
           mutate(remaining = lti_grant  - disbursed) %>%
           mutate(disbursed = prettyNum(disbursed, big.mark = ","),
                  remaining =  prettyNum(remaining, big.mark = ","),
                  lti_grant = prettyNum(lti_grant, big.mark = ",")) %>% 
           select("Grant ID" = grant_id, 
                  "Grant Amount ($)" = lti_grant,
                  "Issue Date" = issue_date,
                  "Expiration Date" = exp_date, 
                  "Vesting Schedule" = vesting.schedule,
                  "Total Paid ($)" = disbursed,
                  "Potential Remaining ($)" = remaining)
 
 return(df_sum)    
}

# Total amount disbursed within a provided range
get_lti_disbursed <- function(issue_date, lti_amount, vesting_sch){
  vc = create_vest_cal(vesting_sch)
  
  elapsed_months = interval(issue_date, Sys.Date()) %/% months(1)
  
  disbursed_df = vc %>% filter(months <= elapsed_months) %>% 
                 mutate(lti_grant = lti_amount) %>%
                 mutate(pro_amount = multiplier * lti_amount)
  disbursed = sum(disbursed_df$pro_amount)
  
  return(disbursed)
}

# Generate the Paid Grants Table
create_paid_tbl <- function(empl_df){
  #empl_df = dat %>% filter(name == "Vu Ha")
  
  paid_tbl = data.frame()
  for(row in 1:nrow(empl_df)){
      pr = get_paid_rows(empl_df[row,]$issue_date, empl_df[row,]$lti_grant, 
                         empl_df[row,]$vesting.schedule, empl_df[row,]$grant_id)
      paid_tbl = rbind( paid_tbl, pr)
  }
  
  # Format numbers in table
  paid_tbl <- paid_tbl %>% 
              mutate(Grant.Amount = prettyNum(Grant.Amount, big.mark = ","),
                     Amount = prettyNum(Amount, big.mark = ",") ) %>%
              select("Grant ID" = Grant.ID, "Grant Amount ($)" = Grant.Amount,
                     "Issue Date" = Issue.Date, "Date Disbursed" = date_disbursed,
                      "Vesting %" = vestperc,
                     "Disbursed Amount ($)" = Amount)
              
  
  return(paid_tbl)
}

# Calculate the Paid Grant rows from Issue Date and vesting Schedulee
get_paid_rows <- function(issue_date, lti_amount, vesting_sch, grant_id){
  
  vc <- create_vest_cal(vesting_sch)
  elapsed_months = interval(issue_date, Sys.Date()) %/% months(1)
  
  rows = data.frame("date_disbursed" = numeric(), "Amount" = numeric(), "vestperc" = numeric())
  if( elapsed_months >= 12)
    rows <- rows %>% rbind( data.frame("date_disbursed" = issue_date + months(12), 
                                       "Amount" = lti_amount * vc[vc$months == 12,]$vest_perc,
                                       "vestperc" =  vc[vc$months == 12,]$vest_perc))
  if( elapsed_months >= 24)
    rows <- rows %>% rbind( data.frame("date_disbursed" = issue_date + months(24), 
                                       "Amount" = lti_amount * vc[vc$months == 24,]$vest_perc,
                                       "vestperc" =  vc[vc$months == 24,]$vest_perc))
  if( elapsed_months >= 36)
    rows <- rows %>% rbind( data.frame("date_disbursed" = issue_date + months(36), 
                                       "Amount" = lti_amount * vc[vc$months == 36,]$vest_perc,
                                       "vestperc" =  vc[vc$months == 36,]$vest_perc))
  if( elapsed_months == 48)
    rows <- rows %>% rbind( data.frame("date_disbursed" = issue_date + months(48), 
                                       "Amount" = lti_amount * vc[vc$months == 48,]$vest_perc,
                                       "vestperc" =  vc[vc$months == 48,]$vest_perc))
  
  # Add in Grant id, issue date, amount
  rl = nrow(rows) # length of rows
  row_details <- data.frame("Grant ID" = rep(grant_id, rl),
                            "Grant Amount" = rep(lti_amount, rl),
                            "Issue Date" = rep(issue_date, rl))
  
  rows <- cbind(row_details, rows)  
  
  return(rows)
}

create_upcoming_tbl <- function(empl_df){
  
  #empl_df = dat %>% filter(name == "Amos Ng")
  upc_tbl = data.frame()
  for(row in 1:nrow(empl_df)){
    upcr = get_upcoming_rows(empl_df[row,]$issue_date, empl_df[row,]$lti_grant, 
                       empl_df[row,]$vesting.schedule, empl_df[row,]$grant_id)
    upc_tbl = rbind( upc_tbl, upcr)
  }
  
  # Format numbers in table
  upc_tbl <- upc_tbl %>% 
    mutate(Grant.Amount = prettyNum(Grant.Amount, big.mark = ","),
           Amount = prettyNum(Amount, big.mark = ",") ) %>%
    select("Grant ID" = Grant.ID, "Grant Amount ($)" = Grant.Amount,
           "Issue Date" = Issue.Date, "Upcoming Date" = upcoming,
           "Vesting %" = vestperc,
           "Upcoming Amount ($)" = Amount)
  
  
  return(upc_tbl)
}

# Upcoming Grants
get_upcoming_rows <- function(issue_date, lti_amount, vesting_sch, grant_id){
  
  vc <- create_vest_cal(vesting_sch)
  elapsed_months = interval(issue_date, Sys.Date()) %/% months(1)
  
  rows = data.frame("upcoming" = numeric(), "Amount" = numeric(), "vestperc" = numeric())
  if( elapsed_months < 12)
    rows <- rows %>% rbind( data.frame("upcoming" = issue_date + months(12), 
                                       "Amount" = lti_amount * vc[vc$months == 12,]$vest_perc,
                                       "vestperc" =  vc[vc$months == 12,]$vest_perc))
  if( elapsed_months < 24)
    rows <- rows %>% rbind( data.frame("upcoming" = issue_date + months(24), 
                                       "Amount" = lti_amount * vc[vc$months == 24,]$vest_perc,
                                       "vestperc" =  vc[vc$months == 24,]$vest_perc))
  if( elapsed_months < 36)
    rows <- rows %>% rbind( data.frame("upcoming" = issue_date + months(36), 
                                       "Amount" = lti_amount * vc[vc$months == 36,]$vest_perc,
                                       "vestperc" =  vc[vc$months == 36,]$vest_perc))
  if( elapsed_months < 48)
    rows <- rows %>% rbind( data.frame("upcoming" = issue_date + months(48), 
                                       "Amount" = lti_amount * vc[vc$months == 48,]$vest_perc,
                                       "vestperc" =  vc[vc$months == 48,]$vest_perc))
  
  # Add in Grant id, issue date, amount
  rl = nrow(rows) # length of rows
  row_details <- data.frame("Grant ID" = rep(grant_id, rl),
                            "Grant Amount" = rep(lti_amount, rl),
                            "Issue Date" = rep(issue_date, rl))
  
  rows <- cbind(row_details, rows)  
  return(rows)
}


