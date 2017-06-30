test_function <- function(){
  grant_tbl <- read.csv("data/grant_tbl.csv", stringsAsFactors = FALSE)
  
  
  # 1. For each grant, expand out duration in year and start and end for each year
  grant_temp <- grant_tbl %>%
    group_by(grant_id, empl_id, issue_date, issue_year, expiration_date, amount, vesting_schedule) %>%
    do(data.frame(year = seq(.$issue_year, .$exp_year, by = 1))) %>%                       # a.  expand out years for duration of LTI
    mutate( payment_start = ifelse( issue_year == year, issue_date, paste0(year,"-01-01") ), # b. calculate start and end periods 
            payment_end = paste0(year, "-12-31") )  %>%
    mutate( elm = elapsed_months(payment_end, Sys.Date()) ) %>% # c. identify which years have been paid out
    ungroup()
  
  # 2. Create paid grants table
  paid_calc <- grant_temp %>% filter(elm >= 0)  # a. identify years where payment periods have passed
  
  paid_calc <- paid_calc %>% 
               mutate( start_month = elapsed_months(issue_date, payment_start),
                        end_month   = elapsed_months(issue_date, payment_end)) %>%
               group_by(grant_id, empl_id, issue_date, payment_start, payment_end, amount, vesting_schedule, year ) %>%
               do( data.frame( vest_month = seq(.$start_month, .$end_month, by = 1)) ) %>%
               filter(vest_month <= 48) %>%
               left_join(vesting_cal, by = c("vest_month" = "months")) %>%
               mutate(pr_amount = amount * multiplier) %>%
               group_by(grant_id, empl_id, issue_date, amount, payment_start, payment_end, vesting_schedule, year) %>%
               summarise(full_vested_amount = sum(pr_amount),
                          captured_months = paste0(min(vest_month), "-",max(vest_month))) %>%
               left_join(bod_match_tbl, match = c("year" = "year")) %>%
               mutate(gm = .50, adj_perc = gm + comp_perc) %>%
               ungroup()
  
  paid_calc <- paid_calc %>%
               mutate(paid_amount = adj_perc * full_vested_amount) %>%
               select(grant_id, empl_id, issue_date, grant_amount = amount, 
                     payment_start, payment_end,captured_months,vesting_schedule, bod_perc = comp_perc,
                     unadjusted_amount = full_vested_amount, paid_amount)
  
  grant_paid_tbl <<- paid_calc
  
  write.csv(paid_calc, "data/grants_paid_tbl.csv", row.names = FALSE)  
}


create_paid_tbl <- function(){
  
  
  grant_tbl <- read.csv("data/grant_tbl.csv", stringsAsFactors = FALSE)
  
  
  # 1. For each grant, expand out duration in year and start and end for each year
  grant_temp <- grant_tbl %>%
                group_by(grant_id, empl_id, issue_date, issue_year, expiration_date, amount, vesting_schedule) %>%
                do(data.frame(year = seq(.$issue_year, .$exp_year, by = 1))) %>%                       # a.  expand out years for duration of LTI
                mutate( payment_start = ifelse( issue_year == year, issue_date, paste0(year,"-01-01") ), # b. calculate start and end periods 
                        payment_end = paste0(year, "-12-31") )  %>%
                mutate( elm = elapsed_months(payment_end, Sys.Date()) ) %>% # c. identify which years have been paid out
                ungroup()

  # 2. Create paid grants table
  paid_calc <- grant_temp %>% filter(elm >= 0)  # a. identify years where payment periods have passed
  
  paid_calc <- paid_calc %>% 
    mutate( start_month = elapsed_months(issue_date, payment_start),
            end_month   = elapsed_months(issue_date, payment_end)) %>%
    group_by(grant_id, empl_id, issue_date, payment_start, payment_end, amount, vesting_schedule, year ) %>%
    do( data.frame( vest_month = seq(.$start_month, .$end_month, by = 1)) ) %>%
    filter(vest_month <= 48) %>%
    left_join(vesting_cal, by = c("vest_month" = "months")) %>%
    mutate(pr_amount = amount * multiplier) %>%
    group_by(grant_id, empl_id, issue_date, amount, payment_start, payment_end, vesting_schedule, year) %>%
    summarise(full_vested_amount = sum(pr_amount),
              captured_months = paste0(min(vest_month), "-",max(vest_month))) %>%
    left_join(bod_match_tbl, match = c("year" = "year")) %>%
    mutate(gm = .50, adj_perc = gm + comp_perc) %>%
    ungroup()
  
  paid_calc <- paid_calc %>%
    mutate(paid_amount = adj_perc * full_vested_amount) %>%
    select(grant_id, empl_id, issue_date, grant_amount = amount, 
           payment_start, payment_end,captured_months,vesting_schedule, bod_perc = comp_perc,
           unadjusted_amount = full_vested_amount, paid_amount)
  
  grant_paid_tbl <<- paid_calc
  
  write.csv(paid_calc, "data/grants_paid_tbl.csv", row.names = FALSE)
}

create_upcoming_payments_tbl <- function(){
  
  grant_temp <- grant_tbl %>%
    group_by(grant_id, empl_id, issue_date, issue_year, expiration_date, amount, vesting_schedule) %>%
    do(data.frame(year = seq(.$issue_year, .$exp_year, by = 1))) %>%                       # a.  expand out years for duration of LTI
    mutate( payment_start = ifelse( issue_year == year, issue_date, paste0(year,"-01-01") ), # b. calculate start and end periods 
            payment_end = paste0(year, "-12-31") )  %>%
    mutate( elm = elapsed_months(payment_end, Sys.Date()) ) %>% # c. identify which years have been paid out
    ungroup()
  
  # 3. Create upcoming grant payments table
  upcoming_calc <-grant_temp %>% filter(elm < 0)
  
  upcoming_calc <- upcoming_calc %>%
    mutate( start_month  = elapsed_months(issue_date, payment_start),
            end_month   = elapsed_months(issue_date, payment_end)) %>%
    group_by(grant_id, empl_id, issue_date, payment_start, payment_end, amount, vesting_schedule, year ) %>%
    do( data.frame( vest_month = seq(.$start_month, .$end_month, by = 1)) ) %>%
    filter(vest_month <= 48) %>%
    left_join(vesting_cal, by = c("vest_month" = "months")) %>%
    mutate(pr_amount = amount * multiplier) %>%
    group_by(grant_id, empl_id, issue_date, amount, payment_start, payment_end, vesting_schedule, year) %>%
    summarise(potential_payment = sum(pr_amount),
              captured_months = paste0(min(vest_month), "-",max(vest_month))) 
  
  upcoming_calc <- upcoming_calc %>%
    select(grant_id, empl_id, issue_date, grant_amount = amount, 
           payment_start, payment_end,captured_months,vesting_schedule,
           potential_payment)
  
  upcoming_tbl <<- upcoming_calc
  
  write.csv(upcoming_calc, "data/upcoming_grants.csv", row.names = FALSE)
}