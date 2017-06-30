# Create a vesting cal based on passed schedule

# Schedule should follow format '.%-.%-.%-.%' (e.g. '.21-.25-.25-.29')
create_vest_cal <- function(schedule){
  sch = strsplit(schedule, "-")[[1]]
  sch = sapply(X = sch, FUN = as.numeric)

    vc = data.frame(months = 0:48, 
                  vest_perc = c(0,
                                rep( sch[[1]], 12),
                                rep( sch[[2]], 12),
                                rep( sch[[3]], 12),
                                rep( sch[[4]], 12)),
                  multiplier = c(0,
                                 rep( sch[[1]] / 12, 12),
                                 rep( sch[[2]] / 12, 12),
                                 rep( sch[[3]] / 12, 12),
                                 rep( sch[[4]] / 12, 12))
  )
  return(vc)
}

vesting_cal = create_vest_cal(".21-.25-.25-.29")

current_year = as.integer(format(Sys.Date(), "%Y"))


# Calculate # of months between two dates. 
# If date2 < date 1, return -1
elapsed_months <- function( date1, date2 ){
  
  date1 = as.Date(date1)
  date2 = as.Date(date2)
  
  return_val = ifelse( difftime( date2, date1, units = "days") < 0, -1, 
                       length(seq( date1, date2, by = 'month')) )
  
  class(return_val) = "integer" # corece return type to integer to satisfy Vectorize constraints
  
  return(return_val)
}

elapsed_months <- Vectorize(elapsed_months)

################################         Load data              ################################
source("controllers/tbl_create_functions.R")

empl_tbl       <- read.csv("data/empl_tbl.csv", stringsAsFactors = FALSE)
grant_tbl      <- read.csv("data/grant_tbl.csv", stringsAsFactors = FALSE)
grant_paid_tbl <- read.csv("data/grants_paid_tbl.csv", stringsAsFactors = FALSE )
bod_match_tbl  <- read.csv("data/perc_match_tbl.csv", stringsAsFactors = FALSE) 
upcoming_tbl   <- read.csv("data/upcoming_grants.csv", stringsAsFactors = FALSE)

# 1. Generate active employee list
empl_list <- (empl_tbl %>% filter(Active == "Yes") %>% select(full_name))$full_name %>% sort
all_empl_list <- (empl_tbl %>% select(full_name))$full_name %>% sort

curr_year <- year(Sys.Date())

create_paid_tbl() 
create_upcoming_payments_tbl()
            

