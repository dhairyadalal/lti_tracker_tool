# Add new employee to empl_tbl and grant to grant_tbl
create_new_empl <- function(first, middle_i,  last, hire_date, grant_amount, grant_start, vest_sch){
  
  # 1. Insert empl into empl_tble
  in_rs <- insert_new_empl(first, middle_i, last, hire_date)
  
  new_empl <- in_rs$empl_id
  
  # 2. Update Grant table
  ugt_rs <- create_new_grant(new_empl, grant_amount, grant_start, vest_sch)
  return("success")
  
}

get_empl_id <- function(name){
  id <- empl_tbl %>% filter(full_name == name) %>% select(empl_id)
  id <- id$empl_id
  return(id)
}


# Create a new grant in the grant_tbl
create_new_grant <- function(eid, grant_amount, grant_start, vest_sch){

  grant_tbl = read.csv("data/grant_tbl.csv", stringsAsFactors = FALSE)
  
  # 1. Create Grant ID number
  existing_grants = grant_tbl %>% filter(empl_id == eid) # Check if user has existing grants
  new_id = ifelse(nrow(existing_grants) >= 1 , paste0("LTI-",nrow(existing_grants) +1), "LTI-1" ) 
    
  # 2. Create a new row for table
  grant_start = ymd(grant_start)
  exp_date <- grant_start + months(48) - days(1)
   
  new_row = list(new_id, eid, NA, year(grant_start), NA, year(exp_date) , grant_amount, vest_sch)
  names(new_row) = c("grant_id", "empl_id", "issue_date","issue_year", "expiration_date" ,"exp_year", "amount", "vesting_schedule")
  
  # 3. Update grant_tbl. Note rbind breaks formatting of date objects
  grant_tbl = rbind(grant_tbl, new_row)
  
  # 4. Add in the dates
  grant_tbl[grant_tbl$grant_id == new_id & grant_tbl$empl_id == eid,]$issue_date = as.character(grant_start)
  grant_tbl[grant_tbl$grant_id == new_id & grant_tbl$empl_id == eid,]$expiration_date = as.character(exp_date)
  write.csv(grant_tbl, "data/grant_tbl.csv", row.names = FALSE)
  grant_tbl <<- grant_tbl
  
  return("success")
}

# insert empl into empl_tbl
insert_new_empl <-function(first, middle_i, last, hire_date){
  # Read in table
  empl_tbl <- read.csv("data/empl_tbl.csv", stringsAsFactors = FALSE) 
  empl_tbl <- empl_tbl %>% arrange(empl_id)
  
  # Create new emplid and check it doesn't exist
  new_id = max(empl_tbl$empl_id) + 1
  hire_date = ymd(hire_date)
  
  full_name = ifelse( middle_i == "Middle Initial",  paste0(last, ", ", first), paste0(last, ", ", first, " ", middle_i, "."))
  
  new_row = list(new_id,last, middle_i, first, full_name, NA , year(hire_date), NA, NA, "Active"  )
  empl_tbl <- empl_tbl %>% rbind(new_row)
  
  # rbind doesn't properly translate the date format. need to reupdate the table
  empl_tbl[empl_tbl$empl_id == new_id,]$hire_date = as.character(hire_date)  
  print(class(empl_tbl$hire_date))
  
  # Write table
  write.csv(empl_tbl, "data/empl_tbl.csv", row.names = FALSE)
  empl_tbl <<- empl_tbl
  
  return(list("status"= "success", empl_id = new_id))
}