get_bod_tbl <- function(){
    
    bod_tbl = read.csv("data/perc_match_tbl.csv")
    
    bod_tbl <- bod_tbl %>% mutate(comp_perc = paste0(comp_perc * 100, "%")) %>%
               select(Year = year, "BOD %" = comp_perc)
    
    return(bod_tbl)
}

update_bod <- function(y, perc){
  bod_tbl = read.csv("data/perc_match_tbl.csv")
  bod_tbl[bod_tbl$year == y, ]$comp_perc <- (perc/100)
  
  write.csv(bod_tbl, "data/perc_match_tbl.csv", row.names = FALSE)
  bod_match_tbl <<- bod_tbl
  
  return(bod_tbl)
}