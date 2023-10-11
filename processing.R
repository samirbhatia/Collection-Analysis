library(pacman)                       
p_load(tidyverse, janitor, lubridate, data.table, ggplot2, formattable, bslib, openxlsx)
select <- dplyr::select
branch <- c("jp", "nc", "pvs", "pvj")

pull <- function(branch) {
  allbranches <- read_rds(paste0("/Users/samirbhatia/Dropbox/SSMS/Dashboard/", 
                                 branch, "_newmaster.rds"))
  return(allbranches)
}

f1 <- function(x){
  y <- accounting(x, format = "f")
  return(y)
}

# Use function to gather data for all branches
all <- branch %>% map(~ pull(eval(.))) %>% set_names(branch)
stu <- all %>% map_df(~.[[1]]) %>% select(school, stu_id, admission_number, student_name, student_class, student_section)
ipj <- all %>% map_df(~.[[4]]) %>% select(school, ipj_id, inv_id, pay_id, stu_id, amount)
pay <- all %>% map_df(~.[[5]])
pay %>% select(school)
rm(all)
pay %>% str()
pay$payment_date <- as.POSIXct(ymd(pay$payment_date))
pay %>% filter(as.Date(payment_date) == Sys.Date())

pay <- pay %>% mutate(School = case_when(
  school == "jp" ~ "Janakpuri",
  school == "pvs" ~ "Paschim Vihar Sr",
  school == "nc" ~ "Narang Colony",
  TRUE ~ "Paschim Vihar Jr"
)) 


# pay %>% left_join(ipj %>% group_by(school, pay_id, stu_id) %>% summarise(s=sum(amount)), by = c("school", "pay_id")) %>% 
#   left_join(stu, by = c("school", "stu_id"))


pay %>% left_join(ipj %>% group_by(school, pay_id, stu_id) %>% summarise(s=sum(amount)), by = c("school", "pay_id")) %>% 
  left_join(stu, by = c("school", "stu_id")) %>% 
  group_by(school, pay_id, payment_date, admission_number, student_name, student_class, student_section) %>% 
  summarise(payment= sum(payment_amount_new, na.rm = TRUE)) %>% 
  mutate(payment_date= as.Date(payment_date)) %>% 
  arrange(payment_date, pay_id)


rep <- pay %>%    left_join(ipj %>% group_by(school, pay_id, stu_id) %>% summarise(s=sum(amount)), by = c("school", "pay_id")) %>% 
  left_join(stu, by = c("school", "stu_id")) %>% 
  group_by(school, pay_id, payment_date, admission_number, student_name, student_class, student_section) %>% 
  summarise(payment= sum(payment_amount_new, na.rm = TRUE)) %>% 
  mutate(payment_date= as.Date(payment_date)) %>% 
  arrange(payment_date, pay_id) %>% 
  adorn_totals(where = c("row"))