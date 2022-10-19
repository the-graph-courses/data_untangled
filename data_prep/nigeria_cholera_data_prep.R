# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, table1)

nigeria_cholera_elimian <- 
  rio::import(here("ch04_data_wrangling/data_prep/nigeria_cholera_elimian.dta")) %>%
  as_tibble()

nigeria_cholera_elimian %>% 
  select(sex,
         age, 
         geo, 
         season, 
         setting, 
         )
  gtsummary::tbl_summary()
