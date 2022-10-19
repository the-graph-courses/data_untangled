# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

set.seed(1)

outbreaks::fluH7N9_china_2013 %>% 
  as_tibble() %>% 
  mutate(case_id = as.numeric(case_id)) %>% 
  mutate(province = if_else(province == "Shanghai", "Shangh", as.character(province))) %>% 
  mutate(age = ifelse(age == "?", as.factor(NA), age)) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age = ifelse(row_number() %in% sample(1:nrow(.), size = 20),
                     NA, age)) %>% 
  write_csv(here("ch04_data_wrangling/data/flu_h7n9_china_2013.csv"))

outbreaks::ebola_sierraleone_2014 %>% 
  write_csv(here("ch04_data_wrangling/data/ebola_sierraleone_2014.csv"))

