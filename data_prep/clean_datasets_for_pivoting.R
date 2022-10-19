# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, rio, clipr)


# deaths
infant_deaths_raw <- rio::import("https://docs.google.com/spreadsheets/d/1jVIpengplH1EtZ9D5QSQ0yLTf4VKrrOaUbP7zJpzAXk/edit?usp=sharing", 
            setclass = "tbl") %>% 
  row_to_names(1) %>% 
  clean_names()
  

infant_deaths_clean <- 
  infant_deaths_raw %>% 
  mutate(across(.cols = x2010:x2015, 
                .fns = 
                  ~ ifelse(str_detect(.x, "k"), 
                            as.numeric(str_remove(.x, "k")) * 1000,
                           ifelse(str_detect(.x, "M"), 
                                    as.numeric(str_remove(.x, "M")) * 1000000,
                                    as.numeric(.x)
                            )),
  )) 
  
infant_deaths_clean %>%
  clipr::write_clip()


# births
infant_births_raw <- rio::import("https://docs.google.com/spreadsheets/d/1QgAxsJTUyNWr_hUKC055XE-A50niszifdQwDEdLpLEU/edit?usp=sharing", 
                                 setclass = "tbl") %>% 
  row_to_names(1) %>% 
  clean_names() %>% 
  select(country, x2010:x2015)


infant_births_clean <- 
  infant_births_raw %>% 
  mutate(across(.cols = x2010:x2015, 
                .fns = 
                  ~ ifelse(str_detect(.x, "k"), 
                            as.numeric(str_remove(.x, "k")) * 1000,
                           ifelse(str_detect(.x, "M"), 
                                    as.numeric(str_remove(.x, "M")) * 1000000,
                                    as.numeric(.x)
                                    ))
         )) 


infant_births_clean %>%
  clipr::write_clip()


# euro births

# Births in Europe
# From [Eurostat](https://ec.europa.eu/eurostat/databrowser/view/tps00204/default/table). 
# Tidied in [GSheets](https://docs.google.com/spreadsheets/d/1qvSVYuTKBHt0a0lAO16kHzQvJT3XW8RWB9uonhJNlVA/edit?usp=sharing)
euro_births_wide <- rio::import("https://docs.google.com/spreadsheets/d/1qvSVYuTKBHt0a0lAO16kHzQvJT3XW8RWB9uonhJNlVA/edit?usp=sharing", setclass = "tbl") %>% 
  row_to_names(1) %>% 
  clean_names() %>% 
  rename(country = entity) %>% 
  mutate(across(.cols = x2015:x2021, 
                .fns = ~ str_remove_all(., "'") %>% parse_number()
  )) %>% 
  readr::type_convert()


euro_births_wide %>% 
  clipr::write_clip()
