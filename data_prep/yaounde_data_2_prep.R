#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Big Data Frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("ch04_data_wrangling/data_prep/yaounde_original_cleaning_script.R"))

yao_subset <-
  yao %>%
  ## arrange columns in a systematic order
  ## in quotes just in case you need to pull out as vector of names
  select("id_ind", "dt_quest",
         ## basic demographics
         "val_age", "cat_age", "cat_sex", "cat_educ", "mcat_occup",
         ## biometric
         "val_weight_kg", "val_height_cm",     
         ## risk factor
         "is_smoker", "is_pregnant", "is_medicated",   
         ## family xteristics                                            
         "loc_hhld_area", "has_hhld_children", "mcat_breadwin", "mcat_rev_source",
         ## COVID
         "has_contact_COVID", "cat_pos", "cat_igm_result",
         ## COVID symp and treatments
         "mcat_symp", "mcat_consult", "mcat_drug", "mcat_drugsource", "mcat_hospitalised", "mcat_sequela",
         ## vital signs
         "val_freq_respiration", 
         ## if there is an is_drug variable suffixed with "2", then the first variable is problematic. Easiest to pick by hand
         "is_drug_parac2",
         "is_drug_antibio2", 
         "is_drug_hydrocortisone",
         "is_drug_other_anti_inflam2",
         "is_drug_antiviral2", 
         "is_drug_chloro2", 
         "is_drug_tradn2",
         "is_drug_oxygen",
         "is_drug_other",
         "is_drug_no_resp",
         "is_drug_none2") %>% 
  rename(id = id_ind, 
         date_surveyed = dt_quest,
         highest_education = cat_educ,
         respiration_frequency = val_freq_respiration,
         symptoms = mcat_symp,
         consultation = mcat_consult,
         treatment_combinations = mcat_drug,
         occupation = mcat_occup,
         drugsource = mcat_drugsource,
         hospitalised = mcat_hospitalised,
         breadwinner = mcat_breadwin,
         source_of_revenue = mcat_rev_source, 
         sequelae = mcat_sequela,
         age_category = cat_age, 
         age = val_age, 
         sex = cat_sex, 
         igg_result = cat_pos, 
         igm_result = cat_igm_result,
         has_contact_covid = has_contact_COVID,
         weight_kg = val_weight_kg, 
         height_cm = val_height_cm,
         neighborhood = loc_hhld_area,
         household_with_children = has_hhld_children) %>% 
  dplyr::rename_with(.cols = matches("2"), 
                     .fn = ~ str_remove_all(.x, "2"))


## Introduce error in dates so that read_csv fails to convert date. For teaching purposes later on. 

yao_modified <- 
  yao_subset %>% 
  mutate(date_surveyed = as.character(date_surveyed)) %>% 
  mutate(date_surveyed = case_when(date_surveyed == as.Date("2020-10-22") & row_number() == 1 ~"2020 October 22", 
                                   TRUE ~ date_surveyed ))
                                   
write_csv(yao_modified, here::here('ch04_data_wrangling/data/yaounde_data2.csv')) 


