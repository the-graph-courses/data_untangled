#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Big Data Frame
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("ch04_data_wrangling/data_prep/yaounde_original_cleaning_script.R"))

yaounde_original_cleaned <- yao
  
big_yao <-
  yaounde_original_cleaned %>%
  ## add specific symptoms
  mutate(symp_fever = case_when(str_detect(mcat_symp, "Fever") ~ "Yes", TRUE ~ "No"), 
         symp_headache = case_when(str_detect(mcat_symp, "Headache") ~ "Yes", TRUE ~ "No"),
         symp_cough = case_when(str_detect(mcat_symp, "Cough") ~ "Yes", TRUE ~ "No"),
         symp_rhinitis = case_when(str_detect(mcat_symp, "Rhinitis") ~ "Yes", TRUE ~ "No"),
         symp_sneezing = case_when(str_detect(mcat_symp, "Sneezing") ~ "Yes", TRUE ~ "No"),
         symp_fatigue = case_when(str_detect(mcat_symp, "Fatigue") ~ "Yes", TRUE ~ "No"),
         symp_muscle_pain = case_when(str_detect(mcat_symp, "Muscle pain") ~ "Yes", TRUE ~ "No"),
         symp_nausea_or_vomiting = case_when(str_detect(mcat_symp, "Nausea or vomiting") ~ "Yes", TRUE ~ "No"),
         symp_diarrhoea = case_when(str_detect(mcat_symp, "Diarrhoea") ~ "Yes", TRUE ~ "No"),
         symp_short_breath = case_when(str_detect(mcat_symp, "Shortness of breath") ~ "Yes", TRUE ~ "No"),
         symp_sore_throat = case_when(str_detect(mcat_symp, "Sore throat") ~ "Yes", TRUE ~ "No"),
         symp_anosmia_or_ageusia = case_when(str_detect(mcat_symp, "Anosmia or ageusia") ~ "Yes", TRUE ~ "No"),
         symp_stomach_ache = case_when(str_detect(mcat_symp, "Stomach ache") ~ "Yes", TRUE ~ "No")) %>% 
  ## modify `is_pregnant` column. Needed for illustration in case_when() lesson
  mutate(is_pregnant = case_when(is.na(is_pregnant) & cat_sex == "Female" ~ "No response", 
                                 TRUE ~ is_pregnant)) %>% 
  ## modify `is_pregnant` column 
  mutate(mcat_drug = if_else(mcat_drug == "No medication", NA_character_, 
                             mcat_drug)) %>% 
  mutate(mcat_drug = if_else(mcat_drug == "No response", NA_character_, 
                             mcat_drug)) %>% 
  mutate(age_category_3 = case_when(val_age < 18 ~ "Child", 
                                 val_age >= 18 & val_age <= 64 ~ "Adult",
                                 val_age >= 65 ~ "Senior"
                                    )) %>% 
  ## arrange columns in a systematic order
  ## in quotes just in case you need to pull out as vector of names
  select("id_ind", "dt_quest",
         ## basic demographics
         "val_age", "cat_age",
         age_category_3,
         "cat_sex", "cat_educ", "mcat_occup",
         ## biometric
         "val_weight_kg", "val_height_cm",     
         ## risk factor
         "is_smoker", "is_pregnant", "is_medicated",   
         ## family xteristics                                            
         "loc_hhld_area", "has_hhld_children", "mcat_breadwin", "mcat_rev_source",
         ## COVID
         "has_contact_COVID", "cat_pos", "cat_igm_result",
         ## COVID symp and treatments
         "mcat_symp", 
         starts_with("symp_"),
         n_days_miss_work,
         n_bedridden_days,
         "mcat_consult", "mcat_drug", "mcat_drugsource", "mcat_hospitalised", "mcat_sequela",
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

write_csv(big_yao, here::here('ch04_data_wrangling/data/yaounde_data.csv')) 


 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gender, age, IgG 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yao_data_gender <- 
  yao %>% 
  select(cat_age, val_age, cat_sex, cat_pos, val_weight_kg, val_height_cm) %>%
  mutate(cat_pos = case_when(cat_pos=='Positive' ~ 'IgG seropositive',
                             cat_pos=='Negative' ~ 'IgG seronegative')) %>%
  rename(IgG_label = cat_pos,
         age = val_age,
         age_category = cat_age,
         sex = cat_sex,
         weight_kg = val_weight_kg,
         height_cm = val_height_cm)

#write.csv(yao_data_gender, here::here('3_data_bivariate_analysis/yao_data_gender.csv'),row.names=FALSE) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Breathing Frequency 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yao_data_breathing_frequency <- 
  yao %>% 
  select(cat_age, val_age, cat_sex, val_weight_kg, val_height_cm, val_freq_respiration) %>%
  filter(!is.na(val_freq_respiration)) %>%
  rename(age = val_age,
         age_category = cat_age,
         breathing_frequency = val_freq_respiration,
         sex = cat_sex,
         weight_kg = val_weight_kg,
         height_cm = val_height_cm)

#write.csv(yao_data_breathing_frequency, here::here('3_data_bivariate_analysis/yao_data_breathing_frequency.csv'), row.names=FALSE) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Drugs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yao_data_drugs <- 
  yao %>% 
  select(cat_age, val_age, cat_sex, cat_pos, val_weight_kg, val_height_cm, 
         contains("is_drug_"))  %>%
  mutate(treatment = case_when(is_drug_none2 == 1 ~ 'None',
                               is_drug_none == 1 ~ 'None',
                               is_drug_antibio2 == 1 ~ 'Antibiotics',
                               is_drug_antibio == 1 ~ 'Antibiotics',
                               is_drug_parac == 1 ~ 'Paracetamol',
                               is_drug_parac2 == 1 ~ 'Paracetamol',
                               is_drug_oxygen == 1 ~ 'Oxygen',
                               is_drug_chloro == 1 ~ 'Chloroquine' )) %>% #exercice remove NA from the list 
  select(!contains("is_drug_")) %>%
  rename(IgG_label = cat_pos,
         age = val_age,
         age_category = cat_age,
         sex = cat_sex,
         weight_kg = val_weight_kg,
         height_cm = val_height_cm)

#write.csv(yao_data_drugs, here::here('3_data_bivariate_analysis/yao_data_drugs.csv'),row.names=FALSE)  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Medical Consultation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

yao_data_consult <- 
  yao %>% 
  select(cat_age, val_age, cat_sex, 
         contains("is_consult")) %>%
  mutate(consultation = case_when(is_consult_pub_health_cent == 1 ~ 'Public Health Center',
                                  is_consult_priv_health_cent == 1 ~ 'Private Health Center',
                                  is_consult_pub_hosp == 1 ~ 'Public Hospital',
                                  is_consult_priv_hosp == 1 ~ 'Private Hospital',
                                  is_consult_tradn == 1 ~ 'Traditional Healer',
                                  is_consult_relig == 1 ~ 'Priest/religious',
                                  is_consult_doctor == 1 ~ 'Doctor',
                                  is_consult_nurse == 1 ~ 'Nurse',
                                  is_consult_covid_urg== 1 ~ 'Emergency Room',
                                  is_consult_opt_15 == 1 ~ 'Covid Unit',
                                  is_consult_test_center == 1 ~ 'Testing Center',
                                  is_consult_pub_health_cent2 == 1 ~ 'Public Health Center',
                                  is_consult_priv_health_cent2 == 1 ~ 'Private Health Center',
                                  is_consult_pub_hosp2 == 1 ~ 'Public Hospital',
                                  is_consult_priv_hosp2 == 1 ~ 'Private Hospital',
                                  is_consult_tradn2 == 1 ~ 'Traditional Healer',
                                  is_consult_relig2 == 1 ~ 'Priest/religious',
                                  is_consult_unit_covid == 1 ~ 'Covid Unit',
                                  is_consult_non_COVID_pub_health_cent == 1 ~ 'Public Health Center',
                                  is_consult_non_COVID_priv_health_cent == 1 ~ 'Private Health Center',
                                  is_consult_non_COVID_pub_hosp == 1 ~ 'Public Hospital',
                                  is_consult_non_COVID_priv_hosp == 1 ~ 'Private Hospital',
                                  is_consult_non_COVID_tradn == 1 ~ 'Traditional Healer',
                                  is_consult_non_COVID_relig == 1 ~ 'Priest/religious',
                                  is_consult_non_COVID_doctor == 1 ~ 'Doctor',
                                  is_consult_non_COVID_nurse == 1 ~ 'Nurse'
                                  )) %>% #exercice remove NA from the list
  select(!contains("is_consult_")) %>%
  rename(age = val_age,
         age_category = cat_age,
         sex = cat_sex)

#write.csv(yao_data_consult, here::here('3_data_bivariate_analysis/yao_data_consult.csv'), row.names=FALSE)
  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Symptoms and IgG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

count_pos <- 
  yao %>% 
  count(cat_pos) %>% 
  rename(sum = n)

symptoms_per_sero_cat <- 
  yao %>% 
  separate_rows(mcat_symp, sep = "--") %>% 
  group_by(cat_pos, mcat_symp) %>% 
  count() %>%
  ungroup() %>% 
  left_join(count_pos) %>% 
  mutate(pct = 100*n/sum) %>% 
  filter(!is.na(cat_pos)) %>% 
  filter(mcat_symp != "No symptoms") %>% 
  filter(mcat_symp != "Other")

yao_symptoms_igg <- 
  symptoms_per_sero_cat %>%
  select(cat_pos, mcat_symp, n , sum) %>%
  mutate(symptom_id = sample(1:500, 26, replace=FALSE)) %>%
  mutate(mcat_symp = fct_reorder(mcat_symp, n)) %>%
  mutate(cat_pos = fct_rev(cat_pos)) %>%
  mutate(cat_pos = dplyr::recode(cat_pos,
                                 "Negative" = "IgG seronegative",
                                 "Positive" = "IgG seropositive"))

#write.csv(yao_symptoms_igg, here::here('3_data_bivariate_analysis/yao_symptoms_igg.csv'), row.names=FALSE)

# symptoms_data <- 
#   symptoms_per_sero_cat %>%
#   left_join(symptoms_chi) %>%
#   mutate(mcat_symp = fct_reorder(mcat_symp, n)) %>%
#   mutate(cat_pos = fct_rev(cat_pos)) %>%
#   mutate(cat_pos = dplyr::recode(cat_pos,
#                           "Negative" = "IgG seronegative",
#                           "Positive" = "IgG seropositive")) %>%
#   mutate(p_val = round(p_val, digits = 2),
#          p_val_paste = paste0("p = ", p_val), 
#          pct = round(pct, 2),
#          pct_1 = round(pct, 1),
#          pct_paste = paste0("**", pct_1, "%","**&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;", "<span style='color:gray30'>", 
#                             "(",  n, ")", "</span>"),
#          #hjust = ifelse(pct < 2, 0, 1), 
#          #color = ifelse(hjust == 0, "black", "white"), 
#          hjust = -0.1,
#          color = "black")
