# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, AMR)

set.seed(1)
sarcopenia_elderly <- 
  read_csv(here("ch04_data_wrangling/data_prep/sarcopenia_elderly_project.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(sex = as.numeric(sex == "male")) %>% 
  rename(sex_male_1_female_0 = sex) %>% 
  mutate(height = height/100) %>% 
  rename(height_meters = height) %>% 
  ## add random decimals to ages 
  mutate(age = age + sample(0:9/10, size = nrow(.), replace = T)) %>% 
  # age groups 
  mutate(age_group = AMR::age_groups(age, split_at = "tens"),
         age_group = fct_drop(age_group),
         age_group = case_when(age >= 60 & age < 70 ~ "Sixties", 
                               age >= 70 & age < 80 ~ "Seventies", 
                               age >= 80 & age < 90 ~ "Eighties"))

# Deduce and remove non-asians to simplify sarcopenia logic.
# Based on this sentence from the source paper(https://doi.org/10.12688/f1000research.22580.1): 
# "ASM = (0.244 * body weight) + (7.8 * height) + (6.6 *gender) – (0.098 * age) + (race – 3.3). For Asian people this was calculated as -1.2"

non_asian_id_number <- 
  sarcopenia_elderly %>% 
  mutate(appendicular_skeletal_mass_new = 
           0.244 * weight + 
           7.8 * height_meters +
           6.6 * sex_male_1_female_0 - 
           0.098 * age - 
           (3.3 + 1.2) ) %>% 
  select(number, 
         appendicular_skeletal_mass, 
         appendicular_skeletal_mass_new) %>%
  mutate(formula_works = appendicular_skeletal_mass_new > appendicular_skeletal_mass - 0.5 & 
           appendicular_skeletal_mass_new < appendicular_skeletal_mass + 0.5 ) %>% 
  filter(!formula_works) %>% 
  .$number

sarcopenia_elderly %>% 
  filter(number != non_asian_id_number) %>% 
  select(number, 
         age, 
         age_group,
         sex_male_1_female_0, 
         #socioeconomic_class, 
         marital_status, 
         height_meters, 
         weight_kg = weight, 
         #gait_speed_meters_per_second, 
         grip_strength_kg = gripstrength, 
         # appendicular_skeletal_mass, 
         skeletal_muscle_index) %>% 
  write_csv(here("ch04_data_wrangling/data/sarcopenia_elderly.csv"))

