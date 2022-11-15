if(!require('pacman')) install.packages('pacman')

pacman::p_install_gh('graph-courses/autograder')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(here, 
               glue,
               praise,
               janitor,
               tidyverse)

# adapt to questions of this lesson

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.yaounde <- read_csv(here("data/yaounde_data.csv"))
.yaounde <- .yaounde %>% rename(age_years = age)

.diet <- read_csv(here("data/vietnam_diet_diversity.csv"))
.diet <- .diet %>% rename(household_id = hhid)

.febrile_diseases <- read_csv(here("data/febrile_diseases_burkina_faso.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 10)   # Put total number of questions as `times` argument

# adapt to questions of this lesson
.NUM_Q_febrile_disease_symptoms <- 1
.NUM_Q_febrile_disease_symptoms_to_lower <- 2
.NUM_Q_febrile_disease_symptoms_to_numeric <- 3
.NUM_Q_diet_to_grams <- 4
.NUM_Q_febrile_disease_symptoms_to_numeric_new_variables <- 5
.NUM_Q_diet_FAO_mean <- 6
.NUM_Q_febrile_disease_symptoms_count <- 7
.NUM_Q_yaounde_median <- 8 
.NUM_Q_diet_food_composition_mean_sd <- 9 
.NUM_Q_febrile_diseases_mean_blood_composition <- 10


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  ANSWERS ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_febrile_disease_symptoms ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_febrile_disease_symptoms <-
  function() {
    
    .problem_number <<- .NUM_Q_febrile_disease_symptoms
    correct_answer <- .febrile_diseases %>% 
      mutate(across(.cols = abd_pain:splenomegaly,
                    .fns = toupper))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_febrile_disease_symptoms"))
          .na("You have not yet defined the answer object, `Q_febrile_disease_symptoms`.")
        if (!is.data.frame(Q_febrile_disease_symptoms))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_febrile_disease_symptoms, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_febrile_disease_symptoms <- function(){
  '
HINT.
Your answer should look this: 

Q_febrile_disease_symptoms <- 
  febrile_diseases %>% 
  mutate(across(COLUMNS,
                FUNCTION))' -> out
  cat(out)
}

.SOLUTION_Q_febrile_disease_symptoms <- function(){
  '
SOLUTION
Q_febrile_disease_symptoms <- 
  febrile_diseases %>% 
  mutate(across(.cols = abd_pain:splenomegaly,
                .fns = toupper))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_febrile_disease_symptoms_to_lower ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_febrile_disease_symptoms_to_lower <-
  function() {
    
    .problem_number <<- .NUM_Q_febrile_disease_symptoms_to_lower
    correct_answer <- .febrile_diseases %>% 
      mutate(across(.cols = abd_pain:splenomegaly,
                    .fns = tolower))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_febrile_disease_symptoms_to_lower"))
          .na("You have not yet defined the answer object, `Q_febrile_disease_symptoms_to_lower`.")
        if (!is.data.frame(Q_febrile_disease_symptoms_to_lower))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_febrile_disease_symptoms_to_lower, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_febrile_disease_symptoms_to_lower <- function(){
  '
HINT.
Your answer should look this: 

Q_febrile_disease_symptoms_to_lower <- 
  febrile_diseases %>% 
  mutate(across(COLUMNS,
                FUNCTION))' -> out
  cat(out)
}

.SOLUTION_Q_febrile_disease_symptoms_to_lower <- function(){
  '
SOLUTION
Q_febrile_disease_symptoms_to_lower <- 
  febrile_diseases %>% 
  mutate(across(.cols = abd_pain:splenomegaly,
                .fns = tolower))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_febrile_disease_symptoms_to_numeric ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_febrile_disease_symptoms_to_numeric <-
  function() {
    
    .problem_number <<- .NUM_Q_febrile_disease_symptoms_to_numeric
    correct_answer <- .febrile_diseases %>% 
      mutate(across(.cols = abd_pain:splenomegaly,
                    .fns = ~ if_else(.x == "yes", 1, 0)))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_febrile_disease_symptoms_to_numeric"))
          .na("You have not yet defined the answer object, `Q_febrile_disease_symptoms_to_numeric`.")
        if (!is.data.frame(Q_febrile_disease_symptoms_to_numeric))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_febrile_disease_symptoms_to_numeric, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_febrile_disease_symptoms_to_numeric <- function(){
  '
HINT.
Your answer should look this: 

Q_febrile_disease_symptoms_to_numeric <- 
  febrile_diseases %>% 
  mutate(across(COLUMNS,
                CUSTOM FUNCTION))' -> out
  cat(out)
}

.SOLUTION_Q_febrile_disease_symptoms_to_numeric <- function(){
  '
SOLUTION
Q_febrile_disease_symptoms_to_numeric <- 
  febrile_diseases %>% 
  mutate(across(.cols = abd_pain:splenomegaly,
                .fns = ~ if_else(.x == "yes", 1, 0)))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_diet_to_grams ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_diet_to_grams <-
  function() {
    
    .problem_number <<- .NUM_Q_diet_to_grams
    correct_answer <- .diet %>% 
      mutate(across(.cols = retinol:zinc,
                    .fns = ~ .x/1000))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_diet_to_grams"))
          .na("You have not yet defined the answer object, `Q_diet_to_grams`.")
        if (!is.data.frame(Q_diet_to_grams))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_diet_to_grams, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_diet_to_grams <- function(){
  '
HINT.
Your answer should look this: 

Q_diet_to_grams <- 
  diet %>% 
  mutate(across(COLUMNS,
                CUSTOM FUNCTION))' -> out
  cat(out)
}

.SOLUTION_Q_diet_to_grams <- function(){
  '
SOLUTION
Q_diet_to_grams <- 
  diet %>% 
  mutate(across(.cols = retinol:zinc,
                .fns = ~ .x/1000))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_febrile_disease_symptoms_to_numeric_new_variables ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_febrile_disease_symptoms_to_numeric_new_variables <-
  function() {
    
    .problem_number <<- .NUM_Q_febrile_disease_symptoms_to_numeric_new_variables
    correct_answer <- .febrile_diseases %>% 
      mutate(across(.cols = abd_pain:splenomegaly,
                    .fns = ~ if_else(.x == "yes", 1, 0),
                    .names = "numeric_{.col}"))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_febrile_disease_symptoms_to_numeric_new_variables"))
          .na("You have not yet defined the answer object, `Q_febrile_disease_symptoms_to_numeric_new_variables`.")
        if (!is.data.frame(Q_febrile_disease_symptoms_to_numeric_new_variables))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_febrile_disease_symptoms_to_numeric_new_variables, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_febrile_disease_symptoms_to_numeric_new_variables <- function(){
  '
HINT.
Your answer should look this: 

Q_febrile_disease_symptoms_to_numeric_new_variables <- 
  febrile_diseases %>% 
  mutate(across(COLUMNS
                CUSTOM FUNCTION,
                NAMES))' -> out
  cat(out)
}

.SOLUTION_Q_febrile_disease_symptoms_to_numeric_new_variables <- function(){
  '
SOLUTION
Q_febrile_disease_symptoms_to_numeric_new_variables <- 
  febrile_diseases %>% 
  mutate(across(.cols = abd_pain:splenomegaly,
                .fns = ~ if_else(.x == "yes", 1, 0),
                .names = "numeric_{.col}"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_diet_FAO_mean ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_diet_FAO_mean <-
  function() {
    
    .problem_number <<- .NUM_Q_diet_FAO_mean
    correct_answer <- .diet %>% 
      summarize(across(.cols = fao_fgw1:fao_fgw21, 
                       .fns = ~ mean(.x, na.rm = TRUE)))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_diet_FAO_mean"))
          .na("You have not yet defined the answer object, `Q_diet_FAO_mean`.")
        if (!is.data.frame(Q_diet_FAO_mean))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_diet_FAO_mean, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_diet_FAO_mean <- function(){
  '
HINT.
Your answer should look this: 

Q_diet_FAO_mean <- 
  diet %>% 
  summarize(across(COLUMNS
                CUSTOM FUNCTION))' -> out
  cat(out)
}

.SOLUTION_Q_diet_FAO_mean <- function(){
  '
SOLUTION
Q_diet_FAO_mean <- 
  diet %>% 
  summarize(across(.cols = fao_fgw1:fao_fgw21, 
                   .fns = ~ mean(.x, na.rm = TRUE)))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_febrile_disease_symptoms_count ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_febrile_disease_symptoms_count <-
  function() {
    
    .problem_number <<- .NUM_Q_febrile_disease_symptoms_count
    correct_answer <- .febrile_diseases %>% 
      summarize(across(.cols = abd_pain:splenomegaly, 
                       .fns = ~ sum(.x == "yes")))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_febrile_disease_symptoms_count"))
          .na("You have not yet defined the answer object, `Q_febrile_disease_symptoms_count`.")
        if (!is.data.frame(Q_febrile_disease_symptoms_count))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_febrile_disease_symptoms_count, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_febrile_disease_symptoms_count <- function(){
  '
HINT.
Your answer should look this: 

Q_febrile_disease_symptoms_count <- 
  febrile_diseases %>% 
  summarize(across(COLUMNS
                CUSTOM FUNCTION))' -> out
  cat(out)
}

.SOLUTION_Q_febrile_disease_symptoms_count <- function(){
  '
SOLUTION
Q_febrile_disease_symptoms_count <- 
  febrile_diseases %>% 
  summarize(across(.cols = abd_pain:splenomegaly, 
                   .fns = ~ sum(.x == "yes")))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_yaounde_median ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_yaounde_median <-
  function() {
    
    .problem_number <<- .NUM_Q_yaounde_median
    correct_answer <- .yaounde %>%
      summarize(across(.cols = c(age_years,height_cm, weight_kg, n_days_miss_work, n_bedridden_days),
                       .fns = ~ median(.x, na.rm=TRUE)))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_yaounde_median"))
          .na("You have not yet defined the answer object, `Q_yaounde_median`.")
        if (!is.data.frame(Q_yaounde_median))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_yaounde_median, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_yaounde_median <- function(){
  '
HINT.
Your answer should look this: 

Q_yaounde_median <- 
  yaounde %>% 
  summarize(across(COLUMNS
                CUSTOM FUNCTION))' -> out
  cat(out)
}

.SOLUTION_Q_yaounde_median <- function(){
  '
SOLUTION
Q_yaounde_median <- 
  yaounde %>%
  summarize(across(.cols = c(age_years,height_cm, weight_kg, n_days_miss_work, n_bedridden_days),
                   .fns = ~ median(.x, na.rm=TRUE)))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_diet_food_composition_mean_sd ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_diet_food_composition_mean_sd <-
  function() {
    
    .problem_number <<- .NUM_Q_diet_food_composition_mean_sd
    correct_answer <- .diet %>% 
      summarize(across(.cols = kilocalories_consumed:carbs_consumed_grams,
                       .fns = list(mean = mean,
                                   sd = sd)))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_diet_food_composition_mean_sd"))
          .na("You have not yet defined the answer object, `Q_diet_food_composition_mean_sd`.")
        if (!is.data.frame(Q_diet_food_composition_mean_sd))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_diet_food_composition_mean_sd, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_diet_food_composition_mean_sd <- function(){
  '
HINT.
Your answer should look this: 

Q_diet_food_composition_mean_sd <- 
  diet %>% 
  summarize(across(COLUMNS
                  FUNCTIONS))' -> out
  cat(out)
}

.SOLUTION_Q_diet_food_composition_mean_sd <- function(){
  '
SOLUTION
Q_diet_food_composition_mean_sd <- 
  diet %>% 
  summarize(across(.cols = kilocalories_consumed:carbs_consumed_grams,
                   .fns = list(mean = mean,
                               sd = sd)))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_febrile_diseases_mean_blood_composition ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_febrile_diseases_mean_blood_composition <-
  function() {
    
    .problem_number <<- .NUM_Q_febrile_diseases_mean_blood_composition
    correct_answer <- .febrile_diseases %>%
      summarize(across(.cols = wbc:relymp_a,
                       .fns = list( mean = ~mean(.x, na.rm=TRUE),
                                    sd = ~sd(.x, na.rm=TRUE))))
    
    
    .autograder <<-
      function(){
        if(!exists("Q_febrile_diseases_mean_blood_composition"))
          .na("You have not yet defined the answer object, `Q_febrile_diseases_mean_blood_composition`.")
        if (!is.data.frame(Q_febrile_diseases_mean_blood_composition))
          .na("Invalid answer. Your answer should be a data frame.")
        
        if (isTRUE(all_equal(Q_febrile_diseases_mean_blood_composition, correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_febrile_diseases_mean_blood_composition <- function(){
  '
HINT.
Your answer should look this: 

Q_febrile_diseases_mean_blood_composition <- 
  febrile_diseases %>% 
  summarize(across(COLUMNS
                  CUSTOM FUNCTIONS))' -> out
  cat(out)
}

.SOLUTION_Q_febrile_diseases_mean_blood_composition <- function(){
  '
SOLUTION
Q_febrile_diseases_mean_blood_composition <- 
  febrile_diseases %>%
  summarize(across(.cols = wbc:relymp_a,
                   .fns = list( mean = ~mean(.x, na.rm=TRUE),
                                sd = ~sd(.x, na.rm=TRUE))))' -> out
  cat(out)
}

