if(!require('pacman')) install.packages('pacman')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(here, 
               glue,
               praise,
               janitor,
               tidyverse)

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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_data_type ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_data_type <-
  function() {
    
    .problem_number <<- 1
    
    correct_answer <- "wide"
    
    .autograder <<-
      function() {
        if (!exists("Q_data_type"))
          .na("You have not yet defined the answer object, `Q_data_type`.")
        
        if (!is.character(Q_data_type))
          .na("Invalid answer. Your answer should be a single character string")
        
        if (isTRUE(all_equal(tolower(trimws(Q_data_type)),
                             correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_data_type <- function() {
  '
HINT.
  Each observational unit (each country) occupies just one row
' -> out
  cat(out)
}

.SOLUTION_Q_data_type <- function() {
  '
SOLUTION
  "Wide"' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_euro_births_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_euro_births_long <-
  function() {
    
    .problem_number <<- 2
    
    correct_answer <- 
      read_csv(here("data/euro_births_wide.csv")) %>% 
      pivot_longer(2:8, 
                   names_to = "year", 
                   values_to = "births_count")
    
    .autograder <<-
      function() {
        if (!exists("Q_euro_births_long"))
          .na("You have not yet defined the answer object, `Q_euro_births_long`.")
        
        if (!is.character(Q_euro_births_long))
          .na("Invalid answer. Your answer should be a single character string")
        
        if (isTRUE(all_equal(tolower(trimws(Q_euro_births_long)),
                             correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_euro_births_long <- function() {
  '
HINT.
  Each observational unit (each country) occupies just one row
' -> out
  cat(out)
}

.SOLUTION_Q_euro_births_long <- function() {
  '
SOLUTION
  "Wide"' -> out
  cat(out)
}



