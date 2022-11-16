if(!require('pacman')) install.packages('pacman')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(here, 
               glue,
               praise,
               janitor,
               tidyverse)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Put total number of questions as `times` argument

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
    
    .euro_births_wide <- read_csv(here("data/euro_births_wide.csv"))
    
    correct_answer <- 
      .euro_births_wide %>% 
      pivot_longer(2:8, 
                   names_to = "year", 
                   values_to = "births_count") %>% 
     mutate(year = readr::parse_number(year))
    
   
    
    .autograder <<-
      function() {
        if (!exists("Q_euro_births_long"))
          .na("You have not yet defined the answer object, `Q_euro_births_long`.")
        
        if (!is.data.frame(Q_euro_births_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_euro_births_long) != 3)
          .wrong("Wrong. Your answer should have three columns.")
        
        if (! all(names(Q_euro_births_long) %in% names(correct_answer)) )
          .wrong(paste0("Wrong. Your answer should have the following columns:", 
                        paste0(names(correct_answer), collapse = ", ")
                        ))
        
        parsed_answer <- Q_euro_births_long %>% mutate(year = readr::parse_number(year))
        
        if (isTRUE(all_equal(parsed_answer,
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
  
  Your code should look like this:
  
  euro_births_wide %>% 
      pivot_longer(COLS_TO_PIVOT, 
                   names_to = NAMES_COL_NAME, 
                   values_to = VALUES_COL_NAME)' -> out
  cat(out)
}

.SOLUTION_Q_euro_births_long <- function() {
  '
SOLUTION
  euro_births_wide %>% 
      pivot_longer(2:8, 
                   names_to = "year", 
                   values_to = "births_count")
' -> out
  cat(out)
}




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_euro_births_long ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_euro_births_long <-
  function() {
    
    .problem_number <<- 2
    
    .euro_births_wide <- read_csv(here("data/euro_births_wide.csv"))
    
    correct_answer <- 
      .euro_births_wide %>% 
      pivot_longer(2:8, 
                   names_to = "year", 
                   values_to = "births_count")
    
    
    .autograder <<-
      function() {
        if (!exists("Q_euro_births_long"))
          .na("You have not yet defined the answer object, `Q_euro_births_long`.")
        
        if (!is.data.frame(Q_euro_births_long))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_euro_births_long) != 3)
          .wrong("Wrong. Your answer should have three columns.")
        
        if (! all(names(Q_euro_births_long) %in% names(correct_answer)) )
          .wrong(paste0("Wrong. Your answer should have the following columns:", 
                        paste0(names(correct_answer), collapse = ", ")
          ))
        
        parsed_answer <- Q_euro_births_long %>% mutate(year = readr::parse_number(year))
        
        if (isTRUE(all_equal(parsed_answer,
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
  
  Your code should look like this:
  
  euro_births_wide %>% 
      pivot_longer(COLS_TO_PIVOT, 
                   names_to = NAMES_COL_NAME, 
                   values_to = VALUES_COL_NAME)' -> out
  cat(out)
}

.SOLUTION_Q_euro_births_long <- function() {
  '
SOLUTION
  euro_births_wide %>% 
      pivot_longer(2:8, 
                   names_to = "year", 
                   values_to = "births_count")
' -> out
  cat(out)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_infant_deaths_wide ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_infant_deaths_wide <-
  function() {
    
    .problem_number <<- 3
    
    
    correct_answer <- 
      tidyr::population %>% 
      pivot_wider(names_from = year, 
                  values_from = population)
    
    
    .autograder <<-
      function() {
        if (!exists("Q_infant_deaths_wide"))
          .na("You have not yet defined the answer object, `Q_infant_deaths_wide`.")
        
        if (!is.data.frame(Q_infant_deaths_wide))
          .na("Invalid answer. Your answer should be a data frame")
        
        if (ncol(Q_infant_deaths_wide) != 3)
          .wrong("Wrong. Your answer should have three columns.")
        
        if (! all(names(Q_infant_deaths_wide) %in% names(correct_answer)) )
          .wrong(paste0("Wrong. Your answer should have the following columns:", 
                        paste0(names(correct_answer), collapse = ", ")
          ))
        
        parsed_answer <- Q_infant_deaths_wide %>% mutate(year = readr::parse_number(year))
        
        if (isTRUE(all_equal(parsed_answer,
                             correct_answer)))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_infant_deaths_wide <- function() {
  '
HINT.
  
  Your code should look like this:
  
  euro_births_wide %>% 
      pivot_longer(COLS_TO_PIVOT, 
                   names_to = NAMES_COL_NAME, 
                   values_to = VALUES_COL_NAME)' -> out
  cat(out)
}

.SOLUTION_Q_infant_deaths_wide <- function() {
  '
SOLUTION
  euro_births_wide %>% 
      pivot_longer(2:8, 
                   names_to = "year", 
                   values_to = "births_count")
' -> out
  cat(out)
}



