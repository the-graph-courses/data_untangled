if(!require('pacman')) install.packages('pacman')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(praise,
               tidyverse,
               dplyr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.yaounde <- read_csv(here::here('ch04_data_wrangling/data/yaounde_data.csv'))
.yao <- .yaounde %>% 
  select(neighborhood, is_smoker, occupation, 
                            treatment_combinations, symptoms, 
                            age, sex, weight_kg, height_cm, 
                            igg_result) %>%
  filter(!is.na(is_smoker) & !is.na(sex) & !is.na(igg_result)) %>%
  mutate(is_smoker = as.factor(is_smoker), 
         sex = as.factor(sex), 
         igg_result = as.factor(igg_result))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1 <- .yao %>% 
              summarise(
                  q25 = quantile(weight_kg, .25, na.rm = TRUE),
                  q50 = quantile(weight_kg, .50, na.rm = TRUE),
                  q75 = quantile(weight_kg, .75, na.rm = TRUE),
                )
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q1)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q1, .q1))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q1 <- function(){ 
  "Check how it was applied to the variable `age` above." -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2 <- .yao %>% 
      group_by(is_smoker)%>%
      summarize(weight_mean = mean(weight_kg, na.rm = TRUE))
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q2)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q2, .q2))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q2 <- function(){ 
  "You only need to use `group_by()` and `summarise()`" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3 <- .yao %>%
        group_by(neighborhood, sex, igg_result, .drop = FALSE) %>%
        summarize(weight_median = median(weight_kg, na.rm = TRUE),
                  weight_IQR = IQR(weight_kg, na.rm = TRUE)) %>%
        ungroup()
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q3)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q3, .q3))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q3 <- function(){ 
  "Do not forget to use the `.drop = FALSE` argument and `ungroup()` after a `group_by()` plus `summarise()` combination!" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4 <- .yao%>%
      count(sex)%>%
      filter(symptoms=='No symptoms',
             sex=='Female')
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q4)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q4, .q4))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q4 <- function(){ 
  "Use `dplyr::filter()` before `dplyr::count()` to only keep the observation without symptoms" -> out
  cat(out)
}

