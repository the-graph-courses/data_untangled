if(!require('pacman')) install.packages('pacman')

pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(praise)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.yaounde <- read_csv(here::here('data/yaounde_data.csv'))

.yao <-
  .yaounde %>% 
  select(age, sex, weight_kg, highest_education, neighborhood, 
         occupation, is_smoker, is_pregnant, 
         igg_result, igm_result)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 9)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1 <- .yao %>% filter(is_pregnant == "Yes")
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q1)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q1, .q1))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q1 <- function(){ 
  'YOURDATA %>% filter(CONDITION)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2 <- .yao %>%
      filter(sex == "Female") %>%
      nrow()
    
    .autograder <<- 
      function(){
        if (!is.numeric(q2)) return(c(value = -1, message = "Your result should be numeric."))
        
        if (identical(q2, .q2)) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q2 <- function(){ 
'YOURDATA %>% 
  filter(sex == "YOURSTRING") %>%
  nrow()' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3 <- .yao %>%
      filter(age < 18)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q3)) return(c(value = -1, message = "Your result should be a data frame."))
        
        if (isTRUE(all_equal(q3, .q3))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q3 <- function(){ 
'YOURDATA %>% 
  filter(CONDITION)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4 <- .yao %>%
      filter(neighborhood %in% c("Tsinga", "Messa") ) 
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q4)) return(c(value = -1, message = "Your result should be a data frame."))
        
        if (isTRUE(all_equal(q4, .q4))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q4 <- function(){ 
'YOURDATA %>% 
  filter(neighborhood %in% c("STRING1", "STRING2"))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5 <- .yao %>%
      filter(sex == "Male" & igg_result == "Positive") 
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q5)) return(c(value = -1, message = "Your result should be a data frame."))
        
        if (isTRUE(all_equal(q5, .q5))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q5 <- function(){ 
'YOURDATA %>% 
  filter(sex == "Male" & CONDITION2) %>%
  nrow()' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q6 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q6 <-
  function() {
    .problem_number <<- 6
    
    .q6 <- .yao %>% filter(age < 18 | highest_education == "Primary")
    
    .q6_wrong_operator <- .yao %>% filter(age < 18 & highest_education == "Primary")
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q6)) return(c(value = -1, message = "Your result should be a data frame."))
        
        if (isTRUE(all_equal(q6, .q6_wrong_operator))) return(c(value = 0, message = paste("Wrong. You should use the `|` operator.",
                                                                                "We actually want children OR anyone with a 'Primary' highest_education",
                                                                                "(despite it saying 'and' in the question).")))
        
        if (isTRUE(all_equal(q6, .q6))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q6 <- function(){ 
  'YOURDATA %>% filter(CONDITION1 | CONDITION2)' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q7 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q7 <-
  function() {
    .problem_number <<- 7
    
    .q7 <- .yao %>%
      filter(!(neighborhood %in% c("Tsinga", "Messa"))) 
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q7)) return(c(value = -1, message = "Your result should be dataframe."))
        
        if (isTRUE(all_equal(q7, .q7))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q7 <- function(){ 
'YOURDATA %>% 
  filter(neighborhood %in% c("STRING1", "STRING2") )' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q8 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q8 <-
  function() {
    .problem_number <<- 8
    
    .q8 <- .yao %>% filter(is.na(is_smoker)) 
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q8)) return(c(value = -1, message = "Your result should be dataframe."))
        
        if (isTRUE(all_equal(q8, .q8))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q8 <- function(){ 
'YOURDATA %>% 
  filter(is.na(VARIABLE))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q9 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q9 <-
  function() {
    .problem_number <<- 9
    
    .q9 <- .yaounde %>% filter(respiration_frequency >= 20 | is.na(respiration_frequency))
    
    .q9_wrong_forgot_na <- .yaounde %>% filter(respiration_frequency > 20)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q9)) return(c(value = -1, message = "Your result should be a data frame."))
        
        if (isTRUE(all_equal(q9, .q9_wrong_forgot_na))) return(c(value = 0, message = "Remember to to also keep those with NA values."))
        
        if (isTRUE(all_equal(q9, .q9))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q9 <- function(){ 
  'YOURDATA %>% filter(CONDITION1 | NA_CONDITION )' -> out
  cat(out)
}


# 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## ~  q10 ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .check_q10 <-
#   function() {
#     .problem_number <<- 10
#     
#     .q10 <- .yao %>% filter(row_number() %in% c(8:20, 80))
#     
#     .autograder <<- 
#       function(){
#         if (!is.data.frame(q10)) return(c(value = -1, message = "Your result should be a data frame."))
#         
#         if (isTRUE(all_equal(q10, .q10))) return(c(value = 1, message = paste("Correct!", praise())))
#         
#         return(c(value = 0, message = "Wrong. Please try again."))
#       }
#     .apply_autograder()
#   }
# 
# .hint_q10 <- function(){ 
#   'YOURDATA %>% filter(row_number() %in% SELECTION)' -> out
#   cat(out)
# }
# 
# 
# 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## ~  q11 ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# .check_q11 <-
#   function() {
#     .problem_number <<- 11
#     
#     .q11 <- .yao %>% filter(str_detect(occupation, "Student"))
#     
#     .q11_wrong_used_equality_test <- .yao %>% filter(occupation == "Student")
#     
#     .autograder <<- 
#       function(){
#         if (!is.data.frame(q11)) return(c(value = -1, message = "Your result should be a data frame."))
#         
#         if (isTRUE(all_equal(q11, .q11_wrong_used_equality_test))) return(c(value = 0, message = cat("Wrong.",
#                                                                                                     "It seems you tested for equality with `== 'Student'`", 
#                                                                                                     "That will keep people who are ONLY students.",
#                                                                                                     "You want to include students who have other occupations.")))
#         
#         if (isTRUE(all_equal(q11, .q11))) return(c(value = 1, message = paste("Correct!", praise())))
#         
#         return(c(value = 0, message = "Wrong. Please try again."))
#       }
#     .apply_autograder()
#   }
# 
# 
# .hint_q11 <- function(){ 
#   'YOURDATA %>% filter(str_detect(COLUMN, PATTERN))' -> out
#   cat(out)
# }


