if(!require('pacman')) install.packages('pacman')

pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(praise)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.yaounde <- read_csv(here::here('data/yaounde_data.csv'))

.yao <-
  .yaounde %>% select(age,
                      sex,
                      highest_education,
                      occupation,
                      is_smoker,
                      is_pregnant,
                      igg_result,
                      igm_result)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1 <- .yaounde %>% select(weight_kg, height_cm)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q1)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q1, .q1))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q1 <- function(){ 
  "YOURDATA %>% select(COL1, COL2)" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2 <- .yaounde %>% select(16, 22)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q2)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q2, .q2))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q2 <- function(){ 
  "YOURDATA %>% select(NUM1, NUM2)" -> out
  cat(out)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3 <- .yaounde %>% select(symptoms:sequelae)
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q3)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q3, .q3))) return(c(value = 1, message = paste("Correct!", praise()) ))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    
    .apply_autograder()
  }

.hint_q3 <- function(){ 
  "YOURDATA %>% select(STARTCOL : ENDCOL)" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4 <- .yaounde %>% select(!c(highest_education:consultation))
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q4)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q4, .q4))) return(c(value = 1, message = paste("Correct!", praise()) ))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    
    .apply_autograder()
  }

.hint_q4 <- function(){ 
  "You can use `:` to select a range, and `!` to negate that range." -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.check_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5 <- .yaounde %>% select(starts_with("is_"))
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q5)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q5, .q5))) return(c(value = 1, message = paste("Correct!", praise()) ))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    
    .apply_autograder()
  }

.hint_q5 <- function(){ 
  "YOURDATA %>% select(starts_with('YOURSTRING'))" -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q6 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q6 <-
  function() {
    .problem_number <<- 6
    
    .q6 <- .yaounde %>% select(starts_with("is_"), everything())
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q6)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q6, .q6))) return(c(value = 1, message = paste("Correct!", praise()) ))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    
    .apply_autograder()
  }

.hint_q6 <- function(){ 
  "YOURDATA %>% select(starts_with('is_'), REMAINING_COLS)" -> out
  cat(out)
}

