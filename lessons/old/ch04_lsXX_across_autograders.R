if(!require('pacman')) install.packages('pacman')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(praise,
               tidyverse,
               dplyr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.yaounde <- read_csv(here::here('data/yaounde_data.csv'))
# .yao <- .yaounde %>% 
#   select(neighborhood, is_smoker, occupation, 
#          treatment_combinations, symptoms, 
#          age, sex, weight_kg, height_cm, 
#          igg_result) %>%
#   filter(across(c("is_smoker","sex","igg_result"), ~ !is.na(.x)))%>%
#   mutate(is_smoker=as.factor(is_smoker),
#          sex=as.factor(sex),
#          igg_result=as.factor(igg_result))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 5)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3 <- .yao1 %>% mutate(across(c("height_m", "weight_kg", "age"),
                                   ~ (.x - mean(.x)) / (sd(.x)) ,
                                   .names = "mean_std_normalization_{.col}"),
                            .keep="unused")
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q3)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q3, .q3))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q3 <- function(){ 
  "Use mutate() combined with across(). Don't forget to specify the .keep argument. This one is a little complex but you can do it !" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.check_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5 <- .yao %>%
      filter(str_detect(occupation, "Informal worker") & igg_result=='Positive')%>%
      summarize(across(c("weight_kg", "height_cm"), 
                       mean, na.rm = TRUE))
    
    .autograder <<- 
      function(){
        if (!is.data.frame(q5)) return(c(value = -1, message = "Your result should be a dataframe."))
        
        if (isTRUE(all_equal(q5, .q5))) return(c(value = 1, message = paste("Correct!", praise())))
        
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

.hint_q5 <- function(){ 
  "Try to use `dplyr::filter()` (and remember `str_detect()`) before `dplyr::summarise()`" -> out
  cat(out)
}
