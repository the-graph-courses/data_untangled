if(!require('pacman')) install.packages('pacman')

pacman::p_install_gh('graph-courses/autograder')
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(here,
               praise,
               glue,
               janitor,
               tidyverse)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  DATA ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.sarcopenia <- read_csv(here('data/sarcopenia_elderly.csv'))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  INIT ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument

.NUM_Q_weight_to_g <- 1
.NUM_Q_sarcopenia_resp_id <- 2
.NUM_Q_women_low_grip_strength <- 3
.NUM_Q_prop_women_low_grip_strength <- 4
.NUM_Q_asm_calculation <- 5
.NUM_Q_age_integer <- 6

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_weight_to_g ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_weight_to_g <-
  function() {
    
    .problem_number <<- .NUM_Q_weight_to_g
    correct_answer <- .sarcopenia %>% mutate(weight_grams = weight_kg * 1000)
    
    .autograder <<-
      function(){
        if(!exists("Q_weight_to_g"))
          .na("Vous n'avez pas encore défini l'objet réponse, `Q_weight_to_g`.")
        if (!is.data.frame(Q_weight_to_g))
          .na("Réponse invalide. Votre réponse devrait être un dataframe.")
        if (!"weight_grams" %in% names(Q_weight_to_g))
          .fail("Votre réponse devrait comporter une colonne appelée `weight_grams`.")
        if (isTRUE(all.equal(Q_weight_to_g$weight_grams, correct_answer$weight_grams/1000000)))
          .fail("Il semble que vous ayez utilisé la formule `weight_kg/1000` au lieu de `weight_kg * 1000`.")
        
        
        if (isTRUE(all.equal(select(Q_weight_to_g, weight_grams) , 
                             select(correct_answer, weight_grams))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_weight_to_g <- function(){

'INDICE :
Votre réponse devrait ressembler à ceci :

Q_weight_to_g <- 
  sarcopenia %>% 
  mutate(weight_grams = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_weight_to_g <- function(){
  '
SOLUTION :
Q_weight_to_g <- 
  sarcopenia %>% 
  mutate(weight_grams = weight_kg*1000)' -> out
  cat(out)
}

# Tests
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   _____________________
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   "sarcopenia"
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   select(weight_kg) %>%
#   mutate(weight_grams = weight_kg * 1000)
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   mutate(weight_grammes = weight_kg * 1000)
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   mutate(weight_grams = weight_kg / 1000)
# .CHECK_Q_weight_to_g()
# 
# Q_weight_to_g <-
#   sarcopenia %>%
#   mutate(weight_grams = weight_kg * 1000)
# .CHECK_Q_weight_to_g()
# 
# 
# .HINT_Q_weight_to_g()
# .SOLUTION_Q_weight_to_g()


# 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## ~  Q_sarcopenia_resp_id ----
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_sarcopenia_resp_id <-
  function() {
    
    .problem_number <<- .NUM_Q_sarcopenia_resp_id
    correct_answer <- .sarcopenia %>% mutate(respondent_id = row_number())
    
    .autograder <<-
      function(){
        if(!exists("Q_sarcopenia_resp_id"))
          .na("Vous n'avez pas encore défini l'objet réponse, `Q_sarcopenia_resp_id`.")
        if (!is.data.frame(Q_sarcopenia_resp_id))
          .na("Réponse invalide. Votre réponse devrait être un dataframe.")
        if (!"respondent_id" %in% names(Q_sarcopenia_resp_id))
          .fail("Votre réponse devrait comporter une colonne appelée 'respondent_id'.")
        
        if (isTRUE(all.equal(select(Q_sarcopenia_resp_id, respondent_id) ,
                             select(correct_answer, respondent_id))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_sarcopenia_resp_id <- function(){
  '
INDICE :
Votre réponse devrait ressembler à ceci :

Q_sarcopenia_resp_id <-
  sarcopenia %>%
  mutate(respondent_id = FORMULA_HERE)' -> out
  cat(out)
}

.SOLUTION_Q_sarcopenia_resp_id  <- function(){
  '
SOLUTION :
Q_sarcopenia_resp_id <-
  sarcopenia %>%
  mutate(respondent_id = 1:n())' -> out
  cat(out)
}


# Tests

# Q_sarcopenia_resp_id <-
#   sarcopenia %>%
#   _____________________
# .CHECK_Q_sarcopenia_resp_id()
# 
# Q_sarcopenia_resp_id <-
#   "sarcopenia"
# .CHECK_Q_sarcopenia_resp_id()
# 
# Q_sarcopenia_resp_id <-
#   sarcopenia %>%
#   mutate(respondent_id = 1:n())
# .CHECK_Q_sarcopenia_resp_id()
# 
# .HINT_Q_sarcopenia_resp_id()
# .SOLUTION_Q_sarcopenia_resp_id()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_sarcopenia_grip_strength ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_sarcopenia_grip_strength <-
  function() {
    
    .problem_number <<- .NUM_Q_sarcopenia_grip_strength
    correct_answer <- .sarcopenia %>% mutate(grip_strength_rank = rank(desc(grip_strength_kg), ties.method = "min"))
    incorrect_forgot_ties <-  .sarcopenia %>% mutate(grip_strength_rank = rank(desc(grip_strength_kg))) 
    incorrect_forgot_descending <-  .sarcopenia %>% mutate(grip_strength_rank = rank(grip_strength_kg, ties.method = "min")) 
    incorrect_forgot_descending_and_ties <-  .sarcopenia %>% mutate(grip_strength_rank = rank(grip_strength_kg))
    
    .autograder <<-
      function(){
        if(!exists("Q_sarcopenia_grip_strength"))
          .na("Vous n'avez pas encore défini l'objet réponse, `Q_sarcopenia_grip_strength`.")
        
        if (!is.data.frame(Q_sarcopenia_grip_strength))
          .na("Réponse invalide. Votre réponse devrait être un dataframe.")
        
        if (!"grip_strength_rank" %in% names(Q_sarcopenia_grip_strength))
          .fail("Votre réponse devrait comporter une colonne appelée 'grip_strength_rank'.")
        
        if (isTRUE(all.equal(select(Q_sarcopenia_grip_strength, grip_strength_rank) ,
                             select(incorrect_forgot_ties, grip_strength_rank))))
          .fail("Mauvaise réponse. Il semble que vous ayez oublié de spécifier `ties_method = 'min'`")
        
        if (isTRUE(all.equal(select(Q_sarcopenia_grip_strength, grip_strength_rank) ,
                             select(incorrect_forgot_descending, grip_strength_rank))))
          .fail("Mauvaise réponse. Il semble que vous avez oublié d'encadrer la variable `grip_strength_kg` dans `desc()`")
        
        if (isTRUE(all.equal(select(Q_sarcopenia_grip_strength, grip_strength_rank) ,
                             select(incorrect_forgot_descending_and_ties, grip_strength_rank))))
          .fail("Mauvaise réponse. Il semble que vous avez oublié d'encadrer la variable `grip_strength_kg` dans `desc()` et que vous ayez oublié de spécifier `ties_method = 'min'`")
        
        if (isTRUE(all.equal(select(Q_sarcopenia_grip_strength, grip_strength_rank) ,
                             select(correct_answer, grip_strength_rank))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_sarcopenia_grip_strength <- function(){
  '
INDICE :
Votre réponse devrait avoir la forme :

Q_sarcopenia_grip_strength <-
  sarcopenia %>%
  mutate(grip_strength_rank = RANKING_CODE)
  
où RANKING_CODE devrait classer `grip_strength_kg` par ordre décroissant avec la méthode `ties.method` définie sur "min"
' -> out
  cat(out) 
}

.SOLUTION_Q_sarcopenia_grip_strength <- function(){
  '
SOLUTION :
Q_sarcopenia_grip_strength <-
  sarcopenia %>%
  mutate(grip_strength_rank = rank(desc(grip_strength_kg)), ties.method = "min")' -> out
  cat(out)
}


# # Tests
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   _____________________
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   "sarcopenia"
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   select(weight_kg) %>%
#   mutate(grip_strength_rank = weight_kg * 1000)
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   mutate(grip_strength_rank = rank(grip_strength_kg))
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   mutate(grip_strength_rank = rank(desc(grip_strength_kg)))
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   mutate(grip_strength_rank = rank(grip_strength_kg, ties.method = "min"))
# .CHECK_Q_sarcopenia_grip_strength()
# 
# Q_sarcopenia_grip_strength <-
#   sarcopenia %>%
#   mutate(grip_strength_rank = rank(desc(grip_strength_kg), ties.method = "min"))
# .CHECK_Q_sarcopenia_grip_strength()
# 
# .HINT_Q_sarcopenia_grip_strength()
# .SOLUTION_Q_sarcopenia_grip_strength()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_women_low_grip_strength ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_women_low_grip_strength <-
  function() {
    
    .problem_number <<- .NUM_Q_women_low_grip_strength
    correct_answer <- 
      .sarcopenia %>%
      filter(sex_male_1_female_0 == 0) %>%
      mutate(low_grip_strength = grip_strength_kg < 20)
    
    incorrect_less_than_equal_to <-  
      .sarcopenia %>%
      filter(sex_male_1_female_0 == 0) %>%
      mutate(low_grip_strength = grip_strength_kg <= 20)
    
    incorrect_greater_than <-  
      .sarcopenia %>%
      filter(sex_male_1_female_0 == 0) %>%
      mutate(low_grip_strength = grip_strength_kg > 20)
    
    incorrect_greater_than_equal_to <-  
      .sarcopenia %>%
      filter(sex_male_1_female_0 == 0) %>%
      mutate(low_grip_strength = grip_strength_kg >= 20)
    
    .autograder <<-
      function(){
        if(!exists("Q_women_low_grip_strength"))
          .na("Vous n'avez pas encore défini l'objet réponse, `Q_women_low_grip_strength`.")
        
        if (!is.data.frame(Q_women_low_grip_strength))
          .na("Réponse invalide. Votre réponse devrait être un dataframe.")
        
        if (!"low_grip_strength" %in% names(Q_women_low_grip_strength))
          .fail("Votre réponse devrait comporter une colonne appelée 'low_grip_strength'.")
        
        if (nrow(Q_women_low_grip_strength) == nrow(.sarcopenia))
          .fail("Mauvaise réponse. Il semble que vous n'ayez pas filtré uniquement les femmes dans l'ensemble de données. 
                Pour filtrer les femmes, le code `filter(sex_male_1_female_0 == 0)` doit être dans votre pipeline.")
        
        if (isTRUE(all.equal(select(Q_women_low_grip_strength, low_grip_strength),
                             select(incorrect_less_than_equal_to, low_grip_strength))) |
            isTRUE(all.equal(select(Q_women_low_grip_strength, low_grip_strength),
                             select(incorrect_greater_than, low_grip_strength))) |
            isTRUE(all.equal(select(Q_women_low_grip_strength, low_grip_strength),
                             select(incorrect_greater_than_equal_to, low_grip_strength))))
          .fail("Mauvaise réponse. Il semble que vous utilisiez le mauvais opérateur de comparaison. Vous devriez avoir `grip_strength_kg < 20`.")
        
        if (isTRUE(all.equal(select(Q_women_low_grip_strength, low_grip_strength) ,
                             select(correct_answer, low_grip_strength))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_women_low_grip_strength <- function(){
  '
INDICE :
Votre réponse devrait avoir la forme :

Q_women_low_grip_strength <-
  sarcopenia %>%
  filter(sex_male_1_female_0 == 0) %>%
  mutate(low_grip_strength = COMPARISON_CODE)
  
où le COMPARISON_CODE devrait vérifier si `grip_strength_kg` est inférieur à 20
' -> out
  cat(out)
}

.SOLUTION_Q_women_low_grip_strength <- function(){
  '
SOLUTION :

Q_women_low_grip_strength <-
  sarcopenia %>%
  filter(sex_male_1_female_0 == 0) %>%
  mutate(low_grip_strength = grip_strength_kg < 20)
  ' -> out
  cat(out)
}

# # Tests
# rm(Q_women_low_grip_strength)
# .CHECK_Q_women_low_grip_strength()
# 
# Q_women_low_grip_strength <- 
# "sarcopenia"
# .CHECK_Q_women_low_grip_strength()
# 
# Q_women_low_grip_strength <- 
#   sarcopenia %>% 
#   filter(sex_male_1_female_0 == 0)
# .CHECK_Q_women_low_grip_strength()
# 
# Q_women_low_grip_strength <- 
#   sarcopenia %>% 
#   filter(sex_male_1_female_0 == 0) %>% 
#   mutate(low_grip_strength = grip_strength_kg > 20)
# .CHECK_Q_women_low_grip_strength()
# 
# Q_women_low_grip_strength <- 
#   sarcopenia %>% 
#   filter(sex_male_1_female_0 == 0) %>% 
#   mutate(low_grip_strength = grip_strength_kg < 20)
# .CHECK_Q_women_low_grip_strength()
# 
# .HINT_Q_women_low_grip_strength()
# .SOLUTION_Q_women_low_grip_strength()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_prop_women_low_grip_strength ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_prop_women_low_grip_strength <- 
  function(){
    .problem_number <<- .NUM_Q_prop_women_low_grip_strength
    
    .autograder <<- function(){
      
      if(!exists("Q_prop_women_low_grip_strength"))
        .na("Vous n'avez pas encore défini l'objet réponse, `Q_prop_women_low_grip_strength`.")
      
      if(! (is.numeric(Q_prop_women_low_grip_strength) & 
            (length(Q_prop_women_low_grip_strength) == 1)))
        .na("Réponse invalide. Votre réponse doit être un seul chiffre sans guillemets.")
      
      if(between(Q_prop_women_low_grip_strength, 0, 1))
        .fail("Vous avez fourni une réponse décimale. Votre réponse devrait être un pourcentage entre 1 et 100.")
      
      if(between(Q_prop_women_low_grip_strength, 17, 18))
        .fail("Mauvaise réponse. Il semble que vous comptiez le pourcentage de personnes qui N'ont PAS une faible force de préhension. En réalité, vous voulez l'opposé de cela." )
      
      if(between(Q_prop_women_low_grip_strength, 82, 83))
        .pass()
      
      else
        .fail()
      
    }
    .run_autograder()
    
  }

.HINT_Q_prop_women_low_grip_strength <- function(){
  '
INDICE :
Transmettez la sortie de la question précédente à la fonction `janitor::tabyl()`, en utilisant `low_grip_strength` comme argument de la fonction.
' -> out
  cat(out)
}

.SOLUTION_Q_prop_women_low_grip_strength <- function(){
  '
SOLUTION :

Q_prop_women_low_grip_strength <- 
  sarcopenia %>% 
  filter(sex_male_1_female_0 == 0) %>% 
  mutate(low_grip_strength = grip_strength_kg < 20) %>% 
  tabyl(low_grip_strength) %>% 
  .[2,3] * 100
  ' -> out
  cat(out)
}

# Tests
# rm(Q_prop_women_low_grip_strength)
# .CHECK_Q_prop_women_low_grip_strength()
# 
# Q_prop_women_low_grip_strength <- "YOUR ANSWER HERE"
# .CHECK_Q_prop_women_low_grip_strength()
# 
# Q_prop_women_low_grip_strength <- 0.4
# .CHECK_Q_prop_women_low_grip_strength()
# 
# Q_prop_women_low_grip_strength <- 17.5
# .CHECK_Q_prop_women_low_grip_strength()
# 
# Q_prop_women_low_grip_strength <- 82.6
# .CHECK_Q_prop_women_low_grip_strength()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_asm_calculation ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.CHECK_Q_asm_calculation <-
  function() {
    
    .problem_number <<- .NUM_Q_asm_calculation
    
    correct_answer <- 
      .sarcopenia %>%
      mutate(asm = 0.244 * weight_kg + 7.8 * height_meters + 6.6 * sex_male_1_female_0 - 0.098 * age - 4.5)
    
    .autograder <<-
      function(){
        if(!exists("Q_asm_calculation"))
          .na("Vous n'avez pas encore défini l'objet réponse, `Q_asm_calculation`.")
        
        if (!is.data.frame(Q_asm_calculation))
          .na("Réponse invalide. Votre réponse devrait être un dataframe.")
        
        if (!"asm" %in% names(Q_asm_calculation))
          .fail("Votre réponse devrait comporter une colonne appelée 'asm'.")
        
        if (isTRUE(all.equal(select(Q_asm_calculation, asm) ,
                             select(correct_answer, asm))))
          .pass()
        
        else
          .fail(glue("Mauvaise réponse. Les cinq premières valeurs de la colonne `asm` devraient être : \n{paste0(head(correct_answer$asm, 5), collapse = '\n') }"))
      }
    .run_autograder()
  }

.HINT_Q_asm_calculation <- function(){
  "INDICE :
Les trois derniers éléments de la formule sont les suivants : 6,6 * sex_male_1_female_0 - 0,098 * age - 4,5. 
Maintenant, il ne vous manque plus que les variables `weight_kg` et `height_meters`." -> out
  cat(out)
}

.SOLUTION_Q_asm_calculation <- function(){
  '
SOLUTION :

Q_asm_calculation <-
   sarcopenia %>%
   mutate(asm = 0.244 * weight_kg + 7.8 * height_meters + 6.6 * sex_male_1_female_0 - 0.098 * age - 4.5)
  ' -> out
  cat(out)
}

# # Tests
# rm(Q_asm_calculation)
# .CHECK_Q_asm_calculation()
# 
# Q_asm_calculation <- 
#   .sarcopenia
# .CHECK_Q_asm_calculation()
# 
# Q_asm_calculation <- 
#   .sarcopenia %>%
#   mutate(asm = 0.244 * weight_kg + 7.8 * height_meters + 6.6 * sex_male_1_female_0 - 0.098)
# .CHECK_Q_asm_calculation()
# 
# Q_asm_calculation <- 
#   .sarcopenia %>%
#   mutate(asm = 0.244 * weight_kg + 7.8 * height_meters + 6.6 * sex_male_1_female_0 - 0.098  * age - 4.5)
# .CHECK_Q_asm_calculation()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~  Q_age_integer ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_Q_age_integer <-
  function() {
    
    .problem_number <<- .NUM_Q_age_integer
    
    correct_answer <- 
      .sarcopenia %>%
      mutate(age_integer = as.integer(age))
    
    .autograder <<-
      function(){
        if(!exists("Q_age_integer"))
          .na("Vous n'avez pas encore défini l'objet réponse, `Q_age_integer`.")
        
        if (!is.data.frame(Q_age_integer))
          .na("Réponse invalide. Votre réponse devrait être un dataframe.")
        
        if (!"age_integer" %in% names(Q_age_integer))
          .fail("Votre réponse devrait comporter une colonne appelée 'age_integer'.")
        
        if (isTRUE(all.equal(select(Q_age_integer, age_integer) ,
                             select(correct_answer, age_integer))))
          .pass()
        
        else
          .fail()
      }
    .run_autograder()
  }

.HINT_Q_age_integer <- function(){
  "INDICE :
Vous devriez avoir `as.integer(age)` quelque part dans votre code." -> out
  cat(out)
}

.SOLUTION_Q_age_integer <- function(){
  '
SOLUTION :

Q_age_integer <-
   sarcopenia %>%
   mutate(age_integer = as.integer(age))
  ' -> out
  cat(out)
}


# # # Tests
# rm(Q_age_integer)
# .CHECK_Q_age_integer()
# 
# Q_age_integer <-
#   .sarcopenia
# .CHECK_Q_age_integer()
# 
# Q_age_integer <-
#   .sarcopenia %>%
#   mutate(age_integer = age)
# .CHECK_Q_age_integer()
# 
# Q_age_integer <-
#   .sarcopenia %>%
#   mutate(age_integer = as.integer(age))
# .CHECK_Q_age_integer()

