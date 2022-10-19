# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(devtools)
devtools::source_gist("https://gist.github.com/kendavidn/05c7055e487ef22e5a336a4cb489a937")
quiz_path <- "ch04_ls04_conditional_mutate_theory_quiz/quiz.Rmd"
process_theory_quiz(here(paste0("ch04_data_wrangling/quizzes/",quiz_path)))
