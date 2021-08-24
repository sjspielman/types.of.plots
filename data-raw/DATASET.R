## code to prepare `DATASET` dataset goes here
penguins <- palmerpenguins::penguins %>% 
  dplyr::mutate(year = as.factor(year))
usethis::use_data(penguins, overwrite = TRUE, internal=TRUE)
