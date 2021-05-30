
library(readr)
library(janitor)
library(dplyr)
library(rsample)

pokemon <-
  file.path('data-raw', 'Pokemon.csv') %>%
  readr::read_csv() %>%
  janitor::clean_names() %>%
  dplyr::group_by(number) %>%
  dplyr::filter(dplyr::row_number() == 1L) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    type_1 %in% c('Normal', 'Water', 'Fire', 'Grass', 'Bug')
  ) %>%
  dplyr::mutate(
    dplyr::across(legendary, as.integer),
    dplyr::across(generation, ordered)
  )

set.seed(42)
split <- pokemon %>% rsample::initial_split(strata = type_1)
pokemon_multi_trn <- split %>% rsample::training()
pokemon_multi_tst <- split %>% rsample::testing()

use_data(pokemon, pokemon_multi_trn, pokemon_multi_tst, overwrite = TRUE)
