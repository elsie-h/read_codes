# All Read codes in tibble

#### load libraries ####
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(tidyr)

#### Functions ####
read_cprd <- function(name) {
  # although file name is allergic, the list includes codes for allergic and chronic
  if (name == 'rhinitis') name <- 'allergic_rhinitis'
  else if (name == 'dermatitis') name <- 'derm_merge'
  file <- str_c('lists_in/CPRD/CPRD_', name, '.csv')
  rc_ <- suppressMessages(read_csv(file)) %>%
    select(read_code = `Read code`,
           read_term = Descr) %>%
    mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
    distinct()
}

# function for cleaning code lists from the Nwaru2020 paper
clean_nwaru <- function(.data, cat1_string) {
  .data %>%
    gather() %>%
    select(read_code = value) %>%
    mutate_at('read_code', list(~ str_remove_all(., '\\\"'))) %>%
    mutate_at('read_code', list(~ str_trim(., side = 'both'))) %>%
    mutate(read_term = NA_character_,
           cat2 = cat1_string)
}
