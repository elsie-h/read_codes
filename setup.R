# All Read codes in tibble

#### load libraries ####
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(tidyr)
library(xtable)

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

# function for reading code lists from the Nwaru2020 paper
read_nwaru <- function(variable) {
  filename <- str_c('lists_in/Nwaru2020/cl_', variable,'_nwaru')

    read_table2(file = filename, col_names = FALSE) %>%
    pivot_longer(cols = 1:ncol(.)) %>%
    transmute(read_code = str_remove_all(value, '\\"|,'))
}

#### path for latex tables ####
latex_table_path <- '/Users/elsiehorne/Docs/thesis_repo/thesis_tex/tables/rc_'

# xtable single page
print_xtable_single <- function(.data, filename) {
  print(.data,
        include.colnames = T,
        include.rownames = F,
        caption.placement = 'top',
        file = str_c(latex_table_path, filename),
        comment = F,
        booktabs = T,
        floating = F)
}

# xtable single page
print_xtable_multi <- function(.data, filename) {
  print(.data,
        include.colnames = T,
        include.rownames = F,
        caption.placement = 'top',
        file = str_c(latex_table_path, filename),
        comment = F,
        booktabs = T,
        tabular.environment = "longtable",
        floating = F)
}
