#### CCI ####

# Description: codes for derving the Charlson Comorbidity Index (CCI)
# cat1: CCI
# cat2: CCI component
# score: CCI weight

source('setup.R')

# Read codes from Nwaru2020
cci_nwaru <- read_csv(file = 'lists_in/Nwaru2020/cl_cci_nwaru.csv')

# use the scores from the khan paper
cci <- read_csv("lists_in/Khan2010/khan_2010_cci.csv") %>%
  # because code_id in OPCRD is only 5 chatacters
  mutate(read_code = str_extract(`Read/OXMIS code`, '^.{5}')) %>%
  select(read_code, read_term = `Read/OXMIS term`, 
         score = `Charlson score weight`,
         cat2 = `Charlson disease category`) %>%
  mutate(cat1 = 'cci') %>%
  # some of the codes here are OXMIS, join to Nwaru list to only select Read codes
  right_join(cci_nwaru, by = 'read_code') %>%
  mutate_at('cat2', list(~ case_when(. %in% 'Mod liver disease' ~ 'moderate liver disease',
                                     TRUE ~ str_to_lower(.)))) %>%
  group_by(cat2) %>%
  mutate(score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at('read_code', list(~ if_else(str_detect(., '^K458'),
                                        'K458.',
                                        .))) %>%
  mutate_at('read_term', list(~ str_remove_all(., '\\?'))) %>%
  filter(!is.na(read_code)) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  mutate_at('cat2', list(~ if_else(. %in% 'aids', 'AIDS', .))) %>%
  # if there are multiple terms for a Read code, select one term (the first one listed)
  distinct(read_code, score, cat1, cat2, .keep_all = TRUE)

# save
write_csv(cci, 
          path = file.path(opcrd_analysis_path, 'CCI.csv'))
write_csv(cci %>%
            arrange(cat2, read_code, read_term) %>%
            select(Disease = cat2,
                   `Read code` = read_code,
                   Term = read_term,
                   weight = score),
          path = 'lists_out/CCI.csv')

# latex tables
cci_list <- cci %>% 
  group_split(cat2)

cci_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>%
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))

  caption <- str_c('Read codes for \\emph{\'', category, '\'} category (return to \\nameref{cha:ehr:methods:pre:cci} methods)')
  label <- str_c('tab:app:rc_', category_)

  table <- .data %>%
    arrange(read_term) %>%
    select(`Read code` = read_code,
           `Term` = read_term) %>%
    xtable(caption = caption,
           label = label,
           align=c('l',"p{2cm}","p{10cm}")) %>%
    print_xtable_multi(filename = category_)
  
}

lapply(cci_list, cci_latex_table)
