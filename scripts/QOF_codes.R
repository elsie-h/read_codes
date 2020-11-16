# Read the QOF expanded cluster lists
# All downloaded from:
# https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-collections/quality-and-outcomes-framework-qof

source('setup.R')

cluster_names <- c('ASTEXC', # Exception reporting from asthma monitoring
                   'AST',  # Asthma
                   'ASTSPIR', # Spirometry
                   'PEFR', # PEFR
                   'SMOK', # Smoking
                   'REV', # Review
                   'RCPEXCER', # RCGP 3Q exercise
                   'RCPSLP', # RCGP 3Q sleep
                   'RCPDSYMP', # RCGP 3Q day symptoms
                   'SMOKASTEXC', # Refuse smoking status
                   'SPEX', # Spirometry declined
                   'ASTRES', # Asthma resolved
                   'ASTTRT',
                   'DEPR',
                   'FLU',
                   'OSTEO') # treatments
cluster_names <- str_c(cluster_names, '_COD')

clean_QOF <- function(.data, 
                      read_v,
                      QOF_v) {
  version <- str_c('v', QOF_v, '_v', read_v)
  version_term <- str_c(version, '_term')
  version_id <- str_c(version, '_id')
  .data %>%
    rename_all(tolower) %>%
    filter(cluster_id %in% cluster_names) %>%
    select(read_code, !! version_id := cluster_id, !! version_term := code_description) %>%
    mutate(!! version := 1)
}

qof_v36_v2 <- read_excel("lists_in/QOF/qof_v36.0_expanded_cluster_list.xlsx", sheet = "ReadV2_V36.0_Expanded Clusters") %>%
  clean_QOF(read_v = '2',
            QOF_v = '36')

qof_v36_v3 <- read_excel("lists_in/QOF/qof_v36.0_expanded_cluster_list.xlsx", sheet = "CTV3_V36.0_Expanded Clusters") %>%
  clean_QOF(read_v = '3',
            QOF_v = '36')

qof_v37_v2 <- read_excel("lists_in/QOF/qof_v37.0_expanded_cluster_list__.xlsx", sheet = "ReadV2_V37.0_Expanded Clusters") %>%
  clean_QOF(read_v = '2',
            QOF_v = '37')

qof_v37_v3 <- read_excel("lists_in/QOF/qof_v37.0_expanded_cluster_list__.xlsx", sheet = "CTV3 V37.0 Expanded Clusters") %>%
  clean_QOF(read_v = '3',
            QOF_v = '37')

qof_v38_v2 <- read_excel("lists_in/QOF/qof_v38_expanded_cluster_list_.xlsx", sheet = "Read V2 QOF V38 Expanded Cluste") %>%
  clean_QOF(read_v = '2',
            QOF_v = '38')

qof_v38_v3 <- read_excel("lists_in/QOF/qof_v38_expanded_cluster_list_.xlsx", sheet = "CTV3 QOF V38 Expanded Clusters") %>%
  clean_QOF(read_v = '3',
            QOF_v = '38')

qof_codes <- qof_v36_v2 %>%
  full_join(qof_v36_v3, by = 'read_code') %>%
  full_join(qof_v37_v2, by = 'read_code') %>%
  full_join(qof_v37_v3, by = 'read_code') %>%
  full_join(qof_v38_v2, by = 'read_code') %>%
  full_join(qof_v38_v3, by = 'read_code')

saveRDS(qof_codes, file = 'lists_in/QOF/QOF_codes.RDS', compress = FALSE)
