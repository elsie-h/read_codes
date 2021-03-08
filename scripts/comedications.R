# comedications

source('setup.R')

# beta-blockers, paracetemol, NASID, statins

# beta-blockers
bb_rms <- read_delim("lists_in/RMS/dl_beta_blockers_rms.txt", 
                     "|", escape_double = FALSE,
                     trim_ws = TRUE, col_names = TRUE) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  select(-Category) 

bb_qof <- read_csv("lists_in/QOF/QOF_codes.csv") %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'LBB_COD')) 
# bb_qof %>%
#   select(contains('_v2_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# bb_qof %>%
#   select(contains('_v3_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v3 the same

bb_qof <- bb_qof %>%
  mutate(read_term = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                               !is.na(v38_v3_term) ~ v38_v3_term,
                               TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  mutate(QOF = 'yes') %>%
  filter(read_code != 'bdf7.') # not sure why this is in the QOF list, maybe an error

bb <- bb_rms %>%
  bind_rows(bb_qof) %>%
  # so that QOF = yes comes first and are kept with distinct
  arrange(read_code, QOF) %>% 
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'beta-blocker')

# paracetemol
paracetemol <- read_delim("lists_in/RMS/dl_paracetemol_rms.txt", 
                 "|", escape_double = FALSE,
                 trim_ws = TRUE, col_names = TRUE) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'paracetemol')

# NSAID
NSAID <- read_delim("lists_in/RMS/dl_nsaid_rms.txt", 
                          "|", escape_double = FALSE,
                          trim_ws = TRUE, col_names = TRUE) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'NSAID')

# Statins
statin_qof <- read_csv("lists_in/QOF/QOF_codes.csv") %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'STAT_COD')) 
# statin_qof %>%
#   select(contains('_v2_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# statin_qof %>%
#   select(contains('_v3_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v3 the same

statin_qof <- statin_qof %>%
  mutate(read_term = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                               !is.na(v38_v3_term) ~ v38_v3_term,
                               TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  mutate(QOF = 'yes')

statin_rms <- read_delim("lists_in/RMS/dl_statins_rms.txt", 
                    "|", escape_double = FALSE,
                    trim_ws = TRUE, col_names = TRUE) %>%
  select(read_code, read_term = MX_PRODUCT_NAME) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  filter(read_code != 'a222.') # I checked the browser and this is the wrong Read code

statin <- bind_rows(statin_qof, statin_rms) %>%
  # so that QOF = yes comes first and are kept with distinct
  arrange(read_code, QOF) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'statin')

# bind all
other_prescriptions <- bind_rows(bb, NSAID, paracetemol, statin) %>%
  mutate(cat1 = 'other prescription') %>%
  mutate_at('QOF', list(~ if_else(. %in% 'yes', ., 'no')))

# save list
write_csv(other_prescriptions, 
          path = file.path(opcrd_analysis_path, 'comedications.csv'))
# write_csv(other_prescriptions %>%
#             arrange(cat2, read_code) %>%
#             select(Comedication = cat2,
#                    `Read code` = read_code,
#                    Term = read_term,
#                    QOF) %>%
#             mutate_at('QOF', list(~ toupper(str_extract(., '.{1}')))), 
#           path = 'lists_out/comedications.csv')

# latex tables for thesis
drug_list <- other_prescriptions %>%
  group_split(cat2)

drug_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  caption <- str_c('Read codes for \\emph{\'', category, '\'} prescription group (return to \\nameref{cha:ehr:methods:pre:other_prescriptions} methods)')
  label <- str_c('tab:app:rc_', category_)
  
  table <- .data %>%
    arrange(read_term) %>%
    select(`Read code` = read_code, 
           `Term` = read_term,
           QOF) %>%
    xtable(caption = caption,
           label = label,
           align=c('l',"p{2cm}","p{8cm}","p{2cm}")) %>%
    print_xtable_multi(filename = category_)
  
}

lapply(drug_list, drug_latex_table)