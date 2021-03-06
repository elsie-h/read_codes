# comedications

source('setup.R')

# beta-blockers, paracetamol, NASID, statins

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

# paracetamol
paracetamol <- read_delim("lists_in/RMS/dl_paracetamol_rms.txt", 
                 "|", escape_double = FALSE,
                 trim_ws = TRUE, col_names = TRUE) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'paracetamol')

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
other_prescriptions <- bind_rows(bb, NSAID, paracetamol, statin) %>%
  mutate(cat1 = 'other prescription') %>%
  mutate_at('QOF', list(~ if_else(. %in% 'yes', ., 'no')))

# make sure all Read codes are 5 characters and fix if not
other_prescriptions %>%
  filter(str_length(read_code)<5)
other_prescriptions <- other_prescriptions %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# save list
write_csv(other_prescriptions, 
          path = file.path(opcrd_analysis_path, 'comedications.csv'))
write_csv(other_prescriptions %>%
            arrange(cat2, read_code) %>%
            select(Comedication = cat2,
                   `Read code` = read_code,
                   Term = read_term,
                   QOF) %>%
            mutate_at('QOF', list(~ toupper(str_extract(., '.{1}')))),
          path = 'lists_out/comedications.csv')