#### influenza vaccination  ####
# Description: indicator that influenza vaccination given
# cat1: influenza_vaccination
# cat2: NA
# score: NA

source('setup.R')

fluvac_qof <- readRDS(file = 'lists_in/QOF/QOF_codes.RDS') %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'FLU_COD')) 

fluvac_qof %>%
  select(contains('_v2_')) %>%
  filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
  print(n=nrow(.))
# all v2 the same
fluvac_qof %>%
  select(contains('_v3_')) %>%
  filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
  print(n=nrow(.))
# v38 v3 has some extras

fluvac_qof <- fluvac_qof %>%
  mutate(read_term = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                                   !is.na(v38_v3_term) ~ v38_v3_term,
                                   TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term) %>%
  mutate(cat1 = 'influenza_vaccination', cat2 = NA_character_)

saveRDS(fluvac_qof, file = 'lists_out/influenza_vaccination.RDS', compress = FALSE)

# table for appendix
fluvac_qof %>%
  arrange(read_term) %>%
  select(`Read code` = read_code, 
         `Term` = read_term) %>%
  xtable(caption = 'Read codes to indicate influenza vaccination given (see \\sectionref{cha:ehr:methods:pre:influenza_vaccination} for methods)',
         label = 'tab:app:rc_influenza_vaccination',
         align=c('l',"p{2cm}","p{10cm}")) %>%
  print_xtable_multi(filename = 'influenza_vaccination')
