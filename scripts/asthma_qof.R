#### Asthma QOF codes ####

source('setup.R')

asthma_qof <- readRDS(file = 'lists_in/QOF/QOF_codes.RDS') %>%
  filter_at(vars(ends_with('id')), any_vars(. %in% 'AST_COD')) 

# asthma_qof %>%
#   select(contains('_v2_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # all v2 the same
# asthma_qof %>%
#   select(contains('_v3_')) %>%
#   filter_at(vars(ends_with('term')), any_vars(!is.na(.))) %>%
#   print(n=nrow(.))
# # v38 v3 has some extras

asthma_qof <- asthma_qof %>%
  mutate(read_term = case_when(!is.na(v38_v2_term) ~ v38_v2_term,
                                   !is.na(v38_v3_term) ~ v38_v3_term,
                                   TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term) %>%
  mutate(cat1 = 'asthma', cat2 = NA_character_)

# save the code list
saveRDS(asthma_qof, file = 'lists_out/asthma_qof.RDS', compress = FALSE)

# table for appendix
asthma_qof %>%
  arrange(read_term) %>%
  select(`Read code` = read_code, 
         `Term` = read_term) %>%
  xtable(caption = 'Asthma Read codes from QOF business rules (see \\nameref{cha:ehr:methods:pre:asthma_prescriptions} for methods)',
         label = 'tab:app:rc_asthma_qof',
         align=c('l',"p{2cm}","p{10cm}")) %>%
  print_xtable_multi(filename = 'asthma_qof')

