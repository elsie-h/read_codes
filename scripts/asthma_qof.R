#### Asthma QOF codes ####

source('setup.R')

asthma_qof <- read_csv('lists_in/QOF/QOF_codes.csv') %>%
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

# make sure all Read codes are 5 characters and fix if not
asthma_qof %>%
  filter(str_length(read_code)<5)
asthma_qof <- asthma_qof %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# save the code list
write_csv(asthma_qof, 
          path = file.path(opcrd_analysis_path, 'asthma_qof.csv'))
write_csv(asthma_qof %>%
            arrange(read_code, read_term) %>%
            select(`Read code` = read_code,
                   Term = read_term) %>%
            mutate(QOF = 'Y'), path = 'lists_out/asthma_qof.csv')

# table for appendix
asthma_qof %>%
  arrange(read_term) %>%
  select(`Read code` = read_code, 
         `Term` = read_term) %>%
  xtable(caption = 'Asthma Read codes from QOF business rules (see \\nameref{cha:ehr:methods:pre:asthma_prescriptions} for methods)',
         label = 'tab:app:rc_asthma_qof',
         align=c('l',"p{2cm}","p{10cm}")) %>%
  print_xtable_multi(filename = 'asthma_qof')

