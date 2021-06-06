source('setup.R')

blood_eos <- tribble(~read_code, ~read_term, ~cat1, ~cat2,
                     '42K..', 'blood eosinophil count', 'eos', NA_character_)

# check mapping
blood_eos %>%
  left_join(rctctv3map %>% 
              distinct(V2_CONCEPTID, CTV3_CONCEPTID), 
            by = c('read_code' = 'V2_CONCEPTID'))

blood_eos %>%
  left_join(ctv3rctmap %>% 
              distinct(V2_CONCEPTID, CTV3_CONCEPTID), 
            by = c('read_code' = 'CTV3_CONCEPTID'))

write_csv(blood_eos, 
          path = file.path(opcrd_analysis_path, 'blood_eosinophils.csv'))
write_csv(blood_eos %>%
            select(`Read code` = read_code,
                   Term = read_term), 
          path = 'lists_out/blood_eosinophils.csv')
