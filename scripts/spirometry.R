#### Spirometry ####

# Description: identify events with a spirometry read code
# cat1: spirometry
# cat2: NA, FEV1, FEV1FVC normal, FEV1FVC abnormal, FEV1FVC ratio or percent, ppFEV1, FEV1FVC < 70%, FEV1FVC > 70%, predicted FEV1, predicted FVC
# score: NA 

source('setup.R')

# QOF
spirometry_qof <- read_csv('lists_in/QOF/QOF_codes.csv') %>%
  filter_at(vars(ends_with('_id')), any_vars(. %in% 'ASTSPIR_COD')) %>%
  mutate(read_term = case_when(!is.na(v36_v2_term) ~ v36_v2_term,
                               !is.na(v36_v3_term) ~ v36_v3_term,
                               !is.na(v37_v2_term) ~ v37_v2_term,
                               !is.na(v37_v3_term) ~ v37_v3_term,
                               !is.na(v38_v2_term) ~ v38_v2_term,
                               !is.na(v38_v3_term) ~ v38_v3_term,
                               TRUE ~ NA_character_)) %>%
  select(read_code, read_term) %>%
  mutate(QOF = 1)

# Elsie
spirometry_elsie <- read_delim("lists_in/Elsie/cl_spirometry_elsie",
                            "|",
                            escape_double = FALSE, 
                            trim_ws = TRUE) %>%
  filter(!(read_code %in% '745C0')) %>%
  add_row(read_code = '68M..',
          read_term = 'Spirometry screening',
          cat1 = 'spirometry') %>%
  add_row(read_code = 'XaCJK',
          read_term = 'Expected FEV1',
          cat2 = 'predicted FEV1') %>%
  add_row(read_code = '339i.',	
          read_term = 'FVC/Expected FVC percent',
          cat2 = NA_character_) %>%
  add_row(read_code = '339P.',
          read_term = 'Expected FEV1',
          cat2 = 'predicted FEV1') %>%
  add_row(read_code = 'XaCJL',
          read_term = 'Expected FVC',
          cat2 = 'predicted FVC') %>%
  add_row(read_code = '339Q.',
          read_term = 'Expected FVC',
          cat2 = 'predicted FVC') %>%
  mutate(cat1 = 'spirometry',
         score = NA_real_) %>%
  mutate_at('cat2', list(~ case_when(read_code %in% c('XaPpI') ~ 'FVC',
                                     read_code %in% c('339j.', '339l.', 'XaJ9B', 'XaJ9D') ~ 'FEV1FVC ratio or percent',
                                     read_code %in% c('339a.', '339e.', 'XaIxQ', 'XaIxU') ~ 'FEV1',
                                     read_code %in% c('33971') ~ NA_character_,
                                     TRUE ~ .))) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  anti_join(spirometry_qof, by = 'read_code') %>%
  distinct()

spirometry <- bind_rows(spirometry_qof, spirometry_elsie)

# make sure all Read codes are 5 characters and fix if not
spirometry %>%
  filter(str_length(read_code)<5)
spirometry <- spirometry %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check for duplicates in read_code
spirometry$read_code[duplicated(spirometry$read_code)]

# check mapping
# map V2 -> CTV3
spirometry %>% map_V2_CTV3() %>% arrange(CTV3_CONCEPTID) %>% print(n=Inf)
# map CTV3 -> V2
spirometry %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

spirometry <- spirometry %>%
  bind_rows(tribble(~read_code, ~read_term, ~cat2,
                    'XaJ5w',	'FVC/Expected FVC percent', NA_character_,
                    '33963',  'FVC after change of bronchodilator', NA_character_,
                    '339h.', 'FVC after bronchodilation', NA_character_,
                    '339h0', 'FVC post bronchodi percnt chng', NA_character_,
                    '339s.', 'FVC before bronchodilation', 'FVC') %>%
              mutate(cat1 = 'spirometry')) %>%
  filter(!is.na(cat1)) %>%
  select(read_code, read_term, cat1, cat2)


write_csv(spirometry, 
          path = file.path(opcrd_analysis_path, 'spirometry.csv'))
write_csv(spirometry %>%
            mutate_at('cat2', list(~ factor(if_else(is.na(.), 'other', .),
                                            levels = c('FEV1', 'predicted FEV1', 'ppFEV1',
                                                       'FVC', 'predicted FVC',
                                                       'FEV1FVC ratio or percent',
                                                       'FEV1FVC < 70%', 'FEV1FVC > 70%',
                                                       'FEV1FVC abnormal', 'FEV1FVC normal',
                                                       'other')))) %>%
            arrange(cat2, read_code) %>%
            select(Category = cat2,
                   `Read code` = read_code,
                   Term = read_term),
          path = 'lists_out/spirometry.csv')
