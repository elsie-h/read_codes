#### Spirometry ####

# Description: identify events with a spirometry read code
# cat1: spirometry
# cat2: NA, FEV1, FEV1FVC normal, FEV1FVC abnormal, FEV1FVC ratio or percent, ppFEV1, FEV1FVC < 70%, FEV1FVC > 70%, predicted FEV1, predicted FVC
# score: NA 

source('setup.R')

# QOF
spirometry_qof <- readRDS(file = 'lists_in/QOF/QOF_codes.RDS') %>%
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

# check for duplicates in read_code
spirometry$read_code[duplicated(spirometry$read_code)]

saveRDS(spirometry, file = 'lists_out/spirometry.RDS', compress = FALSE)

# table for appendix
spirometry_table <- spirometry %>%
  mutate_at('cat2', list(~ case_when(. %in% 'FEV1FVC ratio or percent' ~ 'FEV1/FVC',
                                     . %in% '% predicted PEFR' ~ 'ppPEFR',
                                     str_detect(., 'predicted F') ~ str_replace(., 'predicted F', 'pF'),
                                     str_detect(., 'FEV1FVC') ~ str_replace(., 'FEV1FVC', 'FEV1/FVC'),
                                     is.na(.) ~ 'none',
                                     TRUE ~ .))) %>% 
  mutate_at('cat2', factor, 
            levels = c('FEV1',
                       'pFEV1', 
                       'ppFEV1', 
                       'FVC', 
                       'pFVC',
                       'FEV1/FVC', 
                       'FEV1/FVC > 70%',
                       'FEV1/FVC < 70%',
                       'FEV1/FVC normal',
                       'FEV1/FVC abnormal',
                       'none')) %>%
  mutate_at('QOF', list(~ if_else(is.na(.), 'N', 'Y'))) %>%
  arrange(cat2, read_term) %>%
  select(Category = cat2,
         `Read code` = read_code, 
         Term = read_term,
         QOF) %>%
  mutate(order = as.numeric(Category)) %>%
  mutate_at('Category', as.character)

# split on value column
split_list <- spirometry_table %>% 
  group_split(order)
new_col <- character()
# loop for removing duplicate labels
for (i in seq_along(unique(spirometry_table$Category))) {
  tmp <- c(split_list[[i]]$Category[1], 
           rep('', length.out = length(split_list[[i]]$Category[-1])))
  new_col <- c(new_col, tmp)
}

# produce table
spirometry_table <- spirometry_table %>%
  mutate(Category = new_col) %>%
  arrange(order, Term) %>%
  select(-order)

spirometry_table %>%
  xtable(caption = 'Read codes for identifiying spirometry events (see \\nameref{cha:ehr:methods:pre:spirometry} for methods)',
         label = 'tab:app:rc_spirometry',
         align=c('l',"p{3cm}","p{2cm}","p{6cm}", "p{1cm}")) %>%
  print_xtable_multi(filename = 'spirometry')
