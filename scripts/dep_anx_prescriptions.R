source('setup.R')

#### anxiety ####
anx_tx <- read_csv("lists_in/CPRD_Cambridge/CPRDCAM_ANX141_PC_V1-1_Oct2018.csv") %>%
  select(read_term = productname)

anx_tx_os <- read_csv("lists_in/CPRD_Cambridge/elsie-horne-anxiety_drug-2368fbca.csv")

anxiety_prescriptions <- anx_tx %>%
  mutate_at('read_term', list(~ str_remove(., '\\s+\\(.+\\)'))) %>%
  mutate_at('read_term', list(~ str_to_lower(.))) %>%
  mutate_at('read_term', list(~ str_replace(., 'microgram', 'mcg'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'micrograms', 'mcg'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'mcgs', 'mcg'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'tablets', 'tablet'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'capsules', 'capsule'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'modified-release', 'm/r'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'gel-fill', 'thick gel'))) %>% # checked and these seem to be equivalent
  mutate_at('read_term', list(~ str_replace(., '0\\.5mg', '500mcg'))) %>%
  # mutate_at('read_term', list(~ str_replace(., 'oral s.+', 'liquid'))) %>%
  mutate(gemscript = TRUE) %>%
  full_join(anx_tx_os %>%
              mutate_at('term', list(~ str_to_lower(.))) %>%
              mutate_at('term', list(~ str_replace(., 'microgram', 'mcg'))) %>%
              mutate_at('term', list(~ str_replace(., 'micrograms', 'mcg'))) %>%
              mutate_at('term', list(~ str_replace(., 'mcgs', 'mcg'))) %>%
              mutate_at('term', list(~ str_replace(., 'tablets', 'tablet'))) %>%
              mutate_at('term', list(~ str_replace(., 'capsules', 'capsule'))) %>%
              mutate_at('term', list(~ str_replace(., 'trazodone hydrochloride', 'trazodone'))) %>%
              mutate_at('term', list(~ str_replace(., 'buspirone hydrochloride', 'buspirone'))), 
            by = c('read_term' = 'term')) %>%
  mutate(notes = NA_character_) %>%
  distinct() %>%
  arrange(read_term) %>%
  mutate_at('notes', list(~ case_when(# remove if drug not in cambridge list
                                      str_detect(read_term, 'unisomnia') ~ 'remove', 
                                      str_detect(read_term, 'tensium') ~ 'remove',
                                      str_detect(read_term, 'rimapam') ~ 'remove',
                                      str_detect(read_term, 'noctesed') ~ 'remove',
                                      str_detect(read_term, 'meprate') ~ 'remove',
                                      str_detect(read_term, 'alupram') ~ 'remove',
                                      str_detect(read_term, 'almazine') ~ 'remove',
                                      str_detect(read_term, 'zolpidem tartrate') ~ 'remove',
                                      str_detect(read_term, 'remnos') & is.na(gemscript) ~ 'remove',
                                      str_detect(read_term, 'oxanid') & is.na(gemscript) ~ 'remove',
                                      str_detect(read_term, 'heminevrin') & is.na(gemscript) ~ 'remove',
                                      str_detect(read_term, 'evacalm') & is.na(gemscript) ~ 'remove',
                                      str_detect(read_term, 'equanil') & is.na(gemscript) ~ 'remove',
                                      TRUE ~ .))) %>%
  filter(is.na(notes)) %>%
  select(read_term, read_code = code) %>%
  filter(!is.na(read_code)) %>%
  distinct()

#### depression ####
dep_tx <- read_csv("lists_in/CPRD_Cambridge/CPRDCAM_DEP153_PC_V1-1_Oct2018.csv") %>%
  select(read_term = productname)

dep_tx_os <- read_csv("lists_in/CPRD_Cambridge/elsie-horne-depression_drug-261bbbc0.csv")


depression_prescriptions <- dep_tx %>%
  mutate_at('read_term', list(~ str_remove(., '\\s+\\(.+\\)'))) %>%
  mutate_at('read_term', list(~ str_to_lower(.))) %>%
  mutate_at('read_term', list(~ str_replace(., 'microgram', 'mcg'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'micrograms', 'mcg'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'mcgs', 'mcg'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'tablets', 'tablet'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'capsules', 'capsule'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'modified-release', 'm/r'))) %>%
  mutate_at('read_term', list(~ str_replace(., 'gel-fill', 'thick gel'))) %>% # checked and these seem to be equivalent
  mutate_at('read_term', list(~ str_replace(., '0\\.5mg', '500mcg'))) %>%
  # mutate_at('read_term', list(~ str_replace(., 'oral s.+', 'liquid'))) %>%
  mutate(gemscript = TRUE) %>%
  full_join(dep_tx_os %>%
              mutate_at('term', list(~ str_to_lower(.))) %>%
              mutate_at('term', list(~ str_replace(., 'microgram', 'mcg'))) %>%
              mutate_at('term', list(~ str_replace(., 'micrograms', 'mcg'))) %>%
              mutate_at('term', list(~ str_replace(., 'mcgs', 'mcg'))) %>%
              mutate_at('term', list(~ str_replace(., 'tablets', 'tablet'))) %>%
              mutate_at('term', list(~ str_replace(., 'capsules', 'capsule'))),
            by = c('read_term' = 'term')) %>%
  mutate(notes = NA_character_) %>%
  distinct() %>%
  arrange(read_term) %>% mutate_at('notes', 
                                   list(~ case_when(# remove if drug not in cambridge list
                                     str_detect(read_term, 'winfex') ~ 'remove', 
                                     str_detect(read_term, 'venladex') ~ 'remove', 
                                     str_detect(read_term, 'vaxalin') ~ 'remove', 
                                     str_detect(read_term, 'trixat') ~ 'remove', 
                                     str_detect(read_term, 'tranquax') ~ 'remove', 
                                     str_detect(read_term, 'sunveniz') ~ 'remove', 
                                     str_detect(read_term, 'olena') ~ 'remove', 
                                     str_detect(read_term, 'motival') ~ 'remove', 
                                     str_detect(read_term, 'lentizol') ~ 'remove', 
                                     str_detect(read_term, 'no drugs here') ~ 'remove', 
                                     str_detect(read_term, 'foraven xl 150mg m/r capsule') ~ 'remove', 
                                     str_detect(read_term, 'feprapax') ~ 'remove', 
                                     str_detect(read_term, 'dothiepin') ~ 'remove', 
                                     str_detect(read_term, 'cymbalta') ~ 'remove', 
                                     str_detect(read_term, 'bonilux') ~ 'remove', 
                                     str_detect(read_term, 'aventyl') ~ 'remove', 
                                     str_detect(read_term, 'elavil') & is.na(gemscript) ~ 'remove',
                                     str_detect(read_term, 'praminil') & is.na(gemscript) ~ 'remove',
                                     TRUE ~ .))) %>% 
  filter(is.na(notes)) %>%
  select(read_term, read_code = code) %>%
  filter(!is.na(read_code)) %>%
  distinct()

dep_anx_prescriptions <- bind_rows(depression_prescriptions %>%
                                     mutate(cat2 = 'depression prescriptions'),
                                   anxiety_prescriptions %>% 
                                     mutate(cat2 = 'anxiety prescriptions')) %>%
  mutate(cat2 = 'comorbidity prescriptions')

write_csv(allergies, path = 'lists_out/allergic_conditions.csv')

# latex tables
allergies_list <- allergies %>%
  group_split(cat2)

allergies_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  caption <- str_c('Read codes for \\emph{\'', category, '\'} (return to \\nameref{cha:ehr:methods:pre:allergy} methods)')
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

lapply(allergies_list, allergies_latex_table)

