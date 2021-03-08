#  comorbidity prescriptions

source('setup.R')

# proton pump inhibitiros (PPIs), h2-antagonists, anti-depressants etc

# use the pseudo-BNF list from OpenSafely to get a list of PPI drug names
ppi_bnf <- read_csv("lists_in/OpenSafely/opensafely-proton-pump-inhibitors-ppi-oral-2020-04-20.csv")

drugname <- sort(unique(unname(sapply(ppi_bnf$nm, function(x) str_split(x, '\\s', n=2)[[1]][1]))))

# search all drugname in NHS browser (v2) and open safely browser tool (v3)
drugs <- sort(unique(unname(sapply(ppi_bnf$nm, function(x) str_split(x, '\\s\\(', n=2)[[1]][1]))))

drugs[str_detect(drugs, regex('ventra', ignore_case = T))]
# no OpenSafely results for guardium or ventra

ppi_v2 <- read_delim("lists_in/Elsie/PPI_v2", 
                     "\t", escape_double = FALSE,
                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*')))

ppi_v3 <- read_csv("lists_in/Elsie/elsie-horne-proton-pump-inhibitors-63cc5591.csv") %>%
  rename_all(~ str_c('read_', .)) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*')))

ppi <- bind_rows(ppi_v2, ppi_v3) %>%
  filter(!str_detect(read_term, 'injection|intravenous')) %>%
  # so that QOF = yes comes first and are kept with distinct
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'proton pump inhibitor')

# all H2-antagonists in the a6... branch of the v2 NHS Read code browser
h2antagonist_v2 <- read_delim("lists_in/Elsie/h2antagonist_v2", 
                              "\t", escape_double = FALSE,
                              trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*')))

h2names <- h2antagonist_v2 %>%
  filter(!str_detect(read_term, 'injection')) %>%
  select(read_term) %>%
  unlist() %>% unname()

h2names <- sort(unique(unname(sapply(h2names, function(x) str_split(x, '\\s', n=2)[[1]][1]))))

# search these in the OpenSafely browser
# no results for raciran or ranzac

h2antagonist_v3 <- read_csv("lists_in/Elsie/elsie-horne-h2antagonist-50226248.csv") %>%
  rename_all(~ str_c('read_', .)) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  filter(! str_detect(read_code, '^a2'))

h2antagonist <- bind_rows(h2antagonist_v2, h2antagonist_v3) %>%
  filter(!str_detect(read_term, 'injection|intravenous')) %>%
  # so that QOF = yes comes first and are kept with distinct
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'h2-antagonist')

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
  distinct() %>% 
  mutate(cat2 = 'anti-anxiety')

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
  distinct()  %>%
  mutate(cat2 = 'anti-depressant')

# bind all
comorbidity_prescriptions <- bind_rows(h2antagonist, 
                                       ppi, 
                                       depression_prescriptions, 
                                       anxiety_prescriptions) %>%
  mutate(cat1 = 'comorbidity prescription') 

# save list
write_csv(comorbidity_prescriptions, 
          path = file.path(opcrd_analysis_path, 'comorbidity_prescriptions.csv'))
write_csv(comorbidity_prescriptions %>%
            arrange(cat2, read_code, read_term) %>%
            select(Comedication = cat2,
                   `Read code` = read_code,
                   Term = read_term),
          path = 'lists_out/comorbidity_prescriptions.csv')

# latex tables for thesis
drug_list <- comorbidity_prescriptions %>%
  group_split(cat2)

drug_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  caption <- str_c('Read codes for \\emph{\'', category, '\'} prescription group (return to \\nameref{cha:ehr:methods:pre:comorbidities} methods)')
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

lapply(drug_list, drug_latex_table)