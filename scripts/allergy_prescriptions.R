#  allergy drugs

source('setup.R')

# version 2 code lists
# all starting l8...
nasal_allergy_drugs_v2 <- read_delim("lists_in/Elsie/nasal_allergy_drugs_v2", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

# subset of those starting k6...
eye_allergy_drugs_v2 <- read_delim("lists_in/Elsie/eye_allergy_drugs_v2", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

# all starting c8...
oral_antihistamines_v2 <- read_delim("lists_in/Elsie/oral_antihistamines_v2", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

# all starting m34..
topical_antihistamines_v2 <- read_delim("lists_in/Elsie/topical_antihistamines_v2", 
                                "\t", escape_double = FALSE,
                                trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

# better to be over-inclusive at this stage
# can talk to clinician/consult the BNF and trim down based on read term
# from googling, I don't think treatments in the hyposensitisation category are prescribed any more

# these lists are over-inclusive and need to be trimmed down
antihistamines_v3 <-  read_csv("lists_in/Elsie/elsie-horne-antihistamine-667f1587.csv") %>% 
  rename_all(list(~ str_c('read_', .)))
allergy_drugs_v3 <-  read_csv("lists_in/Elsie/elsie-horne-allergy-drug-4d6fa252.csv") %>% 
  rename_all(list(~ str_c('read_', .)))

antihistamines_bnf <- read_csv("lists_in/OpenSafely/opensafely-antihistamines-oral-2020-10-22.csv")

# label read codes in antihistamines_v3 starting with x0 as:
# antihistamine (non-nasal), nasal allergy drug, other
v3_codes <- antihistamines_v3 %>%
  bind_rows(allergy_drugs_v3 %>%
              # get rid of allergy kits, maintenence sets, treatment sets and vaccines
              filter(!(str_detect(read_term, '\\skit|\\svaccine|treatment\\sset|maintenance\\sset')))) %>%
  filter(str_detect(read_code, '^x0')) %>% 
  mutate(read_term_trunc = read_term) %>%
  # keep only first word of read_term in lowercase
  mutate_at('read_term_trunc', list(~ str_remove_all(., '\\s\\S+'))) %>%
  mutate_at('read_term_trunc', tolower) %>%
  arrange(read_term_trunc) %>%
  # join first word to first word of antihistamines_v2 read_term
  left_join(oral_antihistamines_v2 %>%
              select(read_term) %>%
              mutate_at('read_term', list(~ str_remove(., '^\\*'))) %>%
              mutate_at('read_term', list(~ str_remove_all(., '\\s\\S+'))) %>%
              mutate_at('read_term', tolower) %>%
              distinct() %>%
              mutate(antihistamine_v2 = TRUE),
            by = c('read_term_trunc' = 'read_term')) %>%
  # join first word to first word of antihistamines_bnf read_term
  left_join(antihistamines_bnf %>%
              select(read_term = nm) %>%
              mutate_at('read_term', list(~ str_remove(., '^\\*'))) %>%
              mutate_at('read_term', list(~ str_remove_all(., '\\s\\S+'))) %>%
              mutate_at('read_term', tolower) %>%
              distinct() %>%
              mutate(antihistamine_bnf = TRUE),
            by = c('read_term_trunc' = 'read_term')) %>%
  mutate(antihistamine = case_when(str_detect(read_term, regex('antihistamine', ignore_case = TRUE)) ~ TRUE,
                                   antihistamine_v2 | antihistamine_bnf ~ TRUE,
                                   TRUE ~ FALSE),
         nasal_allergy_drug = str_detect(read_term, regex('\\snose|\\snasal|spray', ignore_case = TRUE)),
         eye_allergy_drug = str_detect(read_term, regex('\\seye', ignore_case = TRUE)),
         topical_antihistamine = str_detect(read_term, regex('\\scream|\\slotion|\\sskin', ignore_case = TRUE)))

v3_codes %>%
  filter_at(vars(c('antihistamine', 'nasal_allergy_drug', 'eye_allergy_drug', 'topical_antihistamine')),
            all_vars(!.)) %>%
  select(read_code, read_term, antihistamine:topical_antihistamine) %>%
  print(n=100)

#  google search the rest to check if antihistamine (oral, nasal, eye, topical) or other nasal / eye allergy drug
v3_codes <- v3_codes %>%
  mutate(type = case_when(read_code %in%  'x05cW' ~ 'eye',
                          read_code %in%  'x01Dp' ~ 'eye and nose',
                          read_code %in%  'x05By' ~ 'nose',
                          read_code %in%  'x01Dv' ~ 'no',
                          read_code %in%  'x04xx' ~ 'eye',
                          read_code %in%  'x05lt' ~ 'nose',
                          read_code %in%  c('x03am', 'x044U', 'x05j3') ~ 'eye',
                          read_code %in%  'x01E8' ~ 'oral antihistamine',
                          read_code %in%  'x01E9' ~ 'parenteral antihistamine',
                          read_code %in%  'x04ov' ~ 'no', # wasp venom extract
                          read_code %in%  'x01E7' ~ 'no', # for headaches
                          read_code %in%  'x0564' ~ 'no',
                          read_code %in%  'x0566' ~ 'no',
                          read_code %in%  'x00fo' ~ 'oral antihistamine',
                          read_code %in%  'x02KI' ~ 'no',
                          read_code %in%  c('x05FH', 'x04r6') ~ 'nose',
                          read_code %in%  'x00fx' ~ 'no',
                          read_code %in%  'x00fy' ~ 'oral antihistamine',
                          read_code %in%  'x01eh' ~ 'no',
                          TRUE ~ NA_character_)) %>%
  mutate_at('nasal_allergy_drug', list(~ case_when(str_detect(read_term, regex('nasal|nose', ignore_case = TRUE)) ~ TRUE,
                                                   str_detect(type, regex('nose', ignore_case = TRUE)) ~ TRUE,
                                                   antihistamine ~ FALSE,
                                                   TRUE ~ .))) %>%
  mutate_at('eye_allergy_drug', list(~ case_when(str_detect(read_term, regex('eye', ignore_case = TRUE)) ~ TRUE,
                                                 str_detect(type, regex('eye', ignore_case = TRUE)) ~ TRUE,
                                                 antihistamine ~ FALSE,
                                                   TRUE ~ .))) %>%
  mutate_at('topical_antihistamine', list(~ case_when(str_detect(read_term, regex('cream|lotion|skin', ignore_case = TRUE)) ~ TRUE,
                                                      antihistamine ~ FALSE,
                                                      TRUE ~ .))) %>%
  mutate(oral_antihistamine = case_when(nasal_allergy_drug | eye_allergy_drug | topical_antihistamine ~ FALSE,
                                        antihistamine_v2 | antihistamine_bnf ~ TRUE,
                                        TRUE ~ FALSE)) 

allergy_drugs <- bind_rows(oral_antihistamines_v2 %>%
                             bind_rows(v3_codes %>%
                                         filter(oral_antihistamine) %>%
                                         select(read_code, read_term)) %>%
                                         mutate(cat2 = 'oral antihistamines'),
                           nasal_allergy_drugs_v2 %>%
                             bind_rows(v3_codes %>%
                                         filter(nasal_allergy_drug) %>%
                                         select(read_code, read_term)) %>%
                                         mutate(cat2 = 'nasal allergy drug'),
                           eye_allergy_drugs_v2 %>%
                             bind_rows(v3_codes %>%
                                         filter(eye_allergy_drug) %>%
                                         select(read_code, read_term)) %>%
                                         mutate(cat2 = 'eye allergy drug'),
                           topical_antihistamines_v2 %>%
                             bind_rows(v3_codes %>%
                                         filter(topical_antihistamine) %>%
                                         select(read_code, read_term)) %>%
                                         mutate(cat2 = 'topical antihistamines')) %>%
  mutate(cat1 = 'allergy drugs')
  
