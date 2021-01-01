#  allergy drugs

source('setup.R')

# version 2 code lists
# all starting l8...
nasal_allergy_drugs_v2 <- read_delim("lists_in/Elsie/nasal_allergy_drugs_v2", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

# all starting c8...
antihistamines_v2 <- read_delim("lists_in/Elsie/antihistamines_v2", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

all_v2 <- nasal_allergy_drugs_v2 %>%
  bind_rows(antihistamines_v2) %>%
  distinct()

# better to be over-inclusive at this stage
# can talk to clinician/consult the BNF and trim down based on read term
# from googling, I don't think treatments in the hyposensitisation category are prescribed any more

# these lists are over-inclusive and need to be trimmed down
antihistamines_v3 <-  read_csv("lists_in/OpenSafely/elsie-horne-antihistamine-667f1587.csv") %>% 
  rename_all(list(~ str_c('read_', .)))
allergy_drugs_v3 <-  read_csv("lists_in/OpenSafely/elsie-horne-allergy-drug-4d6fa252.csv") %>% 
  rename_all(list(~ str_c('read_', .)))

antihistamines_bnf <- read_csv("lists_in/OpenSafely/opensafely-antihistamines-oral-2020-10-22.csv")

# check for read codes in v3 antihistamine list starting with x0 and with the 
# first word of description matching one starting c8 from the v2 list
antihistamine_check <- antihistamines_v3 %>%
  bind_rows(allergy_drugs_v3 %>%
              # get rid of allergy kits, maintenence sets, treatment sets and vaccines
              filter(!(str_detect(read_term, '\\skit|\\svaccine|treatment\\sset|maintenance\\sset')))) %>%
  filter(str_detect(read_code, '^x0')) %>% 
  mutate(read_term_full = read_term) %>%
  mutate(antihistamine = str_detect(read_term, regex('antihistamine', ignore_case = TRUE))) %>%
  mutate_at('read_term', list(~ str_remove_all(., '\\s\\S+'))) %>%
  mutate_at('read_term', tolower) %>%
  arrange(read_term) %>%
  left_join(antihistamines_v2 %>%
              select(read_term) %>%
              mutate_at('read_term', list(~ str_remove(., '^\\*'))) %>%
              mutate_at('read_term', list(~ str_remove_all(., '\\s\\S+'))) %>%
              mutate_at('read_term', tolower) %>%
              distinct() %>%
              mutate(antihistamine_v2 = TRUE),
            by = 'read_term') %>%
  left_join(antihistamines_bnf %>%
              select(read_term = nm) %>%
              mutate_at('read_term', list(~ str_remove(., '^\\*'))) %>%
              mutate_at('read_term', list(~ str_remove_all(., '\\s\\S+'))) %>%
              mutate_at('read_term', tolower) %>%
              distinct() %>%
              mutate(antihistamine_bnf = TRUE),
            by = 'read_term')  %>%
  mutate_at('antihistamine', list(~ case_when(antihistamine_v2 | antihistamine_bnf ~ TRUE,
                                              TRUE ~ .)))

antihistamine_check %>% 
  filter(!antihistamine) %>%
  print(n=100)

#  google search the rest to check if antihistamine
antihistamine_check <- antihistamine_check %>%
  mutate_at('antihistamine', list(~ case_when(str_detect(read_term, regex('^Alomide', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('^Azelastine', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('cinnarizine', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('emadine', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('emedastine', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('levocabastine', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('livostin', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('opatanol', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('optilast', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('oral', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('Otrivine-Antistin', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('parenteral', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('pollon-eze', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('rhinolast', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('seldane', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('vibrocil', ignore_case = TRUE)) ~ TRUE,
                                              str_detect(read_term, regex('vasocon', ignore_case = TRUE)) ~ TRUE,
                                              TRUE ~ .)))

antihistamines_v3 <- antihistamine_check %>% 
  filter(antihistamine) %>%
  select(read_code) %>%
  left_join(antihistamines_v3, by = 'read_code') %>%
  left_join(allergy_drugs_v3, by = 'read_code') %>%
  mutate(read_term = if_else(!is.na(read_term.x), read_term.x, read_term.y)) %>%
  select(read_code, read_term)


###############
#  do the same for nasal allergy drugs
nasal_allergy_check <- allergy_drugs_v3 %>%
  filter(str_detect(read_code, '^x0')) %>%
  # get rid of allergy kits, maintenence sets, treatment sets, vaccines, cream, lotions and eye drops
  filter(!(str_detect(read_term, '\\skit|\\svaccine|\\seye\\sdrop|treatment\\sset|maintenance\\sset|\\scream|\\slotion'))) %>%
  mutate(nasal_allergy = FALSE) %>%
  mutate(read_term_full = read_term) %>%
  mutate_at('read_term', list(~ str_remove_all(., '\\s\\S+'))) %>%
  mutate_at('read_term', tolower) %>%
  arrange(read_term) %>%
  left_join(nasal_allergy_drugs_v2 %>%
              select(read_term) %>%
              mutate_at('read_term', list(~ str_remove(., '^\\*'))) %>%
              mutate_at('read_term', list(~ str_remove_all(., '\\s\\S+'))) %>%
              mutate_at('read_term', tolower) %>%
              distinct() %>%
              mutate(nasal_allergy_v2 = TRUE),
            by = 'read_term') %>% print(n=Inf)
  mutate_at('nasal_allergy', list(~ case_when(nasal_allergy_v2 ~ TRUE,
                                              TRUE ~ .)))





all_v3 <- allergy_drugs_v3 %>%
  bind_rows(antihistamines_v3) %>%
  distinct()

all <- all_v2 %>%
  bind_rows(rename_all(all_v3, list(~ str_c('read_', .)))) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  mutate_at('read_term', list(~ str_to_lower(str_remove(., '^\\*'))))  %>%
  mutate(version = if_else(str_detect(read_code, '^w|^x'), 3, 2)) %>%
  group_split(version)

tmp <- all[[1]] %>%
  rename(read_code_v2 = read_code) %>%
  select(-version) %>%
  full_join(all[[2]] %>%
              rename(read_code_v3 = read_code) %>%
              select(-version),
            by = 'read_term')

tmp %>% filter(str_detect(read_term, 'aller-eze'))



antihistamine_v3 %>%
  mutate_at('code', 
            list(~ str_trunc(., width = 2, side = 'right', ellipsis = ''))) %>%
  group_by(code) %>% count()

allergy_drugs_v3 %>%
  mutate_at('code', 
            list(~ str_trunc(., width = 2, side = 'right', ellipsis = ''))) %>%
  group_by(code) %>% count()

both <- antihistamine_v3 %>%
  rename(term_antihistamine = term) %>%
  full_join(allergy_drugs_v3 %>%
              rename(term_allergy_drugs = term),
            by = 'code')


both %>%
  mutate_at('code', 
            list(~ str_trunc(., width = 2, side = 'right', ellipsis = ''))) %>%
  group_by(code) %>% count()
# 1  bn        3 = peripheral vasodilators
# 2  c8      222 = antihistamines
# 3  c9       50 = hyposensitisation
# 4  d1        1 = hypnotics
# 5  d2        9 = anxiolytics
# 6  dh       28 = nausea and vertugo drugs
# 7  dm        7 = prophylaxis of migraine
# 8  k6       28 = corticosteroids + anti-infl (eye)
# 9  l8       19 = nasal allergy drugs
# 10 l9        2 = topical nasal decongestants
# 11 la        3 = anti-infective nasal preps
# 12 lA        1 = other nasal preparations
# 13 m3       14 = LOCAL ANAESTHETIC/ANTIPRURITIC
# 14 m9        1 = SUNSCREENING PREPARATIONS
# 15 o5        2 = PERIOPERATIVE ANXIOLYTICS/NEUROLEPTICS
# 16 wy        1 = V3
# 17 x0      166 = V3

tmp <- both %>%
  mutate(term = if_else(!is.na(term_antihistamine), term_antihistamine, term_allergy_drugs)) %>%
  select(code, term) %>%
  mutate(version = if_else(str_detect(code, '^w|^x'), 3, 2)) %>%
  group_split(version)

tmp[[1]] %>%
  select(code_3 = code, term) %>%
  full_join(tmp[[2]] %>%
              select(code_2 = code, term), by = 'term')
