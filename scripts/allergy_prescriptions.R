#  allergy drugs

source('setup.R')

# better to be over-inclusive at this stage
# can talk to clinician/consult the BNF and trim down based on read term
# from googling, I don't think treatments in the hyposensitisation category are prescribed any more

# be over-inclusive, then run on 2016 data and count occurences of each read code in 1-year pre-index

antihistamine_v3 <-  read_csv("lists_in/OpenSafely/elsie-horne-antihistamine-3ff1e284.csv")
allergy_drugs_v3 <-  read_csv("lists_in/OpenSafely/elsie-horne-allergy-drug-533d3084.csv")

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
