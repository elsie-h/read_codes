source('setup.R')

#### from Mukherjee 2016 allergy paper ####
# https://onlinelibrary.wiley.com/doi/abs/10.1111/all.12928
mukherjee <- read_excel("lists_in/Mukherjee2016/Mukherjee2016_supinfo.xlsx", 
                        skip = 1) %>%
  filter(!(`Disease Area` %in% 'Asthma'),
         !(Susannah %in% 'no')) %>%
  filter((`Allergy or not allergy` %in% 'A')|
           (Susannah %in% 'yes')) %>%
  select(read_code = Code, read_term = Term,
         `Disease Area`, `Type of code`) %>%
  mutate_at('read_term', list(~ str_trim(str_remove_all(., '\\.\\.\\.'), side = 'right')))

# non-allergy codes to remove based on Susannah's feedback
nonallergy_codes <- c('A5320', 'A5441', 'F4D33', 'F4D4.',
                      'F4D6.', 'FyuB4', 'FyuB5', 'G831.',
                      'M10..', 'M103.', 'M10z.', 'X00YQ',
                      'X00YR', 'X00YS', 'X00YT', 'X00YU',
                      'X00YV', 'X00YW', 'X506H', 'X506j',
                      'X00kv', 'X00kz', 'X00l0', 'X00l1',
                      'X00l2', 'XE2RT', 'Xa2dV', 'XaNkV',
                      'D2101', 'M1100', 'SN520')

# Function for joining V2 & CTV3 Read code tables
# read_code_v2 = in version 2 list
# read_code_v3.2 = in version 3 list but also in version 2 list
# read_code_v3 = only in version 3 list
join_read <- function(data_v2, data_v3) {
  
  if (!all(names(data_v2) %in% c('read_code_v2', 'read_term'))) stop('Columns of data_v2 must be \'read_code_v2\' and \'read_term\'')
  if (!all(names(data_v3) %in% c('id', 'term'))) stop('Columns of data_v3 must be \'id\' and \'term\'')
  
  tmp1 <- data_v2 %>% 
    distinct() %>%
    left_join(data_v3, by = c('read_code_v2' = 'id')) %>%
    mutate(read_code_v3.2 = if_else(is.na(term),
                                    read_code_v2,
                                    NA_character_)) %>%
    select(read_code_v2, read_code_v3.2, read_term)
  
  tmp2 <- data_v3 %>%
    distinct() %>%
    anti_join(data_v2, by = c('id' = 'read_code_v2')) %>%
    rename(read_code_v3 = id, read_term = term)

  tmp1 %>%
    full_join(tmp2, by = 'read_term') %>%
    select(read_term, read_code_v2, read_code_v3.2, read_code_v3) %>%
    distinct() %>%
    # remove based on Susannah's feedback
    filter_at(vars(starts_with('read_code')), all_vars(!(. %in% nonallergy_codes))) 
}

################################################################################
# Anaphylaxis
anaphylaxis_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Anaphylaxis') %>%
  select(read_code_v2 = read_code, read_term)

#### Codes for anaphylaxis from manual search of CTV3 ####
anaphylaxis_v3 <-  read_csv("lists_in/Elsie/elsie-horne-anaphylaxis-2020-11-09T13-56-17.csv")
  
# join to check how well the two lists correspond
anaphylaxis <- join_read(data_v2 = anaphylaxis_v2,
                         data_v3 = anaphylaxis_v3) %>%
  filter(!(read_term %in% 'Allergic urticaria'))

anaphylaxis <- anaphylaxis %>%
  pivot_longer(cols = starts_with('read_code')) %>%
  rename(read_code = value) %>%
  select(-name) %>%
  filter(!is.na(read_code)) %>% 
  distinct(read_code, .keep_all = TRUE) %>%
  add_row(read_code = 'XaIPo', read_term = 'Carries adrenaline preloaded injection pen') %>%
  add_row(read_code = '66G8.', read_term = 'Carries adrenaline preloaded injection pen')

# check mapping
# map V2 -> CTV3
anaphylaxis %>% map_V2_CTV3()
# map CTV3 -> V2
anaphylaxis %>% map_CTV3_V2()

################################################################################
# Angioedema
angioedema_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Angioedema') %>%
  select(read_code_v2 = read_code, read_term)

#### Codes for Angioedema from manual search of CTV3 ####
angioedema_v3 <-  read_csv("lists_in/Elsie/elsie-horne-angioedema-2020-11-09T13-43-48.csv")

# join to check how well the two lists correspond
angioedema <- join_read(data_v2 = angioedema_v2,
                         data_v3 = angioedema_v3)

angioedema <- angioedema %>%
  pivot_longer(cols = starts_with('read_code')) %>%
  rename(read_code = value) %>%
  select(-name) %>%
  filter(!is.na(read_code)) %>% 
  distinct(read_code, .keep_all = TRUE)

# check mapping
# map V2 -> CTV3
angioedema %>% map_V2_CTV3()
# map CTV3 -> V2
angioedema %>% map_CTV3_V2()

################################################################################
# Conjunctivitis
conjunctivitis_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Conjunctivitis') %>%
  select(read_code_v2 = read_code, read_term)

#### Codes for allergic conjunctivits from manual search of CTV3 ####
conjunctivitis_v3 <-  read_csv("lists_in/Elsie/elsie-horne-conjunctivitis-2020-11-09T13-18-07.csv")

# join to check how well the two lists correspond
conjunctivitis <- join_read(data_v2 = conjunctivitis_v2,
                        data_v3 = conjunctivitis_v3) 

conjunctivitis <- conjunctivitis %>%
  pivot_longer(cols = starts_with('read_code')) %>%
  rename(read_code = value) %>%
  select(-name) %>%
  filter(!is.na(read_code)) %>% 
  distinct(read_code, .keep_all = TRUE)

# check mapping
# map V2 -> CTV3
conjunctivitis %>% map_V2_CTV3()
# map CTV3 -> V2
conjunctivitis %>% map_CTV3_V2()

################################################################################
# Drug allergy
drug_allergy_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Drug allergy') %>%
  select(read_code_v2 = read_code, read_term)

drug_allergy_v3 <- read_csv("lists_in/Elsie/elsie-horne-drug-allergy-2020-11-05T08-48-21.csv") 

# join to check how well the two lists correspond
drug_allergy <- join_read(data_v2 = drug_allergy_v2,
                            data_v3 = drug_allergy_v3)

drug_allergy <- drug_allergy %>%
  pivot_longer(cols = starts_with('read_code')) %>%
  rename(read_code = value) %>%
  select(-name) %>%
  filter(!is.na(read_code)) %>% 
  distinct(read_code, .keep_all = TRUE)

# check mapping
# map V2 -> CTV3
drug_allergy %>% map_V2_CTV3()
# map CTV3 -> V2
drug_allergy %>% map_CTV3_V2()

drug_allergy <- drug_allergy %>%
  bind_rows(drug_allergy %>% 
              map_CTV3_V2() %>%
              rename(read_code = V2_CONCEPTID, read_term = Term)) %>%
  add_row(read_code = 'XaQaf', read_term = '[X]Personal history of allergy to bisoprolol') %>%
  add_row(read_code = 'XaQag', read_term = '[X]Personal history of allergy to carvedilol') %>%
  add_row(read_code = 'M130.', read_term = 'Ingestion dermatitis due to drugs') %>%
  add_row(read_code = 'M1300', read_term = 'Generalized skin eruption due to drugs and medicaments') %>%
  add_row(read_code = 'M1301', read_term = 'Localized skin eruption due to drugs and medicaments') %>%
  add_row(read_code = 'M1302', read_term = 'Drug-induced erythroderma') %>%
  add_row(read_code = 'XE1Aw', read_term = 'Ingestion dermatitis due to drugs') %>%
  add_row(read_code = 'X50Ge', read_term = 'Drug eruption') %>%
  add_row(read_code = 'X50Gm', read_term = 'Drug-induced erythroderma') %>%
  add_row(read_code = 'ZVu6q', read_term = '[X]Pers hist allergy nebivolol') %>%
  add_row(read_code = 'XaQah', read_term = '[X]Personal history of allergy to nebivolol')

# check mapping again
# map V2 -> CTV3
drug_allergy %>% map_V2_CTV3()
# map CTV3 -> V2
drug_allergy %>% map_CTV3_V2()

################################################################################
# Eczema
eczema_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Eczema') %>%
  select(read_code_v2 = read_code, read_term)

eczema_v3 <- read_csv("lists_in/Elsie/elsie-horne-eczema-allergy-2020-11-09T13-28-09.csv") 

# join to check how well the two lists correspond
eczema <- join_read(data_v2 = eczema_v2,
                          data_v3 = eczema_v3) %>%
  # add from mapped lists
  bind_rows(tribble(~read_code_v2, ~read_term,
                    'M1B..',	'Juvenile plantar dermatosis',
                    '26C4.',	'Nipple eczema',
                    'F4D02',	'Squamous blepharitis',
                    'M1y2.',	'Gravitational eczema',
                    'M1273',	'Photodermatitis',
                    'M130.',	'Ingestion dermatitis due to drugs',
                    'M12z4',	'Erythrodermic eczema',
                    'M1302',	'Drug-induced erythroderma',
                    'Myu22',	'[X]Exacerbation of eczema',
                    'F4D30',	'Eczematous eyelid dermatitis',
                    'M1536',	'Periocular dermatitis'
                    ))

eczema <- eczema %>%
  pivot_longer(cols = starts_with('read_code')) %>%
  rename(read_code = value) %>%
  select(-name) %>%
  filter(!is.na(read_code)) %>% 
  distinct(read_code, .keep_all = TRUE)

# check mapping
# map V2 -> CTV3
eczema %>% map_V2_CTV3()
# map CTV3 -> V2
eczema %>% map_CTV3_V2()

# X50Ge	Drug eruption

################################################################################
# Food allergy
food_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Food allergy') %>%
  select(read_code_v2 = read_code, read_term)

food_v3 <- read_csv("lists_in/Elsie/elsie-horne-food-allergy-2020-11-09T16-28-48.csv") 

# join to check how well the two lists correspond
food <- join_read(data_v2 = food_v2,
                    data_v3 = food_v3) %>%
  # add from mapped lists
  bind_rows(tribble(~read_code_v2, ~read_term,
                    'SN58B',	'Allergy to banana',
                    'SN58C',	'Allergy to tomato',
                    '8CA4S',	'Dietary education for food allergy',
                    'Xa1nn',	'Cows milk allergy'))

food <- food %>%
  pivot_longer(cols = starts_with('read_code')) %>%
  rename(read_code = value) %>%
  select(-name) %>%
  filter(!is.na(read_code)) %>% 
  distinct(read_code, .keep_all = TRUE)

# check mapping
# map V2 -> CTV3
food %>% map_V2_CTV3()
# map CTV3 -> V2
food %>% map_CTV3_V2()

## add to other allergy:
# J432.	Allergic gastroenteritis and colitis

################################################################################
# Rhinitis
rhinitis_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Rhinitis') %>%
  select(read_code_v2 = read_code, read_term) %>%
  add_row(read_code_v2 = 'H170.',
          read_term = 'Hay fever - pollens') %>%
  add_row(read_code_v2 = 'H3300',
          read_term = 'Hay fever with asthma') %>%
  add_row(read_code_v2 = 'H171.',
          read_term = 'Hay fever - other allergen') %>%
  add_row(read_code_v2 = 'H172.',
          read_term = 'Hay fever - unspecified allergen')

rhinitis_v3 <- read_csv("lists_in/Elsie/elsie-horne-rhinitis-allergic-2020-11-09T16-35-40.csv") 

# join to check how well the two lists correspond
rhinitis <- join_read(data_v2 = rhinitis_v2,
                  data_v3 = rhinitis_v3) %>%
  add_row(read_code_v3 = 'X00kv', read_term = 'Acute rhinitis')

rhinitis <- rhinitis %>%
  pivot_longer(cols = starts_with('read_code')) %>%
  rename(read_code = value) %>%
  select(-name) %>%
  filter(!is.na(read_code)) %>% 
  distinct(read_code, .keep_all = TRUE)

# check mapping
# map V2 -> CTV3
rhinitis %>% map_V2_CTV3()
# map CTV3 -> V2
rhinitis %>% map_CTV3_V2()

# Do not add:
# XE0Xl	Common cold
# XM00h	Rhinorrhoea
# XM1Md	Catarrh
# X00l3	Cat allergy
# X00l4	Dander (animal) allergy
# X00l5	Feather allergy
# X00l6	House dust allergy
# X00l7	House dust mite allergy
# XaIpW	Allergy to animal
# XaOb5	Allergy to dog dander
# XE0YR	Extrinsic asthma without status asthmaticus

################################################################################
# Urticaria
urticaria_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Urticaria') %>%
  select(read_code_v2 = read_code, read_term)

urticaria_v3 <- read_csv("lists_in/Elsie/elsie-horne-urticaria-2020-11-09T13-40-47.csv") 

# join to check how well the two lists correspond
urticaria <- join_read(data_v2 = urticaria_v2,
                      data_v3 = urticaria_v3) %>%
  bind_rows(tribble(~ read_code_v2, ~ read_term,
                    'SN511',	'Hereditary C1 esterase inhibitor deficiency - deficient factor',
                    'M12A2',	'Solar urticaria',
                    'SN510',	'Acquired C1 esterase inhibitor deficiency'))

urticaria <- urticaria %>%
  pivot_longer(cols = starts_with('read_code')) %>%
  rename(read_code = value) %>%
  select(-name) %>%
  filter(!is.na(read_code)) %>% 
  distinct(read_code, .keep_all = TRUE)

# check mapping
# map V2 -> CTV3
urticaria %>% map_V2_CTV3()
# map CTV3 -> V2
urticaria %>% map_CTV3_V2()

# M18yz	Other pruritic conditions NOS
# M130.	Ingestion dermatitis due to drugs
# SN53.	Allergy, unspecified

################################################################################
# Other
other_v2 <- mukherjee %>%
  # my OpenSafely search for 'venom allergy' did not identify any codes 
  filter(`Disease Area` %in% c('Other', 'Venom allergy'),
         # None of the read codes under 'Test' or 'Other' indicate that 
         # an allergy is present, only that it has been tested
         !(`Type of code` %in% c('Test',
                                 'Other')),
         !(read_term %in% c('Hypersens. skin test done',
                       'Hypersens. skin test: no react')))%>%
  select(read_code_v2 = read_code, read_term)

other_v3 <- read_csv("lists_in/Elsie/elsie-horne-other-allergy-2020-11-09T17-09-26.csv") 

# remove any that have appeared in other code lists
other <- bind_rows(other_v2 %>% rename(read_code = read_code_v2),
                   other_v3 %>% rename(read_code = id, read_term = term)) %>%
  distinct(read_code, .keep_all = TRUE)

remove <- bind_rows(anaphylaxis,
                 angioedema,
                 conjunctivitis,
                 drug_allergy,
                 eczema,
                 food,
                 rhinitis,
                 urticaria) %>%
  distinct(read_code) 

other <- other %>%
  anti_join(remove, by = 'read_code')

# check mapping
# map V2 -> CTV3
other %>% map_V2_CTV3() %>% anti_join(remove, by = c('CTV3_CONCEPTID' = 'read_code'))
# map CTV3 -> V2
other %>% map_CTV3_V2() %>% anti_join(remove, by = c('V2_CONCEPTID' = 'read_code')) %>% print(n=Inf)

other <- other %>%
  bind_rows(tribble(~read_code, ~read_term,
                    'XE0al',	'Allergic gastroenteritis and colitis',
                    'X30Bq',	'Allergic diarrhoea',
                    'SN533', 'Atopy',
                    'SN590', 'Allergic reaction to bee sting',
                    'SN592', 'Allergic reaction to wasp sting',
                    'SN531', 'Latex allergy',
                    '9NlX.', 'Seen by clinic allergy - serv',
                    '8Hld.', 'Refer to clinical allergy serv',
                    '9b9W.', 'Allergy - specialty ',
                    '9bJ5.', 'Trans-degrade non-drug allergy',
                    'SN5A.', 'Oral allergy syndrome',
                    'SN532', 'Allergic reaction tattoo ink',
                    'ZVu6s', '[X]Pers hist allergy cosmetic',
                    '8HVK0', 'Private ref allergy specialist',
                    'SN5B.', 'Allergy to stainless steel'))

# Do not add:
# XE0Xl	Common cold
# XM00h	Rhinorrhoea
# XM1Md	Catarrh
# X30Kv	Henoch-Schonlein nephritis
# X303z	Hypersensitivity colitis
# X70Qs	Napkin candidiasis
# Xa0O2	Suxamethonium apnoea
# XE0YR	Extrinsic asthma without status asthmaticus

################################################################################
# combine urticaria and angioedema and remove duplicates
angioedema_urticaria <- bind_rows(angioedema, urticaria) %>%
  distinct(read_code, .keep_all = TRUE)

################################################################################
# food/drug/venom allergy
# if someone has a code corresponding to angioedema, urticaria or anaphylaxis 
# due to food/drug/venom allergy, include this under the corresponding list

food_indicator <- str_c(c('food', 'peanut', 'seafood', 'egg', 'cow'), collapse = '|')

drug_indicator <- str_c(c('drug', 'nsaid', 'aspirin', 'penicillin'), collapse = '|')

venom_indicator <- str_c(c('venom', 'bee', 'wasp', 'insect'), collapse = '|')

food <- food %>%
  bind_rows(filter(anaphylaxis, 
                   str_detect(read_term, 
                              regex(food_indicator, 
                                    ignore_case = TRUE))),
            filter(angioedema_urticaria, 
                   str_detect(read_term, 
                              regex(food_indicator, 
                                    ignore_case = TRUE))))


drug_allergy <- drug_allergy %>%
  bind_rows(filter(anaphylaxis, 
                   str_detect(read_term, 
                              regex(drug_indicator, 
                                    ignore_case = TRUE))),
            filter(angioedema_urticaria, 
                   str_detect(read_term, 
                              regex(drug_indicator, 
                                    ignore_case = TRUE))))

other <- other %>%
  bind_rows(filter(anaphylaxis, 
                   str_detect(read_term, 
                              regex(venom_indicator, 
                                    ignore_case = TRUE))),
            filter(angioedema_urticaria, 
                   str_detect(read_term, 
                              regex(venom_indicator, 
                                    ignore_case = TRUE))))

################################################################################
allergies <- bind_rows(
  mutate(anaphylaxis, cat2 = 'anaphylaxis'),
  mutate(angioedema_urticaria, cat2 = 'angioedema or urticaria'),
  mutate(conjunctivitis, cat2 = 'conjunctivitis'),
  mutate(drug_allergy, cat2 = 'drug allergy'),
  mutate(eczema, cat2 = 'eczema'),
  mutate(food, cat2 = 'food allergy'),
  mutate(other, cat2 = 'other'),
  mutate(rhinitis, cat2 = 'rhinitis')) %>%
  filter(!is.na(read_code)) %>%
  mutate_at('cat2', list(~ case_when(read_code %in% c('H1711', 'H1710') ~ 'other',
                                    TRUE ~ .))) %>%
  distinct(cat2, read_code, .keep_all = TRUE) %>%
  # remove based on Susannah's feedback
  filter(!(read_code %in% nonallergy_codes)) %>%
  mutate_at('cat2', list(~ if_else(. %in% 'other', 'other allergy', .)))

# make sure all Read codes are 5 characters and fix if not
allergies %>%
  filter(str_length(read_code)<5)
allergies <- allergies %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

write_csv(allergies, 
          path = file.path(opcrd_analysis_path, 'allergic_conditions.csv'))
write_csv(allergies %>%
            arrange(cat2, read_code, read_term) %>%
            select(`Allergic condition` = cat2,
                   `Read code` = read_code, 
                   `Term` = read_term), 
          path = 'lists_out/allergic_conditions.csv')
