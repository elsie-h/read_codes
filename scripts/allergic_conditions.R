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

# Function for joining V2 & CTV3 Read code tables
# read_code_v2 = in version 2 list
# read_code_v3.2 = in version 3 list but in version 2 terminology
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
    distinct()
}

################################################################################
# Anaphylaxis
anaphylaxis_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Anaphylaxis') %>%
  select(read_code_v2 = read_code, read_term)

#### Codes for anaphylaxis from manual search of CTV3 ####
anaphylaxis_v3 <-  read_csv("lists_in/OpenSafely/elsie-horne-anaphylaxis-2020-11-09T13-56-17.csv")
  
# join to check how well the two lists correspond
anaphylaxis <- join_read(data_v2 = anaphylaxis_v2,
                         data_v3 = anaphylaxis_v3) %>%
  filter(!(read_term %in% 'Allergic urticaria'))

################################################################################
# Angioedema
angioedema_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Angioedema') %>%
  select(read_code_v2 = read_code, read_term)

#### Codes for Angioedema from manual search of CTV3 ####
angioedema_v3 <-  read_csv("lists_in/OpenSafely/elsie-horne-angioedema-2020-11-09T13-43-48.csv")

# join to check how well the two lists correspond
angioedema <- join_read(data_v2 = angioedema_v2,
                         data_v3 = angioedema_v3)

################################################################################
# Conjunctivitis
conjunctivitis_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Conjunctivitis') %>%
  select(read_code_v2 = read_code, read_term)

#### Codes for allergic conjunctivits from manual search of CTV3 ####
conjunctivitis_v3 <-  read_csv("lists_in/OpenSafely/elsie-horne-conjunctivitis-2020-11-09T13-18-07.csv")

# join to check how well the two lists correspond
conjunctivitis <- join_read(data_v2 = conjunctivitis_v2,
                        data_v3 = conjunctivitis_v3)

################################################################################
# Drug allergy
drug_allergy_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Drug allergy') %>%
  select(read_code_v2 = read_code, read_term)

drug_allergy_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-drug-allergy-2020-11-05T08-48-21.csv") 

# join to check how well the two lists correspond
drug_allergy <- join_read(data_v2 = drug_allergy_v2,
                            data_v3 = drug_allergy_v3)

################################################################################
# Eczema
eczema_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Eczema') %>%
  select(read_code_v2 = read_code, read_term)

eczema_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-eczema-allergy-2020-11-09T13-28-09.csv") 

# join to check how well the two lists correspond
eczema <- join_read(data_v2 = eczema_v2,
                          data_v3 = eczema_v3)

################################################################################

# Food allergy
food_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Food allergy') %>%
  select(read_code_v2 = read_code, read_term)

food_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-food-allergy-2020-11-09T16-28-48.csv") 

# join to check how well the two lists correspond
food <- join_read(data_v2 = food_v2,
                    data_v3 = food_v3)

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

rhinitis_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-rhinitis-allergic-2020-11-09T16-35-40.csv") 

# join to check how well the two lists correspond
rhinitis <- join_read(data_v2 = rhinitis_v2,
                  data_v3 = rhinitis_v3)

################################################################################
# Urticaria
urticaria_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Urticaria') %>%
  select(read_code_v2 = read_code, read_term)

urticaria_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-urticaria-2020-11-09T13-40-47.csv") 

# join to check how well the two lists correspond
urticaria <- join_read(data_v2 = urticaria_v2,
                      data_v3 = urticaria_v3)

################################################################################
# Venom
venom_allergy <- mukherjee %>%
  filter(`Disease Area` %in% 'Venom allergy') %>%
  select(read_code_v2 = read_code, read_term)

# my OpenSafely search for 'venom allergy' did not identify any codes 

################################################################################
# Other
other_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Other',
         # None of the read codes under 'Test' or 'Other' indicate that an allergy is present, only that it has been tested
         !(`Type of code` %in% c('Test',
                                 'Other')),
         !(read_term %in% c('Hypersens. skin test done',
                       'Hypersens. skin test: no react')))%>%
  select(read_code_v2 = read_code, read_term)

other_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-other-allergy-2020-11-09T17-09-26.csv") 

# remove any that have appeared in other code lists
all <- bind_rows(anaphylaxis,
                 angioedema,
                 conjunctivitis,
                 # drugs,
                 drug_allergy,
                 eczema,
                 food,
                 rhinitis,
                 urticaria) %>%
  select(-read_term) %>% 
  pivot_longer(cols = 1:3) %>%
  distinct(value) %>%
  unlist() %>%
  unname()

# join to check how well the two lists correspond
other <- join_read(data_v2 = filter(other_v2, !(read_code_v2 %in% all)),
                       data_v3 = filter(other_v3, !(id %in% all))) 

################################################################################
# combine urticaria and angioedema and remove duplicates
angioedema_urticaria <- bind_rows(angioedema, urticaria)

angioedema_urticaria %>% 
  group_by(read_term) %>%
  count() %>%
  ungroup() %>%
  filter(n>1) %>%
  select(read_term) %>%
  left_join(angioedema_urticaria, by = 'read_term') %>%
  print(n=100)
# fine to just remove duplicated with distinct()

angioedema_urticaria <- angioedema_urticaria %>%
  distinct()

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
                                    ignore_case = TRUE)))
            )


drug_allergy <- drug_allergy %>%
  bind_rows(filter(anaphylaxis, 
                   str_detect(read_term, 
                              regex(drug_indicator, 
                                    ignore_case = TRUE))),
            filter(angioedema_urticaria, 
                   str_detect(read_term, 
                              regex(drug_indicator, 
                                    ignore_case = TRUE)))
  )

venom_allergy <- venom_allergy %>%
  bind_rows(filter(anaphylaxis, 
                   str_detect(read_term, 
                              regex(venom_indicator, 
                                    ignore_case = TRUE))),
            filter(angioedema_urticaria, 
                   str_detect(read_term, 
                              regex(venom_indicator, 
                                    ignore_case = TRUE)))
            # ,filter(drugs, 
            #        str_detect(read_term, 
            #                   regex(venom_indicator, 
            #                         ignore_case = TRUE)))
  )

################################################################################
allergies <- bind_rows(
  mutate(anaphylaxis, cat2 = 'anaphylaxis'),
  mutate(angioedema_urticaria, cat2 = 'angioedema or urticaria'),
  mutate(conjunctivitis, cat2 = 'conjunctivitis'),
  mutate(drug_allergy, cat2 = 'drug allergy'),
  # mutate(drugs, cat2 = 'drugs'),
  mutate(eczema, cat2 = 'eczema'),
  mutate(food, cat2 = 'food allergy'),
  mutate(other, cat2 = 'other'),
  mutate(rhinitis, cat2 = 'rhinitis'),
  mutate(venom_allergy, cat2 = 'venom allergy')) %>%
  select(cat2, everything()) %>%
  pivot_longer(cols = starts_with('read_code')) %>%
  rename(read_code = value) %>%
  select(-name) %>%
  filter(!is.na(read_code)) %>%
  mutate_at('cat2', list(~ case_when(read_code %in% c('H1711', 'H1710')
                                     ~ 'other',
                                     (. %in% 'other') &
                                       str_detect(read_term, 
                                                regex('bee|wasp|insect|venom', 
                                                      ignore_case = T))
                                     ~ 'venom allergy',
                                    TRUE ~ .))) %>%
  distinct(cat2, read_code, .keep_all = TRUE) %>%
  # remove based on Susannah's feedback
  filter(!(read_code %in% c('A5320', 'A5441', 'F4D33', 'F4D4.',
                            'F4D6.', 'FyuB4', 'FyuB5', 'G831.',
                            'M10..', 'M103.', 'M10z.', 'X00YQ',
                            'X00YR', 'X00YS', 'X00YT', 'X00YU',
                            'X00YV', 'X00YW', 'X506H', 'X506j',
                            'X00kv', 'X00kz', 'X00l0', 'X00l1',
                            'X00l2', 'XE2RT', 'Xa2dV', 'XaNkV',
                            'D2101', 'M1100', 'SN520'))) %>%
  mutate_at('cat2', list(~ if_else(. %in% 'other', 'other allergy', .)))

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
