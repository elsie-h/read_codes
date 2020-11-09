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
    select(read_term, read_code_v2, read_code_v3.2, read_code_v3)
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
  select(read_code_v2 = read_code, read_term)

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
#  Drugs

# the Mukherjee paper did not include drugs for allergies, 
# will have to idenfity v2 list manually

drugs <- read_csv("lists_in/OpenSafely/elsie-horne-allergy-drug-2020-11-09T17-13-02.csv") %>%
  rename(read_code_v3 = id, read_term = term)

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
                 drugs,
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
                                    ignore_case = TRUE))),
            filter(drugs, 
                   str_detect(read_term, 
                              regex(venom_indicator, 
                                    ignore_case = TRUE)))
  )
