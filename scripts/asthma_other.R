#### asthma_other ####

# Description: read codes corresponding to asthma that are not included in:
# asthma_review, RCGP3Q, asthma_plan, exacerbation, emergency
# cat1: asthma_other
# cat2: NA; asthma_control_step; trigger; health education
# score: NA

source('setup.R')

asthma_V3 <- read_csv("lists_in/OpenSafely/opensafely-asthma-diagnosis-2020-04-15.csv") %>% 
  filter(CTV3Source == 'CTV3Map_Code_Only' |
           CTV3Source == 'CTV3Map_Code_And_Term') %>%
  select(read_code = CTV3ID, read_term = CTV3PreferredTermDesc) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  mutate(cat1 = 'asthma_other',
         cat2 = NA_character_) %>%
  distinct()

CPRD_asthma <- read_cprd('asthma') %>% 
  mutate(cat1 = 'asthma_other',
         cat2 = NA_character_)

asthma_other <- bind_rows(asthma_V3, CPRD_asthma) %>%
  add_row(read_code = '38DL.', 
          read_term = 'Asthma control test') %>%
  add_row(read_code =  '679J.', 
          read_term = 'Health education - asthma') %>%
  add_row(read_code = '1789.',
          read_term = 'Asthma trigger - respiratory infection') %>%
  add_row(read_code = '1788.',
          read_term = 'Asthma trigger - cold air') %>%
  add_row(read_code = '178B.',
          read_term = 'Asthma trigger - exercise') %>%
  add_row(read_code = 'XaIww',
          read_term = 'Asthma trigger') %>%
  add_row(read_code = '178..',
          read_term = 'Asthma trigger') %>%
  add_row(read_code = '1781.',
          read_term = 'Asthma trigger - pollen') %>%
  add_row(read_code = '1787.',
          read_term = 'Asthma trigger - seasonal') %>%
  add_row(read_code = '1786.',
          read_term = 'Asthma trigger - animals') %>%
  add_row(read_code = '178A.',
          read_term = 'Asthma trigger - airborne dust') %>%
  add_row(read_code = 'XaIer',
          read_term = 'Asthma follow-up') %>%
  add_row(read_code = '1785.',	
          read_term = 'Asthma trigger - damp') %>%
  add_row(read_code = '1784.',
          read_term = 'Asthma trigger - emotion') %>%
  add_row(read_code = '663j.',
          read_term = 'Asthma - currently active') %>%
  add_row(read_code = '1783.',
          read_term = 'Asthma trigger - warm air') %>%
  add_row(read_code = '1782.',
          read_term = 'Asthma trigger - tobacco smoke') %>%
  anti_join(readRDS('lists_out/asthma_review.RDS'), by = 'read_code') %>%
  anti_join(readRDS('lists_out/asthma_plan.RDS'), by = 'read_code') %>%
  anti_join(readRDS('lists_out/RCGP3Q.RDS'), by = 'read_code') %>%
  anti_join(readRDS('lists_out/exacerbation.RDS'), by = 'read_code') %>%
  anti_join(readRDS('lists_out/emergency.RDS'), by = 'read_code') %>%
  # get rid of 'asthma resolved'
  filter(!(read_term %in% 'Asthma resolved')) %>%
  # tidy up duplicate read_code with different read_term
  mutate_at('read_term', list(~ case_when(read_code %in% 'H330.' ~ 'Extrinsic (atopic) asthma',
                                          read_code %in% 'H3300' ~ 'Extrinsic asthma without status asthmaticus',
                                          read_code %in% 'H331.' ~ 'Intrinsic asthma',
                                          read_code %in% 'H33zz' ~ 'Asthma NOS',
                                          . %in% 'Exercise induced asthma' ~ 'Exercise-induced asthma',
                                          . %in% 'Aspirin induced asthma' ~ 'Aspirin-induced asthma',
                                          . %in% 'Late onset asthma' ~ 'Late-onset asthma',
                                          TRUE~ .))) %>%
  mutate_at('cat2', list(~  case_when(str_detect(read_term, 'Asthma control step') ~ 'asthma_control_step',
                                      str_detect(read_term, 'trigger') ~ 'trigger',
                                      str_detect(read_term, 'Health education') ~ 'health_education',
                                      TRUE ~ NA_character_))) %>%
  mutate(cat1 = 'asthma_other',
         score = NA_real_) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

# check for duplicates in read_code
asthma_other$read_code[duplicated(asthma_other$read_code)]

# sort duplicates
asthma_other <- asthma_other %>%
  mutate_at('read_term', list(~ case_when(read_code %in% 'H33z.' ~'Asthma unspecified',
                                          read_code %in% 'H33..' ~ 'Asthma',
                                          TRUE ~ .))) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

saveRDS(asthma_other, file = 'lists_out/asthma_other.RDS', compress = FALSE)


