
source('setup.R')

#### Comorbidities ####

# I want code lists for the following comorbidities:
# Eczema
# Rhinitis
# Anxiety
# Depression
# GORD
# Nasal polyps
# Osteoporosis
# Anaphylaxis

comorbs <- c('rhinitis',
             'anxiety',
             'depression',
             'dermatitis',
             'GORD',
             'nasal_polyp',
             'osteoporosis')

#### from CPRD codes: ####
comorbs_data <- bind_rows(lapply(comorbs, 
                                 function(x) 
                                   read_cprd(x) %>% 
                                   mutate(cat1 = 'comorbidity',
                                          cat2 = x,
                                          version = 2))) 

#### add RMS codes if available ####
# anxiety and depression grouped together in RMS codes, so I categorise them here
anxiety_depression_rms <- read_delim("lists_in/RMS/cl_anxiety_depression_dx_rms.txt",
                                     "|", 
                                     escape_double = FALSE, 
                                     trim_ws = TRUE)  %>%
  mutate(cat1 = 'comorbidity') %>%
  left_join(select(comorbs_data, read_term, cat2), by = 'read_term') %>%
  filter(!(is.na(cat2) & 
             (str_detect(read_term, regex('bipolar', ignore_case = T))|
                str_detect(read_term, regex('manic', ignore_case = T))))) %>%
  mutate_at('cat2', list(~ case_when(!is.na(.) ~ .,
                                     str_detect(read_term, regex('anxiety', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('anxious', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('fear', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('phobia', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('panic', ignore_case = T)) ~ 'anxiety',
                                     str_detect(read_term, regex('depress', ignore_case = T)) ~ 'depression',
                                     TRUE ~ NA_character_))) %>%
  filter(!is.na(cat2))

# nasal polyps
nasal_polyps_rms <- read_delim("lists_in/RMS/cl_nasal_polyps_rms.txt", 
                               "|", 
                               escape_double = FALSE, 
                               trim_ws = TRUE)  %>%
  mutate(cat1 = 'comorbidity', cat2 = 'nasal_polyp')

# rhinitis
rhinitis_rms <- read_delim("lists_in/RMS/cl_rhinitis_dx_rms.txt", 
                           "|", 
                           escape_double = FALSE, 
                           trim_ws = TRUE) %>%
  mutate(cat1 = 'comorbidity', cat2 = 'rhinitis') 

# GERD
gerd_rms <- read_delim("lists_in/RMS/cl_gerd_dx_rms.txt", 
                       "|", 
                       escape_double = FALSE,
                       trim_ws = TRUE) %>%
  mutate(cat1 = 'comorbidity', cat2 = 'GORD')

rhinitis <- comorbs_data %>%
  filter(cat2 %in% 'rhinitis') %>%
  full_join(rhinitis_rms, by = c('read_term')) %>%
  select(read_code_CPRD = read_code.x,
         read_code_RMS = read_code.y,
         read_term) %>%
  filter(!(read_term %in% 'FH: Hay fever'))


comorbs_all <- bind_rows(comorbs_data,
                         anxiety_depression_rms,
                         nasal_polyps_rms,
                         rhinitis_rms,
                         gerd_rms) %>%
  distinct()

#### from Mukherjee 2016 allergy paper ####
# https://onlinelibrary.wiley.com/doi/abs/10.1111/all.12928
mukherjee <- readxl::read_excel("lists_in/Mukherjee2016/Mukherjee2016_supinfo.xlsx", 
                          skip = 1)

anaphylaxis <- mukherjee %>% filter(`Disease Area` %in% 'Anaphylaxis')

tmp %>% filter(`Disease Area` %in% 'Rhinitis')