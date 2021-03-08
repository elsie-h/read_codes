#### emergency ####

# Description: asthma-related emergency admission or visit to A&E
# cat1: emergency
# cat2: NA
# score: NA

source('setup.R')

emergency <- read_delim("lists_in/Elsie/cl_hospitalisation_elsie",
                              ",",
                              escape_double = FALSE, 
                              trim_ws = TRUE) %>%
  add_row(read_code = '663m.',
          read_term = 'Asthma accident and emergency attendance since last visit',
          cat2 = NA_character_) %>%
  mutate(cat1 = 'emergency',
         score = NA_real_) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

# check for duplicates in read_code
emergency$read_code[duplicated(emergency$read_code)]

write_csv(emergency, 
          path = file.path(opcrd_analysis_path, 'emergency.csv'))
write_csv(emergency %>%
            select(`Read code` = read_code,
                   Term = read_term),
          path = 'lists_out/emergency.csv')

# table for appendix
emergency %>%
  arrange(read_term) %>%
  select(`Read code` = read_code, 
         `Term` = read_term) %>%
  xtable(caption = 'Read codes from emergency asthma events (see \\nameref{cha:ehr:methods:pre:emergency} for methods)',
         label = 'tab:app:rc_emergency',
         align=c('l',"p{2cm}","p{10cm}")) %>%
  print_xtable_multi(filename = 'emergency')
