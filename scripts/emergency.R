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

# make sure all Read codes are 5 characters and fix if not
emergency %>%
  filter(str_length(read_code)<5)
emergency <- emergency %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check for duplicates in read_code
emergency$read_code[duplicated(emergency$read_code)]

# check mapping
# map V2 -> CTV3
emergency %>% map_V2_CTV3()
# map CTV3 -> V2
emergency %>% map_CTV3_V2()

write_csv(emergency, 
          path = file.path(opcrd_analysis_path, 'emergency.csv'))
write_csv(emergency %>%
            select(`Read code` = read_code,
                   Term = read_term),
          path = 'lists_out/emergency.csv')
