#### ethnicity ####

# Description: ethnicity codes
# cat1: ethnicity
# cat2: NA
# score: NA

source('setup.R')

ethnicity <- read_csv("lists_in/Elsie/rc_ethnicity_final.csv") %>%
  rename(cat2 = cat_final) %>%
  mutate(cat1 = 'ethnicity')

# make sure all Read codes are 5 characters and fix if not
ethnicity %>%
  filter(str_length(read_code)<5)
ethnicity <- ethnicity %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check mapping
# map V2 -> CTV3
ethnicity %>% map_V2_CTV3()
# map CTV3 -> V2
ethnicity %>% map_CTV3_V2()

ethnicity <- ethnicity %>%
  add_row(read_code = '9T2..', read_term = 'Traveller - gypsy, ', cat1 = 'ethnicity', cat2 = 'White')

write_csv(ethnicity, 
          path = file.path(opcrd_analysis_path, 'ethnicity.csv'))
write_csv(ethnicity %>%
            arrange(cat2, read_code, read_term) %>%
            select(Category = cat2,
                   `Read code` = read_code,
                   Term = read_term),
          path = 'lists_out/ethnicity.csv')
