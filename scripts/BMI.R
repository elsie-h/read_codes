#### BMI ####

# Description: BMI
# cat1: bmi
# cat2: NA
# score: NA

source('setup.R')

bmi <- read_csv("lists_in/Elsie/bmi_codes_elsie.csv") %>%
  select(read_code, read_term = desc, cat2 = cat) %>%
  mutate_at('read_code', list(~ str_extract(., pattern = '^.{5}'))) %>%
  mutate(cat1 = 'bmi') %>%
  distinct()

# make sure all Read codes are 5 characters and fix if not
bmi %>%
  filter(str_length(read_code)<5)
bmi <- bmi %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check for duplicates in read_code
bmi$read_code[duplicated(bmi$read_code)]

# check mapping
# map V2 -> CTV3
bmi %>% map_V2_CTV3()
# map CTV3 -> V2
bmi %>% map_CTV3_V2()

write_csv(bmi, 
          path = file.path(opcrd_analysis_path, 'BMI.csv'))
write_csv(bmi %>%
            mutate_at('cat2', list(~ if_else(is.na(cat2), 'numeric', .))) %>%
            mutate_at('cat2', list(~ case_when(. %in% '25+' ~ '25-29',
                                               . %in% '30+' ~ '30-34',
                                               TRUE ~ .))) %>%
            mutate_at('cat2', factor,
                      levels = c('numeric', '<20', '20-24', '25-29', '30-34', '35-39', '40+')) %>%
            arrange(cat2, read_code, read_term) %>%
            select(`Read code` = read_code,
                   Term = read_term,
                   Category = cat2), 
          path = 'lists_out/BMI.csv')
