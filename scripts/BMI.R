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

# table for appendix
bmi_table <- bmi %>%
  filter(!is.na(cat2)) %>%
  mutate_at('cat2', list(~ case_when(. %in% '25+' ~ '25-29',
                                     . %in% '30+' ~ '30-34',
                                     TRUE ~ .))) %>%
  arrange(cat2, read_term) %>%
  select(Category = cat2,
         `Read code` = read_code, 
         Term = read_term) %>%
  mutate(rn = row_number()) %>%
  group_by(Category) %>%
  mutate(order = mean(rn)) %>%
  ungroup() %>%
  select(-rn)

# split on value column
split_list <- bmi_table %>% 
  group_split(order)
new_col <- character()
# loop for removing duplicate labels
for (i in seq_along(unique(bmi_table$Category))) {
  tmp <- c(split_list[[i]]$Category[1], 
           rep('', length.out = length(split_list[[i]]$Category[-1])))
  new_col <- c(new_col, tmp)
}

# produce table
bmi_table <- bmi_table %>%
  mutate(Category = new_col) %>%
  arrange(order, Term) %>%
  select(-order)

bmi_table %>%
  xtable(caption = 'Read codes for BMI categories (see \\nameref{cha:ehr:methods:pre:bmi} for methods)',
         label = 'tab:app:rc_bmi',
         align=c('l',"p{2cm}","p{2cm}","p{8cm}")) %>%
  print_xtable_multi(filename = 'bmi')
