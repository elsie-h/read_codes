#### asthma_plan ####

# Description: codes suggesting the patient has an asthma action plan
# cat1: asthma_plan
# cat2: NA
# score: NA

source('setup.R')

asthma_plan <- read_delim("lists_in/Elsie/cl_asthma_plan_elsie",
                          ",",
                          escape_double = FALSE, 
                          trim_ws = TRUE) %>%
  add_row(read_code = '8CMA0',
          read_term = 'Patient has a written asthma personal action plan') %>%
  add_row(read_code = '8CR0.',
          read_term = 'Asthma clinical management plan') %>%
  add_row(read_code = '661M1',
          read_term = 'Asthma self-management plan agreed') %>%
  add_row(read_code = '66Y5.',
          read_term = 'Change in asthma management plan') %>%
  # additions based on mapping
  add_row(read_code = '66Y9.',
          read_term = 'Step up change in asthma management plan') %>%
  add_row(read_code = '66YA.',
         read_term = 'Step down change in asthma management plan') %>%
  add_row(read_code = '661N1',
          read_term = 'Asthma self-management plan review') %>%
  mutate(cat1 = 'asthma_plan',
         score = NA_real_) %>%
  mutate_at('cat2', as.character)  %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

# check mapping
# map V2 -> CTV3
asthma_plan %>% map_V2_CTV3()
# map CTV3 -> V2
asthma_plan %>% map_CTV3_V2()

# make sure all Read codes are 5 characters and fix if not
asthma_plan %>%
  filter(str_length(read_code)<5)
asthma_plan <- asthma_plan %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

# check for duplicates in read_code
asthma_plan$read_code[duplicated(asthma_plan$read_code)]

write_csv(asthma_plan, 
          path = file.path(opcrd_analysis_path, 'asthma_plan.csv'))
write_csv(asthma_plan %>%
            arrange(read_code, read_term) %>%
            select(`Read code` = read_code,
                   Term = read_term),
          path = 'lists_out/asthma_plan.csv')
