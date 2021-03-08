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
  mutate(cat1 = 'asthma_plan',
         score = NA_real_) %>%
  mutate_at('cat2', as.character)  %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

# check for duplicates in read_code
asthma_plan$read_code[duplicated(asthma_plan$read_code)]

write_csv(asthma_plan, 
          path = file.path(opcrd_analysis_path, 'asthma_plan.csv'))
write_csv(asthma_plan %>%
            arrange(read_code, read_term) %>%
            select(`Read code` = read_code,
                   Term = read_term),
          path = 'lists_out/asthma_plan.csv')

# table for appendix
asthma_plan %>%
  arrange(read_term) %>%
  select(`Read code` = read_code, 
         `Term` = read_term) %>%
  xtable(caption = 'Read codes from emergency asthma management plan (see \\nameref{cha:ehr:methods:pre:asthma_plan} for methods)',
         label = 'tab:app:rc_asthma_plan',
         align=c('l',"p{2cm}","p{10cm}")) %>%
  print_xtable_multi(filename = 'asthma_plan')

