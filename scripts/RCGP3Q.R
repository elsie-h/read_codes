#### RCGP3Q ####

# Description: codes suggesting the patient was asked (at least one of) the Royal College of GPs 3 Questions
# cat1: asthma_review
# cat2: general; sleeping; day; activities;
# score: NA; 0; 1

source('setup.R')

rcgp3q <- read_delim("lists_in/Elsie/rcgp3q_elsie",
                     ",",
                     escape_double = FALSE, 
                     trim_ws = TRUE) %>%
  add_row(read_code = '66Ys.',
          read_term = 'Asthma never causes night symptoms',
          cat1 = 'RCGP3Q',
          cat2 = 'sleeping',
          score = 0) %>%
  add_row(read_code = '388t.',	
          read_term = 'Royal College of Physicians asthma assessment',
          cat1 = 'RCGP3Q',
          cat2 = 'general') %>%
  add_row(read_code = '66Yp.',
          read_term = 'Asthma review using Royal College of Physicians three questions',
          cat1 = 'RCGP3Q',
          cat2 = 'general') %>%
  add_row(read_code = '663P2',	
          read_term = 'Asthma limits activities most days',
          cat1 = 'RCGP3Q',
          cat2 = 'activities',
          score = 1) %>%
  add_row(read_code = '663P1',
          read_term = 'Asthma limits activities 1 to 2 times per week',
          cat1 = 'RCGP3Q',
          cat2 = 'activities',
          score = 1) %>%
  add_row(read_code = '663P0',	
          read_term = 'Asthma limits activities 1 to 2 times per month',
          cat1 = 'RCGP3Q',
          cat2 = 'activities',
          score = 1) %>%
  add_row(read_code = '663x.',
          read_term = 'Asthma limits walking on the flat',
          cat1 = 'RCGP3Q',
          cat2 = 'activities',
          score = 1) %>%
  select(read_code, read_term, cat1, cat2, score) %>%
  distinct()

# check for duplicates in read_code
rcgp3q$read_code[duplicated(rcgp3q$read_code)]

saveRDS(rcgp3q, file = 'lists_out/RCGP3Q.RDS', compress = FALSE)
