#### infection  ####
# Description: infection
# cat1: infection
# cat2: cold_cough, urti, lrti, antibiotic
# score: NA

source('setup.R')

lrti_rms <- read_delim("lists_in/RMS/cl_lrti_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(cat2 = 'LRTI')

# lrti_cc <- read_csv("lists_in/ClinicalCodes/res70-lrti.csv") 
urti_cc <- read_csv("lists_in/ClinicalCodes/res70-urti.csv") 
cold_cc <- read_csv("lists_in/ClinicalCodes/res70-coughcold.csv") 
throat_cc <- read_csv("lists_in/ClinicalCodes/res70-sore-throat.csv") 
throat_bacterial_cc <- read_csv("lists_in/ClinicalCodes/res70-sore-throat_bacterial.csv") 

influenza_v2 <- tribble(~read_code, ~read_term,
                        'H27..',	'Influenza',
                        'H270.',	'Influenza with pneumonia',
                        'H2700',	'Influenza with bronchopneumonia',
                        'H2701',	'Influenza with pneumonia, influenza virus identified',
                        'H270z',	'Influenza with pneumonia NOS',
                        'H271.',	'Influenza with other respiratory manifestation',
                        'H2710',	'Influenza with laryngitis',
                        'H2711',	'Influenza with pharyngitis',
                        'H271z',	'Influenza with respiratory manifestations NOS',
                        'H27y.',	'Influenza with other manifestations',
                        'H27y0',	'Influenza with encephalopathy',
                        'H27y1',	'Influenza with gastrointestinal tract involvement',
                        'H27yz',	'Influenza with other manifestations NOS',
                        'H27z.',	'Influenza NOS',
                        'H29..',	'Avian influenza',
                        'H27z.',	'Influenza like illness',
                        '16L..',	'Influenza-like symptoms'
)

infections_v2 <- bind_rows(
  # lrti_cc %>% mutate(cat2 = 'LRTI'),
  urti_cc %>% mutate(cat2 = 'URTI'),
  cold_cc %>% mutate(cat2 = 'URTI'),
  throat_cc %>% mutate(cat2 = 'URTI'),
  throat_bacterial_cc %>% 
    mutate(cat2 = 'URTI')) %>%
  filter(coding_system %in% 'Read') %>%
  select(read_code = code, read_term = description, cat2) %>%
  distinct() %>%
  arrange(cat2, read_code) %>%
  # first 5 characters of Read code
  mutate_at('read_code', list(~ str_pad(str_trunc(., 
                                                  width = 5, 
                                                  side = 'right', 
                                                  ellipsis = ''), 
                                        width = 5, 
                                        side = 'right', 
                                        pad = '.'))) %>%
  distinct(read_code, cat2, .keep_all = TRUE) %>%
  mutate(cat1 = 'infection') %>%
  bind_rows(mutate(influenza_v2, cat1 = 'infection', cat2 = 'influenza'))


cold_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-common-cold-2020-11-18T14-55-04.csv")
cough_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-cough-2020-11-18T15-03-44.csv")
lrti_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-lrti-2020-11-23T08-05-18.csv")
urti_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-urti-2-2020-11-23T08-06-45.csv")
influenza_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-influenza-2020-11-19T16-17-53.csv")
sorethroat_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-sore-throat-2020-11-19T17-41-44.csv")
sorethroat_other_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-sore-throat-other-2020-11-19T17-49-18.csv")

infections_v3 <- bind_rows(mutate(cold_v3, cat2 = 'URTI'),
                           mutate(cough_v3, cat2 = 'URTI'),
                           mutate(lrti_v3, cat2 = 'LRTI'),
                           mutate(urti_v3, cat2 = 'URTI'),
                           mutate(influenza_v3, cat2 = 'influenza'),
                           mutate(sorethroat_v3, cat2 = 'URTI'),
                           mutate(sorethroat_other_v3, cat2 = 'URTI')) %>%
  select(read_code = id, read_term = term, cat2) %>%
  distinct() %>%
  mutate(cat1 = 'infection')

infections <- bind_rows(
  infections_v2, 
  infections_v3,
  lrti_rms) %>% 
  filter(!(read_code %in% c('X73lO'))) %>%
  distinct()

##### Following Susannah's feedback #####
infections <- infections %>%
  select(-cat1) %>%
  mutate_at('cat2', list(~ case_when(read_code %in% 'H0404' ~ 'URTI',
                                     read_code %in% 'H243.' ~ 'LRTI',
                                     (cat2 %in% 'influenza') &
                                       (read_code %in% c('A3841',
                                                         'A3B5.',
                                                         'Ayu3U',
                                                         'AyuKC'))
                                     ~ 'exclude',
                                     (cat2 %in% 'URTI') &
                                       (read_code %in% c('Ayu55',
                                                         'H3101',
                                                         'X00lM',
                                                         'X70Pk',
                                                         'X76I0',
                                                         'X76I1',
                                                         'Xa0lc',
                                                         'Xa7mD',
                                                         'XaLCS',
                                                         'XaYYO',
                                                         'XE0S4',
                                                         'XE0Y9',
                                                         'XE0Z7',
                                                         'XM0rr',
                                                         'XM1QR'))
                                     ~ 'exclude',
                                     TRUE ~ .))) %>%
  # Whooping cough in both URTI & LRTI (suggested by Susannah and also consistent)
  # with the V2 URTI list & LRTI lists
  add_row(read_code = 'A33y.',
          read_term = 'Whooping cough - other specified organism',
          cat2 = 'LRTI') %>%
  add_row(read_code = 'A33yz',
          read_term = 'Other whooping cough NOS',
          cat2 = 'LRTI') %>%
  add_row(read_code = 'A33z.',
          read_term = 'Whooping cough NOS',
          cat2 = 'LRTI') %>%
  add_row(read_code = 'Ayu39',
          read_term = '[X]Whooping cough due to other Bordetella species',
          cat2 = 'LRTI') %>%
  add_row(read_code = 'Ayu3A',
          read_term = '[X]Whooping cough, unspecified',
          cat2 = 'LRTI') %>%
  add_row(read_code = 'Y9896',
          read_term = 'Whooping cough - pertussis',
          cat2 = 'LRTI') %>%
  add_row(read_code = 'Y9897',
          read_term = 'Whooping cough pneumonia',
          cat2 = 'LRTI') %>%
  filter(!(cat2 %in% 'exclude')) %>%
  mutate(cat1 = 'infections')

write_csv(infections, path = 'lists_out/infections.csv')

# latex tables
infections_list <- infections %>%
  group_split(cat2)

infections_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  caption <- str_c('Read codes for ', category, ' (return to \\nameref{cha:ehr:methods:pre:infections} methods)')
  label <- str_c('tab:app:rc_', category_)
  
  table <- .data %>%
    arrange(read_term) %>%
    select(`Read code` = read_code, 
           `Term` = read_term) %>%
    xtable(caption = caption,
           label = label,
           align=c('l',"p{2cm}","p{10cm}")) %>%
    print_xtable_multi(filename = category_)
  
}

lapply(infections_list, infections_latex_table)
