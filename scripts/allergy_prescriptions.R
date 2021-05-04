#  allergy drugs

source('setup.R')

# version 2 code lists
# all starting l8...
nasal_allergy_drugs_v2 <- read_delim("lists_in/Elsie/nasal_allergy_drugs_v2", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

# subset of those starting k6...
eye_allergy_drugs_v2 <- read_delim("lists_in/Elsie/eye_allergy_drugs_v2", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

# all starting c8...
oral_antihistamines_v2 <- read_delim("lists_in/Elsie/oral_antihistamines_v2", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

# all starting m34..
topical_antihistamines_v2 <- read_delim("lists_in/Elsie/topical_antihistamines_v2", 
                                "\t", escape_double = FALSE,
                                trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

# better to be over-inclusive at this stage
# can talk to clinician/consult the BNF and trim down based on read term
# from googling, I don't think treatments in the hyposensitisation category are prescribed any more

# these lists are over-inclusive and need to be trimmed down
antihistamines_v3 <-  read_csv("lists_in/Elsie/elsie-horne-antihistamine-667f1587.csv") %>% 
  rename_all(list(~ str_c('read_', .)))
allergy_drugs_v3 <-  read_csv("lists_in/Elsie/elsie-horne-allergy-drug-4d6fa252.csv") %>% 
  rename_all(list(~ str_c('read_', .)))

antihistamines_bnf <- read_csv("lists_in/OpenSafely/opensafely-antihistamines-oral-2020-10-22.csv")

# label read codes in antihistamines_v3 starting with x0 as:
# antihistamine (non-nasal), nasal allergy drug, other
v3_codes <- antihistamines_v3 %>%
  bind_rows(allergy_drugs_v3 %>%
              # get rid of allergy kits, maintenence sets, treatment sets and vaccines
              filter(!(str_detect(read_term, '\\skit|\\svaccine|treatment\\sset|maintenance\\sset')))) %>%
  filter(str_detect(read_code, '^x0')) %>% 
  mutate(read_term_trunc = read_term) %>%
  # keep only first word of read_term in lowercase
  mutate_at('read_term_trunc', list(~ str_remove_all(., '\\s\\S+'))) %>%
  mutate_at('read_term_trunc', tolower) %>%
  arrange(read_term_trunc) %>%
  # join first word to first word of antihistamines_v2 read_term
  left_join(oral_antihistamines_v2 %>%
              select(read_term) %>%
              mutate_at('read_term', list(~ str_remove(., '^\\*'))) %>%
              mutate_at('read_term', list(~ str_remove_all(., '\\s\\S+'))) %>%
              mutate_at('read_term', tolower) %>%
              distinct() %>%
              mutate(antihistamine_v2 = TRUE),
            by = c('read_term_trunc' = 'read_term')) %>%
  # join first word to first word of antihistamines_bnf read_term
  left_join(antihistamines_bnf %>%
              select(read_term = nm) %>%
              mutate_at('read_term', list(~ str_remove(., '^\\*'))) %>%
              mutate_at('read_term', list(~ str_remove_all(., '\\s\\S+'))) %>%
              mutate_at('read_term', tolower) %>%
              distinct() %>%
              mutate(antihistamine_bnf = TRUE),
            by = c('read_term_trunc' = 'read_term')) %>%
  mutate(antihistamine = case_when(str_detect(read_term, regex('antihistamine', ignore_case = TRUE)) ~ TRUE,
                                   antihistamine_v2 | antihistamine_bnf ~ TRUE,
                                   TRUE ~ FALSE),
         nasal_allergy_drug = str_detect(read_term, regex('\\snose|\\snasal|spray', ignore_case = TRUE)),
         eye_allergy_drug = str_detect(read_term, regex('\\seye', ignore_case = TRUE)),
         topical_antihistamine = str_detect(read_term, regex('\\scream|\\slotion|\\sskin', ignore_case = TRUE)))

v3_codes %>%
  filter_at(vars(c('antihistamine', 'nasal_allergy_drug', 'eye_allergy_drug', 'topical_antihistamine')),
            all_vars(!.)) %>%
  select(read_code, read_term, antihistamine:topical_antihistamine) %>%
  print(n=100)

#  google search the rest to check if antihistamine (oral, nasal, eye, topical) or other nasal / eye allergy drug
v3_codes <- v3_codes %>%
  mutate(type = case_when(read_code %in%  'x05cW' ~ 'eye',
                          read_code %in%  'x01Dp' ~ 'eye and nose',
                          read_code %in%  'x05By' ~ 'nose',
                          read_code %in%  'x01Dv' ~ 'no',
                          read_code %in%  'x04xx' ~ 'eye',
                          read_code %in%  'x05lt' ~ 'nose',
                          read_code %in%  c('x03am', 'x044U', 'x05j3') ~ 'eye',
                          read_code %in%  'x01E8' ~ 'oral antihistamine',
                          read_code %in%  'x01E9' ~ 'parenteral antihistamine',
                          read_code %in%  'x04ov' ~ 'no', # wasp venom extract
                          read_code %in%  'x01E7' ~ 'no', # for headaches
                          read_code %in%  'x0564' ~ 'no',
                          read_code %in%  'x0566' ~ 'no',
                          read_code %in%  'x00fo' ~ 'oral antihistamine',
                          read_code %in%  'x02KI' ~ 'no',
                          read_code %in%  c('x05FH', 'x04r6') ~ 'nose',
                          read_code %in%  'x00fx' ~ 'no',
                          read_code %in%  'x00fy' ~ 'oral antihistamine',
                          read_code %in%  'x01eh' ~ 'no',
                          TRUE ~ NA_character_)) %>%
  mutate_at('nasal_allergy_drug', list(~ case_when(str_detect(read_term, regex('nasal|nose', ignore_case = TRUE)) ~ TRUE,
                                                   str_detect(type, regex('nose', ignore_case = TRUE)) ~ TRUE,
                                                   antihistamine ~ FALSE,
                                                   TRUE ~ .))) %>%
  mutate_at('eye_allergy_drug', list(~ case_when(str_detect(read_term, regex('eye', ignore_case = TRUE)) ~ TRUE,
                                                 str_detect(type, regex('eye', ignore_case = TRUE)) ~ TRUE,
                                                 antihistamine ~ FALSE,
                                                   TRUE ~ .))) %>%
  mutate_at('topical_antihistamine', list(~ case_when(str_detect(read_term, regex('cream|lotion|skin', ignore_case = TRUE)) ~ TRUE,
                                                      antihistamine ~ FALSE,
                                                      TRUE ~ .))) %>%
  mutate(oral_antihistamine = case_when(nasal_allergy_drug | eye_allergy_drug | topical_antihistamine ~ FALSE,
                                        antihistamine_v2 | antihistamine_bnf ~ TRUE,
                                        TRUE ~ FALSE)) 

allergy_drugs <- bind_rows(oral_antihistamines_v2 %>%
                             bind_rows(v3_codes %>%
                                         filter(oral_antihistamine) %>%
                                         select(read_code, read_term)) %>%
                                         mutate(cat2 = 'oral antihistamines'),
                           nasal_allergy_drugs_v2 %>%
                             bind_rows(v3_codes %>%
                                         filter(nasal_allergy_drug) %>%
                                         select(read_code, read_term)) %>%
                                         mutate(cat2 = 'nasal allergy drugs'),
                           eye_allergy_drugs_v2 %>%
                             bind_rows(v3_codes %>%
                                         filter(eye_allergy_drug) %>%
                                         select(read_code, read_term)) %>%
                                         mutate(cat2 = 'eye allergy drugs'),
                           topical_antihistamines_v2 %>%
                             bind_rows(v3_codes %>%
                                         filter(topical_antihistamine) %>%
                                         select(read_code, read_term)) %>%
                                         mutate(cat2 = 'topical antihistamines'))
  
#### eczema #### 
#  this list was constructed as follows (as recommened by susannah)
# all 'realted drugs' here https://bnf.nice.org.uk/treatment-summary/eczema.html
# all topical corticosteroids listed here https://bnf.nice.org.uk/treatment-summary/topical-corticosteroids.html
eczema_bnf <- read_delim("lists_in/Elsie/eczema_bnf.txt", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  mutate_all(tolower) %>%
  distinct() %>%
  arrange(X1)
# search in OpenSafely (only include topical preparations)

eczema_os <- read_delim("lists_in/Elsie/elsie-opensafely-paste-eczema", 
                         "\t", escape_double = FALSE,
                         trim_ws = TRUE, col_names = FALSE) 

# check for any I forgot to expand
eczema_os %>%
  filter(str_detect(X1, '⊞'))

eczema_os <- eczema_os %>%
  mutate_all(list(~ str_remove(., '\\W+'))) %>%
  filter(X1!='') 

# for some reason, the opening bracket is missing for some of the samples,
# so extract Read code as follows
read_code <- str_remove(str_extract(eczema_os$X1, '.{6}$'), '\\)')
read_term <- sapply(seq_along(eczema_os$X1), function(x) str_remove(eczema_os$X1[x], read_code[x]))
read_term <- str_remove(str_remove(read_term, '\\)$'), '\\($')  

eczema <- tibble(read_code = read_code, read_term = read_term)

eczema_v2 <- read_delim("lists_in/Elsie/eczema_psoriasis_preps_v2", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3)

eczema_v2 %>%
  left_join(eczema, by = 'read_code') %>%
  print(n=Inf)

# add
# m51i.
# BUFEXAMAC yes m52..
# TACROLIMUS yes m5E..
# PINE TAR yes m5G..
# DITHRANOL no (psoriasis only)
# ETRETINATE no (psoriasis only)
# SALICYLIC ACID no
# GAMOLENIC ACID no
# CALCIPOTRIOL no
# ACITRETIN no
# TACALCITOL no
# TAZAROTENE no

eczema_v2 <- eczema_v2 %>%
  filter(read_code %in% 'm51i.' |
         str_detect(read_code, '^m52')|
         str_detect(read_code, '^m5E')|
         str_detect(read_code, '^m55'))

eczema <- eczema %>%
  bind_rows(eczema_v2) %>%
  mutate(cat2 = 'eczema preparations')


#### adrenaline ####
adrenaline_pens <- read_csv("lists_in/OpenSafely/opensafely-adrenaline-pens-1964af81.csv") %>%
  select(term = dmd_name)
adrenaline_auto <- read_csv("lists_in/OpenSafely/opensafely-adrenaline-auto-injectors-327422b6.csv") %>%
  select(term)

adrenaline_os <- bind_rows(adrenaline_pens, adrenaline_auto) %>%
  distinct() %>%
  arrange(term)

adrenaline_v3 <- read_csv("lists_in/Elsie/elsie-horne-adrenaline-024148b3.csv") %>%
  rename_all(~ str_c('read_',.))

adrenaline_v2 <- read_delim("lists_in/Elsie/adrenaline_v2", 
                        "\t", escape_double = FALSE,
                        trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3) %>%
  # remove mini-jet products
  filter(!str_detect(read_term, 'MIN.+I-JET'))

adrenaline <- bind_rows(adrenaline_v2, adrenaline_v3) %>%
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'adrenaline')

####

allergy_drugs <- allergy_drugs %>%
  bind_rows(eczema, adrenaline) %>%
  mutate(cat1 = 'allergy prescriptions') %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  distinct()

# make sure all Read codes are 5 characters and fix if not
allergy_drugs %>%
  filter(str_length(read_code)<5)
allergy_drugs <- allergy_drugs %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

####
write_csv(allergy_drugs, 
          path = file.path(opcrd_analysis_path, 'allergy_prescriptions.csv'))
write_csv(allergy_drugs %>%
            arrange(cat2, read_code, read_term) %>%
            select(`Allergy prescription` = cat2,
                   `Read code` = read_code,
                   `Term` = read_term), 
          path = 'lists_out/allergy_prescriptions.csv')

#### latex tables for thesis
allergies_list <- allergy_drugs %>%
  group_split(cat2)

allergies_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  caption <- str_c('Read codes for \\emph{\'', category, '\'} (return to \\nameref{cha:ehr:methods:pre:allergy} methods)')
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

lapply(allergies_list, allergies_latex_table)
