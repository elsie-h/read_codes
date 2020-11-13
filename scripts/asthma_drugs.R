#### asthma_drugs ####

# Description: codes corresponding to asthma drugs
# cat1: asthma_drugs
# cat2: ICS, ICSLABA, LABA, LTRA, methylxanthine, OCS, omalizumab, SABA, SABASAMA, SAMA
# score: NA


# rms_drugs_all <- bind_rows(read_delim("lists_in/RMS/dl_ics_inh_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
#                              select(read_code, product_name = MX_PRODUCT_NAME),
#                            read_delim("lists_in/RMS/dl_laba_lama_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
#                              select(read_code, product_name = read_term),
#                            read_delim("lists_in/RMS/dl_lama_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
#                              select(read_code, product_name = MX_PRODUCT_NAME),
#                            read_delim("lists_in/RMS/dl_ltra_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
#                              select(read_code, product_name = MX_PRODUCT_NAME),
#                            read_delim("lists_in/RMS/dl_saba_inh_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
#                              select(read_code, product_name = MX_PRODUCT_NAME),
#                            read_delim("lists_in/RMS/dl_sama_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
#                              select(read_code, product_name = MX_PRODUCT_NAME),
#                            read_delim("lists_in/RMS/dl_step3_inh_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
#                              select(read_code, product_name = MX_PRODUCT_NAME),
#                            read_delim("lists_in/RMS/dl_theo_rms.txt", "|", escape_double = FALSE, trim_ws = TRUE) %>%
#                              select(read_code, product_name = MX_PRODUCT_NAME),
# )
# 
# 
# product_names <- rms_drugs_all %>%
#   select(product_name) %>%
#   unlist() %>%
#   unname()
# 
# drug_names <- unique(unname(sapply(product_names, function(x) str_split(x, ' ')[[1]][1])))
#  search all in opensafely.

QOF_drugs <- readRDS(file = 'lists_in/QOF/QOF_codes.RDS') %>%
  filter_at(vars(ends_with('_id')), any_vars(. %in% 'ASTTRT_COD')) %>%
  mutate(read_term = case_when(!is.na(v36_v2_term) ~ v36_v2_term,
                               !is.na(v36_v3_term) ~ v36_v3_term,
                               !is.na(v37_v2_term) ~ v37_v2_term,
                               !is.na(v37_v3_term) ~ v37_v3_term,
                               !is.na(v38_v2_term) ~ v38_v2_term,
                               !is.na(v38_v3_term) ~ v38_v3_term,
                               TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term) %>%
  mutate(QOF = 1)

opensafely_asthma_drugs <- read_csv("lists_in/OpenSafely/elsie-horne-asthma-drugs-all-2020-11-10T15-42-33.csv") %>%
  rename(read_code = id, read_term = term) %>%
  distinct()

# Read RMS only for terms to join to QOF
LAMA_codes <- read_delim("lists_in/RMS/dl_lama_rms.txt",  
                         "|", escape_double = FALSE, trim_ws = TRUE) %>%
  select(read_code) %>%
  mutate(catRMS = 'LAMA')
ICS_codes <- read_delim("lists_in/RMS/dl_ics_inh_rms.txt",  
                         "|", escape_double = FALSE, trim_ws = TRUE) %>%
  select(read_code) %>%
  mutate(catRMS = 'ICS')
LABALAMA_codes <- read_delim("lists_in/RMS/dl_laba_lama_rms.txt",  
                         "|", escape_double = FALSE, trim_ws = TRUE) %>%
  select(read_code) %>%
  mutate(catRMS = 'LABALAMA')
LTRA_codes <- read_delim("lists_in/RMS/dl_ltra_rms.txt",  
                         "|", escape_double = FALSE, trim_ws = TRUE) %>%
  select(read_code) %>%
  mutate(catRMS = 'LTRA')
SABA_codes <- read_delim("lists_in/RMS/dl_saba_inh_rms.txt",  
                         "|", escape_double = FALSE, trim_ws = TRUE) %>%
  select(read_code) %>%
  mutate(catRMS = 'SABA')

SAMA_codes <- read_delim("lists_in/RMS/dl_sama_rms.txt",  
                         "|", escape_double = FALSE, trim_ws = TRUE) %>%
  select(read_code) %>%
  mutate(catRMS = 'SAMA')
theo_codes <- read_delim("lists_in/RMS/dl_theo_rms.txt",  
                         "|", escape_double = FALSE, trim_ws = TRUE) %>%
  select(read_code) %>%
  mutate(catRMS = 'methylxanthine')


drugs <- c('ICS', 'ICSLABA', 'LABA', 'LTRA', 'methylxanthine', 'OCS', 'omalizumab', 'SABA', 'SAMA', 'SABASAMA')

# drugs from Nwaru lists
asthma_drugs_nwaru <- bind_rows(lapply(drugs, 
                                 function(x) 
                                   read_nwaru(x) %>% mutate(drug = x))) %>%
  left_join(opensafely_asthma_drugs) %>%
  mutate_at('drug', list(~ if_else(. %in% 'omalizumab',
                                   'biologic', 
                                   .))) %>%
  distinct()

asthma_drugs <- asthma_drugs_nwaru %>%
  full_join(QOF_drugs, by = 'read_code') %>%
  mutate(read_term = if_else(is.na(read_term.x), read_term.y, read_term.x)) %>%
  select(-read_term.y, -read_term.x)

# manually fill the remaining ones:
asthma_drugs %>% filter(is.na(read_term)) %>% print(n=nrow(.))

asthma_drugs <- asthma_drugs %>%
  mutate_at('read_term', list(~ case_when(read_code %in% 'c64B.' ~	'BUDESONIDE 50micrograms spacer inhaler',
                                          read_code %in% 'c64C.' ~	'PULMICORT 200micrograms spacer inhaler',
                                          read_code %in% 'c64D.' ~	'PULMICORT LS 50micrograms spacer inhaler',
                                          read_code %in% 'c64z.' ~	'BUDESONIDE 200micrograms spacer inhaler',
                                          read_code %in% 'cA...' ~	'Leukotriene receptor antagonist',
                                          read_code %in% 'fe6w.' ~	'*PREDNISOLONE 2.5mg tablets',
                                          read_code %in% 'fe11.' ~	'BETNELAN 500micrograms tablets',
                                          read_code %in% 'fe12.' ~	'*BETNESOL 500micrograms tabs',
                                          read_code %in% 'fe1x.' ~	'BETAMETHASONE 500micrograms soluble tablets',
                                          read_code %in% 'fe1y.' ~	'BETAMETHASONE 500microgram tablets',
                                          read_code %in% 'fe21.' ~	'*CORTISONE 5mg tablets',
                                          read_code %in% 'fe22.' ~	'*CORTISONE 25mg tablets',
                                          read_code %in% 'fe23.' ~	'*CORTELAN 25mg tablets',
                                          read_code %in% 'fe24.' ~	'*CORTISTAB 5mg tablets',
                                          read_code %in% 'fe25.' ~	'*CORTISTAB 25mg tablets',
                                          read_code %in% 'fe26.' ~	'*CORTISYL 25mg tablets',
                                          read_code %in% 'x01MW' ~	'Oral betamethasone',
                                          read_code %in% 'ck1..' ~	'OMALIZUMAB',
                                          read_code %in% 'ck11.' ~	'OMALIZUMAB 150mg injection(pdr for recon)+solvent',
                                          read_code %in% 'ck12.' ~	'XOLAIR 150mg injection(pdr for recon)+solvent',
                                          read_code %in% 'ck13.' ~	'OMALIZUMAB 75mg/0.5mL soln for injection prefilled syringe',
                                          read_code %in% 'ck14.' ~	'XOLAIR 75mg/0.5mL solution for injection prefilled syringe',
                                          read_code %in% 'ck15.' ~	'OMALIZUMAB 150mg/1mL soln for injection prefilled syringe',
                                          read_code %in% 'ck16.' ~	'XOLAIR 150mg/1mL solution for injection prefilled syringe',
                                          read_code %in% 'c11A.' ~	'*VENTOLIN CR 4mg m/r tablets',
                                          read_code %in% 'c11B.' ~	'*SALBUTAMOL 4mg m/r tablets',
                                          read_code %in% 'c11C.' ~	'*VENTOLIN CR 8mg m/r tablets',
                                          read_code %in% 'c138.' ~	'ROTAHALER DEVICE',
                                          read_code %in% 'c13b.' ~	'*ROTAHALER DEVICE',
                                          read_code %in% 'c31y.' ~	'IPRATROPIUM 250micrograms/mL nebuliser solution',
                                          read_code %in% 'l86..' ~	'IPRATROPIUM BROMIDE [2]',
                                          TRUE ~ .
                                          ))) %>%
  rename(cat2 = drug) %>%
  mutate(cat1 = 'asthma_drug')

asthma_drugs %>%
  group_by(read_code) %>% 
  count() %>% 
  filter( n>1) %>% 
  ungroup() %>% 
  select(read_code) %>% 
  left_join(asthma_drugs) %>% 
  print(n=nrow(.))

# many of the SABASAMA drugs also appear in SABA and SAMA - change so they only appear once in SABASAMA
asthma_drugs <- asthma_drugs %>%
  group_by(read_code) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate_at('cat2', list(~ case_when(cat2 %in% c('SABA', 'SAMA') & n > 1 ~ 'SABASAMA',
                                     TRUE ~ .))) %>% 
  select(-n) %>%
  distinct() 

asthma_drugs <- asthma_drugs %>% 
  left_join(bind_rows(ICS_codes, 
                      LAMA_codes, 
                      LABALAMA_codes, 
                      LTRA_codes, 
                      SABA_codes, 
                      SAMA_codes, 
                      theo_codes) %>%
              distinct(), by = 'read_code') %>%
  mutate_at('cat2', list(~ if_else(is.na(.), catRMS, .))) %>%
  select(-catRMS)
              
asthma_drugs %>% 
  filter(is.na(cat2)) %>% print(n=200)

# the codes from QOF that are not in the Nwaru list are not categorised by drug group
# I did this manually - confirm the ?? with Susannah
asthma_drugs <- asthma_drugs %>%
  mutate_at('cat2', list(~ case_when(!is.na(.) ~ .,
                                     str_detect(read_term, regex('ventolin', ignore_case = T)) ~ 'SABA',
                                     str_detect(read_term, regex('volmax', ignore_case = T)) ~ 'SABA',
                                     str_detect(read_term, regex('SALBUVENT', ignore_case = T)) ~ 'SABA',
                                     read_term %in% c('*SALBUTAMOL 4mg m/r tablets',
                                                      'Salbutamol [obstetric]') ~ 'SABA',
                                     read_term %in% c('Respiratory beclomethasone') ~ 'ICS',
                                     read_term %in% c('Beclomethasone dipropionate+salbutamol') ~ 'ICSSABA',
                                     read_term %in% c('*NEBUHALER spacer device',
                                                      '*VOLUMATIC spacer device') ~ 'spacer',
                                     str_detect(read_term, regex('Fenoterol', ignore_case = T)) ~ 'SABA',
                                     str_detect(read_term, regex('BEROTEC', ignore_case = T)) ~ 'SABA',
                                     str_detect(read_term, regex('BAMB', ignore_case = T)) ~ 'LABA',
                                     str_detect(read_term, regex('SIRDUPLA|AIRFLUSAL FORSPIRO|BECLOMET DIPROP\\+FORMOTERL FUMARATE DIHYD', ignore_case = T)) ~ 'ICSLABA',
                                     str_detect(read_term, regex('ADRENALINE|MEDIHALER-EPI|MIN-I-JET', ignore_case = T)) ~ 'adrenaline',
                                     str_detect(read_term, regex('EPHEDRINE', ignore_case = T)) ~ '??',
                                     str_detect(read_term, regex('ORCIPRENALINE|ALUPENT', ignore_case = T)) ~ 'SABA',
                                     str_detect(read_term, regex('ACLIDINIUM|UMECLIDINIUM', ignore_case = T)) ~ 'LAMA',
                                     read_term %in% c('SELECTIVE BETA-ADRENOCEPTOR STIMULANT',
                                                      'OTHER ADRENOCEPTOR STIMULANTS',
                                                      'ANTICHOLINERGIC BRONCHODILATORS',
                                                      'XANTHINE BRONCHODILATORS',
                                                      'COMPOUND BRONCHODILATORS',
                                                      'COMPOUND BRONCHODILATORS A-Z',
                                                      'ASTHMA PROPHYLAXIS',
                                                      'AMBIGUOUS CODE-DUPLICATED ISSUE',
                                                      'BECLOMETASONE COMPOUNDS',
                                                      'COMPOUND BRONCHODILATORS [1]',
                                                      'CORTICOSTEROIDS [RESPIRATORY USE]',
                                                      'Corticosteroids used in the treatment of asthma') ~ 'group',
                                     str_detect(read_term, regex('ASMA-VYDRIN|DUO-AUTOHALER|MEDIHALER|RYBAR|INTAL|CROMOGEN|cromoglycate|KETOTIFEN|NEDOCROMIL|TILADE|AEROCROM SYNCRONER|ZADITEN', ignore_case = T)) ~ '??',
                                     str_detect(read_term, regex('BRICANYL', ignore_case = T)) ~ 'SABA',
                                     read_term %in% c('*BRONCHILATOR inhaler') ~ '??',
                                     str_detect(read_term, regex('BROVON', ignore_case = T)) ~ 'adrenaline',
                                     str_detect(read_term, regex('NETHAPRIN', ignore_case = T)) ~ 'LABA',
                                     str_detect(read_term, regex('BETAMETHASONE|BEXTASOL|CICLESONIDE|MOMETASONE|fluticasone', ignore_case = T)) ~ 'ICS',
                                     str_detect(read_term, regex('TAUMASTHMAN|TEDRAL', ignore_case = T)) ~ 'methylxanthine',
                                     str_detect(read_term, regex('VENTIDE', ignore_case = T)) ~ 'ICSSABA',
                                     str_detect(read_term, regex('VENTIDE|dipropionate\\+salbutamol|FOSTAIR', ignore_case = T)) ~ 'ICSSABA',
                                     str_detect(read_term, regex('MEPOLIZUMAB|NUCALA', ignore_case = T)) ~ 'biologic',
                                     read_term == 'Sodium cromoglicate+salbutamol' ~ 'SABA',
                                     str_detect(read_term, regex('CROMOGLICATE', ignore_case = T)) ~ '??',
                                     str_detect(read_term, regex('Terbutaline', ignore_case = T)) ~ 'SABA',
                                     TRUE ~ '??'))) 

saveRDS(asthma_drugs, file = 'lists_out/asthma_drugs.RDS', compress = FALSE)

write.csv(asthma_drugs, file = 'lists_out/asthma_drugs.csv')

# latex tables
drug_list <- asthma_drugs %>%
  select(-cat1) %>%
  mutate_at('cat2', list(~ case_when(. %in% c('methylxanthine', 'omalizumab') ~ str_to_sentence(.),
                                     . %in% 'ICSLABA' ~ 'ICS-LABA combo',
                                     . %in% 'SABASAMA' ~ 'SABA-SAMA combo',
                                     TRUE ~ .))) %>%
  group_split(cat2)

drug_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  caption <- str_c('Ethnicity Read codes for \\emph{\'', category, '\'} category (return to \\nameref{cha:ehr:methods:pre:ethnicity} methods)')
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

lapply(ethncity_list, ethnicity_latex_table)

