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


opensafely_asthma_drugs <- read_csv("lists_in/OpenSafely/elsie-horne-asthma-drugs-all-2020-11-10T15-42-33.csv") %>%
  rename(read_code = id, read_term = term)


drugs <- c('ICS', 'ICSLABA', 'LABA', 'LTRA', 'methylxanthine', 'OCS', 'omalizumab', 'SABA', 'SAMA', 'SABASAMA')

asthma_drugs <- bind_rows(lapply(drugs, 
                                 function(x) 
                                   read_nwaru(x) %>% mutate(drug = x))) %>%
  left_join(opensafely_asthma_drugs) %>%
  distinct()

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

saveRDS(asthma_drugs, file = 'lists_out/asthma_drugs.RDS', compress = FALSE)


