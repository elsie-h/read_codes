source('setup.R')

#### from Mukherjee 2016 allergy paper ####
# https://onlinelibrary.wiley.com/doi/abs/10.1111/all.12928
mukherjee <- read_excel("lists_in/Mukherjee2016/Mukherjee2016_supinfo.xlsx", 
                        skip = 1) %>%
  filter(!(`Disease Area` %in% 'Asthma'),
         `Allergy or not allergy` %in% 'A') %>%
  select(read_code = Code, read_term = Term,
         `Disease Area`, `Type of code`) %>%
  mutate_at('read_term', list(~ str_trim(str_remove_all(., '\\.\\.\\.'), side = 'right')))

# For now, only consider those that were categorised by Mukherjee at al. as 'Allergic'
# May want to revise this at a later date, discuss with Susannah.

################################################################################
# Anaphylaxis
anaphylaxis_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Anaphylaxis')

#### Codes for anaphylaxis from manual search of CTV3 ####
anaphylaxis_v3 <- tribble(~read_code_3, ~read_term
                          ,'SN50.',	'Anaphylactic shock'
                          ,'X70vi',	'Venom-induced anaphylaxis'
                          ,'X70vl',	'Blood product-induced anaphylaxis'
                          ,'X70vm',	'Food-induced anaphylaxis'
                          ,'X70vr',	'Drug-induced anaphylaxis'
                          ,'X70vu',	'Human protein-induced anaphylaxis'
                          ,'X70vw',	'Aeroallergen-induced anaphylaxis'
                          ,'SP34.',	'Anaphylactic shock due to serum'
                          ,'X208h',	'Anaphylactoid shock'
                          ,'X70vy',	'Blood and blood product-induced anaphylactoid reaction'
                          ,'X70w1',	'Food-induced anaphylactoid reaction'
                          ,'X70w2',	'Drug-induced anaphylactoid reaction'
                          ,'X70w5',	'Ionic compound-induced anaphylactoid reaction'
                          ,'X70w6',	'Dialysis membrane-induced anaphylactoid reaction'
                          ,'X70w7',	'Exercise anaphylaxis'
                          ,'X70w8',	'Cholinergic anaphylactoid reaction'
                          ,'XaL31',	'H/O: anaphylactic shock'
                          ,'SP34.',	'Anaphylactic shock due to serum'
                          ,'X70vj',	'Anaphylactic shock due to bee sting'
                          ,'X70vk',	'Anaphylactic shock due to wasp sting'
                          ,'X70w1',	'Anaphylactic shock due to adverse food reaction'
                          ,'SN501',	'Anaphylactic shock due to adverse effect of correct drug or medicament properly administered'
                          ,'X70vn',	'Peanut-induced anaphylaxis'
                          ,'X70vo',	'Seafood-induced anaphylaxis'
                          ,'X70vp',	'Egg white-induced anaphylaxis'
                          ,'X70vq',	'Cows milk protein-induced anaphylaxis'
                          ,'X70vs',	'Penicillin-induced anaphylaxis'
                          ,'X70vt',	'Insulin-induced anaphylaxis'
                          ,'X70vu',	'Human protein-induced anaphylaxis'
                          ,'X70vv',	'Seminal fluid-induced anaphylaxis'
                          ,'X70vl',	'Blood product-induced anaphylaxis'
                          ,'X70w9',	'Idiopathic anaphylaxis'
                          ,'X70w9',	'Syndrome of idiopathic anaphylaxis'
                          ,'XaG0G',	'[V]Personal history of food induced anaphylaxis'
                          ,'D3100',	'Anaphylactoid purpura'
                          ,'K0323',	'Anaphylactoid glomerulonephritis'
                          ,'XaZmp',	'Anaphylactoid reaction due to haemodialysis'
                          ,'X208h',	'Anaphylactoid shock'
                          ,'X70vy',	'Blood and blood product-induced anaphylactoid reaction'
                          ,'X70w1',	'Food-induced anaphylactoid reaction'
                          ,'X70w2',	'Drug-induced anaphylactoid reaction'
                          ,'X70w5',	'Ionic compound-induced anaphylactoid reaction'
                          ,'X70w6',	'Dialysis membrane-induced anaphylactoid reaction'
                          ,'X70w7',	'Exercise anaphylaxis'
                          ,'X70w8',	'Cholinergic anaphylactoid reaction'
                          ,'X7060',	'Post-infective Henoch-Schonlein purpura'
                          ,'XE1BR',	'Anaphylactic urticaria'
                          ,'XaL31',	'H/O: anaphylactic shock'
                          ,'X30Lt',	'Anaphylactoid reaction to dialysis'
                          ,'X70w3',	'NSAID-induced anaphylactoid reaction'
                          ,'X70w4',	'Aspirin-induced anaphylactoid reaction'
                          ,'X70vz',	'Whole blood-induced anaphylactoid reaction'
                          ,'X70w0',	'Immunoglobulin-induced anaphylactoid reaction'
                          ,'SN501',	'Anaphylactic shock due to adverse effect of correct drug or medicament properly administered') %>%
  distinct()
#########################################################

anaphylaxis <- anaphylaxis_v2 %>% 
  select(read_code_2 = read_code, read_term) %>%
  full_join(anaphylaxis_v3, by = 'read_term')

################################################################################
# Angioedema
angioedema_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Angioedema')

#### Codes for Angioedema from manual search of CTV3 ####
angioedema_v3 <- tribble(~read_code_3, ~read_term
                         ,'SN51.',	'Angioneurotic oedema'
                         ,'C3760',	'C1 esterase inhibitor deficiency'
                         ,'X70wH',	'ACE inhibitor-aggravated angio-oedema-urticaria'
                         ,'X20IJ',	'Hereditary C1 esterase inhibitor deficiency - deficient factor'
                         ,'X20IK',	'Hereditary C1 esterase inhibitor deficiency - dysfunctional factor'
                         ,'X70wB',	'Acquired C1 esterase inhibitor deficiency'
                         ,'X70wI',	'Chemical-aggravated angio-oedema-urticaria'
                         ,'X70wJ',	'Azo-dye-induced angio-oedema-urticaria'
                         ,'X70wK',	'Sodium benzoate-induced angio-oedema-urticaria'
                         ,'X70wL',	'Latex-induced angio-oedema-urticaria'
                         ,'X70wE',	'Drug-aggravated angio-oedema-urticaria'
                         ,'X70wF',	'NSAID-induced angio-oedema-urticaria'
                         ,'X70wG',	'Aspirin-induced angio-oedema-urticaria'
                         ,'X70wH',	'ACE inhibitor-aggravated angio-oedema-urticaria'
                         ,'Xa0WK',	'Penicillin-induced angio-oedema-urticaria'
                         ,'X70wD',	'Food-induced angio-oedema-urticaria'
                         ,'X70wC',	'Venom-induced angio-oedema-urticaria'
                         ,'X508a',	'Vibratory angio-oedema'
                         ,'X00nD',	'Angioneurotic oedema of larynx'
                         ,'C3760',	'HANE - Hereditary angioneurotic oedema'
                         ,'X70wH',	'ACE inhibitor-aggravated angio-oedema-urticaria'
                         ,'X20IJ',	'Hereditary C1 esterase inhibitor deficiency - deficient factor'
                         ,'X20IK',	'Hereditary C1 esterase inhibitor deficiency - dysfunctional factor'
                         ,'X70wB',	'Acquired C1 esterase inhibitor deficiency'
                         ,'X20IJ',	'Hereditary angioneurotic oedema - type 1'
                         ,'X20IK',	'Hereditary angioneurotic oedema - type 2') %>% 
  distinct()
########################################################

angioedema <- angioedema_v2 %>% 
  select(read_code_2 = read_code, read_term) %>%
  full_join(angioedema_v3, by = 'read_term')

################################################################################
# Conjunctivitis
conjunctivitis_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Conjunctivitis')

#### Codes for allergic conjunctivits from manual search of CTV3 ####
# I searched 'atopic|vernal|allergic conjunctivitis' in the read code browser
# all results and their children included
conjunctivitis_v3 <- tribble(~read_code_3, ~read_term
                             ,'X00Zi', 'Atopic conjunctivitis'
                             ,'X00Zj', 'Seasonal allergic conjunctivitis'
                             ,'X00Zk', 'Perennial allergic conjunctivitis'
                             ,'F4A31', 'Vernal conjunctivitis'
                             ,'X00Zl', 'Giant papillary conjunctivitis'
                             ,'XaF7u', 'Contact lens related giant papillary conjunctivitis'
                             ,'F4C06', 'Acute atopic conjunctivitis'
                             ,'XE16b', 'Other chronic allergic conjunctivitis'
                             ,'X00Zk', 'Adult pattern atopic conjunctivitis')
#####################################################################
conjunctivitis <- conjunctivitis_v2 %>%
  select(read_code_2 = read_code, read_term) %>%
  full_join(conjunctivitis_v3, by = 'read_term')

################################################################################
# Drug allergy
drug_allergy_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Drug allergy')

drug_allergy_v3 <- read_csv("lists_in/OpenSafely/elsie-horne-drug-allergy-2020-11-05T08-48-21.csv") %>%
  rename(read_code_3 = id, read_term = term)

drug_allergy <- drug_allergy_v2 %>%
  select(read_code_2 = read_code, read_term) %>%
  full_join(drug_allergy_v3, by = 'read_term')

################################################################################
# Eczema
eczema_v2 <- mukherjee %>%
  filter(`Disease Area` %in% 'Eczema')

################################################################################

# Food allergy
food_allergy <- mukherjee %>%
  filter(`Disease Area` %in% 'Food allergy')

# Rhinitis
rhinitis <- mukherjee %>%
  filter(`Disease Area` %in% 'Rhinitis')

# Urticaria
urticaria <- mukherjee %>%
  filter(`Disease Area` %in% 'Urticaria')

# Other
other <- mukherjee %>%
  filter(`Disease Area` %in% 'Other',
         # None of the read codes under 'Test' or 'Other' indicate that an allergy is present, only that it has been tested
         !(`Type of code` %in% c('Test',
                                 'Other')),
         !(read_term %in% c('Hypersens. skin test done',
                       'Hypersens. skin test: no react')))
