source('setup.R')

# Use the OpenSAFELY list as a guide
asthma_diagnosis_v3 <- read_csv("lists_in/OpenSafely/opensafely-asthma-diagnosis-2020-04-15.csv")
# From the OpenSAFELY website:
# This codelist was derived by the following method:
# - Read 2 Codelist provided by LSHTM. This codelist had been used in Nissen et al (see references)
# - Codes from Quality Outcome Framework (QOF) were examined and relevant asthma codes taken
# - Codes from SNOMED were taken by searching for relevant asthma terms

# For version 2 Read codes, I will use the Nissen list that OpenSAFELY is based on, 
# and add V2 QOF codes to match the OpenSAFELY approach.

# I will then apply the following exemptions (also applied by OpenSAFELY):
# - Codes that related to inhalers (such as inhaler technique) or steroids were excluded as were not felt to be asthma specific
# - Codes for generic respiratory disease were excluded for similar reasons
# - Specific bronchitis was excluded if not included with asthma (such as chronic asthmatic bronchitis)
# - Some occupation causes of respiratory disease such as wood pulp workers disease or byssinosis were excluded as not asthma diagnosis
# - All drug codes were excluded as these are cover in another codelist

# Nissen list
asthma_diagnosis_v2 <- read_excel("lists_in/Nissen2017/Francis_asthma_specific_JW_numericmedcode.xlsx")

# as these are medcodes and not Read codes, will have to join on Read term to try to identify Read codes
# a list of asthma related Read codes that I've collected for identifying terms
asthma_all <- read_csv(file = 'lists_out/asthma_all.csv')

# add the terms to the Nissen list
asthma_diagnosis_v2_clean <- asthma_diagnosis_v2 %>%
  rename(read_term = readterm) %>%
  left_join(asthma_all, by = 'read_term') %>% 
  # look up any unmatched terms using NHS read code browser and add
  bind_rows(tribble(~read_code, ~read_term,
                    'H33z2', 'late onset asthma',
                    '173A.', 'exercise induced asthma',
                    'H33zz', 'exercise induced asthma',
                    'H330.', 'pollen asthma',
                    '663..', 'asthma monitoring',
                    'H35y7', 'wood asthma',
                    '1780.', 'aspirin induced asthma',
                    'H35y6', 'sequoiosis (red cedar asthma)')) %>%
    # can now remove unmatched ones
  filter(!is.na(read_code)) %>% 
  distinct(read_code, read_term)
# found all apart from  24506 further asthma - drug prevent.  

# identify V2 QOF asthma codes to add to Nissen list
QOF_codes <- read_csv("lists_in/QOF/QOF_codes.csv")
asthma_QOF_v2 <- QOF_codes %>%
  filter_at(vars(contains('v2_id')), any_vars(. == 'AST_COD')) %>%
  select(read_code, contains('v2_term')) %>%
  mutate(read_term = case_when(!is.na(v36_v2_term) ~ v36_v2_term,
                               !is.na(v37_v2_term) ~ v37_v2_term,
                               !is.na(v38_v2_term) ~ v38_v2_term,
                               TRUE ~ NA_character_)) %>%
  distinct(read_code, read_term) %>%
  mutate_at('read_term', tolower)

# final list
asthma_diagnosis <- asthma_diagnosis_v3 %>%
  select(read_code = CTV3ID, read_term = CTV3PreferredTermDesc) %>%
  mutate_at('read_term', tolower) %>%
  distinct() %>%
  # remove history of asthma
  filter(read_term != 'h/o: asthma') %>%
  bind_rows(asthma_diagnosis_v2_clean %>%
              filter(!(read_term %in% c('wood asthma', 'sequoiosis (red cedar asthma)'))),
            asthma_QOF_v2) %>%
  # add asthma control step 0 to align with OpenSAFELY
  # add codes for asthma resolved as I will use this to exclude
  add_row(read_code = '212G.', read_term = 'asthma resolved') %>%
  add_row(read_code = '21262', read_term = 'asthma resolved') %>%
  # add V3 code for ACOS (this is already in V2 from QOF), as I won't exclude COPD
  add_row(read_code = 'Xac33', read_term = 'asthma-chronic obstructive pulmonary disease overlap syndrome') %>%
  distinct()
  
# make sure all Read codes are 5 characters and fix if not
asthma_diagnosis %>%
  filter(str_length(read_code)<5)
asthma_diagnosis <- asthma_diagnosis %>%
  mutate_at('read_code', list(~ str_pad(., width=5, side='right', pad='.')))

write_csv(asthma_diagnosis, 
          path = file.path(opcrd_analysis_path, 'asthma_diagnosis.csv'))
write_csv(asthma_diagnosis %>%
            arrange(read_code, read_term) %>%
            select(`Read code` = read_code, 
                   `Term` = read_term), 
          path = 'lists_out/asthma_diagnosis.csv')
