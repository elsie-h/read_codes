source('setup.R')

asthma_diagnosis_v3 <- read_csv("lists_in/OpenSafely/opensafely-asthma-diagnosis-2020-04-15.csv")

# as these are medcodes and not Read codes, will have to join on Read term to try to identify Readc codes.
asthma_diagnosis_v2 <- read_excel("lists_in/Nissen2017/Francis_asthma_specific_JW_numericmedcode.xlsx")

asthma_all <- read_csv(path = 'lists_out/asthma_all.csv')

asthma_diagnosis <- asthma_diagnosis_v2 %>%
  rename(read_term = readterm) %>%
  left_join(asthma_all, by = 'read_term') %>% 
  filter(!is.na(read_code)) %>%
  # look up any unmatched terms using NHS read code browser
  bind_rows(tribble(~read_code, ~read_term,
                    'H33z2', 'late onset asthma',
                    '173A.', 'exercise induced asthma',
                    'H33zz', 'exercise induced asthma',
                    'H330.', 'pollen asthma',
                    '663..', 'asthma monitoring',
                    'H35y7', 'wood asthma',
                    '1780.', 'aspirin induced asthma',
                    'H35y6', 'sequoiosis (red cedar asthma)')) %>%
  distinct(read_code, read_term)
# found all apart from  24506 further asthma - drug prevent.  

write_csv(asthma_diagnosis, 
          path = file.path(opcrd_analysis_path, 'asthma_diagnosis.csv'))
write_csv(asthma_diagnosis %>%
            arrange(read_code, read_term) %>%
            select(`Read code` = read_code, 
                   `Term` = read_term), 
          path = 'lists_out/asthma_diagnosis.csv')