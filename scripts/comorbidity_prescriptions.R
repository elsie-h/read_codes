#  comorbidity prescriptions

source('setup.R')

# proton pump inhibitiros (PPIs), h2-antagonists, anti-depressants etc

# use the pseudo-BNF list from OpenSafely to get a list of PPI drug names
ppi_bnf <- read_csv("lists_in/OpenSafely/opensafely-proton-pump-inhibitors-ppi-oral-2020-04-20.csv")

drugname <- sort(unique(unname(sapply(ppi_bnf$nm, function(x) str_split(x, '\\s', n=2)[[1]][1]))))

# search all drugname in NHS browser (v2) and open safely browser tool (v3)
drugs <- sort(unique(unname(sapply(ppi_bnf$nm, function(x) str_split(x, '\\s\\(', n=2)[[1]][1]))))

drugs[str_detect(drugs, regex('ventra', ignore_case = T))]
# no OpenSafely results for guardium or ventra

ppi_v2 <- read_delim("lists_in/Elsie/PPI_v2", 
                     "\t", escape_double = FALSE,
                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*')))

ppi_v3 <- read_csv("lists_in/Elsie/elsie-horne-proton-pump-inhibitors-63cc5591.csv") %>%
  rename_all(~ str_c('read_', .)) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*')))

ppi <- bind_rows(ppi_v2, ppi_v3) %>%
  filter(!str_detect(read_term, 'injection|intravenous')) %>%
  # so that QOF = yes comes first and are kept with distinct
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'proton pump inhibitor')

# all H2-antagonists in the a6... branch of the v2 NHS Read code browser
h2antagonist_v2 <- read_delim("lists_in/Elsie/h2antagonist_v2", 
                              "\t", escape_double = FALSE,
                              trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*')))

h2names <- h2antagonist_v2 %>%
  filter(!str_detect(read_term, 'injection')) %>%
  select(read_term) %>%
  unlist() %>% unname()

h2names <- sort(unique(unname(sapply(h2names, function(x) str_split(x, '\\s', n=2)[[1]][1]))))

# search these in the OpenSafely browser
# no results for raciran or ranzac

h2antagonist_v3 <- read_csv("lists_in/Elsie/elsie-horne-h2antagonist-50226248.csv") %>%
  rename_all(~ str_c('read_', .)) %>%
  mutate_at('read_term', list(~ str_remove(tolower(.), '^\\*'))) %>%
  filter(! str_detect(read_code, '^a2'))

h2antagonist <- bind_rows(h2antagonist_v2, h2antagonist_v3) %>%
  filter(!str_detect(read_term, 'injection|intravenous')) %>%
  # so that QOF = yes comes first and are kept with distinct
  distinct(read_code, .keep_all = TRUE) %>%
  mutate(cat2 = 'h2-antagonist')

# bind all
comorbidity_prescriptions <- bind_rows(h2antagonist, ppi) %>%
  mutate(cat1 = 'comorbidity prescription') 

# save list
write_csv(comorbidity_prescriptions, path = 'lists_out/comorbidity_prescriptions.csv')

# latex tables for thesis
drug_list <- comorbidity_prescriptions %>%
  group_split(cat2)

drug_latex_table <- function(.data) {
  category <- .data %>%
    distinct(cat2) %>% 
    unlist() %>% 
    unname()
  
  category_ <- str_to_lower(str_replace_all(category, ' ', '_'))
  
  caption <- str_c('Read codes for \\emph{\'', category, '\'} prescription group (return to \\nameref{cha:ehr:methods:pre:comorbidities} methods)')
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

lapply(drug_list, drug_latex_table)