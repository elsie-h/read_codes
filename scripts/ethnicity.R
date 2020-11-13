#### ethnicity ####

# Description: ethnicity codes
# cat1: ethnicity
# cat2: NA
# score: NA

source('setup.R')

ethnicity <- read_csv("lists_in/Elsie/rc_ethnicity_final.csv") %>%
  rename(cat2 = cat_final) %>%
  mutate(cat1 = 'ethnicity')

saveRDS(ethnicity, file = 'lists_out/ethnicity.RDS', compress = FALSE)


# latex tables
ethncity_list <- ethnicity %>%
  group_split(cat2)

ethnicity_latex_table <- function(.data) {
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
