#  peak flow meters

source('setup.R')

# version 2 code lists
# all starting l8...
peak_flow_meters <- read_delim("lists_in/Elsie/peak_flow_meters.txt", 
                                     "\t", escape_double = FALSE,
                                     trim_ws = TRUE, col_names = FALSE) %>%
  select(read_code = X1, read_term = X3) %>%
  distinct() %>%
  mutate(cat1='peak_flow_meters', cat2 = NA_character_)

# check mapping
# map V2 -> CTV3
peak_flow_meters %>% map_V2_CTV3() %>% arrange(CTV3_CONCEPTID) %>% print(n=Inf)
# map CTV3 -> V2
peak_flow_meters %>% map_CTV3_V2() %>% arrange(V2_CONCEPTID) %>% print(n=Inf)

write_csv(peak_flow_meters, 
          path = file.path(opcrd_analysis_path, 'peak_flow_meter.csv'))
write_csv(peak_flow_meters %>%
            arrange(read_code) %>%
            select(`Read code` = read_code,
                   Term = read_term), 
          path = 'lists_out/peak_flow_meter.csv')
