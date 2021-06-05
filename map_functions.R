# this function maps a Read code list to V3,
# removes any read codes in the mapped list that were in the original list
# and joins to terms

map_V2_CTV3 <- function(.data) {
  # map V2 -> CTV3
  .data %>%
    left_join(rctctv3map %>% 
                distinct(V2_CONCEPTID, CTV3_CONCEPTID), 
              by = c('read_code' = 'V2_CONCEPTID')) %>%
    # keep only mapped Read codes
    filter(!is.na(CTV3_CONCEPTID)) %>%
    distinct(CTV3_CONCEPTID) %>%
    # remove any that were in original list
    anti_join(.data %>% distinct(read_code), 
              by = c('CTV3_CONCEPTID' = 'read_code')) %>%
    # join to Read terms
    left_join(rctermsctmap,
              by = c('CTV3_CONCEPTID' = 'ReadCode')) %>%
    distinct(CTV3_CONCEPTID, .keep_all = TRUE)
}

map_CTV3_V2 <- function(.data) {
  # map CTV3 -> V2
  .data %>%
    left_join(ctv3rctmap %>% 
                distinct(V2_CONCEPTID, CTV3_CONCEPTID), 
              by = c('read_code' = 'CTV3_CONCEPTID')) %>%
    # keep only mapped Read codes
    filter(!is.na(V2_CONCEPTID)) %>%
    distinct(V2_CONCEPTID) %>%
    # remove any that were in original list
    anti_join(.data %>% distinct(read_code), 
              by = c('V2_CONCEPTID' = 'read_code')) %>%
    left_join(rctermsctmap,
              by = c('V2_CONCEPTID' = 'ReadCode')) %>%
    distinct(V2_CONCEPTID, .keep_all = TRUE)
}
