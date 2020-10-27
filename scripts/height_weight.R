source('setup.R')

height_weight <- tribble(~read_code, ~read_term, ~cat1,
                         '229..', 'Height', 'height',
                         '22A..', 'Weight', 'weight')

saveRDS(height_weight, file = 'lists_out/height_weight.RDS', compress = FALSE)