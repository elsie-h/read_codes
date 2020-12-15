source('setup.R')

height_weight <- tribble(~read_code, ~read_term, ~cat1,
                         '229..', 'Height', 'height',
                         '22A..', 'Weight', 'weight')

write_csv(height_weight, path = 'lists_out/height_weight.csv')