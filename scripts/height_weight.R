source('setup.R')

height_weight <- tribble(~read_code, ~read_term, ~cat1,
                         '229..', 'Height', 'height',
                         '22A..', 'Weight', 'weight')

write_csv(height_weight %>%
            select(`Read code` = read_code,
                   Term = read_term),
          path = 'lists_out/height_weight.csv')
