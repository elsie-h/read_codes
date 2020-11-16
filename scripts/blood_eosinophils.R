source('setup.R')

blood_eos <- tribble(~read_code, ~read_term, ~cat1, ~cat2,
                     '42K..', 'blood eosinophil count', 'eos', NA_character_)

saveRDS(blood_eos, file = 'lists_out/blood_eosinophils.RDS', compress = FALSE)
