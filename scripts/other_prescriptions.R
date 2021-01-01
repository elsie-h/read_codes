#  other prescriptions

source('setup.R')

# aspirin, beta-blockers, paracetemol, PPIs, NASID, statins 

# use the pseudo-BNF list from OpenSafely and the buidler tool to get a list of Read codes for PPI
ppi <- read_csv("lists_in/OpenSafely/opensafely-proton-pump-inhibitors-ppi-oral-2020-04-20.csv")

drugname <- sort(unique(unname(sapply(ppi$nm, function(x) str_split(x, '\\s', n=2)[[1]][1]))))

drugs <- sort(unique(unname(sapply(ppi$nm, function(x) str_split(x, '\\s\\(', n=2)[[1]][1]))))

drugs[str_detect(drugs, regex('ventra', ignore_case = T))]
# no OpenSafely results for guardium or ventra
