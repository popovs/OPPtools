# imports example murre data from Movebank
# user must provide Movebank credentials to successfully download

murres <- opp_download_data(248994009)

usethis::use_data(murres, overwrite = T)

# read in a table with values to use in generating reports on central place
# foraging species tracked during the OPP project

cpf_report_params <- read.csv('data-raw/opp-cpf-report-params.csv', stringsAsFactors = F)

usethis::use_data(cpf_report_params, overwrite = T)

# read in a table with list of mb projects associated with OPP

opp_mb_projects <- read.csv('data-raw/opp-mb-projects.csv', stringsAsFactors = F)

usethis::use_data(opp_mb_projects, overwrite = T)

# read in a table with North American bird species list

aou_species <- read.csv('data-raw/NACC_list_species.csv', stringsAsFactors = F)
aou_species <- aou_spp[,c('species', 'common_name', 'french_name')]
usethis::use_data(aou_species, overwrite = T)
