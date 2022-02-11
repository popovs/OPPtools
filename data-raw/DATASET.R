# imports example murre data from Movebank
# user must provide Movebank credentials to successfully download

murres <- opp_download_data(248994009)

usethis::use_data(murres, overwrite = T)

# read in a table with values to use in generating reports on central place
# foraging species tracked during the OPP project
cpf_report_params <- read.csv('data-raw/opp-cpf-report-params.csv', stringsAsFactors = F)

usethis::use_data(cpf_report_params, overwrite = T)
