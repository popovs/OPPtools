#' Example GPS tracks from thick-billed murres breeding at Coats Island, NU
#'
#' Tracking data from thick-billed murres breeding at a colony on Coats Islands, NU, in 2010.
#'
#' @format GPS tracking data formatted to fit functions in track2KBA.
#' Data are a list object of length two, containing tracking data (accessed using: murres$data)
#' and study site location information (accessed using: murres$site).
#'
#' @source Downloaded from the Arctic Animal Movement Archive project 'Thick-billed murre Elliott Coats 2010'
#' \url{https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study248994009} using
#' track2KBA::move2KBA()
#'
"murres"

# -----

#' American Ornithological Society official checklist of North American and Middle American Birds
#'
#' @format A data frame with 2186 rows and 3 variables:
#' \describe{
#'   \item{species}{Latin name}
#'   \item{common_name}{English common name}
#'   \item{french_name}{French common name}
#' }
#' @source  Chesser, R. T., S. M. Billerman, K. J. Burns, C. Cicero, J. L. Dunn, B. E. Hernández-Baños, A. W. Kratter, I. J. Lovette, N. A. Mason, P. C. Rasmussen, J. V. Remsen, Jr., D. F. Stotz, and K. Winker. 2021. Check-list of North American Birds (online). American Ornithological Society. http://checklist.aou.org/taxa
#'
'aou_species'

# -----

#' Report analysis parameters for central place foragers
#'
#' @description This dataset includes parameters used in analysis to identify high-use areas for breeding colonial seabirds. Parameters
#' in this dataframe are passed to OPPtools::opp_render_diagnostic and OPPtools::opp_render_report to run analysis to identify trips away from the colony,
#' calculate individual utilization distributions, quantify overlap among individual distributions, and map high-use areas around the colony.
#'
#'
#' @format A data frame:
#' \describe{
#'   \item{mb_project_num}{Numeric. Movebank project number.}
#'   \item{report_title}{Text. Title used in PDF report.}
#'   \item{file_name}{Text. File name used in all outputs.}
#'   \item{spp}{Text. Species common name.}
#'   \item{breeding_pairs}{Numeric. Number of breeding pairs at colony. Used in calculating number of birds expected to occur in high-use areas.}
#'   \item{count_year}{Numeric. Year when most recent colony count was conducted.}
#'   \item{minDist}{Numeric. Distance, in kilometers, a bird must travel from colony to be considered in away from colony. Passed to OPPtools::opp_get_trips in analysis.}
#'   \item{maxDist}{Numeric. Distance, in kilometers, a bird must return to colony to be considered in a complete trip. Passed to OPPtools::opp_get_trips in analysis.}
#'   \item{minDur}{Numeric. Time, in hours, a bird must be away from the colony to be considered in a complete trip. Passed to OPPtools::opp_get_trips in analysis.}
#'   \item{gapTime}{Numeric. Time, in hours, between consecutive locations to be considered a gap in tracking. Passed to OPPtools::opp_get_trips in analysis.}
#'   \item{gapDist}{Numeric. Distance, in kilometers, between consecutive locations to be considered a gap in tracking. Passed to OPPtools::opp_get_trips in analysis.}
#'   \item{interpolateGaps}{Logical. Should gaps in location data be interpolated? Passed to OPPtools::opp_get_trips in analysis.}
#'   \item{timestep}{Timestep used for interpolating GPS locations, as text (e.g. 5 min, 1 hour). Passed to OPPtools::opp_get_trips in analysis.}
#'   \item{gridRes}{Numeric. Grid resultion, in kilometers, used in calculating utilizations disributions.}
#'   \item{gridExtend}{Numeric. Distance, in kilometers, used to extend grid used in calculating utilizations disributions.}
#'   \item{kernelSmoother}{Text. Type of kernel smoother used in analysis to calculate utilization distributions. Acceptable values are: href, href/2, and step}
#'   \item{stage}{Text. Describing the breeding stage data apply to. Used in text of report.}
#'   \item{dates}{Text. Describing the date range of the breeding stage for this population. Used in text of report.}
#' }
#'
'cpf_report_params'
