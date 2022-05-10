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
