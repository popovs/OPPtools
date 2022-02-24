#' Set Default Analyst Value
#'
#'
#' This function allows you to set the option CIDAtools.analyst permanently
#' (until you change it or reinstall CIDAtools) and will
#' simultanesouly change the default in New Cida Project Template.
#'
#' @param AnalystName A string containing the analyst name
#' @return A message stating the name has been changed.
#' @keywords options Analyst
#' @export
#'

setAnalyst <- function(AnalystName){
  if(!is.character(AnalystName)) stop('Analyst Name must be a character string')
  if(length(AnalystName) > 1) {
    warning('Only First String is Used')
    AnalystName <- AnalystName[1]
  }
  AnalErr <- try(setPermanentAnalyst(AnalystName), silent = T)
  msg1 <- NULL
  if(!is.null(AnalErr)) msg1 <- paste0('Default Analyst can not be ',
                                       'saved permanently.\n',
                                       'You will need to set for each ',
                                       'R session.\n')
  site_path = R.home(component = "home")
  Project_setup <- paste0(site_path,
                          '/library/CIDAtools/rstudio/',
                          'templates/project/proj_setup.dcf')
  if(file.access(Project_setup, 2) == -1)
    stop(paste0(msg1,
                'You do not have permission to change\n',
                'New CIDA Project Template'))
  DCF <- read.dcf(file.path(Project_setup), all = T)
  DCF$Default[DCF$Parameter == 'analyst' &
                !is.na(DCF$Parameter)] <- AnalystName
  write.dcf(DCF, file.path(Project_setup))
  return(paste('The default analyst name has been changed to',
               getOption('CIDAtools.analyst')))
}

#' Get pretty numbers of rows
#'
#'
#' Retrieve the number of rows in dataframe of matrix with commas inserted for
#' nice reports.
#'
#' @param x data frame or matrix
#' @return Number of rows with big.mark = , and trim = T
#' @keywords prettynrow
#' @export
#'



nrowP <- function(x){
  format(nrow(x), big.mark = ',', trim = T)
}

#' Get pretty number of levels
#'
#'
#' Just a wrapper for format(nlevels) with big.mark = , and trim = T
#'
#' @param x factor
#' @return Number of rows with big.mark = , and trim = T
#' @keywords prettynlevels
#' @export
#'

nLevelsP <- function(x){
  format(nlevels(x), big.mark = ',', trim = T)
}

#' Set Default Analyst Value
#'
#'
#' This is an internal function that writes the Default Analyst name to the
#' users Rprofile.
#'
#' @param Name A string containing the analyst name
#'
setPermanentAnalyst <- function(Name){
  options(CIDAtools.analyst = Name)
  fname = file.path("~/.Rprofile")
  opts <- character()
  if(file.exists(fname)){
    opts <- readLines(fname)
  }
  opts[grep('options\\(CIDAtools.analyst = ', opts, invert = T)] -> opts
  opts <- c(opts, paste0("options(CIDAtools.analyst = '",
                           paste0(Name), "')"))
  if(!file.create(fname, showWarnings = F))
    stop()
  writeLines(opts, fname)
}

#' Remove Default Analyst from ~/.Rprofile
#'
#' This function removes the default analyst set with setAnalyst() from the users
#' .Rprofile. If this is the only entry in .Rprofile it will remove the file as well.
#'
#' @param quiet should a message indicating result be returned, if TRUE will only
#' return TRUE or FALSE
#'
#' @return Message indicating sucess or failue
#' @keywords Analyst remove
#' @export
#'
#'
removeAnalyst <- function(quiet = F){
  fname = file.path("~/.Rprofile")
  if(file.access(fname, 4) != 0){
    if(!quiet){
    return('User does not have an Rprofile or Rprofile can not be read')
    }
    return(FALSE)
  }
  opts <- readLines(fname)
  opts[grep('options\\(CIDAtools.analyst = ', opts, invert = T)] -> opts
  if(file.access(fname, 2) != 0){
    if(!quiet){
      return('You do not have permission to write to users Rprofile')
    }
    return(FALSE)
  }
  if(length(opts) == 0){
    file.remove(fname)
    if(!quiet){
    return('Users .Rprofile is empty and was deleted')
    }
    return(TRUE)
  }
  writeLines(opts, fname)
  if(!quiet)
    return('options(CIDAtools.analyst) has been removed from users profile')
  return(TRUE)
}

#' Convert Interval Notation
#'
#' Converts a vector from Interval Notation to less than equal to, less than,
#' etc.
#'
#' @param x a character vector to be converted
#'
#' @return a character vector of same length of x converted
#' @keywords interval notation
#' @export
#'
convertIntervalNotation <- function(x){
  if(!is.character(x)) stop('x must be a character vector')
  x <- gsub('\\(-Inf, ', '', x)
  x <- gsub(',Inf\\)', '', x)
  x <- gsub('\\[', '\u2265', x)
  x <- gsub('([0-9]+)\\]', '\u2264\\1', x)
  x <- gsub(',', ' - ', x)
  x <- gsub("\\(", '>', x)
  x <- gsub("([0-9]+)\\)", "<\\1", x)
  return(x)
}

#' Round and don't drop trailing zeros
#'
#' Shorter wrapper for format(x, digits = n, nsmall = n)
#'
#' @param x numeric to be formatted
#' @param n number of digits for nsmall
#'
#' @return a character vector of same length of x converted
#' @details should not be used unless digits after a decimal are needed.
#' Note for numbers with leading zeros (ie. 0.0349) you will get one more
#' decimal place than n. (ie. \code{Round(O.0349, 2)} will return
#' \code{0.035})
#'
#' @keywords interval notation
#' @export
#'
#'
Round <- function(x, n){
  format(x, digits = n, nsmall = n)
}

#' Sum ignoring NAs
#'
#' Will sum values returning NA only if all values are NA, otherise will ignore
#'
#' @param ... numbers or vectors to be summed. Must be type logical or numeric.
#'
#' @return a numeric vector of the same length as the arguments
#' @details this function will provide vectorized sums with NAs ignored unless
#' only NAs are present
#'
#' @keywords sum
#' @export
#' @examples
#' # ignores NA
#' sum_ignore_NA(2, 3, NA)
#' # returns NA if all values are NA
#' sum_ignore_NA(NA, NA, NA)
#'
#' # returns vectorized sums
#'
#' x <- c(1, 2, NA)
#' y <- c(1:3)
#' sum_xy <- sum_ignore_NA(x, y)
#' data.frame(x, y, sum_xy)
#'
#' x <- c(1, 2, NA)
#' y <- c(1, 2, NA)
#' sum_xy <- sum_ignore_NA(x, y)
#' data.frame(x, y, sum_xy)


sum_ignore_NA <- function(...){
  arguments <- list(...)
  arguments <- lapply(arguments, unlist)
  x <- sapply(arguments, length)
  if(min(x) != max(x)) stop('Vectors must be same length')
  arguments <- lapply(1:min(x), function(i) sapply(arguments, `[[`, i))
  sapply(arguments, function(numbers){
    if(all(is.na(numbers))) return(NA)
    if(!is.numeric(numbers) & !is.logical(numbers))
      stop('Arguments must be numeric or logical')
    sum(numbers, na.rm = T)
  })
}

#' Vectorized power estimates
#'
#'
#' This function allows you to use power.t.test, power.prop.test, etc in
#' vectorized fashion and return a table of results
#'
#' @param fun What is the function to calculate power
#' @param ... other arguments to pass to power_fn, possibly vectorized
#' @return tibble of results
#'
#' @importFrom generics tidy
#' @importFrom stats na.omit
#'
#' @export
#'

vec_power <- function(fun = stats::power.t.test, ...){

  args <- list(...)
  params <- expand.grid(args, stringsAsFactors = FALSE)[,length(args):1]

  results <- tidy(do.call(fun, params[1,]))
  for(i in 1:nrow(params)) {
    res <- try(do.call(fun, params[i,]), silent = TRUE)
    results[i,] <- NA
    if(class(res)[1] != "try-error")
      results[i,] <- tidy(res)
  }

  results <- dplyr::bind_cols(results, params[!(names(params) %in% names(results))])

  return(na.omit(results))
}

# Helper for pwr package version of power fns.
tidy.power.htest <- function(x, ...) {
  class(x) <- "list"
  as.data.frame(x)
}

#' Get CIDA drive path
#'
#' This function attempts to get the proper path for the CIDA drive either on Windows or Mac.
#'
#' @param path (optional) a path to a particular place in the CIDA drive
#'
#' @return full (absolute) file path of CIDA drive
#' @export
#'
#' @examples
#' # Read data from P1234PIname project
#' \dontrun{
#' df <- read.csv(CIDA_drive_path("Projects/P1234PIname/DataRaw/data.csv"))
#' }
#'

CIDA_drive_path <- function(path = ""){

  OS <- .Platform$OS.type

  if (OS == "unix"){
    temp_path <- "/Volumes/CIDA" # MAC file path
  } else if (OS == "windows"){
    temp_path <- "P:/" # windows file path
  } else {
    stop("OS could not be identified")
  }

  fpath <- file.path(temp_path, path)

  if(!dir.exists(fpath) & !file.exists(fpath))
    warning("nothing found at path, check spelling and ensure drive is mounted")

  return(file.path(temp_path, path))
}

# -----

#' Defines a bounding box at a specified zoom level around a set of coordinates
#' @description This function returns a bounding box at a user defined zoom level
#' centered on the mean value of a set of coordinates.
#' @param locs An sf or sp points object. The bounding box will be centered on the mean value of
#' these points
#' @param zoom_level Numeric. Specifies how zoomed in the bounding box should be, eg. 1 = whole world and 4 = 1/4 of world
#' @references Code adapted from: https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/
#'
#' @returns an object with class "bbox" containing four values: xmin, ymin, xmax, and ymax.
#' Values will be in units of locs (either decimal degrees or meters).
#' @export


bbox_at_zoom <- function(locs, zoom_level = 7) {

  if (!(class(locs)[1] %in% c('sf','SpatialPointsDataFrame'))) stop('locs must be an sf or sp points object')

  if (class(locs)[1] == 'SpatialPointsDataFrame') {
    convert_sp <- T
    locs <- as(locs, 'sf')
  }

  if (sf::st_is(locs, "POINT")[1] == F) stop('locs must be an sf or sp points object')

  zoom_to <- sf::st_coordinates(locs) %>%
    as.data.frame() %>%
    dplyr::summarize(
      X = mean(X),
      Y = mean(Y)
    ) %>% sf::st_as_sf(coords = c('X','Y'), crs = sf::st_crs(locs))

  if (sf::st_is_longlat(zoom_to) == T) {
    lon_span <- 360/2^zoom_level
    lat_span <- 180/2^zoom_level
  } else {
    C <- 40075016.686   # ~ circumference of Earth in meters
    lon_span <- C / 2^zoom_level
    lat_span <- C / 2^(zoom_level+1)
  }

  cc <- sf::st_coordinates(zoom_to)

  lon_bounds <- c(cc[,1] - lon_span / 2, cc[,1] + lon_span / 2)
  lat_bounds <- c(cc[,2] - lat_span / 2, cc[,2] + lat_span / 2)

  bb <- sf::st_bbox(c(xmin = lon_bounds[1], xmax = lon_bounds[2],
                  ymax = lat_bounds[1], ymin = lat_bounds[2]), crs = sf::st_crs(locs))

  return(bb)
}

# -----

#' Change YAML header content
#' @description This function updates the YAML content of a given Rmd file.
#' YAML content is passed as a list via the dots argument of the function.
#' @input_file Input .Rmd file with YAML to be updated
#' @output_file Output .Rmd filename with updated YAML (optional; if left blank, updated .Rmd file will be printed in the console.)
#' @references Code from: https://stackoverflow.com/a/66908611/1454785
#' @returns A .Rmd file with updated YAML content, either saved to the `output_file` path location or printed in the console.
#' @export

change_yaml_matter <- function(input_file, ..., output_file) {
  input_lines <- readLines(input_file)
  delimiters <- grep("^---\\s*$", input_lines)
  if (!length(delimiters)) {
    stop("unable to find yaml delimiters")
  } else if (length(delimiters) == 1L) {
    if (delimiters[1] == 1L) {
      stop("cannot find second delimiter, first is on line 1")
    } else {
      # found just one set, assume it is *closing* the yaml matter;
      # fake a preceding line of delimiter
      delimiters <- c(0L, delimiters[1])
    }
  }
  delimiters <- delimiters[1:2]
  yaml_list <- yaml::yaml.load(
    input_lines[ (delimiters[1]+1):(delimiters[2]-1) ])

  dots <- list(...)

  for (element_name in names(dots)){
    if(element_name %in% names(yaml_list)) {
      yaml_list[element_name] <- dots[element_name]
    } else {
      yaml_list <- c(yaml_list,dots[element_name])
    }
  }

  output_lines <- c(
    if (delimiters[1] > 0) input_lines[1:(delimiters[1])],
    strsplit(yaml::as.yaml(yaml_list), "\n")[[1]],
    input_lines[ -(1:(delimiters[2]-1)) ]
  )

  if (missing(output_file)) {
    return(output_lines)
  } else {
    writeLines(output_lines, con = output_file)
    return(invisible(output_lines))
  }
}
