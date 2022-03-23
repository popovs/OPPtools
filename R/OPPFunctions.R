#' Set Movebank credentials
#'
#' @description This function uses the R library `keyring` to interactively
#' and securely set your Movebank credentials for other functions to use.
#' The credentials are encrypted and stored under the name "OPP-Movebank" in
#' Keychain (Mac) or Credential Store (Windows). If you store your credentials
#' in this way, other OPP functions will no longer prompt you for your Movebank
#' username and password each time they are run.
#'
#' For added security, you can further set a keychain password. This means
#' you will need to enter your keychain password any time an `OPPtools` function
#' attempts to access your stored Movebank credentials. By default this is turned
#' off, as the credentials are securely encrypted by `keyring`.
#'
#' @param username Your Movebank username.
#' @param set_keyring Logical (T/F). Set to TRUE if you would like to further password protect your Movebank credentials. You will be prompted for this password immediately upon running the function.
#' @export

opp_movebank_key <- function(username,
                             set_keyring = FALSE) {
  if(set_keyring == TRUE){
    keyring::keyring_create("OPPtools-keyring")
    keyring::key_set(service = "OPP-Movebank",
                     username = username,
                     keyring = "OPPtools-keyring",
                     prompt = "Movebank password: ")
  } else {
    keyring::key_set(service = "OPP-Movebank",
                     username = username,
                     prompt = "Movebank password: ")
  }


}

# -----

#' Download OPP tracking data from Movebank
#'
#' @description This function downloads OPP tracking data from Movebank and returns a
#' dataframe with combined tracking and reference data for all deployments.
#'
#' @param study List of Movebank project ids.
#' @param login Stored Movebank login credentials if provided, otherwise function
#'will prompt users to enter credentials. If credentials were saved with `opp_movebank_key`,
#'this field will be ignored and login credentials will be automatically retrieved.
#' @param start_month Earliest month (1-12) to include in output.
#' @param end_month Latest month (1-12) to include in output.
#' @param season Vector describing the season data can be applied to, eg. 'Breeding (Jun-Jul)'
#'
#' @details The function can be passed a list of movebank study IDs and will append
#'data from all studies.
#'
#' @examples
#'# download ANMU project data from two studies, for May only
#'my_data <- opp_download_data(study = c(1895716931, 1897273090),
#'                             login = NULL, start_month = 5, end_month = 5,
#'                             season = 'Incubation')
#'
#'@export

opp_download_data <- function(study,
                              login = NULL,
                              start_month = NULL,
                              end_month = NULL,
                              season = NULL
                              ) {

  # If credentials were saved using opp_movebank_key, retrieve them
  if (length(keyring::key_list(service = "OPP-Movebank")$username) == 1) {
    mb_user <- keyring::key_list(service = "OPP-Movebank")$username
    mb_pass <- keyring::key_get("OPP-Movebank", username = mb_user)
    login <- move::movebankLogin(username = mb_user, password = mb_pass)
  }

  # Ask for movebank credentials if not provided
  if (is.null(login)) login <- move::movebankLogin()
  if (is.null(season)) season <- NA

  out_data <- data.frame()

  for (ss in study) {

    # Download data from movebank
    mb_data <- suppressMessages(move::getMovebankData(study = ss, login = login,
                                                      removeDuplicatedTimestamps = TRUE,
                                                      includeExtraSensors = FALSE,
                                                      deploymentAsIndividuals = TRUE,
                                                      includeOutliers = FALSE))

    # Extract fields
    if (!("Argos Doppler Shift" %in% unique(mb_data@data$sensor_type))) {
      loc_data <- as(mb_data, 'data.frame') %>%
          dplyr::select(timestamp, location_long, location_lat, sensor_type,
                        local_identifier, ring_id, taxon_canonical_name, sex,
                        animal_life_stage, animal_reproductive_condition, number_of_events,
                        study_site, deploy_on_longitude, deploy_on_latitude,
                        deployment_id, tag_id, individual_id) %>%
          dplyr::mutate(
            timestamp = as.POSIXct(timestamp), # make times POSIXct for compatibility with OGR
            year = as.numeric(strftime(timestamp, '%Y')),
            month = as.numeric(strftime(timestamp, '%m')), # add numeric month field
            season = season,
            sex = ifelse(sex == '' | sex == ' ' | is.na(sex), 'u', sex)
          )
    } else {
      loc_data <- as(mb_data, 'data.frame') %>%
        dplyr::select(timestamp, location_long, location_lat, sensor_type,
                      argos_iq, argos_lc, argos_lat1, argos_lat2, argos_lon1, argos_lon2,
                      local_identifier, ring_id, taxon_canonical_name, sex,
                      animal_life_stage, animal_reproductive_condition, number_of_events,
                      study_site, deploy_on_longitude, deploy_on_latitude,
                      deployment_id, tag_id, individual_id) %>%
        dplyr::mutate(
          timestamp = as.POSIXct(timestamp), # make times POSIXct for compatibility with OGR
          year = as.numeric(strftime(timestamp, '%Y')),
          month = as.numeric(strftime(timestamp, '%m')), # add numeric month field
          season = season,
          sex = ifelse(sex == '' | sex == ' ' | is.na(sex), 'u', sex)
        )
    }

    # Subset data to months if provided
    if (is.null(start_month) == FALSE) loc_data <- subset(loc_data, loc_data$month >= start_month)
    if (is.null(end_month) == FALSE) loc_data <- subset(loc_data, loc_data$month <= end_month)

    if (mb_data@proj4string@projargs != "+proj=longlat +datum=WGS84 +no_defs") {
      warning(paste('CRS for', ss, 'is not longlat. be careful if joining data from multiple studies in different coordinate systems'), call. = FALSE)
    }

    out_data <- rbind(out_data, loc_data)

  }

  out_data

}

# -----

#' Converts movebank data to format required for track2KBA
#'
#' @description Takes tracking data downloaded from Movebank using OPPTools::opp_download_data
#' and converts it to the format needed for track2KBA
#'
#' @param data A dataframe obtained using OPPTools::opp_download_data
#'
#' @details This extracts location, timestamp and deployment data from movebank data
#' and returns a list of dataframes that can be passed to functions in track2KBA.
#' This is useful if the user wants to filter the data downloaded from movebank based on
#' fields contained within the reference data (e.g. sex, animal_reproductive_condition)
#'
#' @returns Returns a list object of length two, containing tracking data
#' (accessed using: dataset$data) and study site location information
#' (accessed using: dataset$site).
#' @examples
#'my_data <- opp_download_data(study = c(1247096889),login = NULL, start_month = NULL,
#'                             end_month = NULL,season = NULL)
#'
#'my_track2kba <- opp2KBA(data = my_data)
#'
#' @export

opp2KBA <- function(data
) {
  locs <- data %>%
    dplyr::select(deployment_id, timestamp, location_lat, location_long) %>%
    dplyr::rename(ID = deployment_id,
                  DateTime = timestamp,
                  Latitude = location_lat,
                  Longitude = location_long)

  sites <- data %>%
    dplyr::select(deployment_id, deploy_on_latitude, deploy_on_longitude) %>%
    dplyr::rename(ID = deployment_id,
                  Latitude = deploy_on_latitude,
                  Longitude = deploy_on_longitude) %>%
    unique()
  row.names(sites) <- 1:nrow(sites)

  out <- list(data = locs, site = sites)

  out
}

# -----

#' Prepare raw Ecotone data for Movebank upload.
#'
#' This function modifies raw Ecotone GPS data to remove
#' any records without lat/long values, inserts a "behavior"
#' column to indicate when a tagged bird is at the colony,
#' adds a timestamp column ("Date_2") if not already there,
#' and removes any duplicate detections. The function also
#' inserts lat/long coordinates for the colony location for
#' periods when the bird is at the colony.
#'
#'
#'@param data Input Ecotone data to be modified.
#'@param colony_lon Longitude of home colony of tagged bird.
#'@param colony_lat Latitude of home colony of tagged bird.
#'@param tz Timezone of GPS timestamps. Default "UTC".
#
#'@export

prep_ecotone <- function(data,
                         colony_lon,
                         colony_lat,
                         tz = "UTC") {
  data$Latitude[data$In.range == 1] <- colony_lat
  data$Longitude[data$In.range == 1] <- colony_lon
  data$Behaviour <- ifelse(data$In.range == 1, 'At colony', NA)
  data <- subset(data, !is.na(data$Latitude))

  if(any(grepl("Date_2", names(data))) == FALSE){
    data$Date_2 <- as.POSIXct(paste0(data$Year, "-",
                                     data$Month, "-",
                                     data$Day, " ",
                                     data$Hour, ":",
                                     data$Minute, ":",
                                     data$Second),
                              format = "%Y-%m-%d %H:%M:%S",
                              tz = tz)
  }

  data <- data[duplicated(data[,c('Logger.ID','Date_2')]) == F,]
  data
}

# -----

#' Prepare Pathtrack data for Movebank upload.
#'
#' This simple function processes Pathtrack data that has
#' been exported from Pathtrack Host software for Movebank
#' upload. It removes any records in Pathtrack data that
#' have `null` latitude or longitude values.
#' Unlike `prep_ecotone()`, this function makes no assumptions
#' on the bird's location when latitude/longitude are null.
#'
#'
#'@param data Input Pathtrack data to be modified.
#
#'@export

prep_pathtrack <- function(data) {
  data <- data[!is.na(data$Lat > 0) & !is.na(data$Long > 0),]
  data
}

# -----

#' Define a custom equal-area CRS centered on your study site
#'
#' @description This function takes a Movebank data object and
#' creates an equal-area projection centered on the Movebank
#' deploy on locations. In the case of central-place foraging
#' seabirds, this effectively equates to a CRS centered on the
#' seabird colony. In cases where multiple deploy on locations
#' are present within the data it centers of the projection on
#' the mean latitude and longitude of all deployment locations.
#' The function returns a proj4 string.
#'
#' @param data Movebank data as returned by opp_download_data.
#'
#' @examples
#' data(murres)
#' colCRS(murres)
#'
#' @export

colCRS <- function(data) {
  return(paste0(
    '+proj=laea',
    ' +lat_0=', mean(data$deploy_on_latitude),
    ' +lon_0=', mean(data$deploy_on_longitude)
  ))
}

# -----

#' Plot raw tracks from Movebank download
#'
#' @description Quickly plot Movebank data downloaded
#' using opp_download_data to visualize tracks.
#'
#' @param data Movebank data as returned by opp_download_data.
#'
#' @examples
#' data(murres)
#' opp_map(murres)
#'
#' @export

opp_map <- function(data,
                    opp_sites = NA) {

  # Check if maps installed
  # maps is used to add simple land features to map
  if (!requireNamespace("maps", quietly = TRUE)) {
    stop("Packages \"maps\"is needed. Please install it.",
         call. = FALSE)
  }
  # Check if mapview is installed
  # mapview is used for interactive mode
  # if (interactive == TRUE){
  #   if (!requireNamespace("mapview", quietly = TRUE)) {
  #     stop("Packages \"mapview\"is needed. Please install it.",
  #          call. = FALSE)
  #   }
  # }

  # Trim down dataset
  site <- unique(data[,c("deploy_on_longitude", "deploy_on_latitude")])
  data <- data[,c("deployment_id", "location_long", "location_lat")]

  # Make ID factor so it plots w appropriate color scheme
  data$deployment_id <- as.factor(data$deployment_id)

  # Convert Movebank data df to sf object
  raw_tracks <- sf::st_as_sf(data,
                             coords = c("location_long", "location_lat"),
                             crs = '+proj=longlat')

  # Extract bounds
  coordsets <- sf::st_bbox(raw_tracks)

  trackplot <- ggplot2::ggplot(raw_tracks) +
    ggplot2::borders("world",
                     colour = "black",
                     fill = grey(0.9),
                     size = 0.3) +
    ggplot2::geom_sf(data = raw_tracks,
                     ggplot2::aes(col = deployment_id),
                     size = 0.3,
                     alpha = 0.75,
                     fill = NA) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::coord_sf(xlim = c(coordsets$xmin, coordsets$xmax),
                      ylim = c(coordsets$ymin, coordsets$ymax),
                      expand = TRUE) +
    ggplot2::geom_point(data = site,
                        ggplot2::aes(x = deploy_on_longitude,
                                     y = deploy_on_latitude),
                        fill = "tomato",
                        color = "black",
                        pch = 21,
                        size = 2.5) +
    ggplot2::theme_light() +
    ggplot2::theme(text = ggplot2::element_text(size = 10)) +
    ggplot2::guides(color = "none") +
    ggplot2::ylab("Latitude") +
    ggplot2::xlab("Longitude")

  if (!missing(opp_sites)) {
    if(class(opp_sites)[1] != "sf") {
      warning("Could not add opp_sites to map. Are you sure you provided a polygon output from `opp_sites()`?")
    } else {
      opp_sites$p_contour <- 100 - opp_sites$percentile
      trackplot <- trackplot + ggnewscale::new_scale_color() +
        ggplot2::geom_sf(data = opp_sites[!is.na(opp_sites$p_contour),],
                         ggplot2::aes(col = as.factor(p_contour)),
                         size = 1,
                         fill = NA) +
        fishualize::scale_color_fish_d("% population contour", option = "Scarus_hoefleri", end = 0.4, direction = -1) +
        ggplot2::geom_point(data = site,
                            ggplot2::aes(x = deploy_on_longitude,
                                         y = deploy_on_latitude),
                            fill = "tomato",
                            color = "black",
                            pch = 21,
                            size = 2.5) +
        ggplot2::coord_sf(xlim = c(coordsets$xmin, coordsets$xmax),
                          ylim = c(coordsets$ymin, coordsets$ymax),
                          expand = TRUE)
    }
  }

  print(trackplot)

  # if(interactive == FALSE){
  #   print(trackplot)
  # } else {
  #   mapview::mapview(raw_tracks, zcol = "deployment_id")
  # }

}

# -----

#' Explore trip data within given tracks
#'
#' @description This function calculates the distance from the
#' study site for each GPS point within a Movebank object and
#' then produces track time vs. distance from origin site plots.
#' Using this function will allow you to assign reasonable
#' estimates for minimum and maximum trip duration and distance
#' for the opp_get_trips function.
#'
#' @param data Movebank data as returned by opp_download_data.
#'
#' @examples
#' data(murres)
#' opp_explore_trips(murres)
#'
#' @export

opp_explore_trips <- function(data) {

  # Make ID factor so it plots w appropriate color scheme
  data$deployment_id <- as.factor(data$deployment_id)

  # Create custom equal-area CRS centered on colony
  colCRS <- colCRS(data)

  # Extract deploy on sites as GPS trips origin
  # If there's only one deploy on loc, origin will be
  # one point. Otherwise it will be a point for each
  # deployment_id.
  if (nrow(unique(data[,c("deploy_on_longitude", "deploy_on_latitude")])) == 1){
    origin <- unique(data[,c("deploy_on_longitude", "deploy_on_latitude")]) %>%
      sf::st_as_sf(coords = c("deploy_on_longitude", "deploy_on_latitude"),
                   crs = '+proj=longlat') %>%
      sf::st_transform(crs = colCRS)
  } else {
    origin <- unique(data[,c("deployment_id", "deploy_on_longitude", "deploy_on_latitude")]) %>%
      sf::st_as_sf(coords = c("deploy_on_longitude", "deploy_on_latitude"),
                   crs = '+proj=longlat') %>%
      sf::st_transform(crs = colCRS)
  }

  # Convert Movebank data df to sf object
  raw_tracks <- sf::st_as_sf(data,
                             coords = c("location_long", "location_lat"),
                             crs = '+proj=longlat') %>%
    sf::st_transform(crs = colCRS)

  # Add distance to colony as column
  if (nrow(origin) == 1) {
    raw_tracks$ColDist <- sf::st_distance(raw_tracks$geometry,
                                          origin) %>%
      as.numeric()
  } else {

    ColDist <- numeric(0)

    for (id in origin$deployment_id) {

      o <- origin[origin$deployment_id == id, ]
      t <- raw_tracks[raw_tracks$deployment_id == id, ]

      ColDist <- append(ColDist,
                        sf::st_distance(t, o) %>%
                          as.numeric()
      )

    }

    raw_tracks$ColDist <- ColDist
  }

  # Plot 4 plots per page
  bb <- unique(raw_tracks$deployment_id)
  idx <- seq(1,length(bb), by = 4)

  for (i in idx) {

    plotdat <- raw_tracks[raw_tracks$deployment_id %in% bb[i:(i+3)],]

    p <- ggplot2::ggplot(plotdat,
                         ggplot2::aes(x = timestamp,
                                      y = ColDist/1000)) +
      ggplot2::geom_point(size = 0.5, col = "black")  +
      ggplot2::facet_wrap(facets = . ~ deployment_id, nrow = 2, scales = 'free') +
      ggplot2::labs(x = 'Time', y = 'Distance from colony (km)') +
      ggplot2::scale_x_datetime(date_labels = '%b-%d') +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::theme_light() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 8)
      )

    print(p)
    #readline('')
  }
  message('Use back arrow in plot pane to browse all plots')

}

# -----

#' Identify foraging trips in tracking data

#' @description Uses criteria related to distance from colony, trip duration, and size of gaps
#' in tracking data to identify and classify trips from a nest or colony. It is
#' a wrapper for track2KBA::tripSplit that applies custom criteria for classifying
#' trips.
#'
#' @param data Tracking data formated using track2KBA or opp2KBA
#' @param innerBuff Minimum distance (km) from the colony to be in a trip.
#' Used to label trips as 'Non-trip'. Defaults to 5
#' @param returnBuff Outer distance (km) to capture trips that start and end
#' away from the colony. Used to label trips as 'Incomplete'. Defaults to 20.
#' @param duration Minimum trip duration (hrs)
# @param missingLocs Proportion (0-1) of trip duration that a gap in consecutive
# locations should not exceed. Used to label trips as 'Gappy'. Defaults to 0.2.
#' @param gapTime Time (hrs) between successive locations at which trips will be flagged as 'Gappy'.
#' Used in connection with gapDist, such that locations must be farther apart in
#' both time and space to be considered a gap.
#' @param gapDist Distance (km) between successive locations at which trips will be flagged as 'Gappy'.
#' Used in connection with gapTime, such that locations must be farther apart in
#' both time and space to be considered a gap.
#' @param gapLimit Maximum time between points to be considered too large to be
#' a contiguous tracking event. Can be used to ensure that deployments on the
#' same animal in different years do not get combined into extra long trips.
#' Defaults to 100 days.
#' @param showPlots Logical (T/F), should plots showing trip classification by generated?
#' @param plotsPerPage Numeric indicating the number of individuals to include
#' in a single plot. Defaults to 4.
#'
#' @details This returns a SpatialPointDataFrame in a longlat projection. Most fields in the dataframe
#' come from the output of track2KBA::tripSplit. This function also adds fields for:
#' \itemize{
#' \item{DiffTime} {- Difference in hours between locations}
#' \item{DiffDist} {- Difference in distance between locations}
#' \item{Type} {- Type of trip: Non-trip, Complete, Incomplete, or Gappy}
#' \item{TripSection} {- An integer index noting sections of a the trip that are separated by gaps}
#'}
#' Gaps in trips are defined as any pair of locations that are farther apart in time than gapTime and
#' farther apart in space than gapDist.
#'
#'
#'
#' @examples
#'my_data <- opp_download_data(study = c(1247096889),login = NULL, start_month = NULL,
#'                             end_month = NULL,season = NULL)
#'
#'my_track2kba <- opp2KBA(data = my_data)
#'
#'my_trips <- opp_get_trips(data = my_track2kba, innerBuff  = 5, returnBuff = 20,
#'                          duration  = 2, gapLimit = 100, gapTime = 2, gapDist = 5,
#'                          showPlots = TRUE)
#' @export


opp_get_trips <- function(data,
                          innerBuff, # (km) minimum distance from the colony to be in a trip
                          returnBuff, # (km) outer buffer to capture incomplete return trips
                          duration, # (hrs) minimum trip duration
                          # missingLocs = 0.2, # Percentage of trip duration that a gap in consecutive locations should not exceed
                          gapTime = 1,
                          gapDist = 5,
                          gapLimit = 100,
                          showPlots = TRUE,
                          plotsPerPage = 4
) {

  trips <- track2KBA::tripSplit(
    dataGroup  = data$data, # data formatted using formatFields()
    colony     = data$site, # data on colony location - can be extracted from movebank data using move2KBA()
    innerBuff  = innerBuff,      # (km) minimum distance from the colony to be in a trip
    returnBuff = returnBuff,     # (km) outer buffer to capture incomplete return trips
    duration   = duration,      # (hrs) minimum trip duration
    gapLimit = gapLimit, # (days) time between points to be considered too large to be a contiguous tracking event
    rmNonTrip  = F,    # T/F removes times when not in trips
    nests = ifelse(nrow(data$site) > 1, TRUE, FALSE)
  )

  trips <- trips[order(trips$ID, trips$DateTime),]
  trips$tripID[trips$ColDist <= innerBuff * 1000] <- -1

  trips_type <- trips@data %>%
    dplyr::group_by(ID, tripID) %>%
    dplyr::mutate(
      tripdur = as.numeric(difftime(max(DateTime), min(DateTime), units = 'hour')),
      dt = as.numeric(difftime(DateTime, dplyr::lag(DateTime), units = 'hour')),
      dt = ifelse(is.na(dt), 0, dt),
      dist = getDist(lon = Longitude, lat = Latitude),
      flag = ifelse(dt > gapTime & dist > gapDist * 1000, 1, 0),
      trip_section = 1 + cumsum(flag),
      n = dplyr::n(),
      tripTime = as.numeric(difftime(max(DateTime), min(DateTime), units = 'hour')),
      Type = NA,
      Type = ifelse(ColDist[1] > returnBuff * 1000 | ColDist[dplyr::n()] > returnBuff * 1000, 'Incomplete', Type),
      Type = ifelse(max(flag, na.rm = T) > 0, 'Gappy', Type),
      Type = ifelse(tripID == -1, 'Non-trip', Type),
      Type = ifelse(n < 3, 'Non-trip', Type),
      Type = ifelse(is.na(Type), 'Complete', Type)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      tripID = ifelse(tripdur < duration | n < 3, -1, tripID),
      Type = ifelse(tripdur < duration, 'Non-trip', Type),
    )

  trips$DiffTime <- trips_type$dt
  trips$DiffDist <- trips_type$dist
  trips$Type <- trips_type$Type
  trips$tripID <- trips_type$tripID
  trips$TripSection <- trips_type$trip_section

  bb <- unique(trips_type$ID)
  idx <- seq(1,length(bb), by = plotsPerPage)
  dummy <- data.frame(Type = c('Non-trip', 'Incomplete', 'Gappy', 'Complete'))

  if (showPlots == TRUE) {
    plot_trip_dist(trips, plotsPerPage = plotsPerPage, showPlots = showPlots,
                   innerBuff = innerBuff, returnBuff = returnBuff)
  }

  return(trips)
}


# -----

#' Interpolate GPS locations at a set time interval using a continuous time correlated
#' random walk (ctcrw) model
#'
#' @description This function is a wrapper for momentuHMM::crawlWrap(), which
#' uses the crawl package to fit ctcrw model to GPS tracks at a user-defined
#' time interval. The function is currently designed to handle GPS data from
#' central place foraging birds. It takes tracking data, where trips have been
#' identified and classified using OPPTools::opp_get_trips(). The function
#' returns a list with four objects: (1) original tracking data (as SPDF),
#' (2) colony location (as SPDF), (3) interpolated locations (as SPDF), and (4)
#' a list of CRAWL fits for each trip. All spatial objects are in the same custom
#' Lambert equal area projection centered on the colony.
#'
#'
#'@param data Trip data ouptut from OPPTools::opp_get_trips().
#'@param site Vector containing coordinates of the study site, in the same
#'format as site information returned by OPPtools::opp2KBA or track2KBA::move2KBA.
#'@param type List indicating the types of trips to include in interpolation.
#'Possible values are: 'Complete', 'Incomplete', 'Gappy', and 'Non-trip'. Default is 'Complete'.
#'@param timestep string indicating time step for track interpolation, eg. '10 min', '1 hour', '1 day'
#'@param showPlots TRUE/FALSE should plots of interpolated tracks against original data be produced
#'@param plotsPerPage Numeric indicating the number of individuals to include
#' in a single plot. Defaults to 4.
#'@param theta starting values for ctcrw parameter optimization, see ?crawl::crwMLE for details
#'
#'@examples
#'my_data <- opp_download_data(study = c(1247096889),login = NULL, start_month = NULL,
#'                             end_month = NULL,season = NULL)
#'
#'my_track2kba <- opp2KBA(data = my_data)
#'
#'my_trips <- opp_get_trips(data = my_track2kba, innerBuff  = 5, returnBuff = 20,
#'                          duration  = 2, gapLimit = 100, gapTime = 2, gapDist = 5,
#'                          showPlots = TRUE)
#'
#'
#'
#'my_interp <- ctcrw_interpolation(data = my_trips,
#'                    site = my_track2kba$site,
#'                    type = c('Complete','Incomplete','Gappy'),
#'                    timestep = '10 min',
#'                    interpolateGaps = T,
#'                    showPlots = T,
#'                    plotsPerPage = 2,
#'                    theta = c(8,2)
#')
#'
#'@export

ctcrw_interpolation <- function(data,
                                site,
                                type,
                                timestep,
                                interpolateGaps = TRUE,
                                showPlots = TRUE,
                                plotsPerPage = 4,
                                theta = c(8, 2),
                                quiet = TRUE
) {
  # Generate custom laea projection centered on colony
  myCRS <- paste0(
    '+proj=laea',
    ' +lat_0=', mean(site$Latitude),
    ' +lon_0=', mean(site$Longitude)
  )

  # Create SpatialPoints object for colony
  site_loc <- sp::SpatialPointsDataFrame(site[,c('Longitude','Latitude')], data = site,
                                         proj4string = sp::CRS('+proj=longlat'))
  site_loc <- sp::spTransform(site_loc, myCRS)

  # Create SpatialPoints object of raw tracking data
  orig_loc <- sp::spTransform(data, myCRS)
  # re-calculate distance from colony for all original locations
  if (nrow(site_loc) == 1)  orig_loc$ColDist <- sp::spDistsN1(orig_loc, site_loc)
  if (nrow(site_loc) > 1) {
    orig_loc$ColDist <- NA
    for (id in site_loc$ID) {
      orig_loc$ColDist[orig_loc$ID == id] <- sp::spDistsN1(orig_loc[orig_loc$ID == id,], site_loc[site_loc$ID == id,])
    }
  }

  interp_loc <- subset(orig_loc, orig_loc$Type %in% type)
  interp_loc$time <- interp_loc$DateTime
  interp_loc$Bird <- interp_loc$ID
  interp_loc$ID <- interp_loc$tripID
  if (interpolateGaps == FALSE)   {
    interp_loc$ID <- paste0(interp_loc$tripID,'.',interp_loc$TripSection)
    tt <- table(interp_loc$ID)
    interp_loc <- subset(interp_loc, !(interp_loc$ID %in% names(tt)[tt < 3]))
  }
  interp_loc <- interp_loc[,c('Bird', 'ID', 'time', 'ColDist', 'Type')]

  if (quiet == TRUE) {
    invisible(capture.output(crwOut <- momentuHMM::crawlWrap(obsData = interp_loc,
                                                             timeStep = timestep,
                                                             theta = theta,
                                                             fixPar = c(NA,NA),
                                                             method = 'Nelder-Mead')))
  } else {
    crwOut <- momentuHMM::crawlWrap(obsData = interp_loc,
                                    timeStep = timestep,
                                    theta = theta,
                                    fixPar = c(NA,NA),
                                    method = 'Nelder-Mead')
  }

  pred <- data.frame(crwOut$crwPredict) %>%
    dplyr::filter(locType == 'p') %>%
    dplyr::select(Bird, ID, time, ColDist, mu.x, mu.y, se.mu.x, se.mu.y, Type) %>%
    tidyr::separate('ID', c('Bird', NA), sep = '_', remove = FALSE) %>%
    dplyr::rename(tripID = ID, ID = Bird, DateTime = time)

  pred$Type <- zoo::na.locf(pred$Type)

  if (interpolateGaps == F) pred <- tidyr::separate(pred, 'tripID', c('tripID', NA), sep = '[.]', remove = FALSE)

  pred <- sp::SpatialPointsDataFrame(coords = pred[,c('mu.x', 'mu.y')],
                                     data = pred[,c('ID', 'tripID', 'DateTime', 'ColDist',
                                                    'Type', 'mu.x', 'mu.y',
                                                    'se.mu.x', 'se.mu.y')],
                                     proj4string = sp::CRS(myCRS)
  )

  pred_longlat <- sp::spTransform(pred, sp::CRS('+proj=longlat'))
  pred$Longitude <- sp::coordinates(pred_longlat)[,1]
  pred$Latitude <- sp::coordinates(pred_longlat)[,2]

  # re-calculate distance from colony for all interpolated locations
  if (nrow(site_loc) == 1)  pred$ColDist <- sp::spDistsN1(pred, site_loc)
  if (nrow(site_loc) > 1) {
    pred$ColDist <- NA
    for (i in 1:nrow(site_loc)) {
      pred$ColDist[pred$ID == site_loc$ID[i]] <- sp::spDistsN1(pred[pred$ID == site_loc$ID[i],], site_loc[site_loc$ID == site_loc$ID[i],])
    }
  }

  out <- list(
    data = orig_loc,
    site = site_loc,
    interp = pred,
    crawl_fit = crwOut$crwFits
  )

  if (showPlots == T) {
    pp <- plot_interp_dist(data = out, showPlots = showPlots, plotsPerPage = plotsPerPage)
 }
  return(out)
}


# -----

#' Calculate trip summaries
#'
#' @description `sum_trips` quickly calculates summary information
#' such as maximum distance from the colony, trip start time,
#' trip end time, and trip duration for each individual trip ID.
#' The function accepts outputs from either `opp_get_trips` or
#' `ctcrw_interpolation`. If interpolated data are provided, the
#' output provides a summary of interpolated trips.
#'
#'
#'@param data Trip data output from opp_get_trips() or ctcrw_interpolation().
#'
#'@examples
#'my_data <- opp_download_data(study = c(1247096889),login = NULL, start_month = NULL,
#'                             end_month = NULL,season = NULL)
#'
#'my_track2kba <- opp2KBA(data = my_data)
#'
#'my_trips <- opp_get_trips(data = my_track2kba, innerBuff  = 5, returnBuff = 20,
#'                          duration  = 2, gapLimit = 100, gapTime = 2, gapDist = 2,
#'                          showPlots = TRUE)
#'
#'my_interp <- ctcrw_interpolation(data = my_trips,
#'                                 site = my_track2kba$site,
#'                                 type = c('Complete','Gappy'),
#'                                 timestep = '10 min',
#'                                 showPlots = T,
#'                                 theta = c(8,2),
#'                                 quiet = TRUE
#')
#'
#'sum_trips(my_trips)
#'sum_trips(my_interp)
#'
#'@import data.table
#'@export

sum_trips <- function(data) {
  # This is an improved version of track2KBA::tripSummary using data.table
  # TO-DO: add support to calc total_distance & direction for outputs

  # First check if output is from opp_get_trips vs. ctcrw_interpolation
  if (class(data) == "SpatialPointsDataFrame") {

    # If it's the output from opp_get_trips
    tripSum <- data.table::setDT(data@data)[tripID != -1, .(n_locs = .N, departure = min(DateTime), return = max(DateTime), max_dist_km = (max(ColDist))/1000, complete = unique(Type)), by = list(ID, tripID)]
    tripSum$duration <- as.numeric(difftime(tripSum$return, tripSum$departure, units = 'hours'))
    tripSum <- tripSum %>% dplyr::select(ID, tripID, n_locs, departure, return, duration, max_dist_km, complete)

  } else if (class(data) == "list") {
    # If it's the output from ctcrw_interpolation
    raw_trips <- data$data@data
    interp_trips <- data$interp@data

    # For now since interp does not return trip type, assuming
    # it's all "complete trip"
    tripSum <- data.table::setDT(interp_trips)[, .(interp_n_locs = .N, departure = min(DateTime), return = max(DateTime), max_dist_km = (max(ColDist))/1000), by = list(ID, tripID)]
    tripSum$duration <- as.numeric(difftime(tripSum$return, tripSum$departure, units = 'hours'))

    raw_n_locs <- data.table::setDT(raw_trips)[tripID != -1, .(raw_n_locs = .N, complete = unique(Type)), by = list(ID, tripID)]
    raw_n_locs$ID <- as.character(raw_n_locs$ID)

    tripSum <- merge(tripSum, raw_n_locs, by = c("ID", "tripID"))

    tripSum <- tripSum %>% dplyr::select(ID, tripID, raw_n_locs, interp_n_locs, departure, return, duration, max_dist_km, complete)
    message("Trip summary provided for interpolated data.")

  } else {
    message("Error: Cannot calculate trip summary. Input data must be the output from either opp_get_trips or ctcrw_interpolation.")
  }

  return(tripSum)
}

# -----

#' Calculate traditional kernel density estimates
#'
#' @description This function is a wrapper of track2KBA::estSpaceUse
#' (which itself is a wrapper of adehabitatHR::kernelUD) to
#' calculate a traditional kernel density estimates
#' on a given set of trips. The function accepts outputs
#' from either opp_get_trips or ctcrw_interpolation.
#' If provided an output from ctcrw_interpolation, the function
#' will calculate kernels using the interpolated tracks by default.
#' This behavior can be changed by setting `interpolated = FALSE`.
#' The default kernel smoother is calculated using the href method.
#' The default UD level is 50%.
#'
#' @param data Tracks to calculate kernels on. Accepts output from either opp_get_trips or ctcrw_interpolation.
#' @param extendGrid Numeric. Distance (km) to expand grid beyond the bounding box of tracking data. Default 10km.
#' @param interpolated Logical (T/F). If provided an output from ctcrw_interpolation, should the interpolated tracks
#' be used for kernel calculation? Default TRUE. This parameter is ignored if the function is provided data from opp_get_tracks.
#' @param smoother Smoother value used in kernel calculations, either a numeric value or 'href', 'href/2', or 'step'. By default uses
#' the calculated href value of the tracks. Using 'step' will use the median step length across tracks.
#' @param res Grid resolution in sq km to use for kernel calculations.
#'
#' @export

opp_kernel <- function(data,
                       interpolated = TRUE,
                       extendGrid = 10,
                       smoother = "href",
                       res = 1) {


  # Check data inputs
  if (interpolated == TRUE) {
    # If interpolated is TRUE, pull out interp df from
    # ctcrw_interpolation output
    kd_data <- data$interp
  } else if (interpolated == FALSE & (class(data) == "list")) {
    # If interpolated is FALSE, but the output provided
    # is still a ctcwr_interpolation output (i.e. a "list")
    kd_data <- data$data
  } else {
    # Otherwise assume the output is from opp_get_trips,
    # i.e. a single SpatialPointsDataFrame
    kd_data <- data
  }

  # Data health check
  if (sp::is.projected(kd_data) == FALSE) {
    stop("Trips data must be in an equal-area projected coordinate system.")
  }

  # Calculate smoother
  # Code taken from track2KBA::findScales
  if (smoother == "href") {
    s <- opp_href(data = kd_data)
  }

  if (smoother == "href/2") {
    s <- opp_href(data = kd_data) / 2
  }

  if (smoother == "step") {
    s <- opp_step(data = kd_data)
  }

  if (is.numeric(smoother)) s <- smoother

  if (is.null(s)) stop("smoother must be a numeric value, 'step', 'href', or href/2.")

  my_grid <- createGrid(data = kd_data, res = res,
                        extendGrid = extendGrid)

  tracks <- kd_data
  tracks@data <- tracks@data %>% dplyr::select(ID)

  # Checking if all or any tracks have less than 5 locations. Tracks with less than 5 locations are not run
  validIDs <- names(which(table(tracks$ID) > 5))
  tracks <- tracks[(tracks@data$ID %in% validIDs), ]
  tracks@data$ID <- droplevels(as.factor(tracks@data$ID))
  out <- adehabitatHR::kernelUD(tracks ,
                                h = (s * 1000),
                                grid = my_grid,
                                same4all = F)

  tryCatch({
    KDE_sp <- adehabitatHR::getverticeshr(out, percent = 99,
                                          unin = "m", unout = "km2")
  }, error = function(e) {
    stop(paste("The grid is too small to allow the estimation of a complete home-range.
            Please use a larger value than", extendGrid, "km for extendGrid, try a larger value for the smoother, or a smaller grid resolution."), call. = FALSE)
  })

  # # Calculate kernel
  # # This part of the function simply makes a call to
  # # track2KBA::estSpaceUse
  # out <- track2KBA::estSpaceUse(tracks = kd_data,
  #                               scale = href,
  #                               levelUD = ud_level,
  #                               res = res)

  return(out)
  if (interpolated == TRUE) {
    message("Kernels calculated for interpolated tracks.")
  } else {
    message("Kernels calculated for raw tracks.")
  }

}

# -----

#' Calculate median step length for kernel density estimates
#'
#' @param data SpatialPointsDataFrame containing projected tracking data,
#' with an ID field indicating unique trips
#'
#'
#' @export

opp_step <- function(data) {

  # Data health check
  if (sp::is.projected(data) == FALSE) {
    stop("Trips data must be in an equal-area projected coordinate system.")
  }

 out <- data@data %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(
      dist = getDist(lon = Longitude, lat = Latitude),
      med_step = median(dist, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(
      step = round(median(med_step)/1000, 2)
    )

 out$step
}

# -----

#' Calculate median href for kernel density estimates
#'
#' @param data SpatialPointsDataFrame containing projected tracking data,
#' with an ID field indicating unique trips
#'
#'
#' @export

opp_href <- function(data) {

  # Data health check
  if (sp::is.projected(data) == FALSE) {
    stop("Trips data must be in an equal-area projected coordinate system.")
  }

  IDs <- unique(data$ID)
  href_list <- vector(mode = "list", length(IDs))
  href_list <- lapply(move::split(data, data$ID), function(x) {
    xy <- sp::coordinates(x)
    varx <- stats::var(xy[, 1])
    vary <- stats::var(xy[, 2])
    sdxy <- sqrt(0.5 * (varx + vary))
    n <- nrow(xy)
    ex <- (-1/6)
    href <- sdxy * (n^ex)
    return(href)
  })
  hrefs <- do.call(rbind, href_list)
  href <- median(na.omit(hrefs))
  href <- round(href/1000, 2)
  return(href)

}

# -----

#' Create a base grid for kernel calculations
#'
#' This helper function takes the output from either opp_get_trips
#' or ctcrw_interpolation and produces a base grid from the trips.
#' Grid resolution is provided in meters. If ctcrw_interpolation
#' output is provided, by default the extents are calculated from
#' the raw (non-interpolated) data.
#'
#' @param data Output from either ctcrw_interpolation.
#' @param res Numeric. Resolution in km of grid cells.
#' @param extendGrid Numeric. Distance (km) to expand grid beyond the bounding box of tracking data. Default 10km.
#' @param interpolated Logical (T/F). If output from ctcrw_interpolation is provided, should the raw or interpolated data be used for calculating the grid extent? This parameter is ignored if opp_get_trips data is provided.
#'
#' @export

createGrid <- function(data,
                       res = 1,
                       extendGrid = 10){

  # Data health check
  if (sp::is.projected(data) == FALSE) {
    stop("Data must be the output from ctcrw_interpolation.")
  }

  bounds <- sp::bbox(data)

  x <- seq(floor(bounds[1,1] - (extendGrid * 1000)),
           ceiling(bounds[1,2] + (extendGrid * 1000)),
           by = res * 1000)
  y <- seq(floor(bounds[2,1] - (extendGrid * 1000)),
           ceiling(bounds[2,2] + (extendGrid * 1000)),
           by = res * 1000)
  xy <- expand.grid(x=x,y=y)
  sp::coordinates(xy) <- ~x+y # bit of a mess mixing sp & sf, not ideal
  sp::proj4string(xy) <- data@proj4string
  sp::gridded(xy) <- TRUE # make into a spatialPixels object

  return(xy)

}

# -----

#' Calculate Brownian Bridge Movement Model (bbmm) kernels
#'
#' @description This function is a wrapper of adehabitatHR::kernelbb
#' and calculates Brownian Bridge Movement Model (bbmm) kernels for
#' a given set of tracks.
#'
#' @param data Trips, as output by either opp_get_trips or ctcrw_interpolation.
#' @param sig2 Numeric. `sig2` parameter in the bbmm model, typically expressed as GPS error in meters.
#' @param bridgeSmooth Numeric. Parameter to over-smooth bbmm kernels.
#' @param res Numeric. Resolution in km of base kernel grid.
#' @param extendGrid Numeric. Distance (km) to expand grid beyond the bounding box of tracking data. Default 10km.
#' @param useGappy Logical (T/F). Should "Gappy" trips as identified by opp_get_trips be included in bbmm? Default TRUE. If FALSE, only trips identified as "Complete" are used. Note this parameter is ignore if using interpolated data, where by definition the gaps have been interpolated over.
#' @param interpolated Logical (T/F). If an output from ctcrw_interpolation is provided, should the interpolated data be used? Default is TRUE.
#' @param showLiker Logical (T/F). Show plot outputs from adehabitatHR::liker (used to calculate bbmm `sig1` parameter)? Default FALSE.
#'
#' @export

opp_bbmm <- function(data, # Output from either opp_get_trips or ctcrw_interpolation
                     sig2 = 20, # GPS error
                     bridgeSmooth = 1, # parameter to over-smooth sig1 values, to make larger KDE patches
                     res, # resolution in km of bbmm kernel grid
                     extendGrid = 10, # Numeric. Distance (km) to expand grid beyond the bounding box of tracking data. Default 10km.
                     useGappy = TRUE, # should "Gappy" trips as identified by opp_get_trips be included in bbmm? Default TRUE. If FALSE, only trips identified as "Complete" are used. Note this parameter is ignore if using interpolated data, where by definition the gaps have been interpolated over.
                     interpolated = TRUE, # use interpolated data? default is FALSE, as bbmm methods do not rely on even sampling interval
                     showLiker = FALSE, # show plot outputs from adehabitatHR::liker? Default FALSE.
                     ...) {
  # Check data inputs
  if (interpolated == TRUE) {
    # If interpolated is TRUE, pull out interp df from
    # ctcrw_interpolation output
    kd_data <- data$interp
  } else if (interpolated == FALSE & (class(data) == "list")) {
    # If interpolated is FALSE, but the output provided
    # is still a ctcwr_interpolation output (i.e. a "list")
    kd_data <- data$data
  } else {
    # Otherwise assume the output is from opp_get_trips,
    # i.e. a single SpatialPointsDataFrame
    kd_data <- data
  }

  # This only calculates bbmm on complete trips
  if (useGappy == TRUE) {
    kd_data <- kd_data[kd_data$Type %in% c("Complete", "Gappy"), ]
  } else {
    kd_data <- kd_data[kd_data$Type == "Complete", ]
  }

  # Convert data to ltraj object for kernelbb functions
  kd_data.ltraj <- adehabitatLT::as.ltraj(xy = sp::coordinates(kd_data),
                                          date =  kd_data$DateTime,
                                          id = kd_data$ID,
                                          burst = kd_data$tripID)

  # Set brownian bridge parameters
  # First step is to estimate the sig1 parameter
  sig1 <- adehabitatHR::liker(kd_data.ltraj,
                              sig2 = sig2,
                              rangesig1 = c(0,300),
                              byburst = TRUE,
                              plotit = showLiker)
  sig1 <- unname(unlist(lapply(sig1, function(x) x$sig1)))
  sig1 <- sig1 * bridgeSmooth

  # Create base grid for bbmm kernel
  xy <- createGrid(kd_data,
                   res = res
  )

  # Calculate bbmm
  bbmm <- adehabitatHR::kernelbb(kd_data.ltraj,
                                 sig1 = sig1,
                                 sig2 = sig2,
                                 grid = xy)

  return(bbmm)

}

# -----

#' Stack kernel UD objects into a population level kernel
#'
#' @description This function accepts a list of estUDm objects
#' as output by either opp_kernel or opp_bbmm, then rasterizes
#' and stacks them to produce a population-level kernel. An optional
#' list of weights can be supplied to emphasize certain tracks over others.
#'
#' @param data List of estUDm objects. Output from either opp_kernel or opp_bbmm.
#' @param weights Optional list of weights.
#'
#' @export

ud_stack <- function(data, weights) {

  # Data health check
  if (class(data) != "estUDm") {
    stop("Kernel data must be class estUDm (outputs from either opp_kernel or opp_bbmm).")
  }

  # Prepare raster stack
  raster_stack <- lapply(data, function(x) (raster::raster(as(x, "SpatialPixelsDataFrame"))))
  raster_stack <- raster::stack(raster_stack)

  # If weights provided, apply weights
  if (!missing(weights)) {
    # Data health check
    if (length(weights) != length(data)) {
      stop("Length of supplied weights list does not match number of kernels.")
    }

    raster_stack <- raster_stack * weights

  }

  pop_raster <- raster::calc(raster_stack, fun = mean)

  return(pop_raster)
}

# -----

#' Extract 50% and 99% volume contours from kernels
#'
#' @description This function allows the user to reclassify utilization distribution kernels
#' to highlight two specified kernel volumes. By default, this function classifies the 50%
#' and 99% kernel volumes.
#'
#' @export

ud_vol <- function(data, lowerVol = 50, upperVol = 99) {
  # In case people pass args without explicitely specifying which is which
  if (lowerVol > upperVol) {
    u <- lowerVol
    l <- upperVol
  } else {
    u <- upperVol
    l <- lowerVol
  }

  u <- u/100
  l <- l/100
  raster::calc(raster::stack(spatialEco::raster.vol(data, p=u), spatialEco::raster.vol(data, p=l)), fun = sum)
}

# -----

#' Delineate sites of importance for OPP emergency response
#'
#' @description This function generates key biological areas that are usable in the context of OPP
#' risk mitigation. By default, the function will delineate areas that are used by at least 10%, 50%,
#' 75%, and if possible, 95% of the population. If a population number is provided, it will also
#' calculate an estimate of the number of individuals using a given area. By providing a list of
#' percentile thresholds, the user can also specify their own custom percentile threshold values.
#'
#' @param kernels Output of class estUDm from either opp_kernels, opp_bbmm, or adehabitatHR::kernelUD.
#' @param repr Numeric (0-100). Representativeness of the sample, derived from track2KBA::repAssess.
#' @param population Numeric (optional). Total number of individuals at a given study site. Used to calculate the number of individuals that use a given polygon area.
#' @param level_ud Numeric (0-100). Utilization distribution level to extract from kernels prior to calculating high-use areas. Defaults to 95.
#' @param thresh List of percentile threshold values to calculate high use areas. By default, the function will calculate polygons for the 10th, 50th, 75th, and 95th percentile use areas (`c(10, 50, 75, 95)`)
#' @param metadata List of additional metadata you wish to append to the output. E.g., `c("smoother" = "href", "smoother_km" = 3.5)`
#' @param save_shp Logical (T/F). Should the output be saved as a shapefile (.shp)?
#' @param out_name Character string. Filename for output shapefile. Defaults to 'opp_sites' if none provided and save_shp = TRUE.
#' @param out_dir Character string. Output directory for saved shapefile, relative to the current working directory. Defaults to the current working directory if no value provided.
#'
#' @return
#' @export
#'

opp_sites <- function(kernels,
                      repr,
                      population = NA,
                      level_ud = 95, # Default 95
                      thresh = c(10, 50, 75, 95), # Default list
                      metadata = list(),
                      save_shp = FALSE,
                      out_name = NA,
                      out_dir = NA) {
  # This function is heavily modified off of track2KBA::findSite
  # 1. Data health checks
  # - Check kernel class
  # - Check if representativeness is at an appropriate level
  # - Check if thresholds & sample sizes are appropriate

  # Kernel checks
  if (class(kernels) != "estUDm") stop("Kernels should be of class 'estUDm' provided by either OPPtools::opp_kernel, \n      OPPtools::opp_bbmm, track2KBA::estSpaceUse, or adehabitatHR::kernelUD.")
  kernels <- adehabitatHR::estUDm2spixdf(kernels) # Convert to spdf
  if (sp::is.projected(kernels) != TRUE) stop("Please re-calculate your kernel UD after projecting the data into an equal-area projection.")

  # Sample size checks
  ss <- ncol(kernels)
  if (ss < 20) {
    warning("LOW SAMPLE SIZE: identifying OPP sites based on <20 tracked individuals is not recommended.\n        If you are delineating sites for species which are NOT central place foragers, you\n        can consider using unique trips (rather than individuals) to calculate your kernels.")
    # In original track2KBA::findSites function, here the
    # author further has:
    # if (SampSize < 5) {
    #   thresh <- SampSize + 1
    # }
    # I am not sure what behavior to do here with a list of thresholds.
  }

  # Thresholds checks
  thresh <- as.list(thresh)
  if (class(thresh) != 'list') {
    stop("Threshold values must be supplied as a list of numeric values, e.g. 'c(10, 50, 95)'.")
    if (all(sapply(thresh, is.numeric)) == FALSE) {
      stop("Threshold values must be supplied as a list of numeric values, e.g. 'c(10, 50, 95)'.")
    }
  }

  thresh <- lapply(thresh, function(x) x/100)
  invisible(lapply(thresh, function(x) if (1/ss > x) message ("NOTE: Your threshold value of ", x * 100, " is lower than 1/sample size, which means an\n        area could be delineated as important although only visited by one tracked individual.")))

  # Representativeness checks
  repr <- ifelse(repr > 1, repr/100, repr)
  if (repr < 0.5) warning("UNREPRESENTATIVE SAMPLE: sample below 50% representativeness. Sites of\n        importance cannot be identified with confidence.")

  # 2. Cumulative sum of kernel data
  k_area <- kernels@grid@cellsize[[1]]^2

  kernels@data <- kernels@data %>%
    dtplyr::lazy_dt() %>%
    mutate(row = seq_len(nrow(.))) %>%
    tidyr::pivot_longer(!.data$row, names_to = "ID",
                        values_to = "UD") %>%
    mutate(usage = UD * k_area) %>%
    arrange(ID, desc(usage)) %>%
    group_by(ID) %>%
    mutate(cumulUD = cumsum(usage)) %>%
    dplyr::select(row, ID, cumulUD) %>%
    arrange(row) %>%
    tidyr::pivot_wider(names_from = ID,
                       values_from = cumulUD) %>%
    ungroup() %>%
    dplyr::select(-row) %>%
    data.table::as.data.table() %>%
    as.data.frame()

  kernels@data <- as.data.frame(ifelse(kernels@data < (level_ud/100), 1, 0))
  kernels@data$n_tracks <- rowSums(kernels@data)
  kernels@data <- kernels@data["n_tracks"] # equivalent to `Noverlaps` in original function
  kernels <- kernels[kernels$n_tracks > 0, ]

  # 3. Calculate n individuals, % population, threshold percentiles
  kernels@data$perc_pop <- repr * (kernels@data$n_tracks / ss)
  kernels@data$n_indiv <- kernels@data$perc_pop * population

  kernels@data$percentile <- NA
  for (i in sort(unlist(thresh))) {
    if (nrow(kernels@data[which(kernels@data$perc_pop > i), ]) != 0) {
      kernels@data[which(kernels@data$perc_pop > i), ]$percentile <- i * 100
    } else {
      break
    }
  }

  # 4. Create sf output
  # See internalHelper.R for c_mean func
  sums <- list(list("min", "n_tracks"),
               list("min", "perc_pop"),
               list("min", "n_indiv"))
  out <- raster::aggregate(as(kernels, "SpatialPolygonsDataFrame"),
                           by = "percentile",
                           sums = sums)
  out <- sf::st_as_sf(out) %>%
    sf::st_union(by_feature = TRUE)

  # 5. Append any additional metadata columns
  out <- out %>% arrange(n_tracks)
  out$percentile <- 100 - out$percentile
  out$repr <- repr
  out$total_ss <- ss

  if (length(metadata) > 0) {
    for (i in 1:length(metadata)) {
      out[ncol(out) + 1] <- metadata[[i]]
      names(out)[ncol(out)] <- names(metadata)[i]
    }
  }

  out <- out %>% dplyr::select(-geometry)
  out <- smoothr::smooth(out, method = "ksmooth", smoothness = 2)

  # 6. Save shapefile
  if (save_shp == TRUE) {
    if (is.na(out_dir)) {
      message("No output directory for shapefile provided. Outputting shapefile to working directory.")
      out_dir <- here::here()
    } else {
      out_dir <- file.path(here::here(), out_dir)
    }

    if (is.na(out_name)) {
      message("No output filename for shapefile provided. Defaulting to `opp_sites.shp`.")
      out_name <- "opp_sites"
    }

    # Now actually save the shapefile
    sf::st_write(out,
                 paste0(file.path(out_dir, out_name), ".shp"),
                 append = FALSE)
    message("Shapefile saved to: ", paste0(file.path(out_dir, out_name), ".shp"))
  }

  return(out)
}


# -----

#' Calculate the distance between consecutive points
#'
#' @description Wrapper for raster::pointDistance that only requires input of a
#' vector of longitudes and a vector of latitudes. Default calculation assumes
#' data are in decimal degrees. If not, then set lonlat = FALSE. Compatible with
#' tidyverse.
#'
#' @param lon Vector of longitudes
#' @param lat Vector of latitudes
#' @param lonlat If TRUE, coordinates should be in degrees; else they should represent planar ('Euclidean') space (e.g. units of meters)
#' @returns A vector of distances in meters.
#'
#' @export

getDist <- function(lon, lat, lonlat = TRUE) {

  dd <- data.frame(lon = lon, lat = lat)

  if (nrow(dd) < 2) {
    out <- rep(NA, nrow(dd))
  } else {

    out <- c(NA,
             raster::pointDistance(dd[2:nrow(dd),c("lon","lat")],
                                   dd[1:(nrow(dd)-1),c("lon","lat")],
                                   lonlat = T))
  }
  out
}

# -----

#' Explore how different values of minDist influence total number of trips
#'
#' Calculates the number of trips that go farther than returnBuff for a range of
#' minimum distance values that a bird must travel from the colony to be considered in a trip.
#' This is useful for ensuring that the minimum trip distance separates all long trips.

#'
#' @param data Polygon output from track2KBA::findSite()
#' @param minDist_range Vector of ditances (km) from the colony to calculate how
#' minimum distance influences total number of long foraging trips.
#' Used to label trips as 'Non-trip'. Defaults to 5
#' @param returnBuff Outer distance (km) to capture trips that start and end
#' away from the colony. Used to label trips as 'Incomplete' and for assessing the
#' effect of minDist on total number of trips. Defaults to 20.
#' @param duration Minimum trip duration (hrs)
#' @param gapTime Time (hrs) between successive locations at which trips will be flagged as 'Gappy'.
#' Used in connection with gapDist, such that locations must be farther apart in
#' both time and space to be considered a gap.
#' @param gapDist Distance (km) between successive locations at which trips will be flagged as 'Gappy'.
#' Used in connection with gapTime, such that locations must be farther apart in
#' both time and space to be considered a gap.
#' @param gapLimit Maximum time between points to be considered too large to be
#' a contiguous tracking event. Can be used to ensure that deployments on the
#' same animal in different years do not get combined into extra long trips.
#' Defaults to 100 days.
#'
#' @details With lower sampling frequency or if GPS fixes are not taken at or near the colony,
#' multiple foraging trips may be combined if minDist is too small. Look for the
#' minDist value where the number of trips begins to asymptote. Selected values should
#' be confirmed by calling opp_get_trips directly with showPlots = TRUE.
#'
#' @export

opp_find_minDist <- function(data,
                             minDist_range = 1:10,
                             returnBuff = 20,
                             duration  = 1,
                             gapLimit = 100,
                             gapTime = 1,
                             gapDist = 10) {

  out <- data.frame(dist = minDist_range,
                    trips = NA)

  for (j in 1:nrow(out)) {
    my_trips <- opp_get_trips(data = data,
                              innerBuff  = out$dist[j],
                              returnBuff = returnBuff,
                              duration  = duration,
                              gapLimit = gapLimit,
                              gapTime = gapTime,
                              gapDist = gapDist,
                              showPlots = F)
    tt <- sum_trips(my_trips)
    out$trips[j] <- (nrow(tt[tt$max_dist_km > returnBuff]))
  }

  out
}

# -----

#' Creates maps of biologger tracks from output of opp2KBA, opp_get_trips, or ctcrw_interpolation
#' @param tracks data.frame, sf points, or SpatialPointsDataFrame containing fields named ID, Longitude, and Latitude
#' @param center Data frame containing columns 'Longitude' and 'Latitude' in decimal degrees,
#' for plotting the colony or nest locations.
#' @param coast_scale Mapping resolution for the coastline basemap. Must be one of: 10 - high resolution,
#' 50 - medium resolution, 110 - low resolution.
#' @param zoom NULL or numeric value from 1:16, indicating the zoom level for map. If left as NULL (default)
#' map extent will be defined by the bounding box of tracks. If numeric value is provided, bounding box will
#' be cenetered on the mean location in tracks at the zoom level provided.
#' @param show_locs Logical. Should biologger locations be plotted as points?
#' @param viridis_option A character string indicating the colormap option to
#' use. Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
#' @returns A ggplot object
#' @export
#'
opp_map_tracks <- function(tracks,
                          center = NULL,
                          zoom = NULL,
                          coast_scale = 10,
                          viridis_option = "D",
                          show_locs = T) {

  if (!('ID' %in% names(tracks)) | !('Longitude' %in% names(tracks)) | !('Latitude' %in% names(tracks))) stop('Tracks must contain fields: ID, Longitude, and Latitude.')

  world <- rnaturalearth::ne_countries(scale = coast_scale, returnclass = 'sf')

  if (class(tracks)[1] == "SpatialPointsDataFrame") tracks <- as(tracks, 'sf')

  if (class(tracks)[1] == "data.frame") {
    tracks <- sf::st_as_sf(tracks,
                           coords = c("Longitude", "Latitude"),
                           crs = '+proj=longlat') %>%
      dplyr::mutate(tripID = ID)
    tracks$Longitude <- sf::st_coordinates(tracks)[,1]
    tracks$Latitude <- sf::st_coordinates(tracks)[,2]

  }

  tracks <- tracks %>%
    dplyr::filter(tripID != -1) %>%
    dplyr::group_by(ID, tripID) %>%
    dplyr::mutate(ID = factor(ID),
           DiffTime = as.numeric(difftime(DateTime, dplyr::lag(DateTime), units = 'hour')),
           DiffDist= getDist(lon = Longitude, lat = Latitude))
  trips <- tracks %>%
    dplyr::arrange(DateTime) %>%
    dplyr::summarize(
      TotalTime = sum(DiffTime, na.rm = T),
      TotalDist = sum(DiffDist, na.rm = T),
      do_union = FALSE,
      .groups = 'drop'
    ) %>%
    sf::st_cast("LINESTRING")

  if (!(coast_scale %in% c(10, 50, 110))) stop('coast_scale must be one of 10, 50, or 110')

  if (!is.null(center)) center <- sf::st_as_sf(center, coords = c('Longitude', 'Latitude'), crs = sf::st_crs(trips))
  tracks <- sf::st_transform(tracks, crs = sf::st_crs(4326))
  trips <- sf::st_transform(trips, crs = sf::st_crs(4326))
  center <- sf::st_transform(center, crs = sf::st_crs(4326))
  world <- sf::st_transform(world, crs = sf::st_crs(4326))

  bb <- bbox_at_zoom(locs = tracks, zoom_level = zoom)

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world, fill = grey(0.9), size = 0.3) +
    ggplot2::geom_sf(data =trips, ggplot2::aes(col = ID), size = 0.3, alpha = 0.75, linetype = 3)  +
    ggplot2::scale_colour_viridis_d(option = viridis_option) +
    ggplot2::theme_light() +
    ggplot2::theme(text = ggplot2::element_text(size = 10))  +
    ggplot2::guides(color = 'none')

  if (show_locs == T) {
    p <- p + ggplot2::geom_sf(data = tracks, ggplot2::aes(col = ID), size = 0.3, alpha = 0.75)  +
      ggplot2::geom_sf(data = center, fill = "dark orange",color = "black",pch = 21,size = 2.5)
  }

  if (!is.null(center)) {
    p <- p + ggplot2::geom_sf(data = center, fill = "dark orange", color = "black", pch = 21, size = 2.5)
  }

  p <- p +
    ggplot2::coord_sf(xlim = bb[c(1,3)],ylim = bb[c(2,4)],expand = T)
  p
}

# -----
#' Map and compare different UD smoothers for individual tracks
#' @param uds Named list of estUDm or estUD objects to plot. As returned by opp_kernel.
#' @param ud_levels List of numeric values between 1-99, indicting the utilization distribution levels to plot.
#' @param tracks SpatialPointsDataFrame of location data used in opp_kernel.
#' @param center Data frame containing columns 'Longitude' and 'Latitude' in decimal degrees,
#' for plotting the colony or nest locations.
#' @param coast_scale Mapping resolution for the coastline basemap. Must be one of: 10 - high resolution,
#' 50 - medium resolution, 110 - low resolution.
#' @param zoom NULL or numeric value from 1:16, indicating the zoom level for map. If left as NULL (default)
#' map extent will be defined by the bounding box of tracks. If numeric value is provided, bounding box will
#' be cenetered on the mean location in tracks at the zoom level provided.
#' @param viridis_option A character string indicating the colormap option to
#' use. Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
#' @returns A named list of ggplot objects, with one slot for each individual in uds.
#'
#' #'@examples
#'
#' my_data <- opp_download_data(study = c(1247096889),
#'                              login = NULL, start_month = NULL,
#'                              end_month = NULL,season = NULL)
#'
#' my_track2kba <- opp2KBA(data = my_data)
#'
#' my_trips <- opp_get_trips(data = my_track2kba, innerBuff  = 1, returnBuff = 10,
#'                           duration  = 1, gapLimit = 100, gapTime = 1, gapDist = 12,
#'                           showPlots = F)
#'
#' my_interp <- ctcrw_interpolation(data = my_trips,
#'                                  site = my_track2kba$site,
#'                                  type = c('Complete','Incomplete','Gappy'),
#'                                  timestep = '10 min',
#'                                  interpolateGaps = F,
#'                                  showPlots = T,
#'                                  theta = c(8,2),
#'                                  quiet = F
#' )
#'
#' (hh <- opp_href(my_interp$interp))
#'
#' my_ud_href1 <- opp_kernel(data = my_interp,
#'                           interpolated = TRUE,
#'                           smoother = hh/1,
#'                           extendGrid = 30,
#'                           res = 1)
#'
#' my_ud_href2 <- opp_kernel(data = my_interp,
#'                           interpolated = TRUE,
#'                           smoother = hh/2,
#'                           extendGrid = 30,
#'                           res = 1)
#'
#' my_plots_step <- opp_map_indUD(
#'   uds = list(Href1 = my_ud_href1,Href2 = my_ud_href2),
#'   tracks = my_interp$interp,
#'   center = my_track2kba$site,
#'   ud_levels = c(50,75,90),
#'   coast_scale = 50,
#'   viridis_option = 'A'
#' )
#'
#' my_plots_step[[1]]
#' my_plots_step[[2]]
#' @export
#'
opp_map_indUD <- function(
  uds,
  ud_levels = c(50,95),
  tracks,
  center = NULL,
  zoom = NULL,
  coast_scale = 50,
  viridis_option = "A"
) {

  if (!(class(uds[[1]]) %in% c("estUDm", "estUD"))) {stop('uds must be named list of estUDm or estUD object')}

  if (!('ID' %in% names(tracks)) | !('Longitude' %in% names(tracks)) | !('Latitude' %in% names(tracks))) stop('Tracks must contain fields: ID, Longitude, and Latitude.')

  world <- rnaturalearth::ne_countries(scale = coast_scale, returnclass = 'sf')

  if (class(tracks)[1] == "SpatialPointsDataFrame") tracks <- as(tracks, 'sf')

  tracks <- tracks %>%
    dplyr::filter(tripID != -1) %>%
    dplyr::group_by(ID, tripID) %>%
    dplyr::mutate(ID = factor(ID),
                  DiffTime = as.numeric(difftime(DateTime, dplyr::lag(DateTime), units = 'hour')),
                  DiffDist= getDist(lon = Longitude, lat = Latitude))
  trips <- tracks %>%
    dplyr::arrange(DateTime) %>%
    dplyr::summarize(
      TotalTime = sum(DiffTime, na.rm = T),
      TotalDist = sum(DiffDist, na.rm = T),
      do_union = FALSE,
      .groups = 'drop'
    ) %>%
    sf::st_cast("LINESTRING")

  if (!(coast_scale %in% c(10, 50, 110))) stop('coast_scale must be one of 10, 50, or 110')

  if (!is.null(center)) center <- sf::st_as_sf(center, coords = c('Longitude', 'Latitude'), crs = sf::st_crs(trips))
  world <- sf::st_transform(world, crs = sf::st_crs(trips))

  ud_levels <- sort(ud_levels, decreasing = T)
  for (k in 1:length(uds)) {
    for (j in 1:length(ud_levels)) {
      temp <- adehabitatHR::getverticeshr(uds[[k]], percent = ud_levels[j])
      temp$ud_level <- ud_levels[j]
      temp$ud_type <- names(uds)[k]
      temp <- as(temp, 'sf')
      if (j == 1 & k == 1) ud_contours <- temp
      if (j > 1 | k > 1) ud_contours <- rbind(ud_contours, y = temp)
    }
  }
  ud_contours$ud_level <- factor(ud_contours$ud_level, levels = ud_levels)
  ud_contours <- as(ud_contours, 'sf')

  if (is.numeric(zoom)) bb <- bbox_at_zoom(locs = tracks, zoom_level = zoom)
  if (is.null(zoom)) {bb <- sf::st_bbox(ud_contours)}

  out <- vector(mode = 'list', length = length(unique(ud_contours$id)))
  names(out) <- unique(ud_contours$id)

  for (iid in unique(ud_contours$id)) {

    ud_temp <- ud_contours[ud_contours$id == iid,]
    ud_temp$ud_facets <- paste0('UD: ', ud_temp$ud_type, '   ID: ', ud_temp$id)
    trips_temp <- trips[trips$ID == iid,]
    tracks_temp <- tracks[tracks$ID == iid,]

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = world, fill = grey(0.9), size = 0.3) +
      ggplot2::geom_sf(data =ud_temp, ggplot2::aes(fill = ud_level), color = 'transparent', size = 0.3, alpha = 0.5)  +
      ggplot2::geom_sf(data =trips_temp, color = 'black', size = 0.3, alpha = 0.75, linetype = 3)  +
      ggplot2::scale_colour_viridis_d(option = viridis_option, begin = 0.7, end = 0.9, direction = -1) +
      ggplot2::scale_fill_viridis_d(option = viridis_option, begin = 0.7, end = 0.9, direction = -1) +
      ggplot2::theme_light() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 8),
        legend.text = ggplot2::element_text(size = 8),
        axis.text = ggplot2::element_text(size = 6, hjust = 1),
        axis.text.x = ggplot2::element_text(angle = 90),
        )  +
      ggplot2::labs(fill = 'UD (%)') +
      ggplot2::guides(color = 'none') +
      ggplot2::facet_wrap(.~ud_facets, nrow = 1)+
      ggplot2::geom_sf(data =tracks_temp, color = 'black', size = 0.3, alpha = 0.75)  +
      ggplot2::geom_sf(data = center, fill = "dark orange",color = "black",pch = 21,size = 2.5)

    if (!is.null(center)) {
      p <- p + ggplot2::geom_sf(data = center, fill = "dark orange", color = "black", pch = 21, size = 2.5)
    }

    p <- p +
      ggplot2::coord_sf(xlim = bb[c(1,3)],ylim = bb[c(2,4)],expand = T)

    out[[iid]] <- p
  }

  return(out)
}
