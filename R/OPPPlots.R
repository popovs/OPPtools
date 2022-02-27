# -----

#' Plot trips identified using opp_get_trips()

#' @description Plots the results of opp_get_trips(), with DateTime on the x-axis and ColDist
#' on the y-axis. Points coloured based on Teyp
#'
#' @param data SpatialPointsDataFrame object output from opp_get_trips
#' @param innerBuff Minimum distance (km) from the colony to be in a trip.
#' Used to label trips as 'Non-trip'. Defaults to 5
#' @param returnBuff Outer distance (km) to capture trips that start and end
#' away from the colony. Used to label trips as 'Incomplete'. Defaults to 20.
#' @param showPlots Logical (T/F), should plots be printed.
#' @param plotsPerPage Numeric indicating the number of individuals to include
#' in a single plot. Defaults to 1.
#'
#' @returns A list of ggplot objects. If plotsPerPage == 1, then each object is
#' named after the corresponding deployment ID in data. If plotsPerPage > 1 then
#' the function returns an unamed list of ggplot obejcts.
#'
#' @export

plot_trip_dist <- function(data, plotsPerPage = 1, showPlots = T,
                           innerBuff = NULL, returnBuff = NULL) {

  dat <- data@data
  dat$ID <- factor(dat$ID)
  bb <- unique(dat$ID)
  idx <- seq(1,length(bb), by = plotsPerPage)
  dummy <- data.frame(Type = c('Non-trip', 'Incomplete', 'Gappy', 'Complete'))

  out <- vector(mode = 'list', length = length(idx))

  for (i in idx) {

    intdat <- dat[dat$ID %in% bb[i:(i+(plotsPerPage-1))],]

    p <- ggplot2::ggplot(intdat) +
      ggplot2::geom_line(ggplot2::aes(x = DateTime, y = ColDist/1000), linetype = 3) +
      ggplot2::geom_point(size = 1, ggplot2::aes(x = DateTime, y = ColDist/1000, col = Type))  +
      ggplot2::facet_wrap(facets = . ~ ID, ncol = 2, scales = 'free') +
      ggplot2::labs(x = 'Time', y = 'Distance from colony (km)', col = 'Type') +
      ggplot2::geom_blank(data = dummy, ggplot2::aes(col = Type)) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::scale_y_continuous(lim = c(0, ifelse(max(intdat$ColDist)< 5000, 5, NA)))+
      ggplot2::theme_light() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.text.x = ggplot2::element_text(size = 7)
      )

    if (!is.null(innerBuff)) {
      p <- p + ggplot2::geom_hline(yintercept = innerBuff, linetype = 2, col = 'black')
    }

    if (!is.null(returnBuff)) {
      p <- p + ggplot2::geom_hline(yintercept = returnBuff, linetype = 2, col = 'black')
    }

    if (showPlots == T) print(p)
    out[[which(idx == i)]] <- p
  }

  if (plotsPerPage == 1) names(out) <- bb
  if (showPlots == T) message('Use back arrow in plot pane to browse all plots')

  return(out)
}
