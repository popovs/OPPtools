# -----

#' Plot trips identified using opp_get_trips()

#' @description Plots the results of opp_get_trips(), with DateTime on the x-axis and ColDist
#' on the y-axis. Points coloured based on Type
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
#' the function returns an unnamed list of ggplot objects.
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
      ggplot2::facet_wrap(facets = . ~ ID, nrow = 2, scales = 'free') +
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


# -----

#' Plot interpolated tracks obtained using ctcrw_interpolation()

#' @description Plots the results of ctcrw_interpolation(), with DateTime on the x-axis
#' and ColDist. Raw GPS data is plotted in purple and interpolated locations are shown in
#' teal.
#'
#' @param data Output from ctcrw_interpolation().
#' @param showPlots Logical (T/F), should plots be printed.
#' @param plotsPerPage Numeric indicating the number of individuals to include
#' in a single plot. Defaults to 1.
#'
#' @returns A list of ggplot objects. If plotsPerPage == 1, then each object is
#' named after the corresponding deployment ID in data. If plotsPerPage > 1 then
#' the function returns an unnnamed list of ggplot objects.
#'
#' @export

#' @export

plot_interp_dist <- function(data, showPlots = T, plotsPerPage = 4) {

  data$interp$ID <- factor(data$interp$ID)
  data$data$ID <- factor(data$data$ID)
  bb <- unique(data$interp$ID)
  idx <- seq(1,length(bb), by = plotsPerPage)
  pal <- hcl.colors(4, "viridis")

  out <- vector(mode = 'list', length = length(idx))

  for (i in idx) {

    intdat <- data$interp[data$interp$ID %in% bb[i:(i + plotsPerPage - 1)],]@data
    intdat$Type <- 'Interpolated'
    obsdat <- data$data[data$data$ID %in% bb[i:(i + plotsPerPage - 1)],]@data
    obsdat$Type <- 'Raw'

    plotdat <- rbind(intdat[,c('ID','DateTime','ColDist','Type', 'tripID')],
                     obsdat[,c('ID','DateTime','ColDist','Type', 'tripID')])
    plotdat$Type <- factor(plotdat$Type, levels = c('Interpolated', 'Raw'))

    pl <- c('Raw' = pal[1], 'Interpolated' = pal[3])
    lt <- c('Raw' =3, 'Interpolated' = 2)

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(x = DateTime, y = ColDist/1000)) +
      ggplot2::geom_line(data = plotdat[plotdat$Type == 'Raw',],
                         ggplot2::aes(col = Type, linetype = Type)) +
      ggplot2::geom_point(data = plotdat[plotdat$Type == 'Raw',],
                          ggplot2::aes(col = Type), size = 1.5, shape = 1) +
      ggplot2::geom_line(data = plotdat[plotdat$Type == 'Interpolated',],
                         ggplot2::aes(col = Type, linetype = Type, group = tripID)) +
      ggplot2::geom_point(data = plotdat[plotdat$Type == 'Interpolated',],
                          ggplot2::aes(col = Type), size = 1, shape = 1) +
      ggplot2::facet_wrap(facets = . ~ ID, nrow = 2, scales = 'free') +
      ggplot2::labs(x = 'Time', y = 'Distance from colony (km)') +
      ggplot2::scale_colour_manual(values = pl) +
      ggplot2::scale_linetype_manual(values = lt) +
      ggplot2::theme_light() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 9),
        axis.text.x = ggplot2::element_text(size = 7)
      )

    if (showPlots == T) print(p)
    out[[which(idx == i)]] <- p
  }

  if (plotsPerPage == 1) names(out) <- bb
  if (showPlots == T) message('Use back arrow in plot pane to browse all plots')

  return(out)

}

# -----

#' Custom plot of representativeness assessment from track2KBA::repAssess
#'
#' @export
#' @represent Output from track2KBA::repAssess with bootTable = TRUE
#' @plot Logical. Should result be plotted
#' @returns A ggplot object showing the results of the call to repAssess

opp_plot_repAssess <- function(represent, plot = TRUE) {

  if (class(represent) != 'list' | length(represent) != 2) {
    stop('represent must be the output of track2KBA::repAssess run with bootTable = TRUE.')
  }

  rep_result <- represent[[1]]
  rep_table <- represent[[2]]

  rep_target <- (max(rep_table$pred)/rep_result$tar_asym) * 100

  rep_label <- paste0("Estimated representativeness: ", signif(rep_target, 3),'%')

  p <- rep_table %>%
    dplyr::group_by(SampleSize) %>%
    dplyr::summarize(
      rep_est = mean(pred),
      min_rep = quantile(InclusionRate, 0),
      max_rep = quantile(InclusionRate, 1)
    ) %>% ggplot2::ggplot(ggplot2::aes(x = SampleSize, y = rep_est)) +
    ggplot2::geom_ribbon(ggplot2::aes(x = SampleSize, ymin = min_rep, ymax = max_rep),
                         fill = grey(0.9)) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = rep_result$tar_asym, linetype = 2) +
    ggplot2::annotate('text', label = rep_label, x = 1, y = 0.9,
                      hjust = 0, size = 3, fontface = 'plain') +
    ggplot2::labs(x = 'Sample size', y = 'Inclusion rate') +
    ggplot2::theme_light() +
    ggplot2::ylim(c(0, 1))

  if (plot == TRUE) print(p)
  return(p)
}
