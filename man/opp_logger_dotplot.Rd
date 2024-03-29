% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OPPPlots.R
\name{opp_logger_dotplot}
\alias{opp_logger_dotplot}
\title{Plot tracking history by date, year, Movebank ID, and animal-reproductive-condition}
\usage{
opp_logger_dotplot(data, yearround = F)
}
\arguments{
\item{data}{Dataframe as returned by opp_download_data()}

\item{yearround}{Logical. If yearround is FALSE (default), each year is plotted as a separate facet}
}
\value{
A ggplot object
}
\description{
Creates a dot plot showing GPS locations for each individual over time
}
\examples{
\dontrun{
my_data <- opp_download_data(study = c(1247096889),login = NULL, start_month = NULL,
                            end_month = NULL,season = NULL)

opp_logger_dotplot(data = my_data)
}
}
