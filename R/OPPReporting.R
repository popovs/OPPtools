

#' Render diagnostic report for a given species
#'
#' @description This function will generate a diagnostic report using the 'opp-diagnostic-report' RMarkdown template in `OPPtools`, with options to save the generated report as an .Rmd file in addition to the output PDF.
#'
#' @param params List of 19 parameter values used to generate report.
#' @param save_rmd Logical (T/F). Should the .Rmd file used to generate the PDF report be saved as well?
#' @param output_dir Output directory for generated files. Defaults to 'temp'. If the directory does not exist, the script will create the directory.
#'
#' @export
#'
#' @examples
#' for (i in 1:nrow(cpf_report_params)) {
#'    opp_render_diagnostic(cpf_report_params[i,],
#'                       out_format = 'pdf_document',
#'                       save_rmd = F,
#'                       output_dir = paste0('temp/OPP_report/',cpf_report_params$file_name[i]))
#'
#' }

opp_render_diagnostic <- function(params,
                                  save_rmd = FALSE,
                                  output_dir = 'temp',
                                  out_format = 'pdf_document',
                                  ...) {

  # Data health checks
  params <- as.list(params)
  if (class(params) != 'list') {
    stop("Your passed params must be class 'list'.")
  }
  if (length(params) != 19) {
    stop("Your passed params list is the incorrect length. Ensure you provide the 18 necessary params.")
  }

  if (any(is.na(params))) warning("There are NA values in your passed params list. This may cause unexpected errors when rendering the document.")

  # Create output dir & file
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  filename <- params$file_name

  # Modify params list
  names(params)[grep("mb", names(params))] <- "movebank_id" # rename 'mb_project_num' to 'movebank_id', if exists
  params <- params[-grep("file_name", names(params))] # remove 'file_name'

  # If saving Rmd file, generate and save it
  if (save_rmd == TRUE) {
    rmd_dir <- paste0(file.path(output_dir, filename))
    dir.create(rmd_dir, showWarnings = FALSE)
    rmd_out <- paste0(file.path(rmd_dir, filename), ".Rmd")
    rmarkdown::draft(rmd_out,
                     template = "opp-diagnostic-report",
                     package = "OPPtools",
                     edit = FALSE)
    change_yaml_matter(rmd_out,
                       output_file = rmd_out,
                       params = params) # Could still be improved

    # If save_rmd true, update output dir
    # This is so Rmd & PDF are in nice little folder together
    output_dir <- rmd_dir
  }


  # Render the file with the modified params
  rmarkdown::render(
    system.file('rmarkdown/templates/opp-diagnostic-report/skeleton', 'skeleton.Rmd',
                package = 'OPPtools'),
    #"rmarkdown/templates/opp-diagnostic-report/skeleton/skeleton.Rmd",
    params = params,
    output_dir = output_dir,
    output_file = paste0(filename, ' - OPP Supporting Methods', ifelse(out_format == 'pdf_document','.pdf', '.html')),
    output_format = out_format,
    output_yaml = system.file('rmarkdown/templates/opp-diagnostic-report/', 'template.yaml',
                              package = 'OPPtools'),
    envir = new.env()
  )
}

# -----


#' Render KBA report for a given species
#'
#' @description This function will generate a KBA report using the 'opp-kba-report' RMarkdown template in `OPPtools`, with options to save the generated report as an .Rmd file in addition to the output PDF.
#'
#' @param params List of 19 parameter values used to generate report.
#' @param iterations Numeric. Number of iterations to perform for track2KBA::repAssess. More iterations will result in a more accurate assessment, but take longer to run. Default 5.
#' @param level_ud Numeric, from 0 to 100. Utilization distribution volume to extract from kernel densities. Defaults to 95.
#' @param save_rmd Logical (T/F). Should the .Rmd file used to generate the PDF report be saved as well?
#' @param save_shp Logical (T/F). Should the final OPP key area polygon be saved as a shapefile?
#' @param save_pts Logical (T/F). Should the raw downloaded Movebank points be saved as a shapefile?
#' @param output_dir Output directory for generated files, relative to your working directory. Defaults to 'temp'. If the directory does not exist, the script will create the directory.
#' @param out_format Output file format for the knitted document. Defaults to 'pdf_document'.
#'
#' @export
#'
#' @examples
#'
#' for (i in 1:nrow(cpf_report_params)) {
#
#'   opp_render_report(params = cpf_report_params[i,],
#'                     kernel_smoother = cpf_report_params$smoother[i],
#'                     iterations = 10,
#'                     level_ud = 95,
#'                     save_rmd = FALSE,
#'                     save_pts = TRUE,
#'                     output_dir = 'temp',
#'                     out_format = 'pdf_document')
#'                     }
#'

opp_render_report <- function(params,
                              iterations = 5,
                              level_ud = 95,
                              save_rmd = FALSE,
                              save_shp = TRUE,
                              save_pts = TRUE,
                              output_dir = 'temp',
                              out_format = 'pdf_document',
                              ...) {

  # Data health checks
  params <- as.list(params)
  if (class(params) != 'list') {
    stop("Your passed params must be class 'list'.")
  }
  if (length(params) != 19) {
    stop("Your passed params list is the incorrect length. Ensure you provide the 18 necessary params.")
  }

  if (any(is.na(params))) warning("There are NA values in your passed params list. This may cause unexpected errors when rendering the document.")

  # Create output dir & filename
  output_dir <- file.path(output_dir, "OPP_Report")
  dir.create(file.path(output_dir), showWarnings = FALSE, recursive = TRUE)
  filename <- params$file_name # set filename

  # Modify params list
  names(params)[grep("mb", names(params))] <- "movebank_id" # rename 'mb_project_num' to 'movebank_id', if exists
  #params <- params[-grep("file_name", names(params))] # remove 'file_name'

  # Add 4 new params to params list
  #params$kernelSmoother <- kernel_smoother
  params$iterations <- iterations
  params$levelUD <- level_ud
  params$saveShp <- save_shp
  params$save_pts <- save_pts
  params$proj_dir <- here::here()

  # If either Rmd or Shp file are saved, update the output_dir
  # This is so all outputs are in nice little folder together
  if (save_rmd == TRUE | save_shp == TRUE | save_pts == TRUE) {
    output_dir <- paste0(file.path(output_dir, filename))
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    # Add the output_dir to the params passed to skeleton.Rmd
    params$output_dir <- output_dir
  }

  # If saving Rmd file, generate and save it
  if (save_rmd == TRUE) {
    #rmd_dir <- paste0(file.path(output_dir, filename))
    #dir.create(rmd_dir, showWarnings = FALSE)
    rmd_out <- paste0(file.path(output_dir, filename), " - OPP High Use Areas.Rmd")
    rmarkdown::draft(rmd_out,
                     template = "opp-sites-report",
                     package = "OPPtools",
                     edit = FALSE)
    change_yaml_matter(rmd_out,
                       output_file = rmd_out,
                       params = params) # Could still be improved

    # If save_rmd true, update output dir
    # This is so Rmd & PDF are in nice little folder together
    #output_dir <- rmd_dir
  }

  # Render the file with the modified params
  rmarkdown::render(
    system.file('rmarkdown/templates/opp-sites-report/skeleton', 'skeleton.Rmd',
                package = 'OPPtools'),
    #"rmarkdown/templates/opp-sites-report/skeleton/skeleton.Rmd",
    params = params,
    output_dir = output_dir,
    output_file = paste0(filename,' - OPP High Use Areas', ifelse(out_format == 'pdf_document','.pdf', '.html')),
    output_format = out_format,
    output_yaml = system.file('rmarkdown/templates/opp-sites-report/skeleton', 'template.yaml',
                              package = 'OPPtools'),
    envir = new.env()
  )
}
