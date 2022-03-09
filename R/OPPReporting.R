#' Generate reports for central place foraging species tracked in OPP
#'
#' @description This function passes a table of parameters to an RMarkdown template to generate summary
#' reports on tracking data analysis.
#'
#' @param analysis_table Data frame that provides all values needed for the RMarkdown template, see details.
#' Defaults to the cpf_report_params data object that is contained within OPPTools.
#' @param login Stored Movebank login credentials if provided, otherwise function
#'will prompt users to enter credentials.
#' @param output_dir Name of of the directory where reports should be saved.
#'
#' @details Still need to write this up, once function is complete.
#'
#' The example below will only work if the user has permissions for all the projects listed in cpf_report_params
#'
#' @examples
#'
#' print(cpf_report_params)
#' opp_reports_cpf()
#'
#'@export


opp_reports_cpf <- function(analysis_table = cpf_report_params,login = NULL, output_dir = 'temp') {

  if (is.null(login)) login <- move::movebankLogin()

  for (i in 1:nrow(analysis_table)) {

    dat <- analysis_table[i,]

    rmarkdown::render(input = "inst/rmarkdown/templates/opp-cpf-report/skeleton/skeleton.Rmd",
                      output_format = "pdf_document",
                      output_file = paste0(dat$file_name, ".pdf"),
                      output_dir = output_dir)

  }

}

# -----

#' Render diagnostic report for a given species
#'
#' @description This function will generate a diagnostic report using the 'opp-diagnostic-report' RMarkdown template in `OPPtools`, with options to save the generated report as an .Rmd file in addition to the output PDF.
#'
#' @param params List of 16 parameter values used to generate report.
#' @param save_rmd Logical (T/F). Should the .Rmd file used to generate the PDF report be saved as well?
#' @param output_dir Output directory for generated files. Defaults to 'temp'. If the directory does not exist, the script will create the directory.
#'
#' @export

render_diagnostic <- function(params,
                              save_rmd = FALSE,
                              output_dir = 'temp',
                              out_format = 'html_document',
                              ...) {

  # Data health checks
  params <- as.list(params)
  if (class(params) != 'list') {
    stop("Your passed params must be class 'list'.")
  }
  if (length(params) != 17) {
    stop("Your passed params list is the incorrect length. Ensure you provide the 17 necessary params.")
  }

  if (any(is.na(params))) warning("There are NA values in your passed params list. This may cause unexpected errors when rendering the document.")

  # Create output dir & file
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  filename <- params$file_name # set filename

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
    "inst/rmarkdown/templates/opp-diagnostic-report/skeleton/skeleton.Rmd",
    params = params,
    output_dir = output_dir,
    output_file = paste0(filename, ifelse(out_format == 'pdf_document','.pdf', '.html')),
    output_format = out_format,
    envir = new.env()
  )
}

# -----


#' Render KBA report for a given species
#'
#' @description This function will generate a KBA report using the 'opp-kba-report' RMarkdown template in `OPPtools`, with options to save the generated report as an .Rmd file in addition to the output PDF.
#'
#' @param params List of 16 parameter values used to generate report.
#' @param kernel_smoother One of four options for kernel smoothers in kernel calculations: "href", "href/2", "step", or "bbmm". Defaults to href/2.
#' @param iterations Numeric. Number of iterations to perform for track2KBA::repAssess. More iterations will result in a more accurate assessment, but take longer to run. Default 5.
#' @param level_ud Numeric, from 0 to 100. Utilization distribution volume to extract from kernel densities. Defaults to 95.
#' @param save_rmd Logical (T/F). Should the .Rmd file used to generate the PDF report be saved as well?
#' @param output_dir Output directory for generated files. Defaults to 'temp'. If the directory does not exist, the script will create the directory.
#'
#' @export

render_kba <- function(params,
                       kernel_smoother = "href/2",
                       iterations = 5,
                       level_ud = 95,
                       save_rmd = FALSE,
                       save_shp = TRUE,
                       output_dir = 'temp',
                       out_format = 'html_document',
                       ...) {

  # Data health checks
  params <- as.list(params)
  if (class(params) != 'list') {
    stop("Your passed params must be class 'list'.")
  }
  if (length(params) != 17) {
    stop("Your passed params list is the incorrect length. Ensure you provide the 17 necessary params.")
  }

  if (any(is.na(params))) warning("There are NA values in your passed params list. This may cause unexpected errors when rendering the document.")

  # Create output dir & filename
  output_dir <- file.path(output_dir, "KBA")
  dir.create(file.path(output_dir), showWarnings = FALSE, recursive = TRUE)
  filename <- params$file_name # set filename

  # Modify params list
  names(params)[grep("mb", names(params))] <- "movebank_id" # rename 'mb_project_num' to 'movebank_id', if exists
  #params <- params[-grep("file_name", names(params))] # remove 'file_name'

  # Add 4 new params to params list
  params$kernelSmoother <- kernel_smoother
  params$iterations <- iterations
  params$levelUD <- level_ud
  params$saveShp <- save_shp

  # If either Rmd or Shp file are saved, update the output_dir
  # This is so all outputs are in nice little folder together
  if (save_rmd == TRUE | save_shp == TRUE) {
    output_dir <- paste0(file.path(output_dir, filename))
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    # Add the output_dir to the params passed to skeleton.Rmd
    params$output_dir <- output_dir
  }

  # If saving Rmd file, generate and save it
  if (save_rmd == TRUE) {
    #rmd_dir <- paste0(file.path(output_dir, filename))
    #dir.create(rmd_dir, showWarnings = FALSE)
    rmd_out <- paste0(file.path(output_dir, filename), ".Rmd")
    rmarkdown::draft(rmd_out,
                     template = "opp-kba-report",
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
    "inst/rmarkdown/templates/opp-kba-report/skeleton/skeleton.Rmd",
    params = params,
    output_dir = output_dir,
    output_file = paste0(filename, ifelse(out_format == 'pdf_document','.pdf', '.html')),
    output_format = out_format,
    envir = new.env()
  )
}
