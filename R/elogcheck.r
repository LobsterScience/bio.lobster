#' Render the ELOG Data Check Report
#'
#' Interactively choose an output directory and render the
#' ELOG data summary RMarkdown report.
#'
#' @param redo.years Numeric vector of years to include in the report.
#' @param data.pull Character string passed to the RMarkdown parameter
#'   `data.pull`. Default is "logs".
#' @param output.dir Optional character path to output directory.
#'   If NULL, the user will be prompted to choose a directory.
#' @param report.file Optional path to the RMarkdown report file.
#'
#' @return Invisibly returns the path to the rendered report.
#'
#' @export
#'
elogcheck <- function(
    redo.years = lubridate::year(Sys.Date()),
    data.pull = "logs",
    output.dir = NULL,
    report.file = NULL
) {
  
  # Check required packages
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required.")
  }
  
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("Package 'rstudioapi' is required.")
  }
  
  # Locate report file inside package if not supplied
  if (is.null(report.file)) {
    
    report.file <- system.file(
      "rmd_Reports",
      "elog_data_summary.Rmd",
      package = "bio.lobster"
    )
    
  }
  
  # Check report exists
  if (!file.exists(report.file)) {
    stop("Could not locate report file.")
  }
  
  # Ask user for output directory if needed
  if (is.null(output.dir)) {
    
    output.dir <- rstudioapi::selectDirectory(
      caption = "Choose output folder"
    )
    
    if (is.null(output.dir) || output.dir == "") {
      message("Rendering cancelled.")
      return(invisible(NULL))
    }
    
  }
  
  # Create output directory if needed
  if (!dir.exists(output.dir)) {
    dir.create(output.dir, recursive = TRUE)
  }
  
  # Build output filename
  output.file <- paste0(
    "elog_data_report_",
    format(Sys.Date(), "%Y%m%d"),
    ".html"
  )
  
  # Render report
  rendered.file <- rmarkdown::render(
    input = report.file,
    output_file = output.file,
    output_dir = output.dir,
    params = list(
      data.pull = data.pull,
      redo.years = redo.years,
      output.dir = output.dir
    ),
    envir = new.env(parent = globalenv())
  )
  
  message("Report written to:")
  message(rendered.file)
  
  invisible(rendered.file)
  
}