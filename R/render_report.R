#' Render Summary Report
#'
#' @param output an output file path
#' @param prams a string "ask", which will launch an interactive window to enter
#'   database connection details, or a list that contains these details:
#'
#'   - driver: string, can be any of: "PostgreSQL", "ODBC", or "SQLite"
#'   - host: string, host name
#'   - port: integer, port number
#'   - dbname: string, database name
#'   - schema: string, database target schema
#'   - user: string, database username
#'   - password: string, database user password
#'   - local_hospital: string, local hospital
#'
#' @return
#' @export
render_summary_report <- function(params = "ask") {

  if (is.null(params$output)) {
    rlang::abort("please provide an output file path")
  }

  report_pth <- system.file("OMOP-Summary.Rmd", package = "ccaa.inspectEHR")
  rmarkdown::render(report_pth, params = params,
                    output_file = params$output,
                    envir = new.env(),
                    output_dir = params$output_path)
}


#' Render Quality Report
#'
#' @param output an output file path
#' @param prams a string "ask", which will launch an interactive window to enter
#'   database connection details, or a list that contains these details:
#'
#'   - driver: string, can be any of: "PostgreSQL", "ODBC", or "SQLite"
#'   - host: string, host name
#'   - port: integer, port number
#'   - dbname: string, database name
#'   - schema: string, database target schema
#'   - user: string, database username
#'   - password: string, database user password
#'   - local_hospital: string, local hospital
#'
#' @return
#' @export
render_quality_report <- function(params = "ask") {

  if (is.null(params$output)) {
    rlang::abort("please provide an output file path")
  }

  report_pth <- system.file("OMOP-Data-Quality.Rmd", package = "ccaa.inspectEHR")
  rmarkdown::render(report_pth, params = params,
                    output_file = params$output,
                    envir = new.env(),
                    output_dir = params$output_path)
}
