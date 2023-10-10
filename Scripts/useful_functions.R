#' Initialize a Logfile
#'
#' This function creates the necessary directories and initializes a log file with the specified prefix and function name. THIS NEEDS TO BE UPDATED
#'
#' @param time_case_prefix Character. Prefix for the log filename which may often represent the time or case.
#' @param function_name Character. Name of the function for which the log file is being created. This helps categorize logs by function.
#'
#' @return TRUE if the log file was successfully created, NA otherwise.
#' @details If there's any error in creating the logfile, the function stops with a message, and if there's a warning, it will be printed out.
#'
#' @examples
#' \dontrun{
#' initialize_logfile("2023_09_13_", "myFunctionName")
#' }
#'
#' @importFrom logr log_open
#' @importFrom here here
#' @export
initialize_logfile <- function(time_case_prefix, function_name){
  dir.create(here("Intermediate"))
  dir.create(here("Intermediate","Logs"))
  dir.create(here("Intermediate","Logs",function_name))
  logr::log_open(here("Intermediate","Logs",function_name, paste0(time_case_prefix,function_name,"_logfile.log")))
}