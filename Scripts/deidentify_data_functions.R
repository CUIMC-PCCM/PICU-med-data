return_room_location <- function(i,data_df, loc_df){
  tryCatch({
    index_gt <- (data_df$time_stamp[i] >= loc_df$time_stamp) & data_df$EMPI_char[i] == loc_df$EMPI_char
    max_gt <- max(which(index_gt == TRUE))
    return(loc_df$LOC__ROOM[max_gt])
  }, error = function(e) {
    message("Caught an error returning room location: ", e$message)
    # quit("no", status = 10)
  })
}

check_mar_vs_identity_key <- function(mar_df, key_df){
  logr::log_print("in MAR and key")
  logr::log_print(length(mrn_in_key_and_mar <- unique(intersect(key_df$identified_key_char,mar_df$EMPI_char))))
  logr::log_print("in MAR but not in key")
  logr::log_print(length(MAR_mrn_wo_deidentified_key <- unique(mar_df$EMPI_char[mar_df$EMPI_char %!in% key_df$identified_key_char ])))
  logr::log_print("in key but not in mrn")
  logr::log_print(length(deidentified_key_mrn_wo_MAR <- unique(key_df$identified_key_char[key_df$identified_key_char %!in%  mar_df$EMPI_char])))
}

writing_deidentified_data <- function(merge_df, cdw_epic, arguments){
  new_names <- names(merge_df)
  new_names <- new_names[new_names != arguments$identity_header]
  write_df = subset(merge_df, select = c(new_names) )
  write_df$EMPI_char <- NULL
  write_df$identified_key <- NULL
  write.table(x = write_df %>% filter(!is.na(deidentified_key)), file = gzfile(here("Intermediate",paste0(time_case_prefix,cdw_epic,"_deidentified_data.tsv.gz"))), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
}
