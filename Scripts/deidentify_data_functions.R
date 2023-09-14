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
