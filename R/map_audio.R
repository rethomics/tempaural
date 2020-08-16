#' Apply an arbitrary function on successive chunks of an audio file
#' 
#' Map a user-defined function to a series of consecutive windows of arbitrary duration. 
#' 
#' @param path the path to the file
#' @param FUN a function to be run on each chunk, see detail
#' @param chunk_duration the length of the chunk, in seconds
#' @param show_progress logical, whether to display progress
#' @param cache_dir an optional directory, where the chunk is indefinitely stored
#' @param ffmpeg_quiet logical, whether to hide the verbose output from ffmpeg
#' @param ... arguments to be passed to `FUN`
#' @return a data.table where each row is a chunk. columns are the start time, in second, and the variables defined by `FUN`
#' @details The function must use a [tuneR::Wave] as an input and output a named list. Each element of the list will be parsed as a new column in the output
#' @export
map_mp3_chunks <- function(path,
                             FUN,
                             chunk_duration, 
                             show_progress=FALSE,
                             cache_dir=NULL,
                             ffmpeg_quiet=TRUE, 
                             ...){
  ID =  start = .N  = NULL
  duration <- mp3_file_duration(path)

  mapper <- function(start) {
    if(show_progress)
      message(sprintf("%s: %f",path,start/duration))
    FUN(mp3_extract_chunk(path, start, chunk_duration,cache_dir, ffmpeg_quiet), ...)
    
  }  
  out <- data.table::data.table(start=seq(from=0, to=duration, by=chunk_duration))
  out[, ID:=1:.N]
  out[, mapper(start), by=ID]
}


map_dir_chunks <- function(dir, FUN, chunk_duration, ){
  #1 get a table of all ranges
  #2 create a table of chunk_id, start, path, start_in_path, duration for each chunk
  #3 fetch each chunk from its respective source file, and apply FUN
  #4 return t (start), + FUN vars
}
#' get all files in a directory that match a requested datetime range
#' @noRd
fetch_scope_files_for_id <- function(dir, start_datetime, end_datetime){
  
  #1 list mp3 files
  #2 apply, to each mp3, get_datetime...
  #3 check ranges are roughly continuous (no gaps, no redundancies)! Error/warn otherwise
  # return the list of files with range and duration in a table
}



#' uses the file name of a audio file to extract the start time, and its duration, for the end time
#' @noRd
get_datetime_range_for_file <- function(file, start_datetime, end_datetime){
  
}

