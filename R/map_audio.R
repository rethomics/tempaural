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

#' @export
map_dir_chunks <- function(metadata, root_dir, FUN, chunk_duration, tz='UTC', show_progress = FALSE, 
                           cache_dir=NULL, ffmpeg_quiet=TRUE, ...){
  # `id` in metadata should exist and match sub-directory name
  if(!'id' %in% colnames(metadata))
    stop("metadata should have a column name `id`")

  if(!'start_datetime' %in% colnames(metadata))
    stop("metadata should have a column name `start`")
  
  if(!'end_datetime' %in% colnames(metadata))
    stop("metadata should have a column name `end_datetime`")
  
  if(!dir.exists(root_dir))
    stop("`root_dir' is not a directory/does not exist")
  
  metadata <- copy(metadata)
  metadata[, subdir_for_id := file.path(root_dir,id)]
  missing_dirs = metadata[!dir.exists(metadata[,subdir_for_id])]
  if(nrow(missing_dirs))
    stop(sprintf("Some directories do not exist/match id, in metadata: %s", past(missing_dirs[,id])))
  

  all_chunks_dt <- metadata[,
          fetch_scope_files_for_id(subdir_for_id,as.POSIXct(start_datetime), 
                                 as.POSIXct(end_datetime), 
                                 chunk_duration, 
                                 tz),
          by='id']
  all_chunks_dt[, chunk_id :=1:.N]
  
  mapper <- function(start, path) {
    if(show_progress)
      message(sprintf("%s: %f",path,start/duration))
    FUN(mp3_extract_chunk(path, start, chunk_duration,cache_dir, ffmpeg_quiet), ...)
    
  }  
  fun_results = all_chunks_dt[, 
                  mapper(t_in_file, path),
                  by=list(id,chunk_id, chunk_start_datetime )]
  setkey(fun_results,id)
  setkey(metadata,id)
  dt <- behavr::behavr(fun_results,metadata)
  dt[, chunk_id := NULL]
  dt[, t:=as.numeric(chunk_start_datetime - xmv(start_datetime), unit='secs') ]
  dt[, chunk_start_datetime := NULL]
  dt
}

#' get all chunks/file in a subdirectory that match a requested datetime range
#' @noRd
fetch_scope_files_for_id <- function(dir, start_datetime, end_datetime, chunk_duration, tz='UTC'){
  
  #1 list mp3 files
  all_files_dt = data.table(path = list.files(dir, pattern='mp3',full.names = TRUE))
  if(nrow(all_files_dt) <1)
    stop(paste('No valid file in', dir))
  #2 apply, to each mp3, get_datetime...
  all_files_dt = all_files_dt[, get_datetime_range_for_file(path, tz=tz), by=path]
  all_files_dt = all_files_dt[order(file_start_datetime)]
  #3 check ranges are roughly contiguous (no gaps, no redundancies)! Error/warn otherwise
  
  if(!all_files_dt[, all(as.numeric(file_start_datetime[2:.N] - file_end_datetime[1:.N-1],unit='secs') < chunk_duration)])
    stop(sprintf('Files in dir %s are not contiguous',dir))
  
  
  seq_chunk <- function(start_datetime, end_datetime, length){
    duration = as.numeric(end_datetime - start_datetime,unit='secs')
    seq(from=0, 
        to=duration,
        by=length)
  }
  all_chunks_dt <- all_files_dt[, .(t_in_file=seq_chunk(file_start_datetime, file_end_datetime, chunk_duration))
                                ,by=list(path,file_start_datetime)]
  
  all_chunks_dt[,chunk_start_datetime := file_start_datetime +t_in_file]
  all_chunks_dt = all_chunks_dt[chunk_start_datetime %between% c(start_datetime,end_datetime)]
  if(nrow(all_chunks_dt) <1)
    stop(paste('No chunk found for selected datetime range in', dir))
  #fixme here, we could actually pick systematically from the second file as the last chunk from the first is likely partial
  all_chunks_dt = unique(all_chunks_dt,by='chunk_start_datetime')
}



#' uses the file name of a audio file to extract the start time, and its duration, for the end time
#' @noRd
get_datetime_range_for_file <- function(path, tz='UTC'){
  start_datetime = parse_start_datetime_from_file(basename(path), tz)
  duration = mp3_file_duration(path)
  end_datetime = start_datetime + duration
  list(file_start_datetime=start_datetime, file_end_datetime=end_datetime)
}

parse_start_datetime_from_file <- function(filename, tz='UTC'){
  datetime = lubridate::parse_date_time(tools::file_path_sans_ext(filename),
                                        "%y%m%d_%H%M", tz=tz)
  if(is.na(datetime))
    stop(paste("Could not parse datetime from filename:", filename))
  datetime
  }
