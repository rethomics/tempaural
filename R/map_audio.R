#' Apply a user-defined function on target audio files 
#' 
#' Uses a metadata table to query a local audio file library and apply an arbitrary function on consecutive time windows
#' 
#' @param metadata a metadata table. See details.
#' @param root_dir the root directory of the local library. See details.
#' @param FUN a function to be run on each chunk. See details.
#' @param chunk_duration the length of the chunk, in seconds
#' @param cache an optional directory, where the chunk is indefinitely stored
#' @param verbose logical, whether to show progress and other messages
#' @param tz, the timezone the files and provided dates are formatted in, default 'UTC'
#' @param ... arguments to be passed to `FUN`
#' @return a [behavr::behavr] table. The metadata corresponds to the user-provided `metadata`. 
#' The data has the variables:
#' * `id` -- matching the `id` in the metadata. 
#' * `t` --  the time in second from the `start_datetime` requested for this `id`
#' * `...` -- variables computed by `FUN`
#' @details 
#' Metadata is a table in which each row defines an individual. It must have the following columns:
#' * `id` -- a character that uniquely identifies a biological individual. 
#' For each value of `id`, a sub-directory, with the same name, must exist in `root_dir`. 
#'* `start_datetime` -- formatted as `YYYY-MM-DD HH:MM:SS` The first requested time point for the individual.
#'**Importantly, `start_datetime` will be used as the reference $t_0$.** in other words, if you want to express time relative to 10:00:00 -- e.g. as it would be a ZT0 --
#'you can specify `start_datetime` as "YYYY-MM-DD 10:00:00".
#' * `end_datetime` -- the last requested time point for an individual.
#' In addition,  metadata can contain user-defined columns that will be used as metavariables (e.g. individual's genotype, treatment, ...)   
#' 
#' Each `id` defined in metadata must correspond to a subdirectory in `root_dir` with the exact same name. 
#' For instance, if you have `"animal_01"` and `"animal_02"` in the id feild of your metadata, 
#' you then will have the directories `"<root_dir>/animal_01"` and `"<root_dir>/animal_02"`.
#' 
#' 
#' The function `FUN` must use a [tuneR::Wave] as an input and output a named list. 
#' Each element of the list will be parsed as a new column in the resulting [behavr::behavr] table.
#' 
#' Processing long audio file might be very long, according to the funcion (`FUN`) that is mapped. 
#' If caching is turned on, the results of the computation on each chunk will be saved in a custom directory (`cache`).
#' In other words, the first time `FUN` is run on a chink, the result is saved, and will not be recomputed, as long as the same function is applied on the same chunk, from the same file.
#' Defining `cache` as a local directory turns caching on, and saves `R` objects accordingly. 
#' Deleting the content of this directory is safe, but implies subsequent calls to `FUN` will be reevaluated.
#' @examples 
#' # get the path the the package-provided example directory
#' exple_dir = tempaural::tempaural_example_dir()
#' # show all the files in it
#' print(list.files(exple_dir, recursive = TRUE))
# define metadata (typically would be in an external CSV)
#' metadata = data.frame(id=c('ID1','ID2'),
#'                       start_datetime = c('2020-08-09 12:09:00','2020-08-09 17:08:00'),
#'                       end_datetime = c('2020-08-09 17:15:00', '2020-08-09 17:15:00'),
#'                       genotype=c('addesaf','fewsfr'))
#' print(metadata)
#' # a function that takes a wave as input and just outputs its duration 
#' # and a random variable
#'my_function <- function(wave){
#'  out <- list(
#'    my_var = rnorm(1),
#'      duration = length(wave@left)/wave@samp.rate
#'      )
#'  out
#'}
#'# now we map this function to all the matching audio chunks
#' dt <- tempaural::map_dir_chunks(metadata, exple_dir, 
#'                                 FUN=my_function, chunk_duration = 60)
#' @export
map_dir_chunks <- function(metadata, root_dir, 
                           FUN, chunk_duration, 
                           tz='UTC', 
                           cache=NULL, 
                           verbose = FALSE, 
                           ...){
  # `id` in metadata should exist and match sub-directory name
  if(!'id' %in% colnames(metadata))
    stop("metadata should have a column name `id`")

  if(!'start_datetime' %in% colnames(metadata))
    stop("metadata should have a column name `start`")
  
  if(!'end_datetime' %in% colnames(metadata))
    stop("metadata should have a column name `end_datetime`")
  
  if(!dir.exists(root_dir))
    stop("`root_dir' is not a directory/does not exist")
  
  metadata <- data.table::as.data.table(metadata)
  metadata[, start_datetime_posix_ := parse_datetime(start_datetime, tz=tz), by=seq_len(nrow(metadata))]
  metadata[, end_datetime_posix_ := parse_datetime(end_datetime, tz=tz), by=seq_len(nrow(metadata))]
  metadata[, start_datetime := NULL]
  metadata[, end_datetime := NULL]
  data.table::setnames(metadata, c('start_datetime_posix_','end_datetime_posix_'),
                     c('start_datetime',       'end_datetime'))
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
  wrapped_mapper <- wrap_mapper(FUN, cache = cache)
  fun_results = all_chunks_dt[, 
                      wrapped_mapper(FUN, path, t_in_file, chunk_duration, verbose),
                  by=list(id,chunk_id, chunk_start_datetime )]
  data.table::setkey(fun_results,id)
  data.table::setkey(metadata,id)
  dt <- behavr::behavr(fun_results,metadata)
  dt[, chunk_id := NULL]
  dt[, t:=as.numeric(chunk_start_datetime - xmv(start_datetime), unit='secs') ]
  dt[, chunk_start_datetime := NULL]
  dt
}


wrap_mapper <- function(FUN, cache){
  mapper <- function(FUN, path, start, chunk_duration, verbose, ...) {
    out <- FUN(extract_audio_chunk(path, start, chunk_duration, !verbose), ...)
    if(verbose)
      message(sprintf("%s: %f. Result: ",path, start/duration, paste(out)))
    out
    } 
  
  if(!is.null(cache)){
    db <- memoise::cache_filesystem(cache, algo="md5")
    out <- memoise::memoise(mapper, cache=db)
  }
  else{
    out = mapper
  }
  out
}


#' get all chunks/file in a subdirectory that match a requested datetime range
#' @noRd
fetch_scope_files_for_id <- function(dir, start_datetime, end_datetime, chunk_duration, tz='UTC'){
  
  #1 FIXME should list arbitrary audio files
  all_files_dt = data.table::data.table(path = list.files(dir, pattern='.mp3',full.names = TRUE))
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
  duration = file_duration(path)
  end_datetime = start_datetime + duration
  list(file_start_datetime=start_datetime, file_end_datetime=end_datetime)
}


# format such as YYMMDD_HHSS.EXT
#' @noRd
parse_start_datetime_from_file <- function(filename, tz='UTC'){
  datetime = lubridate::parse_date_time(tools::file_path_sans_ext(filename),
                                        "%y%m%d_%H%M", tz=tz)
  if(is.na(datetime))
    stop(paste("Could not parse datetime from filename:", filename))
  datetime
  }

# 
# 
# map_mp3_chunks <- function(path,
#                            FUN,
#                            chunk_duration, 
#                            show_progress=FALSE,
#                            cache=NULL,
#                            ffmpeg_quiet=TRUE, 
#                            ...){
#   ID =  start = .N  = NULL
#   duration <- mp3_file_duration(path)
#   
#   mapper <- function(start) {
#     if(show_progress)
#       message(sprintf("%s: %f",path,start/duration))
#     FUN(mp3_extract_chunk(path, start, chunk_duration,cache_dir, ffmpeg_quiet), ...)
#     
#   }  
#   out <- data.table::data.table(start=seq(from=0, to=duration, by=chunk_duration))
#   out[, ID:=1:.N]
#   out[, mapper(start), by=ID]
#   
# }
