#' Retrieve total mp3 file duration
#'
#' uses ffprobe to estimate the duration of an mp3 resource
#'
#' @param path the path to the file
#' @return a number of seconds
#' @noRd
mp3_file_duration <- function(path){
  path = normalizePath(path)
  if(!file.exists(path))
    stop(paste('So such file', path))
  command = sprintf("ffprobe  '%s' 2>&1", path)
  out <- system(command, intern=TRUE)
  duration_line = grep('Duration:',out,value = T)
  re = '\\d+:\\d+:\\d+\\.\\d+'
  str_duration = stringr::str_match(duration_line,re)
  dur = hms::as_hms(str_duration)
  as.numeric(dur)
  
}

#' Extracts a section (chunk) of an mp3 file
#'
#' Read an arbitrary section of an mp3 file as a Wave object
#'
#' @param path the path to the file
#' @param start the staring point, in seconds
#' @param duration the length of the chunk, in seconds
#' @param cache_dir an optional directory, where the chunk is indefinitely stored (see details)
#' @param quiet logical, whether to hide the verbose output 
#' @return a [tuneR::Wave] object corresponding to the parsed chunk
#' @details since seeking and clipping mp3 is computationally consuming using cache (`cache_dir`) \
#' stores the resulting mp3 section. Caching hashes the path of the file as well as the chunk start and duration.
#' In other words, if the path is changed, or a new duration/starting point is requested, a new mp3 will be generated.
#' One can eventually delete the cache directory to free space.
#' @export
mp3_extract_chunk <- function(path, start, duration, cache_dir=NULL, quiet=TRUE){
  path = normalizePath(path)
  if(!is.null(cache_dir)){
    subdir = paste(basename(path),digest::digest(path), sep='-')
    filename = sprintf('%d-%d.mp3', start ,duration)
    target = file.path(cache_dir, subdir, filename)
    dir.create(dirname(target), recursive = TRUE)
    if(file.exists(target))
      return(tuneR::readMP3(target))
  }
  
  tmp_mp3 = tempfile(pattern = 'tempaural_', fileext = '.mp3')
  on.exit(try(unlink(tmp_mp3)))
  command = sprintf("ffmpeg -ss %i -t %i -i '%s' -c:a copy '%s'", start, duration, path, tmp_mp3)
  if(quiet)
    command = paste(command, '-hide_banner -loglevel panic')
  system(command,wait = TRUE)
  
  out <- tuneR::readMP3(tmp_mp3)
  if(!is.null(cache_dir))
    file.copy(tmp_mp3, target)
  out
}
