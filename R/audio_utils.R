#' Retrieve total audio file duration
#'
#' Uses ffprobe to estimate the duration of an audio resource
#'
#' @param path the path to the file
#' @return a number of seconds
#' @export
file_duration <- function(path){
  check_ffmpeg(exe='ffprobe')
  path = normalizePath(path)
  if(!file.exists(path))
    stop(paste('So such file', path))
  out <- system2("ffprobe", args=path ,stdout=TRUE, stderr=TRUE)
  duration_line = grep('Duration:',out,value = T)
  re = '\\d+:\\d+:\\d+\\.\\d+'
  str_duration = stringr::str_match(duration_line,re)
  dur = hms::as_hms(str_duration)
  as.numeric(dur)
  
}

#' Extracts a section (chunk) of an audio file
#'
#' Read an arbitrary section of an mp3 file as a Wave object
#'
#' @param path the path to the file
#' @param start the staring point, in seconds
#' @param duration the length of the chunk, in seconds
#' @param quiet logical, whether to hide the verbose output 
#' @return a [tuneR::Wave] object corresponding to the parsed chunk
#' @export
extract_audio_chunk <- function(path, start, duration, quiet=TRUE){
  check_ffmpeg()
  path = normalizePath(path)
  ext = sprintf(".%s", tools::file_ext(path))
  tmp_mp3 = tempfile(pattern = 'tempaural_', fileext = ext)
  on.exit(try(unlink(tmp_mp3)))
  args = c(paste0("-ss ", start),
          paste0("-t ", duration),
          paste0("-i ", path),
          "-c:a copy",
          tmp_mp3
          )
  if(quiet)
    args = c(args, '-hide_banner', "-loglevel panic")
  system2('ffmpeg', args = args, stdout = TRUE, wait = TRUE)
  
  out <- tuneR::readMP3(tmp_mp3)
  out
}

#' @noRd
check_ffmpeg <- function(exe='ffmpeg'){
 if(!file.exists(Sys.which(exe)))
   stop(sprintf("Could not find %s Please install it and ensure it is in your path.", exe))
}
