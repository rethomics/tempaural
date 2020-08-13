
file_duration <- function(path){
  path = normalizePath(path)
  command = sprintf("ffprobe  '%s' 2>&1", path)
  out <- system(command, intern=TRUE)
  duration_line = grep('Duration:',out,value = T)
  re = '\\d+:\\d+:\\d+\\.\\d+'
  str_duration = stringr::str_match(duration_line,re)
  dur = hms::as_hms(str_duration)
  as.numeric(dur)
  
  }

extract_mp3_chunk <- function(path, start, duration, cache_dir=NULL, quiet=TRUE){
  path = normalizePath(path)
  
  
  if(!is.null(cache_dir)){
    subdir = paste(basename(path),digest::digest(path), sep='-')
    filename = sprintf('%d-%d.mp3', start ,duration)
    target = file.path(cache_dir, subdir, filename)
    dir.create(dirname(target), recursive = TRUE)
    
    print(target)
    if(file.exists(target))
      return(tuneR::readMP3(target))
  }
  
  tmp_mp3 = tempfile(pattern = 'tempaural_', fileext = '.mp3')
  on.exit(try(unlink(tmp_mp3)))
  command = sprintf("ffmpeg -ss %i -t %i -i '%s' '%s'", start, duration, path, tmp_mp3)
  if(quiet)
    command = paste(command, '-hide_banner -loglevel panic')
  system(command,wait = TRUE)
  
  out <- tuneR::readMP3(tmp_mp3)
  if(!is.null(cache_dir))
    file.copy(tmp_mp3, target)
  out
}

map_chunks <- function(FUN, path, chunk_duration, show_progress=FALSE,cache_dir=NULL,ffmpeg_quiet=TRUE, ...){
  duration <- file_duration(path)
  mapper <- function(start) {
    if(show_progress)
      message(sprintf("%s: %f",path,start/duration))
    FUN(extract_mp3_chunk(path, start, chunk_duration,cache_dir, ffmpeg_quiet), ...)

  }  
  out <- data.table::data.table(path = path, 
                                start=seq(from=0, to=duration, by=chunk_duration))
  out[, ID:=1:.N]
  out[, mapper(start), by=ID]
}
test_file <- '~/Desktop/tempaural/Quentin Collab/C16_audio/05_23DEC19_0632_C16.mp3'
chunk_size <- 60

my_function <- function(wave){
  print(length(wave)/wave@samp.rate)
  return(1)
  out <- list(entropy = seewave::H(wave))
  out
}

dt <- map_chunks(my_function, test_file, 60, cache_dir = '~/Desktop', show_progress = TRUE)
dt
# duration = file_duration(test_file)
# 
# w = extract_mp3_chunk(test_file, 100000, 10, F)
#     
library(digest)
