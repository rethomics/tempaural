context("mapping")
# 
# test_that("mp3_mapping works on a single file", {
#   exple_dir = tempaural::tempaural_example_dir()
#   test_file = file.path(exple_dir, "ID2", "200809_1706.mp3")
# 
#   my_function <- function(wave){
#     out <- list(
#                 # entropy = seewave::H(wave),
#                 duration = length(wave@left)/wave@samp.rate)
#     out
#   }
#   dt = tempaural::map__chunks(test_file, my_function, 60)
#   # the sum of duration should be equal to the total duration
#   testthat::expect_true(abs(dt[,sum(duration)] - tempaural:::mp3_file_duration(test_file)) < 1)
#   })



test_that("map_chunk_dir works", {
  set.seed(1)
  exple_dir = tempaural::tempaural_example_dir()
  metadata = data.table::data.table(id=c('ID1','ID2'),
                       start_datetime = c('2020-08-09 12:09:00','2020-08-09 17:08:00'),
                       end_datetime = c('2020-08-09 17:15:00', '2020-08-09 17:15:00'),
                       genotype=c('addesaf','fewsfr'))

  # a function that takes a wave as input and just outputs its duration 
  # and a random variable
  my_function <- function(wave){
    out <- list(
      my_var = rnorm(1),
      duration = length(wave@left)/wave@samp.rate)
      
    out
  }
  dt <- tempaural::map_dir_chunks(metadata, exple_dir, FUN=my_function, chunk_duration = 60)
  d = tempdir()
  # on.exit(unlink(d, resursive=TRUE))
  # now caching on:
  dt1 <- tempaural::map_dir_chunks(metadata, exple_dir, FUN=my_function, 
                                  chunk_duration = 60, cache=d)
  # should be a LOT FASTER
  dt2 <- tempaural::map_dir_chunks(metadata, exple_dir, FUN=my_function, 
                                   chunk_duration = 60, cache=d)
  
  my_other_function <- function(wave){
    out <- list(
      my_var = rnorm(1)+1,
      duration = length(wave@left)/wave@samp.rate)
    
    out
  }
  dt3 <- tempaural::map_dir_chunks(metadata, exple_dir, FUN=my_function, 
                                   chunk_duration = 60, cache=d)
  
  # changing the function recomputed means not using the cache. whould get different results
  testthat::expect_true(sum(abs(dt1$my_var - dt3$my_var)) >0 )
})


test_that("fetch_scope_files_for_id works", {
  exple_dir = tempaural::tempaural_example_dir()
  exple_dir = file.path(exple_dir, 'ID2')
  all_chunks_dt = tempaural:::fetch_scope_files_for_id(exple_dir,
                                                      start_datetime =  as.POSIXct(x = '2020-08-09 17:08:00', tz='UTC'),
                                                      end_datetime =  as.POSIXct(x = '2020-08-09 17:15:00', tz='UTC'),
                                                      chunk_duration = 60, 
                                                      tz='UTC')

  all_chunks_dt
  testthat::expect_true(nrow(all_chunks_dt)>0)
})

test_that("get range from file works", {
  exple_dir = tempaural::tempaural_example_dir()
  test_file = file.path(exple_dir, "ID2", "200809_1706.mp3")
  out = tempaural:::get_datetime_range_for_file(test_file, tz='UTC')
  testthat::expect_true(as.numeric(out$file_end_datetime - out$file_start_datetime, unit='secs') == tempaural:::file_duration(test_file)
  )
})


test_that("parse_datetime from filename", {
  filename = '200809_1706.mp3'
  datetime = tempaural:::parse_start_datetime_from_file(filename, tz='UTC')
  expected_datetime = as.POSIXct(x = '2020-08-09 17:06:00', tz='UTC')
    testthat::expect_equal(expected_datetime, datetime)
})