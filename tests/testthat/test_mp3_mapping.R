context("mp3")

test_that("mp3_mapping works on a single file", {
  exple_dir = tempaural::tempaural_example_dir()
  test_file = file.path(exple_dir, "ID2", "200809_1706.mp3")

  my_function <- function(wave){
    out <- list(
                entropy = seewave::H(wave),
                duration = length(wave@left)/wave@samp.rate)
    out
  }
  dt = tempaural::map_mp3_chunks(test_file, my_function, 60)
  # the sum of duration should be equal to the total duration
  testthat::expect_true(abs(dt[,sum(duration)] - tempaural:::mp3_file_duration(test_file)) < 1)
  })

context("mp3")

test_that("mp3_mapping works on a single file", {
  metadata = data.table(ID = c('ID1', 'ID2'),
                        condition= 1:2,
                        start_datetime=c('2020-08-09 16:00:00', '2020-08-09 16:00:00'),
                        stop_datetime=c('2020-08-09 18:00:00','2020-08-09 19:00:00'))
  
  my_function <- function(wave){
    out <- list(
      entropy = seewave::H(wave),
      duration = length(wave@left)/wave@samp.rate)
    out
  }
  dt = tempaural::map_mp3_chunks(test_file, my_function, 60)
  # the sum of duration should be equal to the total duration
  testthat::expect_true(abs(dt[,sum(duration)] - tempaural:::mp3_file_duration(test_file)) < 1)
})