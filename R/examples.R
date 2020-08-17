#' Get path to tempaural example
#'
#' `tempaural` comes with a sample audio files in its `inst/extdata`
#' directory. `tempaural` allow  make them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' # list all files
#' tempaural_example()
tempaural_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "tempaural"))
  } else {
    system.file("extdata", path, package = "tempaural", mustWork = TRUE)
  }
}


#' @rdname tempaural_example
#' @export
tempaural_example_dir <- function() {
  system.file("extdata",package = "tempaural")
}
