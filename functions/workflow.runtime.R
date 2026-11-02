# Publish only complete, readable files. Temporary files share the target filesystem.
atomic_save <- function(..., file, envir = parent.frame(), compress = TRUE) {
  objects <- as.character(substitute(list(...)))[-1L]
  tmp <- tempfile(pattern = paste0(".", basename(file), "-"), tmpdir = dirname(file))
  on.exit(unlink(tmp), add = TRUE)
  save(list = objects, file = tmp, envir = envir, compress = compress)
  validate_rdata(tmp, objects)
  if (!file.rename(tmp, file)) stop("Cannot publish output: ", file)
  invisible(file)
}

validate_rdata <- function(file, objects) {
  if (!file.exists(file) || is.na(file.info(file)$size) || file.info(file)$size <= 0)
    stop("Missing or empty output: ", file)
  check <- new.env(parent = emptyenv())
  found <- load(file, envir = check)
  if (!all(objects %in% found)) stop("Output lacks required objects: ", file)
  invisible(TRUE)
}

atomic_zip <- function(zipfile, files) {
  tmp <- tempfile(pattern = ".archive-", tmpdir = dirname(zipfile), fileext = ".zip")
  on.exit(unlink(tmp), add = TRUE)
  if (!all(file.exists(files))) stop("Missing archive input")
  status <- utils::zip(zipfile = tmp, files = files)
  if (!identical(as.integer(status), 0L)) stop("zip failed for ", zipfile)
  unzip <- Sys.which("unzip")
  if (!nzchar(unzip)) stop("unzip is required to verify archives")
  result <- system2(unzip, c("-tqq", shQuote(tmp)), stdout = TRUE, stderr = TRUE)
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) stop("Archive verification failed: ", zipfile)
  if (!file.rename(tmp, zipfile)) stop("Cannot publish archive: ", zipfile)
  invisible(zipfile)
}

atomic_write_csv <- function(x, file, ...) {
  tmp <- tempfile(pattern = paste0(".", basename(file), "-"), tmpdir = dirname(file))
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(x, file = tmp, ...)
  if (!file.exists(tmp) || file.info(tmp)$size <= 0) stop("Empty CSV: ", file)
  if (!file.rename(tmp, file)) stop("Cannot publish CSV: ", file)
  invisible(file)
}
