sc <- function(n, x) {
  lc <- grep("Level|dlvl|Pair|pair", names(x), value = TRUE)
  cat(n, nrow(x), "cols:", paste(head(lc, 4), collapse = " "), "\n")
  for (cn in lc) {
    l <- unique(as.character(x[[cn]]))
    s <- vapply(strsplit(l, "_"), function(v) length(v) == 2 && v[1] == v[2], TRUE)
    cat("   ", cn, "labels", length(l), "self", sum(s), paste(head(l[s], 3), collapse = ","), "\n")
  }
}
for (f in commandArgs(TRUE)) {
  e <- new.env(); suppressWarnings(load(f, e)); cat("==", basename(f), "\n")
  for (n in ls(e)) {
    x <- e[[n]]
    if (inherits(x, "ggplot")) next
    if (is.data.frame(x)) sc(n, x) else if (is.list(x)) for (k in names(x)) if (is.data.frame(x[[k]])) sc(paste(n, k), x[[k]])
  }
}
