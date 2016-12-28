.onLoad <- function(libname, pkgname) {
  op.attrCUSUM <- list(attrCUSUM.Need.CheckArgs_getAve = TRUE,
                       attrCUSUM.Need.CheckArgs_getContl = TRUE,
                       attrCUSUM.limit.minnumsubI = 100L,
                       attrCUSUM.limit.maxnumsubI = 6000L,
                       attrCUSUM.limit.maxndec = 7L,
                       attrCUSUM.limit.anss.target = 50000L)
  options(op.attrCUSUM)
  invisible()
}

.onUnload <- function(libpath) {
  op.attrCUSUM <- list(attrCUSUM.Need.CheckArgs_getAve = NULL,
                       attrCUSUM.Need.CheckArgs_getContl = NULL,
                       attrCUSUM.limit.minnumsubI = NULL,
                       attrCUSUM.limit.maxnumsubI = NULL,
                       attrCUSUM.limit.maxndec = NULL,
                       attrCUSUM.limit.anss.target = NULL)
  options(op.attrCUSUM)
  library.dynam.unload("attrCUSUM", libpath)
  invisible()
}

.onAttach <- function(libname, pkgname) {
}

.onDetach <- function(libpath) {
}

