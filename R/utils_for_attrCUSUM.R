Check_IsFiniteNumericVector <-
  function(x, argname = deparse(substitute(x)),
           min.length = 1L, max.length = 2 ^ 31 - 1,
           msg.missing = paste("Argument '", argname,
                               "' is missing, with no default", sep = ""),
           msg.stop = paste("Argument '", argname,
                            "' must be a finite numeric vector",
                            sep = ""),
           eval.stop = FALSE) {
    Check.IsFiniteNumericVector <- FALSE
    if (!missing(x)) {
      if (!is.null(x)) {
        if (is.atomic(x)) {
          if (is.numeric(x)) {
            if (length(x) >= min.length) {
              if (length(x) <= max.length) {
                if (all(is.finite(x))) {
                  Check.IsFiniteNumericVector <- TRUE
                }
              }
            }
          }
        }
      }
    } else {# Rise an error even if eval.stop equal FALSE
      stop(msg.missing, call. = FALSE)
    }
    if (all(eval.stop, !Check.IsFiniteNumericVector)) {
      stop(msg.stop, call. = FALSE)
    }
    Check.IsFiniteNumericVector
  }

Check_IsFiniteNumericMatrix <-
  function(x, argname = deparse(substitute(x)),
           min.nrow = 1L, max.nrow = as.integer((2 ^ 31 - 1) / 2),
           min.ncol = 1L, max.ncol = as.integer((2 ^ 31 - 1) / 2),
           msg.missing = paste("Argument '", argname,
                               "' is missing, with no default", sep = ""),
           msg.stop = paste("Argument '", argname,
                            "' must be a finite numeric matrix",
                            sep = ""),
           eval.stop = FALSE) {
    Check.IsFiniteNumericMatrix <- FALSE
    if (!missing(x)) {
      if (!is.null(x)) {
        if (is.atomic(x)) {
          if (is.matrix(x)) {
            if (is.numeric(x)) {
              if (nrow(x) >= min.nrow) {
                if (nrow(x) <= max.nrow) {
                  if (ncol(x) >= min.ncol) {
                    if (ncol(x) <= max.ncol) {
                      if (all(is.finite(x))) {
                        Check.IsFiniteNumericMatrix <- TRUE
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    } else {# Rise an error even if eval.stop equal FALSE
      stop(msg.missing, call. = FALSE)
    }
    if (all(eval.stop, !Check.IsFiniteNumericMatrix)) {
      stop(msg.stop, call. = FALSE)
    }
    Check.IsFiniteNumericMatrix
  }

Check_IsFiniteIntegerValuedNumericVector <-
  function(x, argname = deparse(substitute(x)),
           min.length = 1L, max.length = 2 ^ 31 - 1,
           msg.missing = paste("Argument '", argname,
                               "' is missing, with no default", sep = ""),
           msg.stop = paste("Argument '", argname,
                            "' must be a finite integer-valued numeric vector",
                            sep = ""),
           eval.stop = FALSE) {
    Check.IsFiniteIntegerValuedNumericVector <- FALSE
    if (Check_IsFiniteNumericVector(x = x, argname = argname,
                                    min.length = min.length,
                                    max.length = max.length,
                                    msg.missing = msg.missing,
                                    msg.stop = msg.stop,
                                    eval.stop = FALSE)) {
      if (all(as.integer(x) == x)) {
        Check.IsFiniteIntegerValuedNumericVector <- TRUE
      }
    }

    if (all(eval.stop, !Check.IsFiniteIntegerValuedNumericVector)) {
      stop(msg.stop, call. = FALSE)
    }
    Check.IsFiniteIntegerValuedNumericVector
  }

Check_IsTRUEFALSEVector <-
  function(x, argname = deparse(substitute(x)),
           min.length = 1L,
           max.length = 2 ^ 31 - 1,
           msg.missing = paste("Argument '", argname,
                               "' is missing, with no default", sep = ""),
           msg.stop = paste("Argument '", argname,
                            "' must be a vector of TRUE/FALSE",
                            sep = ""),
           eval.stop = FALSE) {
    Check.IsTRUEFALSEVector <- FALSE
    if (!missing(x)) {
      if (!is.null(x)) {
        if (is.atomic(x)) {
          if (is.logical(x)) {
            if (length(x) >= min.length) {
              if (length(x) <= max.length) {
                if (all(!is.na(x))) {
                  Check.IsTRUEFALSEVector <- TRUE
                }
              }
            }
          }
        }
      }
    } else {# Rise an error even if eval.stop equal FALSE
      stop(msg.missing, call. = FALSE)
    }
    if (all(eval.stop, !Check.IsTRUEFALSEVector)) {
      stop(msg.stop, call. = FALSE)
    }
    Check.IsTRUEFALSEVector
  }

Check_IsCharacterVector <-
  function(x, argname = deparse(substitute(x)),
           min.length = 1L,
           max.length = 2 ^ 31 - 1,
           msg.missing = paste("Argument '", argname,
                               "' is missing, with no default", sep = ""),
           msg.stop = paste("Argument '", argname,
                            "' must be a character vector",
                            sep = ""),
           eval.stop = FALSE) {
    Check.IsCharacterVector <- FALSE
    if (!missing(x)) {
      if (!is.null(x)) {
        if (is.atomic(x)) {
          if (is.character(x)) {
            if (length(x) >= min.length) {
              if (length(x) <= max.length) {
                Check.IsCharacterVector <- TRUE
              }
            }
          }
        }
      }
    } else {
      stop(msg.missing, call. = FALSE)
    }
    if (all(eval.stop, !Check.IsCharacterVector)) {
      stop(msg.stop, call. = FALSE)
    }
    Check.IsCharacterVector
  }

isFALSE <- function(x) {
  identical(FALSE, x)
}

ndecimal <- function(x, maxndecimal = 15L) {
  # Error Handling
  CheckArgs_ndecimal(x = x, maxndecimal = maxndecimal)

  # Main
  C_ndecimal(x_ = as.numeric(x), as.integer(maxndecimal))
}
CheckArgs_ndecimal <-
  function(x, maxndecimal,
           msg.tryCatch.x = paste("Argument 'x' is missing",
                                  "or corresponding object cannot be found"),
           msg.stop.x = "Argument 'x' must be a length-one numeric vector",
           msg.tryCatch.maxndecimal = paste("Argument 'maxndecimal' is missing",
                                            "or corresponding object cannot be found"),
           msg.stop.maxndecimal = paste("Argument 'maxndecimal'",
                                        "must be a non-negative integer value",
                                        "being less than or equal 15L"),
           eval.stop = TRUE, Check.err = NULL) {
    # Declaration
    if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()

    # Check : x
    Check.x <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = x, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.x,
                                         CheckArgs.Envir = Check.err)
                 NA}) # Rise an error even if eval.stop equal FALSE
    if (all(eval.stop, isFALSE(Check.x))) {
      CheckArgs_AddError(Msg = msg.stop.x,
                              CheckArgs.Envir = Check.err)
    }

    # Check : maxndecimal
    Check.maxndecimal <-
      tryCatch(do.call("Check_IsFiniteIntegerValuedNumericVector",
                       list(x = maxndecimal, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.maxndecimal,
                                         CheckArgs.Envir = Check.err)
                 NA}) # Rise an error even if eval.stop equal FALSE
    if (isTRUE(Check.maxndecimal)) {
      Check.maxndecimal <- ifelse(all(maxndecimal >= 0L, maxndecimal <= 15L),
                                  TRUE,
                                  FALSE)
    }
    if (all(eval.stop, identical(FALSE, Check.maxndecimal))) {
      CheckArgs_AddError(Msg = msg.stop.maxndecimal,
                              CheckArgs.Envir = Check.err)
    }

    # Finish
    CheckArgs_ShowMsg(Check.err)
    list(Check.err = Check.err,
         Check.xxx = c(Check.x, Check.maxndecimal))
  }

indicator_x_in_I <- function(x, Interval,
                             leftmost.closed = TRUE,
                             rightmost.closed = FALSE,
                             ndec = 7L) {
  # Error handling
  CheckArgs_indicator_x_in_I(x, Interval,
                             leftmost.closed, rightmost.closed, ndec)
  # Main
  C_indicator_x_in_I(x_ = as.numeric(x),
                     Interval_ = as.numeric(Interval),
                     leftmost_closed_ = as.logical(leftmost.closed),
                     rightmost_closed_ = as.logical(rightmost.closed),
                     ndec_ = as.integer(ndec))
}
CheckArgs_indicator_x_in_I <-
  function(x, Interval, leftmost.closed, rightmost.closed, ndec,
           msg.tryCatch.x = paste("Argument 'x' is missing",
                                  "or corresponding object cannot be found"),
           msg.stop.x = paste("Argument 'x'",
                              "must be a length-one numeric vector"),
           msg.tryCatch.Interval = paste("Argument 'Interval' is missing",
                                         "or corresponding object cannot be found"),
           msg.stop.Interval = paste("Argument 'Interval'",
                                     "must be a length-two numeric vector",
                                     "with elements l leq u"),
           msg.tryCatch.leftmost.closed = paste("Argument 'leftmost.closed' is missing",
                                                "or corresponding object cannot be found"),
           msg.stop.leftmost.closed = paste("Argument 'leftmost.closed'",
                                            "must be either TRUE or FALSE"),
           msg.tryCatch.rightmost.closed = paste("Argument 'rightmost.closed' is missing",
                                                 "or corresponding object cannot be found"),
           msg.stop.rightmost.closed = paste("Argument 'rightmost.closed'",
                                             "must be either TRUE or FALSE"),
           msg.tryCatch.ndec = paste("Argument 'ndec' is missing",
                                     "or corresponding object cannot be found"),
           msg.stop.ndec = paste("Argument 'ndec'",
                                 "must be a length-one non-negative integer vector",
                                 "being less than 15L"),
           eval.stop = TRUE, Check.err = NULL) {
    # Declaration
    if (is.null(Check.err)) Check.err <- CheckArgs_NewEnvir()

    # Check : x
    Check.x <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = x, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.x,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isFALSE(Check.x)) {
      CheckArgs_AddError(Msg = msg.stop.x,
                              CheckArgs.Envir = Check.err)
    }

    # Check : Interval
    Check.Interval <-
      tryCatch(do.call("Check_IsFiniteNumericVector",
                       list(x = Interval, min.length = 2L, max.length = 2L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.Interval,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.Interval)) {
      Check.Interval <- ifelse(Interval[1] <= Interval[2],
                               TRUE,
                               FALSE)
    }
    if (isFALSE(Check.Interval)) {
      CheckArgs_AddError(Msg = msg.stop.Interval,
                              CheckArgs.Envir = Check.err)
    }

    # Check : leftmost.closed
    Check.leftmost.closed <-
      tryCatch(do.call("Check_IsTRUEFALSEVector",
                       list(x = leftmost.closed, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.x,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isFALSE(Check.leftmost.closed)) {
      CheckArgs_AddError(Msg = msg.stop.leftmost.closed,
                              CheckArgs.Envir = Check.err)
    }

    # Check : rightmost.closed
    Check.rightmost.closed <-
      tryCatch(do.call("Check_IsTRUEFALSEVector",
                       list(x = rightmost.closed, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.x,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isFALSE(Check.rightmost.closed)) {
      CheckArgs_AddError(Msg = msg.stop.rightmost.closed,
                              CheckArgs.Envir = Check.err)
    }

    # Check : ndec
    Check.ndec <-
      tryCatch(do.call("Check_IsFiniteIntegerValuedNumericVector",
                       list(x = ndec, max.length = 1L)),
               error = function(e) {
                 CheckArgs_AddError(Msg = msg.tryCatch.ndec,
                                         CheckArgs.Envir = Check.err)
                 NA})
    if (isTRUE(Check.ndec)) {
      Check.ndec <- ifelse(all(ndec >= 0L, ndec <= 15L),
                           TRUE,
                           FALSE)
    }
    if (isFALSE(Check.ndec)) {
      CheckArgs_AddError(Msg = msg.stop.ndec,
                              CheckArgs.Envir = Check.err)
    }

    # Finish
    if (eval.stop) CheckArgs_ShowMsg(Check.err)
    list(Check.err = Check.err,
         Check.xxx = c(Check.x,
                       Check.Interval,
                       Check.leftmost.closed,
                       Check.rightmost.closed,
                       Check.ndec))
  }

Subintervals_lensubI <- function(minv = 0, maxv = 1, lensubI = 0.1, ndec = 7L) {
  # No error handling
  NULL
  # Main
  dig.lab  <-  round(max(log10(maxv), 0) + 1 + ndec)
  endpoints  <-  round(seq(from = minv, to = maxv, by = lensubI), ndec)
  length_endpoints <- length(endpoints)
  numsubI  <-  length_endpoints - 1;
  reprv.l <- endpoints[1:numsubI]
  reprv.r <- endpoints[2:length_endpoints]
  reprv.m <- (reprv.l + reprv.r) / 2
  I.info.co <- as.character(cut(x = reprv.m, breaks = endpoints,
                                right = FALSE, dig.lab = dig.lab))
  I.info.oc <- as.character(cut(x = reprv.m, breaks = endpoints,
                                right = TRUE, dig.lab = dig.lab))
  list(endpoints = endpoints, lensubI = lensubI, numsubI = numsubI,
       reprv.l = reprv.l, reprv.m = reprv.m, reprv.r = reprv.r,
       I.info.co = I.info.co, I.info.oc = I.info.oc)
}

BisecMethod <- function(foo, interval, tol = 0.1, maxndec = 7L) {
  interval <- round(interval, maxndec)
  a <- interval[1]
  b <- interval[2]
  iter <- 0

  root.a <- a
  value.a <- foo(a)
  root.b <- b
  value.b <- foo(b)
  if (any(value.a * value.b == 0, abs(b - a) < tol)) {
    res <- list(root.a = root.a, value.a = value.a,
                root.b = root.b, value.b = value.b, iter = iter)
    return(res)
  } else if (value.a * value.b > 0) {
    stop("Values of foo(a) and foo(b) have the same sign", call. = FALSE)
  } else if (value.a * value.b < 0) {
    repeat {
      if (abs(b - a) < tol) {
        root.a <- a
        value.a <- foo(a)
        root.b <- b
        value.b <- foo(b)
        break
      }
      x <- round((a + b) / 2, maxndec)
      iter <- iter + 1
      if (foo(a) * foo(x) < 0) {
        b <- x
      } else {
        a <- x
      }
    }
  }
  res <- list(root.a = root.a, value.a = value.a,
              root.b = root.b, value.b = value.b, iter = iter)
  res
}

