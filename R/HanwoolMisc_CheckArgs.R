# CheckArgs ---------------------------------------------------------------
CheckArgs_NewEnvir <- function(){
  CheckArgs.Envir <- as.environment(list(nErr = 0L, ErrMsg = NULL,
                                         nWarn = 0L, WarnMsg = NULL,
                                         nMessage = 0L, MessageMsg = NULL))
  class(CheckArgs.Envir) <- c("CheckArgs_Envir", class(CheckArgs.Envir))
  CheckArgs.Envir
}

CheckArgs_Inherits <-
  function(x, what, which = FALSE,
           msg = paste("Argument 'CheckArgs.Envir'",
                       "must be an object of class 'CheckArgs_Envir'")) {
  if (!isTRUE(inherits(x, what, which))) {
    stop(msg, call. = FALSE)
  }
  TRUE
}

CheckArgs_AddError <- function(Msg, CheckArgs.Envir) {
  CheckArgs_Inherits(CheckArgs.Envir, "CheckArgs_Envir")
  assign(x = "nErr",
         value = get(x = "nErr", envir = CheckArgs.Envir) + 1L,
         envir = CheckArgs.Envir)
  assign(x = "ErrMsg",
         value = c(get(x = "ErrMsg", envir = CheckArgs.Envir), Msg),
         envir = CheckArgs.Envir)
}

CheckArgs_AddWarn <- function(Msg, CheckArgs.Envir) {
  CheckArgs_Inherits(CheckArgs.Envir, "CheckArgs_Envir")
  assign(x = "nWarn",
         value = get(x = "nWarn", envir = CheckArgs.Envir) + 1L,
         envir = CheckArgs.Envir)
  assign(x = "WarnMsg",
         value = c(get(x = "WarnMsg", envir = CheckArgs.Envir), Msg),
         envir = CheckArgs.Envir)
}

CheckArgs_AddMessage <- function(Msg, CheckArgs.Envir) {
  CheckArgs_Inherits(CheckArgs.Envir, "CheckArgs_Envir")
  assign(x = "nMessage",
         value = get(x = "nMessage", envir = CheckArgs.Envir) + 1L,
         envir = CheckArgs.Envir)
  assign(x = "MessageMsg",
         value = c(get(x = "MessageMsg", envir = CheckArgs.Envir), Msg),
         envir = CheckArgs.Envir)
}

CheckArgs_ShowMsg <- function(CheckArgs.Envir) {
  CheckArgs_Inherits(CheckArgs.Envir, "CheckArgs_Envir")
  CheckArgs <- mget(ls(envir = CheckArgs.Envir), envir = CheckArgs.Envir)
  foo.call <- sys.call(which = -1)
  foo.call <- utils::capture.output(foo.call)
  if (CheckArgs$nMessage > 0) {
    message(paste(c("", foo.call, paste0(1:CheckArgs$nMessage, ": ",
                                         CheckArgs$MessageMsg)),
                  sep = "", collapse = "\n"), call. = FALSE)
  }
  if (CheckArgs$nWarn > 0) {
    warning(paste(c("", foo.call, paste0(1:CheckArgs$nWarn, ": ",
                                         CheckArgs$WarnMsg)),
                  sep = "", collapse = "\n"), call. = FALSE)
  }
  if (CheckArgs$nErr > 0) {
    stop(paste(c("", foo.call, paste0(1:CheckArgs$nErr, ": ",
                                      CheckArgs$ErrMsg)),
               sep = "", collapse = "\n"), call. = FALSE)
  }
}
