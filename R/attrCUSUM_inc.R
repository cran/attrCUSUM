## Internal functions related to VSI CUSUM for mean increase
attrCUSUM_inc_XtToCt <- function(Xt, refv, c.zero = 0, maxndec = 7L) {
  refv <- round(refv, maxndec)
  len <- length(Xt)
  Ct <- numeric(len)
  Ct[1] <- max(c.zero, 0) + Xt[1] - refv
  if (len > 1) {
    for (t in 2:len) {
      Ct[t] <- max(Ct[t - 1], 0) + Xt[t] - refv

    }
  }
  stats::as.ts(Ct)
}


attrCUSUM_inc_InitPr_VSI <- function(endpoints, c.zero, refv, pmf.process,
                                     leftmost.closed = TRUE,
                                     rightmost.closed = FALSE) {
  C_attrCUSUM_inc_InitPr_VSI(endpoints = as.numeric(endpoints),
                             c_zero = as.numeric(c.zero),
                             refv = as.numeric(refv),
                             pmf_process = as.numeric(pmf.process),
                             leftmost_closed = as.logical(leftmost.closed),
                             rightmost_closed = as.logical(rightmost.closed))
}

# Calculate A Matrix of Transition Probabilities
# for VSI CUSUM Chart for Mean Increase
attrCUSUM_inc_Q_VSI <- function(endpoints, refv, pmf.process,
                                leftmost.closed = TRUE,
                                rightmost.closed = FALSE) {
  C_attrCUSUM_inc_Q_VSI(endpoints = as.numeric(endpoints),
                        refv = as.numeric(refv),
                        pmf_process = as.numeric(pmf.process),
                        leftmost_closed = as.logical(leftmost.closed),
                        rightmost_closed = as.logical(rightmost.closed))
}

# Compute ANSSs and Other Informations on
# Attribute VSI CUSUM Chart for Mean Increase
attrCUSUM_inc_getAve <- function(refv, contl, c.zero = 0, warnl = 1, ds = 1,
                                 dl = NULL, di = NULL, process, maxndec = 7L,
                                 maxnumsubI = 6000L) {
  c.zero <- round(c.zero, maxndec)
  refv.round <- round(refv, maxndec)
  ndec.refv <- ndecimal(refv, maxndec)
  refv <- round(refv.round, ndec.refv)
  tol <- attrCUSUM_getTol(refv = refv, ndec.refv = ndec.refv, minv = -refv,
                          contl = contl, maxndec = maxndec,
                          maxnumsubI = maxnumsubI)
  contl.round <- round(contl + 1e-8, ndec.refv)
  contl <- ifelse(contl.round < contl, contl.round + tol, contl.round)
  warnl.round <- round(warnl + 1e-8, ndec.refv)
  warnl <- ifelse(warnl.round < warnl, warnl.round + tol, warnl.round)
  L <- round(ceiling(refv + contl) - 1)
  pmf.process <- process(0:L)
  Interval.info <- Subintervals_lensubI(minv = -refv, maxv = contl,
                                        lensubI = tol, ndec = maxndec)
  endpoints <- Interval.info$endpoints
  numsubI <- Interval.info$numsubI
  lst.Q <- attrCUSUM_inc_Q_VSI(endpoints = endpoints, refv = refv,
                               pmf.process = pmf.process,
                               leftmost.closed = TRUE, rightmost.closed = FALSE)
  Q <- lst.Q$Q
  I_minus_Q <- lst.Q$I_minus_Q
  I_minus_Q_inv <- lst.Q$I_minus_Q_inv
  I_minus_Q_inv_1 <- lst.Q$I_minus_Q_inv_1
  if (any(round(I_minus_Q_inv_1, 7L) < 1)) {
    msg <- paste("In getAve(...),\n",
                 "Invalid computations occurred!.\n",
                 "Input arguments should be changed ",
                 "for this function to work properly.", sep = "")
    stop(msg, call. = FALSE)
  }
  dimnames(Q) <- dimnames(I_minus_Q) <- dimnames(I_minus_Q_inv) <-
    list(as.character(Interval.info$I.info.co),
         as.character(Interval.info$I.info.co))
  rownames(I_minus_Q_inv_1) <- as.character(Interval.info$I.info.co)
  initpr <-
    attrCUSUM_inc_InitPr_VSI(endpoints = endpoints, c.zero = c.zero,
                             refv = refv, pmf.process = pmf.process,
                             leftmost.closed = TRUE, rightmost.closed = FALSE)
  colnames(initpr) <- as.character(Interval.info$I.info.co)
  rownames(initpr) <- paste("[", c.zero, "]", sep = "")
  ANSS <- 1 + as.numeric(initpr %*% I_minus_Q_inv_1)
  elements <- Interval.info$reprv.l
  lst.Ats <- attrCUSUM_getATS(ANSS = ANSS, elements = elements, warnl = warnl,
                              c.zero = c.zero, initpr = initpr,
                              I_minus_Q_inv = I_minus_Q_inv, ds = ds, dl = dl,
                              di = di)
  res <- list(endpoints = endpoints, numsubI = numsubI, lensubI = tol,
              Q = Q, I_minus_Q = I_minus_Q, I_minus_Q_inv = I_minus_Q_inv,
              I_minus_Q_inv_1 = I_minus_Q_inv_1, initpr = initpr, ds = ds,
              dl = lst.Ats$dl.act, di = lst.Ats$di.act,
              psi.s = lst.Ats$psi.s, psi.l = lst.Ats$psi.l,
              refv.act = refv, contl.act = contl, warnl.act = warnl,
              c.zero.act = c.zero, ANSS = ANSS, ATS = lst.Ats$ATS)
  res
}

# Compute Control Limits
# for Attribute FSI CUSUM Control Chart for Mean Increase
attrCUSUM_inc_getContl <- function(anss.target = 370.4, refv, c.zero = 0,
                                   process, maxndec = 7L, maxnumsubI = 1000L) {
  refv <- round(refv, maxndec)
  c.zero <- round(c.zero, maxndec)
  ndec.refv <- ndecimal(refv)
  foo <- function(y) {
    res <- attrCUSUM_inc_getAve(refv = refv, contl = y, c.zero = c.zero,
                                warnl = 0, ds = 1, dl = 1, di = 1,
                                process = process, maxndec = maxndec,
                                maxnumsubI = maxnumsubI)
    res$ANSS - anss.target
  }
  maxnumsubI.tmp <- maxnumsubI
  maxnumsubI <- min(maxnumsubI, 500)
  if (foo(1) >= 0) {
    stop(paste("In getContl(...)\n",
               "Argumnet 'anss.target' must be greater than current value",
               sep = ""),
         call. = FALSE)
  }
  contl.tmp <- 1
  while (foo(contl.tmp) < 0) {
    contl.tmp <- contl.tmp + 2
  }
  inter.tmp <- c(contl.tmp - 2, contl.tmp)
  maxnumsubI <- maxnumsubI.tmp
  while (foo(inter.tmp[1]) > 0) {
    inter.tmp <- inter.tmp - 1
  }
  while (foo(inter.tmp[2]) <= 0) {
    inter.tmp <- inter.tmp + 1
  }
  res.bisec <- BisecMethod(foo = foo, interval = inter.tmp,
                           tol = 1e-1 ^ ndec.refv + 1e-8, maxndec = ndec.refv)
  res <- list(refv.act = refv, c.zero.act = c.zero,
              sol1 = c(contl1 = res.bisec$root.a,
                       ANSS1 = res.bisec$value.a + anss.target),
              sol2 = c(contl2 = res.bisec$root.b,
                       ANSS2 = res.bisec$value.b + anss.target))
  res
}
