looCV.mle <- function(y, x) {
  # Least-squares Leave-one-out cross-validation
  # Input
  # - y: outcome
  # - x: covariates
  # Output
  # - pred: cross-validated predictions for y
  # - ssr: cross-validated sum of squared residuals
  x <- data.frame(x)
  pred <- double(nrow(x))
  for (i in 1:nrow(x)) {
      fit <- lm(y[-i] ~ ., data = x[-i, , drop = FALSE])
      pred[i] <- predict(fit, newdata = x[i, , drop = FALSE])
  }
  return(list(pred = pred, ssr = sum((pred - y) ** 2, na.rm = TRUE)))
}

looCV.bma <- function(y, x, type = 'bma', priorCoef,
                      priorDelta = modelbbprior(alpha.p = 1, beta.p = 1),
                      priorVar = igprior(alpha = 0.01, lambda = 0.01),
                      niter = 5000, niter2 = niter, pp = 'norm',
                      center = TRUE, scale = TRUE, mc.cores = 1, ...) {
  # BMA leave-one-out cross-validation
  # Input
  # - y: outcome
  # - x: covariates
  # - priorCoef: prior on coefficients, e.g. momprior(tau=.348)
  # - priorDelta: prior on model space
  # - priorVar: prior on residual variance
  # - ...: other arguments to be passed on to modelSelection
  # Output
  # - pred: cross-validated predictions for y
  # - ssr: cross-validated sum of squared residuals
  pred <- double(nrow(x))
  y <- y - mean(y)
  if (scale) { y <- y / sd(y) }
  x <- scale(x, center = center, scale = scale)
  f <- function(i, ...) {
    ms <- modelSelection(y = y[-i], x = x[-i, , drop = FALSE], center = FALSE,
                         scale = FALSE, niter = niter, priorCoef = priorCoef,
                         priorDelta = priorDelta, priorVar = priorVar,
                         verbose = FALSE, ...)
    thpost <- rnlp(y = y[-i], x = x[-i, , drop = FALSE], msfit = ms,
                   priorCoef = priorCoef, priorVar = priorVar, niter = niter2)
    cat('.')
    return(mean(y[-i]) +
           sum(colMeans(thpost[, -ncol(thpost)]) * t(x[i, , drop = FALSE])))
  }
  fmedian <- function(i, threshold, ...) {
      ms <- modelSelection(y = y[-i], x = x[-i, , drop = FALSE], center = FALSE,
                           scale = FALSE, niter = niter, priorCoef = priorCoef,
                           priorDelta = priorDelta, priorVar = priorVar,
                           verbose = FALSE, ...)
      sel <- ms$margpp > threshold
      thpost <- rnlp(y = y[-i], x = x[-i, sel, drop = FALSE],
                     priorCoef = priorCoef, priorVar = priorVar, niter = niter2)
      cat('.')
      avgs <- colMeans(thpost[, -ncol(thpost)]) * t(x[i, sel, drop = FALSE])
      return(mean(y[-i]) + sum(avgs))
  }
  if (type == 'bma') {
      pred <- unlist(lapply(1:nrow(x), f, ...))
  } else if (type == 'median') {
      pred <- unlist(lapply(1:nrow(x), fmedian, threshold = 0.5, ...))
  } else {
    stop('Invalid argument "type".')
  }
  return(list(pred = pred, ssr = sum((pred - y) ** 2, na.rm = TRUE)))
}

