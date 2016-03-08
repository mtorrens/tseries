################################################################################
predictor.unlike <- function(y) {
################################################################################
  # Separate dataset
  y1 <- y[1:(nrow(y) - 1), ]
  y2 <- as.data.frame(t(as.data.frame(y[nrow(y), ])))

  # Example ARMA (1, 1)
  arma11 <- arima(y1[, 1], order = c(1, 1, 0), xreg = y1[, -1])
  forecast <- predict(arma11, newdata = y2, newxreg = y2[, -1])

  # Next point prediction
  pred <- as.numeric(forecast[['pred']])  
  return(pred)
}

################################################################################
predictor.unlike <- function(y, ...) {
################################################################################
  require(randomForest)

  z <- rbind.data.frame(as.data.frame(y), rep(NA, ncol(y)))
  cols <- colnames(z)[colnames(z) != 'TARGET']
  cols <- c('DGHGR', 'HSIOM', 'XRSXD', 'HBOKK')
  new.cols <- paste(cols, 'lag1', sep = '_')
  z[, new.cols] <- NA

  iobs <- 1:(nrow(z) - 1)
  nobs <- nrow(z)
  for (i in 1:length(cols)) {
    z[, new.cols[i]] <- c(NA, z[iobs, cols[i]])
  }
  z <- z[, c('TARGET', new.cols)]

  # Compute some ARMAS
  set <- expand.grid(0:2, 0:2)[2:nrow(expand.grid(0:2, 0:2)), ]
  tom <- 2:(nrow(z) - 1)
  top <- 2:nrow(z) 
  for (i in 1:nrow(set)) {
    pars <- c(as.numeric(set[i, ]), 0)
    nom <- paste('arma', pars[1], pars[2], sep = '')
    arma <- arima(z[tom, 1], order = pars, xreg = z[tom, new.cols])
    fc <- predict(arma, newdata = z[top, ], newxreg = z[top, new.cols])
    z[top, nom] <- as.numeric(fc[['pred']])
  }

  # Random Forest
  set.seed(666)
  model.cols <- colnames(z)[colnames(z) != 'TARGET']
  rf <- randomForest(y = z[tom, 'TARGET'],
                     x = z[tom, model.cols], ...)

  pred <- predict(rf, newdata = z[nrow(z), 2:ncol(z)])
  return(pred)
}
# END OF SCRIPT
