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
predictor.unlike <- function(y) {
################################################################################
  z <- rbind.data.frame(as.data.frame(y), rep(NA, ncol(y)))
  cols <- colnames(z)[colnames(z) != 'TARGET']
  new.cols <- paste(cols, 'lag1', sep = '_')
  z[, new.cols] <- NA

  inf.obs <- 1:(nrow(z) - 1)
  new.obs <- nrow(z)
  for (i in 1:length(cols)) {
    z[, new.cols[i]] <- c(NA, z[inf.obs, cols[i]])
  }

  # Example ARMA (1, 1)
  arma11 <- arima(z[inf.obs, 1], order = c(1, 1, 0),
                  xreg = z[inf.obs, new.cols])
  forecast <- predict(arma11, newdata = z[new.obs, ],
                      newxreg = z[new.obs, new.cols])

  # Next point prediction
  pred <- as.numeric(forecast[['pred']])  
  return(pred)
}
# END OF SCRIPT
