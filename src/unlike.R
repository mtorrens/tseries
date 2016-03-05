################################################################################
predictor.unlike <- function(y) {
################################################################################
  # Separate dataset
  y1 <- y[1:(nrow(y) - 1), ]
  y2 <- as.data.frame(t(as.data.frame(y[nrow(y), ])))

  # ARMA (1, 1)
  arma11 <- arima(y1[, 1], order = c(1, 1, 0), xreg = y1[, -1])
  forecast <- predict(arma11, newdata = y2, newxreg = y2[, -1])

  # Next point prediction
  pred <- as.numeric(forecast[['pred']])  
  return(pred)
}
# END OF SCRIPT
