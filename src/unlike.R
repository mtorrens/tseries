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
  suppressMessages(library(randomForest))

  z <- rbind.data.frame(as.data.frame(y), rep(NA, ncol(y)))
  cols <- colnames(z)[colnames(z) != 'TARGET']
  #cols <- c('DGHGR', 'HSIOM', 'XRSXD', 'HBOKK')
  #cols <- c('TARGET', 'HSIOM', 'HBOKK', 'OKQPF', 'XRSXD')
  #HSIOM_lag1 + TVTQH_lag1 + XUKCD_lag1 + LIMUE_lag1 + HBOKK_lag1 + ZARXQ_lag1 + FGQVN_lag2 + PHCRP_lag2 + USPLV_lag2 + ZQHHT_lag2 + OKQPF_lag2 + ZARXQ_lag2 + TUCND_lag2
  cols1 <- c('TARGET', 'HSIOM', 'TVTQH', 'XUKCD', 'LIMUE', 'HBOKK', 'ZARXQ')
  cols2 <- c('TARGET', 'FGQVN', 'PHCRP', 'USPLV', 'ZQHHT', 'OKQPF', 'ZARXQ', 'TUCND')
  #new.cols <- paste(cols, 'lag1', sep = '_')
  new.cols1 <- paste(cols1, 'lag1', sep = '_')
  new.cols2 <- paste(cols2, 'lag2', sep = '_')
  #z[, new.cols] <- NA
  z[, new.cols1] <- NA
  z[, new.cols2] <- NA

  iobs <- 1:(nrow(z) - 1)
  iobs2 <- 1:(nrow(z) - 2)
  nobs <- nrow(z)
  # for (i in 1:length(cols)) {
  #   z[, new.cols[i]] <- c(NA, z[iobs, cols[i]])
  # }
  # z <- z[, c('TARGET', new.cols)]
  for (i in 1:length(cols1)) {
    z[, new.cols1[i]] <- c(NA, z[iobs, cols1[i]])
  }
  for (i in 1:length(cols2)) {
    z[, new.cols2[i]] <- c(NA, NA, z[iobs2, cols2[i]])
  }
  z <- z[, c('TARGET', new.cols1, new.cols2)]
  new.cols <- c(new.cols1, new.cols2)

  # Compute some ARMAS
  set <- expand.grid(0:2, 0:2)[2:nrow(expand.grid(0:2, 0:2)), ]
  tom <- 3:(nrow(z) - 1)
  top <- 3:nrow(z) 
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

################################################################################
predictor.unlike <- function(y, ...) {
################################################################################
  suppressMessages(library(randomForest))

  z <- rbind.data.frame(as.data.frame(y), rep(NA, ncol(y)))
  cols <- colnames(z)[colnames(z) != 'TARGET']
  cols1 <- c('TARGET', 'HSIOM', 'TVTQH', 'PHCRP', 'LIMUE', 'XRSXD', 'NUCJQ', 'HBOKK')
  cols2 <- c('TARGET', 'FGQVN', 'PHCRP', 'USPLV', 'OKQPF', 'ZARXQ', 'TUCND')
  cols3 <- c('TARGET', 'DQXAB', 'DGHGR', 'BIWBE')
  new.cols1 <- paste(cols1, 'lag1', sep = '_')
  new.cols2 <- paste(cols2, 'lag2', sep = '_')
  new.cols3 <- paste(cols3, 'lag3', sep = '_')
  z[, new.cols1] <- NA
  z[, new.cols2] <- NA
  z[, new.cols3] <- NA

  iobs1 <- 1:(nrow(z) - 1)
  iobs2 <- 1:(nrow(z) - 2)
  iobs3 <- 1:(nrow(z) - 3)
  nobs <- nrow(z)
  for (i in 1:length(cols1)) {
    z[, new.cols1[i]] <- c(NA, z[iobs1, cols1[i]])
  }
  for (i in 1:length(cols2)) {
    z[, new.cols2[i]] <- c(NA, NA, z[iobs2, cols2[i]])
  }
  for (i in 1:length(cols3)) {
    z[, new.cols3[i]] <- c(NA, NA, NA, z[iobs3, cols3[i]])
  }
  z <- z[, c('TARGET', new.cols1, new.cols2, new.cols3)]
  new.cols <- c(new.cols1, new.cols2, new.cols3)

  # Compute some ARMAS
  set <- expand.grid(0:2, 0:2)[2:nrow(expand.grid(0:2, 0:2)), ]
  tom <- 4:(nrow(z) - 1)
  top <- 4:nrow(z) 
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
