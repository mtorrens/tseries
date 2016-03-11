################################################################################
predictor.runif <- function(y) {
################################################################################
  # Dependencies
  if (! require(randomForest)) {
    stop('uninstalled package required: "randomForest" (install manually).')
  }

  # Define new columns
  z <- rbind.data.frame(as.data.frame(y), rep(NA, ncol(y)))
  cols <- colnames(z)[colnames(z) != 'TARGET']
  cols1 <- c('TARGET', 'HSIOM', 'TVTQH', 'PHCRP', 'LIMUE', 'XRSXD', 'NUCJQ',
             'HBOKK')
  cols2 <- c('TARGET', 'FGQVN', 'PHCRP', 'USPLV', 'OKQPF', 'ZARXQ', 'TUCND')
  cols3 <- c('TARGET', 'DQXAB', 'DGHGR', 'BIWBE')
  new.cols1 <- paste(cols1, 'lag1', sep = '_')
  new.cols2 <- paste(cols2, 'lag2', sep = '_')
  new.cols3 <- paste(cols3, 'lag3', sep = '_')
  z[, new.cols1] <- NA
  z[, new.cols2] <- NA
  z[, new.cols3] <- NA

  # Add new columns
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
  set <- expand.grid(0:1, 0:1)[2:nrow(expand.grid(0:1, 0:1)), ]
  tom <- 4:(nrow(z) - 1)
  top <- 4:nrow(z) 
  for (i in 1:nrow(set)) {
    pars <- c(as.numeric(set[i, ]), 0)
    nom <- paste('arma', pars[1], pars[2], sep = '')
    arma <- arima(z[tom, 1], order = pars, xreg = z[tom, new.cols])
    fc <- suppressWarnings(predict(arma, newdata = z[top, ],
                           newxreg = z[top, new.cols]))
    z[, nom] <- NA
    z[top, nom] <- as.numeric(fc[['pred']])
  }
  model.cols <- colnames(z)[colnames(z) != 'TARGET']

  # Random Forest
  set.seed(666)
  rf <- randomForest(y = z[tom, 'TARGET'],
                     x = z[tom, model.cols],
                     mtry = length(model.cols) / 5,
                     ntree = 170,
                     nodesize = 9,
                     importance = TRUE,
                     corr.bias = TRUE)

  pred <- predict(rf, newdata = z[nrow(z), 2:ncol(z)])
  return(pred)
}
# END OF SCRIPT
