################################################################################
# Define your working directory
PATH <- '/Users/miquel/Desktop/bgse/projects/github/tseries/'

# Package dependencies
library(tseries)

# Other directories
INPDIR <- paste(PATH, 'input/', sep = '')
DATDIR <- paste(PATH, 'data/', sep = '')
SRCDIR <- paste(PATH, 'src/', sep = '')
################################################################################

################################################################################
# Set the working directory
setwd(PATH)

# Load data
file <- paste(DATDIR, 'comp_data.RData', sep = '')
if (! file.exists(file)) {
  csv.file <- paste(INPDIR, 'forecast-competition-data.csv', sep = '')
  series <- read.csv(csv.file, header = TRUE)
  save(series, file = file); cat('Saved file:', file, '\n')
} else {
  series <- get(load(file = file)); cat('Loaded file:', file, '\n')
}

# Target variable
target <- series[, 'TARGET']
################################################################################

################################################################################
# Exploratory analysis
plot(target, type = 'l')
################################################################################

################################################################################
# Some models
################################################################################

# ARIMA (10,0,2)

library(foreach)
library(doMC)

registerDoMC(cores=4)

time1 <- system.time( {
  
  MSE_arima <- foreach(i=1:149) %dopar% {
    
    train <- data[i:(nrow(data_train)+i-1),]
    test <-  data[nrow(data_train)+i,]
    
    model_arima <- arima(train[,1], order = c(1,0,0), xreg = train[,-1])
    predict_arima <- predict(model_arima, newdata = test$TARGET, newxreg=test[,-1])
    return(mean((predict_arima$pred - test$TARGET)^2))
    
  }
  
})

score <- mean(unlist(MSE_arima))

################################################################################
# Variable selection
library(mombf)
y <- scale(target)
x <- scale(series[, -1])
prior <- zellnerprior(tau = nrow(x))

# Run models
ms.unif <- modelSelection(y = y, x = x, priorCoef = prior,
                          priorDelta = modelunifprior(), niter = 10 ** 5)
ms.bbin <- modelSelection(y = y, x = x, priorCoef = prior,
                          priorDelta = modelbbprior(), niter = 10 ** 5)

# See the results
pp.unif <- postProb(ms.unif)
nvars <- sapply(strsplit(as.character(pp.unif[, 1]), split = ','), length)
pp.unif <- tapply(pp.unif$pp, nvars, sum)

pp.bbin <- postProb(ms.bbin)
nvars <- sapply(strsplit(as.character(pp.bbin[, 1]), split = ','),length)
pp.bbin <- tapply(pp.bbin$pp, nvars, sum)

# Plot posterior distribution of model size
plot(names(pp.bbin), pp.bbin, type = 'l', xlab = 'Number of variables',
     ylab = 'Posterior probability', cex.lab = 1.5, ylim = c(0, 0.3))
lines(names(pp.unif), pp.unif, col = 2)
legend('topright', c('Beta-Binomial(1,1)', 'Uniform'),
       lty = 1, col = 1:2, cex = 1.5)

# List top 5 models under Uniform & Beta-Binomial prior
bunif <- postProb(ms.unif)[1:5, ]
bbbin <- postProb(ms.bbin)[1:5, ]

# Some variables stand out

################################################################################
# ADD LAGS
z <- rbind.data.frame(as.data.frame(series))
cols <- colnames(z)[colnames(z) != 'TARGET']
new.cols <- paste(cols, 'lag1', sep = '_')
new.cols2 <- paste(cols, 'lag2', sep = '_')
new.cols3 <- paste(cols, 'lag3', sep = '_')
z[, new.cols] <- NA
z[, new.cols2] <- NA
z[, new.cols3] <- NA
inf.obs <- 1:(nrow(z) - 1)
inf.obs2 <- 1:(nrow(z) - 2)
inf.obs3 <- 1:(nrow(z) - 3)
new.obs <- nrow(z)
for (i in 1:length(cols)) {
  z[, new.cols[i]] <- c(NA, z[inf.obs, cols[i]])
  z[, new.cols2[i]] <- c(NA, NA, z[inf.obs2, cols[i]])
  z[, new.cols3[i]] <- c(NA, NA, NA, z[inf.obs3, cols[i]])
}
z <- z[4:nrow(z), c('TARGET', new.cols)]
y <- scale(z[, 1])
x <- scale(z[, -1])
prior <- zellnerprior(tau = nrow(x))

# Run models
ms.unif <- modelSelection(y = y, x = x, priorCoef = prior,
                          priorDelta = modelunifprior(), niter = 10 ** 5)
ms.bbin <- modelSelection(y = y, x = x, priorCoef = prior,
                          priorDelta = modelbbprior(), niter = 10 ** 5)

# See the results
pp.unif <- postProb(ms.unif)
nvars <- sapply(strsplit(as.character(pp.unif[, 1]), split = ','), length)
pp.unif <- tapply(pp.unif$pp, nvars, sum)

pp.bbin <- postProb(ms.bbin)
nvars <- sapply(strsplit(as.character(pp.bbin[, 1]), split = ','),length)
pp.bbin <- tapply(pp.bbin$pp, nvars, sum)

# Plot posterior distribution of model size
plot(names(pp.bbin), pp.bbin, type = 'l', xlab = 'Number of variables',
     ylab = 'Posterior probability', cex.lab = 1.5, ylim = c(0, 0.3))
lines(names(pp.unif), pp.unif, col = 2)
legend('topright', c('Beta-Binomial(1,1)', 'Uniform'),
       lty = 1, col = 1:2, cex = 1.5)

# List top 5 models under Uniform & Beta-Binomial prior
bunif <- postProb(ms.unif)[1:5, ]
bbbin <- postProb(ms.bbin)[1:5, ]

bunif
bbbin

postProb(ms.unif)[1:20, ]
postProb(ms.bbin)[1:20, ]
################################################################################

################################################################################
# MORE SERIOUS STUFF
# Least Squares Estimator
fit.mle <- looCV.mle(y = y, x = x)

# Bayesian Model Averaging
fit.bma <- looCV.bma(y = y, x = x, type = 'bma', priorCoef = prior,
                     priorDelta = modelbbprior(alpha.p = 1, beta.p = 1),
                     niter = 10 ** 4)

# Median Probability Model
fit.med <- looCV.bma(y = y, x = x, type = 'median', priorCoef = prior,
                     priorDelta = modelbbprior(alpha.p = 1, beta.p = 1),
                     niter = 10 ** 4)

# Compare predictions for different methods
plot(fit.bma$pred, fit.med$pred, xlab = 'Bayesian model averaging',
     ylab = 'Median probability model')
abline(0, 1)
plot(fit.bma$pred, fit.mle$pred, xlab = 'Bayesian model averaging',
     ylab = 'Least squares')
abline(0, 1)
################################################################################


