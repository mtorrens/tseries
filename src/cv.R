################################################################################
# Define your working directory
PATH <- '/Users/miquel/Desktop/bgse/projects/github/tseries/'

# Package dependencies
library(tseries)

# Other directories
setwd(PATH)
INPDIR <- paste(PATH, 'input/', sep = '')
DATDIR <- paste(PATH, 'data/', sep = '')
SRCDIR <- paste(PATH, 'src/', sep = '')
################################################################################

################################################################################
# Load data
file <- paste(DATDIR, 'comp_data.RData', sep = '')
series <- get(load(file = file)); cat('Loaded file:', file, '\n')

# Target variable
target <- series[, 'TARGET']

# Our model
source(paste(SRCDIR, 'var_selection.R', sep = ''))
source(paste(SRCDIR, 'unlike.R', sep = ''))
################################################################################

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
postProb(ms.unif)[1:5, ]
postProb(ms.bbin)[1:5, ]

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
validate <- function(series, p2p) {
# series (data.frame): data with first column being the feature to predict
# p2p       (numeric): number of points to predict
################################################################################
  # Set up lesioned dataset
  series <- as.matrix(series)
  to.predict <- (nrow(series) - p2p + 1):nrow(series)
  truth <- series[to.predict, 'TARGET']
  series[to.predict, 'TARGET'] <- NA

  # Perform all single-point predictions
  repeat {
    empty <- which(is.na(series[, 'TARGET']))
    filled <- which(! is.na(series[, 'TARGET']))
    to.model <- c(filled, empty[1])
    result <- predictor.unlike(y = series[to.model, ])
    series[empty[1], 'TARGET'] <- result
    if (length(empty) == 1) {
      break
    }
  }

  # Calculate mean squared error (?)
  preds <- series[to.predict, 'TARGET']
  mse <- mean((truth - preds) ** 2)
  return(mse)
}

################################################################################
validate <- function(series, p2p) {
# series (data.frame): data with first column being the feature to predict
# p2p       (numeric): number of points to predict
################################################################################
  # Set up lesioned dataset
  series <- as.matrix(series)
  to.predict <- (nrow(series) - p2p + 1):nrow(series)
  
  preds <- sapply(to.predict, function(x) {
    #print(dim(series[1:(x - 1), ])); stop()
    predictor.unlike(y = series[1:(x - 1), ])
  })

  truth <- series[to.predict, 'TARGET']
  mse <- mean((truth - preds) ** 2)
  return(mse)
}

################################################################################
# Cross validation
mse50 <- validate(series, p2p = 50)  # This is not cross validation



