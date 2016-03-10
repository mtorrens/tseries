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
TMPDIR <- paste(PATH, 'temp/', sep = '')
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
validate <- function(series, p2p, ...) {
# series (data.frame): data with first column being the feature to predict
# p2p       (numeric): number of points to predict
################################################################################
  # Set up lesioned dataset
  series <- as.matrix(series)
  to.predict <- (nrow(series) - p2p + 1):nrow(series)
  
  # Apply the function for each new point
  preds <- sapply(to.predict, function(x) {
    predictor.unlike(y = series[1:(x - 1), ], ...)
  })

  # Compute Mean Squared Error
  truth <- series[to.predict, 'TARGET']
  mse <- mean((truth - preds) ** 2)
  return(mse)
}

#source(paste(SRCDIR, 'unlike.R', sep = ''))
################################################################################
# Cross validation
# Try the best parameters of Random Forest
require(doMC)
require(parallel)
result <- matrix(nrow = 0, ncol = 3)
colnames(result) <- c('ntrees', 'nodesize', 'mse10')
registerDoMC(cores = 4)                                                        
#for (n in seq(100, 3000, 50)) {
  #for (s in seq(5, 100, 5)) {
for (s in seq(50, 300, 10)) {
  new.res <- foreach(n = seq(5, 200, 5)) %dopar% {
    mse <- validate(series, p2p = 100, ntree = n, nodesize = s)
    cat('n:', n, 's:', s, 'acc:', round(mse, 4), '\n')
    return(c(n, s, mse))
  }

  # Compile results
  res <- cbind(as.numeric(sapply(new.res, `[`, 1)),
               as.numeric(sapply(new.res, `[`, 2)),
               as.numeric(sapply(new.res, `[`, 3)))
  colnames(res) <- colnames(result)
  result <- as.data.frame(rbind(result, res))
}


best <- result[order(result[, 3]), ][1:10, ]




# mse10 <- validate(series, p2p = 10, ntree = 100, nodesize = 10)
# mse10 <- validate(series, p2p = 10, ntree = 600, nodesize = 25)

mse10 <- validate(series, p2p = 10)  # This is not cross validation
mse50 <- validate(series, p2p = 50)  # This is not cross validation



