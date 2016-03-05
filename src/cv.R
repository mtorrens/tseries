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
source(paste(SRCDIR, 'unlike.R', sep = ''))
################################################################################

################################################################################
validate <- function(series, p2p) {
# series (data.frame): data with first column being the feature to predict
# p2p       (numeric): number of points to predict
################################################################################
  # Set up lesioned dataset
  #series <- as.matrix(series)
  to.predict <- (nrow(series) - p2p + 1):nrow(series)
  truth <- series[to.predict, 'TARGET']
  series[to.predict, 'TARGET'] <- NA

  # Perform all single-point predictions
  repeat {
    empty <- which(is.na(series[, 'TARGET']))
    filled <- which(! is.na(series[, 'TARGET']))
    result <- predictor.unlike(y = series[filled, ])
    series[empty[1], 'TARGET'] <- result
    if (length(empty) == 1) {
      break
    }
  }

  # Calculate mean squared error (?)
  preds <- series[to.predict, 'TARGET']
  mse <- mean((truth - preds) ** 2)
  #print(mse)
  return(mse)
}

################################################################################
# Cross validation
mse50 <- validate(series, p2p = 50)  # This is not cross validation



