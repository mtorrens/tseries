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



