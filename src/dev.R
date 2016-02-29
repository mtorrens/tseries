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
NULL
################################################################################
