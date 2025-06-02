# Set the working directory to the script’s location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Clear console
cat("\014")

source("Correlations.R")
source("Replication_original.R")
source("Replication_analysis.R")