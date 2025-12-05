# Set the working directory to the script’s location
setwd(getSrcDirectory(function(dummy) {dummy}))

# Clear console
cat("\014")

source("Datasets.R")
source("Replication.R")
source("Gap_plot.R")