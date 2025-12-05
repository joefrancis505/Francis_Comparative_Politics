# Multi-Line Percentage Gap Plot + P-value Plot
# Creates a plot similar to ger_vs_synthger_gaps.pdf but with multiple lines
# showing gap as percentage of Synthetic West Germany from all WDI datasets
# Also creates a p-value plot from RMSPE ratios

rm(list=ls())

# Function to read and process gap data
read_gap_data <- function(file_path) {
  if (file.exists(file_path)) {
    data <- read.csv(file_path)
    # Calculate percentage gap
    data$gap_percentage <- (data$gap / data$synthetic_west_germany) * 100
    return(data)
  } else {
    return(NULL)
  }
}

# Function to read and process RMSPE ratios data
read_rmspe_data <- function(file_path) {
  if (file.exists(file_path)) {
    data <- read.csv(file_path)
    return(data)
  } else {
    return(NULL)
  }
}

# Function to calculate p-value from RMSPE ratios
calculate_p_value <- function(rmspe_data) {
  # Find West Germany's ratio
  west_germany_ratio <- rmspe_data$ratio[rmspe_data$country == "West Germany"]
  
  if (length(west_germany_ratio) == 0) {
    # Try alternative names for West Germany
    west_germany_ratio <- rmspe_data$ratio[rmspe_data$country == "Germany, West"]
    if (length(west_germany_ratio) == 0) {
      west_germany_ratio <- rmspe_data$ratio[rmspe_data$country == "Germany"]
      if (length(west_germany_ratio) == 0) {
        return(NA)
      }
    }
  }
  
  # Count countries with ratio >= West Germany's ratio
  countries_above <- sum(rmspe_data$ratio >= west_germany_ratio, na.rm = TRUE)
  total_countries <- nrow(rmspe_data)
  
  # Calculate p-value
  p_value <- countries_above / total_countries
  return(p_value)
}

# Collect all gap data from WDI datasets
all_gaps <- list()
years <- 1960:2003

for (year in years) {
  file_path <- paste0("Results/wdi_", year, "/gap.csv")
  gap_data <- read_gap_data(file_path)
  
  if (!is.null(gap_data)) {
    all_gaps[[paste0("wdi_", year)]] <- gap_data
    cat(paste("Loaded gap data for WDI", year, "\n"))
  } else {
    cat(paste("Warning: Could not find", file_path, "\n"))
  }
}

if (length(all_gaps) == 0) {
  stop("No gap data files found. Make sure the analysis has been run and Results/ directory exists.")
}

cat(paste("Successfully loaded", length(all_gaps), "gap datasets\n"))

# Collect all RMSPE ratios data and calculate p-values
p_values <- data.frame(year = integer(), p_value = numeric())

for (year in years) {
  file_path <- paste0("Results/wdi_", year, "/rmspe_ratios.csv")
  rmspe_data <- read_rmspe_data(file_path)
  
  if (!is.null(rmspe_data)) {
    p_val <- calculate_p_value(rmspe_data)
    p_values <- rbind(p_values, data.frame(year = year, p_value = p_val))
    cat(paste("Calculated p-value for WDI", year, "\n"))
  } else {
    cat(paste("Warning: Could not find", file_path, "\n"))
  }
}

cat(paste("Successfully calculated p-values for", nrow(p_values), "years\n"))

# ===== FIRST PLOT: Percentage Gaps =====

# Determine y-axis limits for gap plot
all_percentage_gaps <- unlist(lapply(all_gaps, function(x) x$gap_percentage))
gap_range <- max(abs(range(all_percentage_gaps, na.rm = TRUE)))
y_min <- -gap_range * 1.1
y_max <- gap_range * 1.1

# Create the percentage gaps plot
pdf(file = "Results/multi_line_percentage_gaps.pdf", width = 5.5, height = 5.0, family = "Times", pointsize = 12)
par(mar = c(5.1, 4.1, 1, 2.1))

# Initialize empty plot
plot(1960:2003, rep(0, length(1960:2003)), 
     type = "n", 
     ylim = c(y_min, y_max), 
     ylab = "gap in per-capita GDP (% of synthetic West Germany)",
     xlab = "year",
     xaxs = "i", 
     yaxs = "i")

# Add horizontal line at 0
abline(h = 0, lty = "dotted")

# Add vertical line at reunification
abline(v = 1990, lty = "dotted")

# Plot all gap lines with 50% transparency
# Using rgb() to create semi-transparent black
transparent_color <- rgb(0, 0, 0, alpha = 0.2)

for (i in 1:length(all_gaps)) {
  gap_data <- all_gaps[[i]]
  lines(gap_data$year, gap_data$gap_percentage, 
        col = transparent_color, 
        lty = "solid", 
        lwd = 1)
}

dev.off()

# ===== SECOND PLOT: P-values =====

# Create the p-values plot
pdf(file = "Results/p_values_plot.pdf", width = 5.5, height = 5.0, family = "Times", pointsize = 12)
par(mar = c(5.1, 4.1, 1, 2.1))

# Initialize empty plot
plot(p_values$year, p_values$p_value, 
     type = "p", 
     pch = 19,  # solid black dots
     col = "black",
     xlim = c(1958, 2005),  # Add padding to x-axis
     ylim = c(0, 1), 
     ylab = expression(italic(p)*"-value"),
     xlab = "Reference year for real GDP per capita",
     xaxs = "i", 
     yaxs = "i")

# Add horizontal line at 0.10 for significance reference
abline(h = 0.10, lty = "dotted", col = "gray")

dev.off()

# ===== SUMMARY OUTPUT =====

# Also create a summary table of the gap data
cat("\nSummary of percentage gaps at key years:\n")
summary_years <- c(1989, 1990, 1991, 1995, 2000, 2003)
summary_data <- data.frame(year = summary_years)

for (i in 1:length(all_gaps)) {
  dataset_name <- names(all_gaps)[i]
  gap_data <- all_gaps[[i]]
  
  # Extract gaps for summary years
  summary_gaps <- gap_data$gap_percentage[gap_data$year %in% summary_years]
  summary_data[[dataset_name]] <- summary_gaps
}

# Save summary data
write.csv(summary_data, "Results/percentage_gaps_summary.csv", row.names = FALSE)

# Save p-values data
write.csv(p_values, "Results/p_values_summary.csv", row.names = FALSE)

# Print some summary statistics for p-values
cat("\nP-value summary statistics:\n")
print(summary(p_values$p_value))

cat("\nPlots saved as:\n")
cat("  - Results/multi_line_percentage_gaps.pdf\n")
cat("  - Results/p_values_plot.pdf\n")
cat("\nSummary data saved as:\n")
cat("  - Results/percentage_gaps_summary.csv\n")
cat("  - Results/p_values_summary.csv\n")
cat(paste("Total gap lines plotted:", length(all_gaps), "\n"))
cat(paste("Total p-values calculated:", nrow(p_values), "\n"))