# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

## GDP Growth Rate Correlation Analysis
# Script to create scatter plots comparing GDP growth rates between datasets
# Compares repgermany.dta with OECD datasets by country and year
# Updated to match figure dimensions and margins from main analysis script

rm(list=ls())

# Load required libraries
if (!require(foreign, quietly = TRUE)) {
  install.packages("foreign")
  library(foreign)
}

# Create output directory
create_correlation_dir <- function() {
  if (!dir.exists("Results/Correlations")) {
    dir.create("Results/Correlations", recursive = TRUE)
    cat("Created Results/Correlations directory\n")
  }
}

# Function to standardize country names
standardize_country_names <- function(country_names) {
  # Comprehensive country name mappings
  # NOTE: Germany and West Germany are kept separate (different entities)
  country_map <- c(
    "United States" = "USA",
    "United Kingdom" = "UK", 
    "GBR" = "UK",
    "USA" = "USA",
    "FRA" = "France",
    "ITA" = "Italy",
    "JPN" = "Japan",
    "CAN" = "Canada",
    "AUS" = "Australia",
    "AUT" = "Austria",
    "BEL" = "Belgium",
    "CHE" = "Switzerland",
    "DNK" = "Denmark",
    "ESP" = "Spain",
    "GRC" = "Greece",
    "NLD" = "Netherlands",
    "NOR" = "Norway",
    "NZL" = "New Zealand",
    "PRT" = "Portugal",
    "SWE" = "Sweden",
    "FIN" = "Finland",
    "ISL" = "Iceland",
    "IRL" = "Ireland",
    "LUX" = "Luxembourg",
    "TUR" = "Turkey"
  )
  
  # Apply mappings where they exist, otherwise keep original
  standardized <- country_names
  for (i in seq_along(country_names)) {
    if (country_names[i] %in% names(country_map)) {
      standardized[i] <- country_map[country_names[i]]
    }
  }
  return(standardized)
}

# Function to calculate growth rates
calculate_growth_rates <- function(data, gdp_col, country_col, year_col) {
  # Sort data by country and year
  data <- data[order(data[[country_col]], data[[year_col]]), ]
  
  # Initialize growth rate column
  data$growth_rate <- NA
  
  # Calculate growth rates for each country
  countries <- unique(data[[country_col]])
  
  for (country in countries) {
    country_data <- data[data[[country_col]] == country, ]
    country_data <- country_data[order(country_data[[year_col]]), ]
    
    # Calculate year-over-year growth rate
    for (i in 2:nrow(country_data)) {
      if (!is.na(country_data[[gdp_col]][i]) && !is.na(country_data[[gdp_col]][i-1]) && 
          country_data[[gdp_col]][i-1] != 0) {
        growth_rate <- (country_data[[gdp_col]][i] - country_data[[gdp_col]][i-1]) / 
          country_data[[gdp_col]][i-1] * 100
        data[data[[country_col]] == country & data[[year_col]] == country_data[[year_col]][i], "growth_rate"] <- growth_rate
      }
    }
  }
  
  return(data)
}

# Function to analyze country matching
analyze_country_matching <- function(repgermany_data, other_data, other_country_col, dataset_name) {
  cat("\n--- Country Matching Analysis for", dataset_name, "---\n")
  
  # Get unique countries from both datasets
  rg_countries <- unique(repgermany_data$country)
  other_countries <- unique(other_data[[other_country_col]])
  
  # Standardize other dataset country names
  other_countries_std <- standardize_country_names(other_countries)
  
  cat("RepGermany countries (", length(rg_countries), "):\n")
  cat(paste(sort(rg_countries), collapse = ", "), "\n\n")
  
  cat("Other dataset countries before standardization (", length(other_countries), "):\n")
  cat(paste(sort(other_countries), collapse = ", "), "\n\n")
  
  cat("Other dataset countries after standardization (", length(unique(other_countries_std)), "):\n")
  cat(paste(sort(unique(other_countries_std)), collapse = ", "), "\n\n")
  
  # Find matches
  matched_countries <- intersect(rg_countries, other_countries_std)
  unmatched_rg <- setdiff(rg_countries, other_countries_std)
  unmatched_other <- setdiff(other_countries_std, rg_countries)
  
  cat("MATCHED countries (", length(matched_countries), "):\n")
  cat(paste(sort(matched_countries), collapse = ", "), "\n\n")
  
  cat("UNMATCHED RepGermany countries (", length(unmatched_rg), "):\n")
  cat(paste(sort(unmatched_rg), collapse = ", "), "\n\n")
  
  cat("UNMATCHED Other dataset countries (", length(unmatched_other), "):\n")
  cat(paste(sort(unmatched_other), collapse = ", "), "\n\n")
  
  return(list(
    matched = matched_countries,
    unmatched_rg = unmatched_rg,
    unmatched_other = unmatched_other
  ))
}

# Function to merge datasets and calculate correlations
merge_and_correlate <- function(repgermany_data, other_data, other_gdp_col, other_country_col, other_year_col, dataset_name) {
  
  cat("Processing", dataset_name, "data...\n")
  
  # Analyze country matching first
  matching_analysis <- analyze_country_matching(repgermany_data, other_data, other_country_col, dataset_name)
  
  # Standardize country names in other dataset
  other_data[[other_country_col]] <- standardize_country_names(other_data[[other_country_col]])
  
  # Create merged GDP dataset (levels, not growth rates)
  merged_gdp_levels <- merge(
    repgermany_data[, c("country", "year", "gdp")],
    other_data[, c(other_country_col, other_year_col, other_gdp_col)],
    by.x = c("country", "year"),
    by.y = c(other_country_col, other_year_col),
    all = FALSE
  )
  
  # Rename columns for CSV output
  names(merged_gdp_levels) <- c("country", "year", "Abadie_et_al", dataset_name)
  
  # Save merged GDP levels to CSV
  csv_filename <- paste0("Results/Correlations/merged_gdp_levels_", tolower(gsub("[^A-Za-z0-9]", "_", dataset_name)), ".csv")
  write.csv(merged_gdp_levels, csv_filename, row.names = FALSE)
  cat("Saved merged GDP levels to:", csv_filename, "\n")
  
  # Calculate growth rates for both datasets
  repgermany_growth <- calculate_growth_rates(repgermany_data, "gdp", "country", "year")
  other_growth <- calculate_growth_rates(other_data, other_gdp_col, other_country_col, other_year_col)
  
  # Merge growth rates for correlation analysis
  merged_growth <- merge(
    repgermany_growth[, c("country", "year", "growth_rate")],
    other_growth[, c(other_country_col, other_year_col, "growth_rate")],
    by.x = c("country", "year"),
    by.y = c(other_country_col, other_year_col),
    suffixes = c("_repgermany", paste0("_", tolower(gsub("[^A-Za-z0-9]", "", dataset_name))))
  )
  
  # Remove rows with missing growth rates
  merged_growth <- merged_growth[complete.cases(merged_growth), ]
  
  cat("Found", nrow(merged_growth), "matching growth rate observations for", dataset_name, "\n")
  cat("Found", nrow(merged_gdp_levels), "matching GDP level observations for", dataset_name, "\n")
  
  if (nrow(merged_growth) == 0) {
    cat("Warning: No matching growth rate data found for", dataset_name, "\n")
    return(NULL)
  }
  
  # Calculate correlation
  correlation <- cor(merged_growth$growth_rate_repgermany, 
                     merged_growth[[paste0("growth_rate_", tolower(gsub("[^A-Za-z0-9]", "", dataset_name)))]], 
                     use = "complete.obs")
  
  cat("Correlation coefficient:", round(correlation, 3), "\n")
  
  return(list(
    data = merged_growth,
    gdp_levels = merged_gdp_levels,
    correlation = correlation,
    x_col = "growth_rate_repgermany",
    y_col = paste0("growth_rate_", tolower(gsub("[^A-Za-z0-9]", "", dataset_name))),
    dataset_name = dataset_name,
    matching_analysis = matching_analysis
  ))
}

# Function to create correlation scatter plot with matched dimensions
create_correlation_plot <- function(merged_result) {
  if (is.null(merged_result)) {
    return()
  }
  
  data <- merged_result$data
  correlation <- merged_result$correlation
  x_col <- merged_result$x_col
  y_col <- merged_result$y_col
  dataset_name <- merged_result$dataset_name
  
  # Fixed plot limits: range of 20 with padding
  xlim <- c(-12, 22)
  ylim <- c(-12, 22)
  
  # Create filename
  filename <- paste0("Results/Correlations/gdp_growth_correlation_", 
                     tolower(gsub("[^A-Za-z0-9]", "_", dataset_name)), ".pdf")
  
  # Create plot with matched dimensions and margins from main analysis script
  pdf(file = filename, width = 5.5, height = 5.0, family = "Times", pointsize = 12)
  par(mar=c(5.1,4.1,1,2.1))  # Match margins from main analysis script
  
  # Determine axis labels based on dataset
  if (grepl("current", dataset_name, ignore.case = TRUE)) {
    x_label <- "OECD Current (%)"
    y_label <- expression("Abadie " * italic("et al.") * " (%)")
  } else {
    x_label <- paste(dataset_name, "(%)")
    y_label <- expression("Abadie " * italic("et al.") * " (%)")
  }
  
  plot(data[[x_col]], data[[y_col]],
       type = "p",
       pch = 19,  # Solid filled circle (no border)
       cex = 0.6,
       col = adjustcolor("black", alpha.f = 0.6),
       xlim = xlim,
       ylim = ylim,
       xlab = x_label,
       ylab = y_label,
       xaxs = "i",
       yaxs = "i"
  )
  
  # Add correlation coefficient and sample size to plot
  legend("topleft", 
         legend = c(paste("r =", round(correlation, 3)),
                    paste("n =", nrow(data))),
         bty = "n",
         cex = 0.8)
  
  # Add reference line (45-degree line)
  abline(a = 0, b = 1, lty = "dashed", col = "darkgrey")
  
  # Add regression line
  lm_fit <- lm(data[[y_col]] ~ data[[x_col]])
  abline(lm_fit, col = "black", lty = "solid", lwd = 1.5)
  
  dev.off()
  
  cat("Saved correlation plot:", filename, "\n")
  
  # Print summary statistics
  cat("Summary for", dataset_name, "correlation:\n")
  cat("  Countries included:", length(unique(data$country)), "\n")
  cat("  Year range:", min(data$year), "-", max(data$year), "\n")
  cat("  Correlation coefficient:", round(correlation, 3), "\n")
  cat("  Regression R-squared:", round(summary(lm_fit)$r.squared, 3), "\n\n")
}

# Function to load and validate data files
load_and_validate_data <- function(filename, required_cols) {
  if (!file.exists(filename)) {
    cat("Error: File", filename, "not found\n")
    return(NULL)
  }
  
  # Try to read the file
  tryCatch({
    if (grepl("\\.csv$", filename, ignore.case = TRUE)) {
      data <- read.csv(filename, stringsAsFactors = FALSE)
    } else if (grepl("\\.dta$", filename, ignore.case = TRUE)) {
      data <- read.dta(filename)
    } else {
      cat("Error: Unsupported file format for", filename, "\n")
      return(NULL)
    }
    
    # Check if required columns exist (only if required_cols is not empty)
    if (length(required_cols) > 0) {
      missing_cols <- required_cols[!required_cols %in% names(data)]
      if (length(missing_cols) > 0) {
        cat("Error: Missing columns in", filename, ":", paste(missing_cols, collapse = ", "), "\n")
        cat("Available columns:", paste(names(data), collapse = ", "), "\n")
        return(NULL)
      }
    }
    
    cat("Successfully loaded", filename, "with", nrow(data), "rows\n")
    return(data)
    
  }, error = function(e) {
    cat("Error loading", filename, ":", e$message, "\n")
    return(NULL)
  })
}

# Main analysis function
main_correlation_analysis <- function() {
  cat("Starting GDP Growth Rate Correlation Analysis\n")
  cat("===========================================\n\n")
  
  # Create output directory
  create_correlation_dir()
  
  # Load RepGermany data
  cat("Loading RepGermany data...\n")
  repgermany_data <- load_and_validate_data("Data/repgermany.dta", c("country", "year", "gdp"))
  
  if (is.null(repgermany_data)) {
    # Try CSV version as fallback
    repgermany_data <- load_and_validate_data("repgermany.csv", c("country", "year", "gdp"))
    if (is.null(repgermany_data)) {
      cat("Cannot proceed without RepGermany data (tried .dta and .csv)\n")
      return()
    }
  }
  
  cat("RepGermany data loaded with", nrow(repgermany_data), "rows\n")
  cat("Year range:", min(repgermany_data$year, na.rm=T), "-", max(repgermany_data$year, na.rm=T), "\n")
  cat("Countries:", length(unique(repgermany_data$country)), "\n\n")
  
  # Dataset 1: OECD_constant.csv
  cat("\n--- Analysis 1: RepGermany vs OECD_constant.csv ---\n")
  
  oecd_constant_data <- load_and_validate_data("Data/OECD_constant.csv", c())
  
  if (!is.null(oecd_constant_data)) {
    cat("OECD_constant.csv data dimensions:", nrow(oecd_constant_data), "rows x", ncol(oecd_constant_data), "columns\n")
    cat("Available columns:", paste(names(oecd_constant_data), collapse = ", "), "\n\n")
    
    # Try to identify GDP, country, and year columns
    gdp_col <- NULL
    country_col <- NULL
    year_col <- NULL
    
    # Look for GDP column
    gdp_candidates <- names(oecd_constant_data)[grepl("gdp|GDP|value|VALUE|obs", names(oecd_constant_data), ignore.case = TRUE)]
    if (length(gdp_candidates) > 0) {
      gdp_col <- gdp_candidates[1]
      cat("Using GDP column:", gdp_col, "\n")
    }
    
    # Look for country column
    country_candidates <- names(oecd_constant_data)[grepl("country|Country|area|Area|location|reference", names(oecd_constant_data), ignore.case = TRUE)]
    if (length(country_candidates) > 0) {
      country_col <- country_candidates[1]
      cat("Using country column:", country_col, "\n")
    }
    
    # Look for year column
    year_candidates <- names(oecd_constant_data)[grepl("year|Year|time|Time|period|Period", names(oecd_constant_data), ignore.case = TRUE)]
    if (length(year_candidates) > 0) {
      year_col <- year_candidates[1]
      cat("Using year column:", year_col, "\n")
    }
    
    # If standard OECD format columns exist, use them
    if ("OBS_VALUE" %in% names(oecd_constant_data)) {
      gdp_col <- "OBS_VALUE"
      cat("Found OBS_VALUE column, using it for GDP\n")
    }
    if ("Reference area" %in% names(oecd_constant_data)) {
      country_col <- "Reference area"
      cat("Found Reference area column, using it for country\n")
    }
    if ("TIME_PERIOD" %in% names(oecd_constant_data)) {
      year_col <- "TIME_PERIOD"
      cat("Found TIME_PERIOD column, using it for year\n")
    }
    
    if (!is.null(gdp_col) && !is.null(country_col) && !is.null(year_col)) {
      
      # Convert year to numeric if needed
      if (is.character(oecd_constant_data[[year_col]])) {
        oecd_constant_data[[year_col]] <- as.numeric(oecd_constant_data[[year_col]])
      }
      
      # Clean the data
      before_clean <- nrow(oecd_constant_data)
      oecd_constant_data <- oecd_constant_data[!is.na(oecd_constant_data[[gdp_col]]) & 
                                                 !is.na(oecd_constant_data[[year_col]]) & 
                                                 oecd_constant_data[[country_col]] != "", ]
      after_clean <- nrow(oecd_constant_data)
      cat("Removed", before_clean - after_clean, "rows with missing values\n")
      
      result1 <- merge_and_correlate(
        repgermany_data, 
        oecd_constant_data, 
        gdp_col, 
        country_col, 
        year_col,
        "OECD Constant"
      )
      
      if (!is.null(result1)) {
        create_correlation_plot(result1)
      }
    } else {
      cat("Could not identify appropriate columns in OECD_constant.csv\n")
      cat("Missing: ")
      if (is.null(gdp_col)) cat("GDP column ")
      if (is.null(country_col)) cat("Country column ")
      if (is.null(year_col)) cat("Year column ")
      cat("\nPlease ensure the file has identifiable GDP, country, and year columns\n")
    }
  }
  
  # Dataset 2: OECD_current.csv
  cat("\n--- Analysis 2: RepGermany vs OECD_current.csv ---\n")
  
  oecd_current_data <- load_and_validate_data("Data/OECD_current.csv", c())
  
  if (!is.null(oecd_current_data)) {
    cat("OECD_current.csv data dimensions:", nrow(oecd_current_data), "rows x", ncol(oecd_current_data), "columns\n")
    cat("Available columns:", paste(names(oecd_current_data), collapse = ", "), "\n\n")
    
    # Try to identify GDP, country, and year columns
    gdp_col <- NULL
    country_col <- NULL
    year_col <- NULL
    
    # Look for GDP column
    gdp_candidates <- names(oecd_current_data)[grepl("gdp|GDP|value|VALUE|obs", names(oecd_current_data), ignore.case = TRUE)]
    if (length(gdp_candidates) > 0) {
      gdp_col <- gdp_candidates[1]
      cat("Using GDP column:", gdp_col, "\n")
    }
    
    # Look for country column
    country_candidates <- names(oecd_current_data)[grepl("country|Country|area|Area|location|reference", names(oecd_current_data), ignore.case = TRUE)]
    if (length(country_candidates) > 0) {
      country_col <- country_candidates[1]
      cat("Using country column:", country_col, "\n")
    }
    
    # Look for year column
    year_candidates <- names(oecd_current_data)[grepl("year|Year|time|Time|period|Period", names(oecd_current_data), ignore.case = TRUE)]
    if (length(year_candidates) > 0) {
      year_col <- year_candidates[1]
      cat("Using year column:", year_col, "\n")
    }
    
    # If standard OECD format columns exist, use them
    if ("OBS_VALUE" %in% names(oecd_current_data)) {
      gdp_col <- "OBS_VALUE"
      cat("Found OBS_VALUE column, using it for GDP\n")
    }
    if ("Reference area" %in% names(oecd_current_data)) {
      country_col <- "Reference area"
      cat("Found Reference area column, using it for country\n")
    }
    if ("TIME_PERIOD" %in% names(oecd_current_data)) {
      year_col <- "TIME_PERIOD"
      cat("Found TIME_PERIOD column, using it for year\n")
    }
    
    if (!is.null(gdp_col) && !is.null(country_col) && !is.null(year_col)) {
      
      # Convert year to numeric if needed
      if (is.character(oecd_current_data[[year_col]])) {
        oecd_current_data[[year_col]] <- as.numeric(oecd_current_data[[year_col]])
      }
      
      # Clean the data
      before_clean <- nrow(oecd_current_data)
      oecd_current_data <- oecd_current_data[!is.na(oecd_current_data[[gdp_col]]) & 
                                               !is.na(oecd_current_data[[year_col]]) & 
                                               oecd_current_data[[country_col]] != "", ]
      after_clean <- nrow(oecd_current_data)
      cat("Removed", before_clean - after_clean, "rows with missing values\n")
      
      result2 <- merge_and_correlate(
        repgermany_data, 
        oecd_current_data, 
        gdp_col, 
        country_col, 
        year_col,
        "OECD_Current"
      )
      
      if (!is.null(result2)) {
        create_correlation_plot(result2)
      }
    } else {
      cat("Could not identify appropriate columns in OECD_current.csv\n")
      cat("Missing: ")
      if (is.null(gdp_col)) cat("GDP column ")
      if (is.null(country_col)) cat("Country column ")
      if (is.null(year_col)) cat("Year column ")
      cat("\nPlease ensure the file has identifiable GDP, country, and year columns\n")
    }
  }
  
  cat("\n===========================================\n")
  cat("GDP Growth Rate Correlation Analysis Complete\n")
  cat("Results saved in Results/Correlations/ directory\n")
  cat("- Correlation plots (PDF) with matched dimensions\n")
  cat("- Merged GDP levels (CSV with columns: year, country, Abadie_et_al, [Dataset])\n")
  cat("\nDatasets analyzed:\n")
  cat("1. RepGermany vs OECD_constant.csv\n")
  cat("2. RepGermany vs OECD_current.csv\n")
}

# Run the analysis
main_correlation_analysis()