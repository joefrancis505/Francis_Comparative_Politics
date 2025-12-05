# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

## Modified Analysis code for dataset creation only
# A. Abadie, A. Diamond, and J. Hainmueller. 2014.
# Comparative Politics and the Synthetic Control Method
# American Journal of Political Science.

rm(list=ls())

# Load required libraries
load_libraries <- function() {
  packages <- c("foreign", "WDI", "readODS")
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}

# Function to save database as DTA
save_database <- function(data, filename, description) {
  if (!dir.exists("Data")) dir.create("Data", recursive = TRUE)
  filepath <- file.path("Data", filename)
  write.dta(data, filepath)
  cat("Database saved:", description, "->", filepath, "\n")
  cat("  Rows:", nrow(data), ", Columns:", ncol(data), "\n")
  cat("  Countries:", length(unique(data$country)), ", Years:", min(data$year), "-", max(data$year), "\n")
}

# Country mapping for WDI
country_mapping <- data.frame(
  original = c("USA", "UK", "Austria", "Belgium", "Denmark", "France", 
               "West Germany", "Italy", "Netherlands", "Norway", "Switzerland", 
               "Japan", "Greece", "Portugal", "Spain", "Australia", "New Zealand"),
  wdi_code = c("US", "GB", "AT", "BE", "DK", "FR",
               "DE", "IT", "NL", "NO", "CH",
               "JP", "GR", "PT", "ES", "AU", "NZ"),
  wdi_name = c("United States", "United Kingdom", "Austria", "Belgium", "Denmark", "France",
               "Germany", "Italy", "Netherlands", "Norway", "Switzerland",
               "Japan", "Greece", "Portugal", "Spain", "Australia", "New Zealand"),
  stringsAsFactors = FALSE
)

# Function to download WDI data
prepare_wdi_data <- function(indicator_code, indicator_name) {
  cat("Downloading WDI data with indicator:", indicator_code, "\n")
  
  wdi_data <- WDI(
    country = country_mapping$wdi_code,
    indicator = indicator_code,
    start = 1960,
    end = 2003,
    extra = FALSE
  )
  
  return(list(data = wdi_data, complete_countries = country_mapping$wdi_name, indicator = indicator_code))
}

# Function to load West Germany series from ODS file
load_west_germany_series <- function() {
  cat("Loading West Germany GDP series from ODS file...\n")
  
  # Load West Germany ODS data
  west_germany_data <- read_ods("Data/West_Germany_gdp.ods")
  cat("West Germany ODS data loaded with", nrow(west_germany_data), "observations\n")
  
  # Ensure the data has the expected structure (year and gdp columns)
  if (!all(c("year", "gdp") %in% names(west_germany_data))) {
    stop("West Germany ODS file must contain 'year' and 'gdp' columns")
  }
  
  # Filter to 1960-2003 range and ensure proper data types
  west_germany_data <- west_germany_data[west_germany_data$year >= 1960 & west_germany_data$year <= 2003, ]
  west_germany_data$year <- as.numeric(west_germany_data$year)
  west_germany_data$gdp <- as.numeric(west_germany_data$gdp)
  
  cat("West Germany series covers years:", min(west_germany_data$year), "-", max(west_germany_data$year), "\n")
  
  return(west_germany_data)
}

# Function to create datasets with different reference years
create_reference_year_datasets <- function(original_data, wdi_result, wg_series) {
  cat("Creating datasets with different reference years...\n")
  
  filtered_original <- original_data[original_data$country %in% country_mapping$original, ]
  
  all_datasets <- list()
  
  for (ref_year in 1960:2003) {
    cat("\n--- Processing reference year:", ref_year, "---\n")
    
    # Create a copy of the original data to modify
    ref_dataset <- filtered_original
    
    for (i in 1:nrow(country_mapping)) {
      original_country <- country_mapping$original[i]
      wdi_code <- country_mapping$wdi_code[i]
      
      # Skip if this country doesn't exist in original data
      if (!original_country %in% ref_dataset$country) next
      
      # Get the base value from original data for this reference year
      original_base <- ref_dataset$gdp[ref_dataset$country == original_country & ref_dataset$year == ref_year]
      
      if (length(original_base) == 0 || is.na(original_base)) {
        cat("Warning: No", ref_year, "data for", original_country, ". Skipping.\n")
        next
      }
      
      # For West Germany, use the ODS series throughout
      if (original_country == "West Germany") {
        # Get the base value from the West Germany ODS series for the reference year
        wg_base <- wg_series$gdp[wg_series$year == ref_year]
        
        if (length(wg_base) == 0 || is.na(wg_base)) {
          cat("Warning: No", ref_year, "West Germany ODS data. Skipping.\n")
          next
        }
        
        # Apply growth rates from the West Germany ODS series
        for (year in 1960:2003) {
          if (year == ref_year) next
          
          wg_value <- wg_series$gdp[wg_series$year == year]
          if (!is.na(wg_value) && wg_base > 0) {
            growth_factor <- wg_value / wg_base
            new_value <- original_base * growth_factor
            row_index <- ref_dataset$country == original_country & ref_dataset$year == year
            ref_dataset$gdp[row_index] <- new_value
          }
        }
      } else {
        # For other countries: use WDI growth rates
        wdi_country_data <- wdi_result$data[wdi_result$data$iso2c == wdi_code, ]
        wdi_base <- wdi_country_data[[wdi_result$indicator]][wdi_country_data$year == ref_year]
        
        if (length(wdi_base) == 0 || is.na(wdi_base)) {
          cat("Warning: No", ref_year, "WDI data for", original_country, ". Skipping.\n")
          next
        }
        
        for (year in 1960:2003) {
          if (year == ref_year) next
          
          wdi_current <- wdi_country_data[[wdi_result$indicator]][wdi_country_data$year == year]
          if (!is.na(wdi_current) && wdi_base > 0) {
            growth_factor <- wdi_current / wdi_base
            new_value <- original_base * growth_factor
            row_index <- ref_dataset$country == original_country & ref_dataset$year == year
            ref_dataset$gdp[row_index] <- new_value
          }
        }
      }
    }
    
    all_datasets[[as.character(ref_year)]] <- ref_dataset
    cat("Successfully created dataset for reference year", ref_year, "\n")
  }
  
  return(all_datasets)
}

# Main execution - dataset creation only
main <- function() {
  load_libraries()
  
  # Create Data directory
  if (!dir.exists("Data")) dir.create("Data", recursive = TRUE)
  
  # Load original data
  cat("Loading original data...\n")
  original_file <- "Data/repgermany.dta"
  if (file.exists(original_file)) {
    d_original <- read.dta(original_file)
    cat("Original data loaded from", original_file, "\n")
  } else {
    stop("Original data file", original_file, "not found!")
  }
  
  # Load West Germany GDP series from ODS file
  cat("\n=== Loading West Germany GDP Series ===\n")
  wg_series <- load_west_germany_series()
  
  # WDI analyses for all 44 years
  cat("\n=== Creating Datasets with West Germany ODS Data for All Reference Years ===\n")
  
  tryCatch({
    wdi_result <- prepare_wdi_data("NY.GDP.PCAP.KD", "GDP per capita (constant 2015 US$)")
    
    # Create datasets for all reference years using West Germany ODS data throughout
    all_datasets <- create_reference_year_datasets(d_original, wdi_result, wg_series)
    
    # Save all datasets
    for (ref_year in 1960:2003) {
      dataset <- all_datasets[[as.character(ref_year)]]
      save_database(dataset, paste0("repgermany_wdi", ref_year, ".dta"), 
                    paste0("WDI ", ref_year, " reference year dataset (West Germany from ODS)"))
    }
    
    cat("\n=== All datasets created ===\n")
    cat("Datasets saved in Data/ folder\n")
    cat("Total datasets created: 44\n")
    cat("Files created:\n")
    cat("  - repgermany_wdi1960.dta to repgermany_wdi2003.dta\n")
    cat("  - West Germany data sourced from West_Germany_gdp.ods throughout\n")
    
  }, error = function(e) {
    cat("Failed to process:", e$message, "\n")
  })
}

# Run the dataset creation
main()