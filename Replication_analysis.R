# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

## Multi-Version Replication Code for
# A. Abadie, A. Diamond, and J. Hainmueller. 2014.
# Comparative Politics and the Synthetic Control Method
# American Journal of Political Science.
# 
# Adapted to run with multiple GDP data sources
# Modified to use data-driven y-axis scaling
# Added GDP normalization options:
# 1. West Germany 1990 = 100
# 2. USA = 100 in each year
# 3. WDI constant 2015 data with West Germany 1990 = 100

rm(list=ls())

# Load required libraries
if (!require(foreign, quietly = TRUE)) {
  install.packages("foreign")
  library(foreign)
}

if (!require(Synth, quietly = TRUE)) {
  install.packages("Synth")
  library(Synth)
}

if (!require(xtable, quietly = TRUE)) {
  install.packages("xtable")
  library(xtable)
}

if (!require(WDI, quietly = TRUE)) {
  install.packages("WDI")
  library(WDI)
}

# Helper function to calculate appropriate y-axis limits
calculate_ylim <- function(data_vectors, symmetric = FALSE, padding = 0.05) {
  # Combine all data vectors and remove NAs
  all_data <- unlist(data_vectors)
  all_data <- all_data[!is.na(all_data)]
  
  if (length(all_data) == 0) {
    return(c(0, 1))  # fallback
  }
  
  data_min <- min(all_data)
  data_max <- max(all_data)
  data_range <- data_max - data_min
  
  if (symmetric) {
    # For gap plots - symmetric around zero
    abs_max <- max(abs(data_min), abs(data_max))
    margin <- abs_max * padding
    return(c(-(abs_max + margin), abs_max + margin))
  } else {
    # For level plots - start from reasonable minimum
    margin <- data_range * padding
    y_min <- max(0, data_min - margin)  # Don't go below 0 for GDP levels
    y_max <- data_max + margin
    return(c(y_min, y_max))
  }
}

# Function to normalize GDP so that West Germany 1990 = 100
normalize_gdp <- function(data, west_germany_id = 7, base_year = 1990) {
  cat("Normalizing GDP data (West Germany", base_year, "= 100)...\n")
  
  # Find West Germany's GDP in the base year
  west_germany_base <- data$gdp[data$index == west_germany_id & data$year == base_year]
  
  if (length(west_germany_base) == 0 || is.na(west_germany_base)) {
    stop("Cannot find West Germany GDP data for base year ", base_year)
  }
  
  cat("West Germany GDP in", base_year, ":", west_germany_base, "\n")
  
  # Normalize all GDP values
  data$gdp <- (data$gdp / west_germany_base) * 100
  
  cat("GDP normalization completed. West Germany", base_year, "GDP is now 100.\n")
  
  return(data)
}

# Function to normalize GDP so that USA = 100 in each year
normalize_gdp_usa_100_each_year <- function(data, usa_id = 1) {
  cat("Normalizing GDP data (USA = 100 in each year)...\n")
  
  # Get all years in the data
  years <- unique(data$year)
  years <- years[order(years)]
  
  # For each year, normalize all countries relative to USA
  for (year in years) {
    # Find USA's GDP in this year
    usa_value <- data$gdp[data$index == usa_id & data$year == year]
    
    if (length(usa_value) == 0 || is.na(usa_value)) {
      cat("Warning: Cannot find USA GDP data for year", year, "\n")
      next
    }
    
    # Normalize all countries' GDP for this year relative to USA
    year_rows <- data$year == year
    data$gdp[year_rows] <- (data$gdp[year_rows] / usa_value) * 100
  }
  
  cat("GDP normalization completed. USA = 100 in each year.\n")
  
  return(data)
}

# Function to save database as CSV
save_database <- function(data, filename, description) {
  # Ensure Data directory exists
  if (!dir.exists("Data")) {
    dir.create("Data", recursive = TRUE)
  }
  
  # Save to Data folder
  filepath <- file.path("Data", filename)
  write.csv(data, filepath, row.names = FALSE)
  cat("Database saved:", description, "->", filepath, "\n")
  cat("  Rows:", nrow(data), ", Columns:", ncol(data), "\n")
  cat("  Countries:", length(unique(data$country)), ", Years:", min(data$year), "-", max(data$year), "\n")
}

# Create output directories
create_directories <- function() {
  dirs <- c("Results/Original_normalized", "Results/Original_USA_100", "Results/WDI_constant", "Data")
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
}

# Country mapping for WDI (using ISO-2 codes)
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

# Function to download and prepare WDI data
prepare_wdi_data <- function(indicator_code, indicator_name) {
  cat("Downloading WDI data for indicator:", indicator_code, "\n")
  
  # Download WDI data
  wdi_data <- WDI(
    country = country_mapping$wdi_code,
    indicator = indicator_code,
    start = 1960,
    end = 2003,
    extra = FALSE
  )
  
  # Check for complete coverage
  years_needed <- 1960:2003
  countries_complete <- c()
  
  for (i in 1:nrow(country_mapping)) {
    country_code <- country_mapping$wdi_code[i]
    country_name <- country_mapping$wdi_name[i]
    country_data <- wdi_data[wdi_data$iso2c == country_code, ]
    available_years <- country_data$year[!is.na(country_data[[indicator_code]])]
    
    if (all(years_needed %in% available_years)) {
      countries_complete <- c(countries_complete, country_name)
    } else {
      cat("Warning: Incomplete data for", country_name, "- excluding from analysis\n")
    }
  }
  
  if (length(countries_complete) == 0) {
    stop("No countries have complete data coverage for ", indicator_code)
  }
  
  # Filter to complete countries only - convert back to country names
  complete_codes <- country_mapping$wdi_code[country_mapping$wdi_name %in% countries_complete]
  wdi_data <- wdi_data[wdi_data$iso2c %in% complete_codes, ]
  
  # Handle Germany ratio splicing
  if ("Germany" %in% countries_complete) {
    wdi_data <- apply_germany_ratio_splice(wdi_data, indicator_code)
  }
  
  return(list(data = wdi_data, complete_countries = countries_complete, indicator = indicator_code))
}

# Function to apply Germany ratio splicing
apply_germany_ratio_splice <- function(wdi_data, indicator_code) {
  # Check if West_Germany.csv exists
  if (!file.exists("Data/West_Germany.csv")) {
    cat("Warning: West_Germany.csv not found. Using Germany data as-is.\n")
    return(wdi_data)
  }
  
  # Read West Germany data
  west_germany <- read.csv("Data/West_Germany.csv", stringsAsFactors = FALSE)
  cat("West Germany data loaded with", nrow(west_germany), "observations\n")
  cat("Years available:", min(west_germany$year, na.rm=T), "-", max(west_germany$year, na.rm=T), "\n")
  
  # Get Germany data from WDI
  germany_data <- wdi_data[wdi_data$iso2c == "DE", ]
  
  if (nrow(germany_data) == 0) {
    cat("No Germany data found in WDI data\n")
    return(wdi_data)
  }
  
  # Strategy: 
  # 1. Use WDI Germany data for 1960-1991 (actual data including reunification year)
  # 2. For 1992-2003, apply West Germany growth rates to 1991 WDI level
  #    Formula: WDI_t = (West_Germany_t / West_Germany_1991) * WDI_1991
  
  # Get 1991 WDI value (reunification year baseline)
  wdi_1991 <- germany_data[[indicator_code]][germany_data$year == 1991]
  
  if (is.na(wdi_1991) || length(wdi_1991) == 0) {
    cat("Warning: No WDI data for Germany in 1991. Cannot perform ratio splicing.\n")
    return(wdi_data)
  }
  
  # Get 1991 West Germany value (baseline for growth rates)
  west_1991 <- west_germany$gdp[west_germany$year == 1991]
  
  if (is.na(west_1991) || length(west_1991) == 0) {
    cat("Warning: No West Germany data for 1991. Cannot perform ratio splicing.\n")
    return(wdi_data)
  }
  
  cat("Ratio splicing Germany data:\n")
  cat("  Using WDI Germany data for 1960-1991\n")
  cat("  WDI Germany 1991 (baseline):", wdi_1991, "\n")
  cat("  West Germany 1991 (baseline):", west_1991, "\n") 
  cat("  Applying West Germany growth rates for 1992-2003:\n")
  
  # Apply West Germany growth pattern for post-1991 years
  for (year in 1992:2003) {
    if (year %in% west_germany$year) {
      west_current <- west_germany$gdp[west_germany$year == year]
      
      if (!is.na(west_current)) {
        # Calculate growth rate from West Germany data
        growth_factor <- west_current / west_1991
        
        # Apply growth rate to 1991 WDI level
        new_value <- growth_factor * wdi_1991
        
        # Replace in WDI data
        germany_row <- wdi_data$iso2c == "DE" & wdi_data$year == year
        if (sum(germany_row) > 0) {
          old_value <- wdi_data[[indicator_code]][germany_row]
          wdi_data[[indicator_code]][germany_row] <- new_value
          cat("    Year", year, ": West Germany =", west_current, 
              ", growth factor =", round(growth_factor, 4),
              ", new WDI value =", round(new_value, 2), 
              ", (was", round(old_value, 2), ")\n")
        }
      }
    }
  }
  
  return(wdi_data)
}

# Function to merge WDI data with original dataset
merge_wdi_with_original <- function(original_data, wdi_result) {
  # Create mapping between original and WDI country names
  complete_mapping <- country_mapping[country_mapping$wdi_name %in% wdi_result$complete_countries, ]
  
  # Filter original data to countries with complete WDI data
  filtered_original <- original_data[original_data$country %in% complete_mapping$original, ]
  
  # Merge WDI GDP data
  for (i in 1:nrow(complete_mapping)) {
    original_country <- complete_mapping$original[i]
    wdi_country <- complete_mapping$wdi_name[i]
    wdi_code <- complete_mapping$wdi_code[i]
    
    wdi_country_data <- wdi_result$data[wdi_result$data$iso2c == wdi_code, ]
    
    for (year in 1960:2003) {
      original_rows <- filtered_original$country == original_country & filtered_original$year == year
      wdi_value <- wdi_country_data[[wdi_result$indicator]][wdi_country_data$year == year]
      
      if (length(wdi_value) > 0 && !is.na(wdi_value)) {
        filtered_original$gdp[original_rows] <- wdi_value
      }
    }
  }
  
  return(filtered_original)
}

# Function to run SCM analysis
run_scm_analysis <- function(data, output_dir, version_name) {
  cat("Running SCM analysis for:", version_name, "\n")
  
  # Determine y-axis label based on version
  if (version_name == "usa100") {
    y_label <- "per-capita GDP (USA = 100)"
    gap_y_label <- "gap in per-capita GDP (USA = 100)"
  } else {
    y_label <- "per-capita GDP (West Germany 1990 = 100)"
    gap_y_label <- "gap in per-capita GDP (West Germany 1990 = 100)"
  }
  
  # Set working directory for outputs
  old_dir <- getwd()
  setwd(output_dir)
  
  tryCatch({
    
    ## Table 1 & 2, Figure 1, 2, & 3
    
    ## pick v by cross-validation
    # data setup for training model
    dataprep.out <- dataprep(
      foo = data,
      predictors = c("gdp","trade","infrate"),
      dependent = "gdp",
      unit.variable = 1,
      time.variable = 3,
      special.predictors = list(
        list("industry", 1971:1980, c("mean")),
        list("schooling",c(1970,1975), c("mean")),
        list("invest70" ,1980, c("mean"))
      ),
      treatment.identifier = 7,
      controls.identifier = unique(data$index)[-7],
      time.predictors.prior = 1971:1980,
      time.optimize.ssr = 1981:1990,
      unit.names.variable = 2,
      time.plot = 1960:2003
    )
    
    # fit training model
    synth.out <- synth(
      data.prep.obj=dataprep.out,
      Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
    )
    
    # data prep for main model
    dataprep.out <- dataprep(
      foo = data,
      predictors = c("gdp","trade","infrate"),
      dependent = "gdp",
      unit.variable = 1,
      time.variable = 3,
      special.predictors = list(
        list("industry" ,1981:1990, c("mean")),
        list("schooling",c(1980,1985), c("mean")),
        list("invest80" ,1980, c("mean"))
      ),
      treatment.identifier = 7,
      controls.identifier = unique(data$index)[-7],
      time.predictors.prior = 1981:1990,
      time.optimize.ssr = 1960:1989,
      unit.names.variable = 2,
      time.plot = 1960:2003
    )
    
    # fit main model with v from training model
    synth.out <- synth(
      data.prep.obj=dataprep.out,
      custom.v=as.numeric(synth.out$solution.v)
    )
    
    #### Table 2
    synth.tables <- synth.tab(
      dataprep.res = dataprep.out,
      synth.res = synth.out
    )
    
    # Replace means for OECD sample (computed externally using proper pop weighting)
    synth.tables$tab.pred[,3] <- c(8021.1,31.9,7.4,34.2,44.1,25.9)
    colnames(synth.tables$tab.pred)[3] <- "Rest of OECD Sample"
    rownames(synth.tables$tab.pred) <- c("GDP per-capita","Trade openness",
                                         "Inflation rate","Industry share",
                                         "Schooling","Investment rate")
    
    # Save Table 2
    table2_output <- xtable(round(synth.tables$tab.pred,1),digits=1)
    print(table2_output, file=paste0("table2_", version_name, ".txt"), type="latex")
    write.csv(round(synth.tables$tab.pred,1), paste0("table2_", version_name, ".csv"))
    
    #### Table 1
    # synth weights
    tab1 <- data.frame(synth.tables$tab.w)
    tab1[,1] <- round(tab1[,1],2) 
    # regression weights
    X0 <- cbind(1,t(dataprep.out$X0))
    X1 <- as.matrix(c(1,dataprep.out$X1))
    W <- X0%*%solve(t(X0)%*%X0)%*%X1
    Wdat <- data.frame(unit.numbers=as.numeric(rownames(X0)),
                       regression.w=round(W,2))
    tab1 <- merge(tab1,Wdat,by="unit.numbers")
    tab1 <- tab1[order(tab1[,3]),]
    
    # Save Table 1
    table1_output <- xtable(cbind(tab1[1:9,c(3,2,4)],
                                  tab1[10:18,c(3,2,4)]))
    print(table1_output, file=paste0("table1_", version_name, ".txt"), type="latex")
    write.csv(tab1, paste0("table1_", version_name, ".csv"))
    
    # Calculate synthetic control
    synthY0 <- (dataprep.out$Y0%*%synth.out$solution.w)
    
    # Calculate OECD average for Figure 1
    oecd_avg <- aggregate(data[,c("gdp")],by=list(data$year),mean,na.rm=T)[,2]
    
    #### Figure 1: Trends in Per-Capita GDP: West Germany vs. Rest of the OECD Sample
    # Set y-axis limits based on version - consistent for original and constant2015
    if (version_name == "original" || version_name == "constant2015") {
      ylim_fig1 <- c(0, 160)  # 0 to 150 with 10 padding above
      use_custom_axes <- TRUE
    } else if (version_name == "usa100") {
      ylim_fig1 <- c(60, 105)  # Upper limit 100 with padding, increments of 10
      use_custom_axes <- TRUE
    } else {
      ylim_fig1 <- calculate_ylim(list(dataprep.out$Y1plot, oecd_avg))
      use_custom_axes <- FALSE
    }
    
    Text.height <- ylim_fig1[2] * 0.7  # Position text at 70% of max height
    Cex.set <- .8
    pdf(file = paste0("ger_vs_oecd_", version_name, ".pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
    par(mar=c(5.1,4.1,1,2.1))  # Remove top margin but keep plot box same size
    plot(1960:2003,dataprep.out$Y1plot,
         type="l",ylim=ylim_fig1,col="black",lty="solid",
         ylab = y_label,
         xlab ="year",
         xaxs = "i", yaxs = "i",
         lwd=2, yaxt=if(use_custom_axes) "n" else "s"
    )
    if (use_custom_axes) {
      if (version_name == "usa100") {
        axis(2, at=seq(60, 100, 10), las=1)
      } else if (version_name == "original" || version_name == "constant2015") {
        axis(2, at=seq(0, 150, 50), las=1)  # Numbers every 50
      }
    }
    lines(1960:2003,oecd_avg,col="black",lty="dashed",lwd=2)
    abline(v=1990,lty="dotted")
    legend(x="bottomright",
           legend=c("West Germany","rest of the OECD sample")
           ,lty=c("solid","dashed"),col=c("black","black")
           ,cex=.8,bg="white",lwd=c(2,2))
    arrows(1987,Text.height,1989,Text.height,col="black",length=.1)
    text(1982.5,Text.height,"reunification",cex=Cex.set)
    dev.off()
    
    #### Figure 2: Trends in Per-Capita GDP: West Germany vs. Synthetic West Germany
    # Set y-axis limits based on version - consistent for original and constant2015
    if (version_name == "original" || version_name == "constant2015") {
      ylim_fig2 <- c(0, 160)  # 0 to 150 with 10 padding above
      use_custom_axes <- TRUE
    } else if (version_name == "usa100") {
      ylim_fig2 <- c(60, 105)  # Upper limit 100 with padding, increments of 10
      use_custom_axes <- TRUE
    } else {
      ylim_fig2 <- calculate_ylim(list(dataprep.out$Y1plot, synthY0))
      use_custom_axes <- FALSE
    }
    
    Text.height <- ylim_fig2[2] * 0.7
    pdf(file = paste0("ger_vs_synthger_", version_name, ".pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
    par(mar=c(5.1,4.1,1,2.1))  # Remove top margin but keep plot box same size
    plot(1960:2003,dataprep.out$Y1plot,
         type="l",ylim=ylim_fig2,col="black",lty="solid",
         ylab = y_label,
         xlab ="year",
         xaxs = "i", yaxs = "i",
         lwd=2, yaxt=if(use_custom_axes) "n" else "s"
    )
    if (use_custom_axes) {
      if (version_name == "usa100") {
        axis(2, at=seq(60, 100, 10), las=1)
      } else if (version_name == "original" || version_name == "constant2015") {
        axis(2, at=seq(0, 150, 50), las=1)  # Numbers every 50
      }
    }
    lines(1960:2003,synthY0,col="black",lty="dashed",lwd=2)
    abline(v=1990,lty="dotted")
    legend(x="bottomright",
           legend=c("West Germany","synthetic West Germany")
           ,lty=c("solid","dashed"),col=c("black","black")
           ,cex=.8,bg="white",lwd=c(2,2))
    arrows(1987,Text.height,1989,Text.height,col="black",length=.1)
    text(1982.5,Text.height,"reunification",cex=Cex.set)
    dev.off()
    
    ### Figure 3: Per-Capita GDP Gap Between West Germany and Synthetic West Germany
    gap <- dataprep.out$Y1-(dataprep.out$Y0%*%synth.out$solution.w)
    
    # Set y-axis limits based on version - consistent for original and constant2015
    if (version_name %in% c("original", "constant2015")) {
      ylim_fig3 <- c(-18, 18)  # -15 to 15 with 3 padding either side
      use_custom_axes <- TRUE
    } else if (version_name == "wg100_each_year") {
      ylim_fig3 <- c(-50, 50)  # Larger range for West Germany = 100 each year
      use_custom_axes <- TRUE
    } else {
      ylim_fig3 <- calculate_ylim(list(gap), symmetric = TRUE)
      use_custom_axes <- FALSE
    }
    
    pdf(file = paste0("ger_vs_synthger_gaps_", version_name, ".pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
    par(mar=c(5.1,4.1,1,2.1))  # Remove top margin but keep plot box same size
    plot(1960:2003,gap,
         type="l",ylim=ylim_fig3,col="black",lty="solid",
         ylab = gap_y_label,
         xlab ="year",
         xaxs = "i", yaxs = "i",
         lwd=2, yaxt=if(use_custom_axes) "n" else "s"
    )
    if (use_custom_axes) {
      if (version_name == "usa100") {
        axis(2, at=seq(-10, 10, 5), las=1)
      } else if (version_name == "original" || version_name == "constant2015") {
        axis(2, at=seq(-15, 15, 5), las=1)  # Numbers every 5
      }
    }
    abline(v=1990,lty="dotted")
    abline(h=0,lty="dotted")
    arrows(1987,ylim_fig3[2]*0.3,1989,ylim_fig3[2]*0.3,col="black",length=.1)
    text(1982.5,ylim_fig3[2]*0.3,"reunification",cex=Cex.set)
    dev.off()
    
    # Save gap data
    gap_data <- data.frame(year = 1960:2003, gap = as.numeric(gap))
    write.csv(gap_data, paste0("gap_data_", version_name, ".csv"), row.names = FALSE)
    
    ### Figure 4: Placebo Reunification 1975 - Trends in Per-Capita GDP: West Germany vs. Synthetic West Germany
    
    # data prep for training model
    dataprep.out.placebo <- dataprep(
      foo = data,
      predictors = c("gdp","trade","infrate"),
      dependent = "gdp",
      unit.variable = 1,
      time.variable = 3,
      special.predictors = list(
        list("industry",1971, c("mean")),
        list("schooling",c(1960,1965), c("mean")),
        list("invest60" ,1980, c("mean"))
      ),
      treatment.identifier = 7,
      controls.identifier = unique(data$index)[-7],
      time.predictors.prior = 1960:1964,
      time.optimize.ssr = 1965:1975,
      unit.names.variable = 2,
      time.plot = 1960:1990
    )
    
    # fit training model
    synth.out.placebo <- synth(
      data.prep.obj=dataprep.out.placebo,
      Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
    )
    
    # data prep for main model
    dataprep.out.placebo <- dataprep(
      foo = data,
      predictors = c("gdp","trade","infrate"),
      dependent = "gdp",
      unit.variable = 1,
      time.variable = 3,
      special.predictors = list(
        list("industry" ,1971:1975, c("mean")),
        list("schooling",c(1970,1975), c("mean")),
        list("invest70" ,1980, c("mean"))
      ),
      treatment.identifier = 7,
      controls.identifier = unique(data$index)[-7],
      time.predictors.prior = 1965:1975,
      time.optimize.ssr = 1960:1975,
      unit.names.variable = 2,
      time.plot = 1960:1990
    )
    
    # fit main model
    synth.out.placebo <- synth(
      data.prep.obj=dataprep.out.placebo,
      custom.v=as.numeric(synth.out.placebo$solution.v)
    )
    
    # Calculate y-axis limits for placebo plot - consistent for original and constant2015
    synthY0_placebo <- (dataprep.out.placebo$Y0%*%synth.out.placebo$solution.w)
    if (version_name == "original" || version_name == "constant2015") {
      ylim_fig4 <- c(0, 140)  # 0 to 120 with 20 padding above
      use_custom_axes <- TRUE
    } else if (version_name == "usa100") {
      ylim_fig4 <- c(60, 105)  # Upper limit 100 with padding, increments of 10
      use_custom_axes <- TRUE
    } else {
      ylim_fig4 <- calculate_ylim(list(dataprep.out.placebo$Y1plot, synthY0_placebo))
      use_custom_axes <- FALSE
    }
    
    pdf(file = paste0("placebo1975_", version_name, ".pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
    par(mar=c(5.1,4.1,1,2.1))  # Remove top margin but keep plot box same size
    plot(1960:1990,dataprep.out.placebo$Y1plot,
         type="l",ylim=ylim_fig4,col="black",lty="solid",
         ylab = y_label,
         xlab ="year",
         xaxs = "i", yaxs = "i",
         lwd=2, yaxt=if(use_custom_axes) "n" else "s"
    )
    if (use_custom_axes) {
      if (version_name == "usa100") {
        axis(2, at=seq(60, 100, 10), las=1)
      } else if (version_name == "original" || version_name == "constant2015") {
        axis(2, at=seq(0, 120, 20), las=1)  # Numbers every 20
      }
    }
    lines(1960:1990,synthY0_placebo,col="black",lty="dashed",lwd=2)
    abline(v=1975,lty="dotted")
    legend(x="bottomright",
           legend=c("West Germany","synthetic West Germany")
           ,lty=c("solid","dashed"),col=c("black","black")
           ,cex=.8,bg="white",lwd=c(2,2))
    arrows(1973,ylim_fig4[2]*0.6,1974.5,ylim_fig4[2]*0.6,col="black",length=.1)
    text(1967.5,ylim_fig4[2]*0.6,"placebo reunification",cex=1)
    dev.off()
    
    ### Figure 5: Ratio of post-reunification RMSPE to pre-reunification RMSPE: West Germany and control countries.
    
    # loop across control units
    storegaps <- matrix(NA, length(1960:2003), length(unique(data$index))-1)
    rownames(storegaps) <- 1960:2003
    i <- 1
    co <- unique(data$index)
    
    for(k in unique(data$index)[-7]){
      
      # data prep for training model
      dataprep.out.control <- dataprep(
        foo = data,
        predictors = c("gdp","trade","infrate"),
        dependent = "gdp",
        unit.variable = 1,
        time.variable = 3,
        special.predictors = list(
          list("industry",1971:1980, c("mean")),
          list("schooling",c(1970,1975), c("mean")),
          list("invest70" ,1980, c("mean"))
        ),
        treatment.identifier = k,
        controls.identifier = co[-which(co==k)],
        time.predictors.prior = 1971:1980,
        time.optimize.ssr = 1981:1990,
        unit.names.variable = 2,
        time.plot = 1960:2003
      )
      
      # fit training model
      synth.out.control <- synth(
        data.prep.obj=dataprep.out.control,
        Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
      )
      
      # data prep for main model
      dataprep.out.control <- dataprep(
        foo = data,
        predictors = c("gdp","trade","infrate"),
        dependent = "gdp",
        unit.variable = 1,
        time.variable = 3,
        special.predictors = list(
          list("industry" ,1981:1990, c("mean")),
          list("schooling",c(1980,1985), c("mean")),
          list("invest80" ,1980, c("mean"))
        ),
        treatment.identifier = k,
        controls.identifier = co[-which(co==k)],
        time.predictors.prior = 1981:1990,
        time.optimize.ssr = 1960:1989,
        unit.names.variable = 2,
        time.plot = 1960:2003
      )
      
      # fit main model
      synth.out.control <- synth(
        data.prep.obj=dataprep.out.control,
        custom.v=as.numeric(synth.out.control$solution.v)
      )
      
      storegaps[,i] <- dataprep.out.control$Y1-(dataprep.out.control$Y0%*%synth.out.control$solution.w)
      i <- i + 1
    } # close loop over control units
    
    data_ordered <- data[order(data$index,data$year),]
    colnames(storegaps) <- unique(data_ordered$country)[-7]
    storegaps <- cbind(gap,storegaps)
    colnames(storegaps)[1] <- c("West Germany")
    
    # compute ratio of post-reunification RMSPE to pre-reunification RMSPE                                                  
    rmse <- function(x){sqrt(mean(x^2))}
    preloss <- apply(storegaps[1:30,],2,rmse)
    postloss <- apply(storegaps[31:44,],2,rmse)
    
    pdf(paste0("ratio_post_to_preperiod_rmse_", version_name, ".pdf"), width = 7, height = 6.5)
    par(mar=c(5.1,4.1,1,2.1))  # Remove top margin but keep plot box same size
    dotchart(sort(postloss/preloss),
             xlab="Post-Period RMSE / Pre-Period RMSE",
             pch=19)
    dev.off()
    
    ### Figure 6: Leave-one-out distribution of the synthetic control for West Germany
    
    # loop over leave one outs
    storegaps_loo <- matrix(NA, length(1960:2003), 5)
    colnames(storegaps_loo) <- c(1,3,9,12,14)
    co <- unique(data$index)[-7]
    
    for(k in 1:5){
      
      # data prep for training model
      omit <- c(1,3,9,12,14)[k]  
      dataprep.out.loo <- dataprep(
        foo = data,
        predictors = c("gdp","trade","infrate"),
        dependent = "gdp",
        unit.variable = 1,
        time.variable = 3,
        special.predictors = list(
          list("industry",1971:1980, c("mean")),
          list("schooling",c(1970,1975), c("mean")),
          list("invest70" ,1980, c("mean"))
        ),
        treatment.identifier = 7,
        controls.identifier = co[-which(co==omit)],
        time.predictors.prior = 1971:1980,
        time.optimize.ssr = 1981:1990,
        unit.names.variable = 2,
        time.plot = 1960:2003
      )
      
      # fit training model
      synth.out.loo <- synth(
        data.prep.obj=dataprep.out.loo,
        Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
      )
      
      # data prep for main model
      dataprep.out.loo <- dataprep(
        foo = data,
        predictors = c("gdp","trade","infrate"),
        dependent = "gdp",
        unit.variable = 1,
        time.variable = 3,
        special.predictors = list(
          list("industry" ,1981:1990, c("mean")),
          list("schooling",c(1980,1985), c("mean")),
          list("invest80" ,1980, c("mean"))
        ),
        treatment.identifier = 7,
        controls.identifier = co[-which(co==omit)],
        time.predictors.prior = 1981:1990,
        time.optimize.ssr = 1960:1989,
        unit.names.variable = 2,
        time.plot = 1960:2003
      )
      
      # fit main model 
      synth.out.loo <- synth(
        data.prep.obj=dataprep.out.loo,
        custom.v=as.numeric(synth.out.loo$solution.v)
      )
      storegaps_loo[,k] <- (dataprep.out.loo$Y0%*%synth.out.loo$solution.w)
    } # close loop over leave one outs
    
    # Calculate y-axis limits for jackknife plot - consistent for original and constant2015
    all_loo_data <- as.vector(storegaps_loo)
    if (version_name == "original" || version_name == "constant2015") {
      ylim_fig6 <- c(0, 170)  # 0 to 150 with 20 padding above
      use_custom_axes <- TRUE
    } else if (version_name == "usa100") {
      ylim_fig6 <- c(60, 105)  # Upper limit 100 with padding, increments of 10
      use_custom_axes <- TRUE
    } else {
      ylim_fig6 <- calculate_ylim(list(dataprep.out$Y1plot, synthY0, all_loo_data))
      use_custom_axes <- FALSE
    }
    
    pdf(file = paste0("jackknife_", version_name, ".pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
    par(mar=c(5.1,4.1,1,2.1))  # Remove top margin but keep plot box same size
    plot(1960:2003,dataprep.out$Y1plot,
         type="l",ylim=ylim_fig6,col="black",lty="solid",
         ylab = y_label,
         xlab ="year",
         xaxs = "i", yaxs = "i",lwd=2, yaxt=if(use_custom_axes) "n" else "s"
    )
    if (use_custom_axes) {
      if (version_name == "usa100") {
        axis(2, at=seq(60, 100, 10), las=1)
      } else if (version_name == "original" || version_name == "constant2015") {
        axis(2, at=seq(0, 150, 50), las=1)  # Numbers every 50
      }
    }
    
    abline(v=1990,lty="dotted")
    arrows(1987,ylim_fig6[2]*0.7,1989,ylim_fig6[2]*0.7,col="black",length=.1)
    for(i in 1:5){
      lines(1960:2003,storegaps_loo[,i],col="darkgrey",lty="solid")
    }
    lines(1960:2003,synthY0,col="black",lty="dashed",lwd=2)
    lines(1960:2003,dataprep.out$Y1plot,col="black",lty="solid",lwd=2)
    text(1982.5,ylim_fig6[2]*0.7,"reunification",cex=.8)
    legend(x="bottomright",
           legend=c("West Germany",
                    "synthetic West Germany",
                    "synthetic West Germany (leave-one-out)")
           ,lty=c("solid","dashed","solid"),
           col=c("black","black","darkgrey")
           ,cex=.8,bg="white",lwd=c(2,2,1))
    dev.off()
    
    ### Figure 7: Per-Capita GDP Gaps Between West Germany and Sparse Synthetic Controls
    # Requires gtools and kernlab libraries
    if (!require(gtools, quietly = TRUE)) {
      cat("Warning: gtools package not available. Skipping sparse synthetic controls analysis.\n")
    } else if (!require(kernlab, quietly = TRUE)) {
      cat("Warning: kernlab package not available. Skipping sparse synthetic controls analysis.\n")
    } else {
      
      # Get W and v from main analysis
      solution.w <- round(synth.out$solution.w,3)
      V <- diag(as.numeric(synth.out$solution.v))
      
      # compute scaled Xs
      nvarsV <- dim(dataprep.out$X0)[1]
      big.dataframe <- cbind(dataprep.out$X0, dataprep.out$X1)
      divisor <- sqrt(apply(big.dataframe, 1, var))
      scaled.matrix <- t(t(big.dataframe) %*% ( 1/(divisor) * diag(rep(dim(big.dataframe)[1], 1)) ))
      X0.scaled <- scaled.matrix[,c(1:(dim(dataprep.out$X0)[2]))]
      X1.scaled <- as.matrix(scaled.matrix[,dim(scaled.matrix)[2]])
      
      dn <- data[data$year==1970,c("country","index")]
      dn <- dn[order(dn$index),]
      dn <- dn[-7,]
      
      table2store <- matrix(NA,nrow(dataprep.out$X1),4)
      fig7store <- matrix(NA,length(1960:2003),4)  
      table3 <- list()
      
      # loop through number of controls
      for(pp in 4:1){
        store <- combinations(length(unique(data$index)[-7]), r=pp, v=unique(data$index)[-7])
        store.loss <- matrix(NA,nrow=nrow(store),1)
        store.w <- matrix(NA,nrow=nrow(store),pp)
        store.c <- store.w
        
        # loop through combinations 
        for(k in 1:nrow(store)){
          # index positions of control units
          posvector <- c()
          for(i in 1:pp){
            posvector <- c(posvector,which(dn$index==store[k,i]))
          }
          
          # run quad optimization  
          X0temp <- X0.scaled[ , posvector ]
          H <- t(X0temp) %*% V %*% (X0temp)
          c <- -1*c(t(X1.scaled) %*% V %*% (X0temp) )
          
          if(pp == 1){
            solution.w.temp <- matrix(1)
          } else {      
            res <- ipop(c = c, H = H, A = t(rep(1, length(c))),
                        b = 1, l = rep(0, length(c)),
                        u = rep(1, length(c)), r = 0,
                        margin = 0.005,sigf = 7, bound = 6)
            solution.w.temp <- as.matrix(primal(res))
          }
          loss.w <- t(X1.scaled - X0temp %*% solution.w.temp) %*% V %*% (X1.scaled - X0temp %*% solution.w.temp)
          
          store.loss[k] <- loss.w
          store.w[k,] <- t(solution.w.temp)
          store.c[k,] <- dn$country[posvector]
        } # close loop over combinations
        
        # get best fitting combination
        dat <- data.frame(store.loss, store, store.c, store.w)
        colnames(dat) <- c("loss", paste("CNo",1:pp,sep=""), paste("CNa",1:pp,sep=""), paste("W",1:pp,sep=""))
        dat <- dat[order(dat$loss),]
        Countries <- dat[1,paste("CNo",1:pp,sep="")]
        Cweights <- as.numeric(dat[1,paste("W",1:pp,sep="")])
        
        outdat <- data.frame(unit.names=as.vector(
          (t(as.vector(dat[1,paste("CNa",1:pp,sep="")])))),
          w.weights=round(Cweights,2))
        
        table3[[pp]] <- outdat[order(-1*outdat$w.weights),]
        
        # get posvector for fitting
        posvector <- c()
        if(pp == 1 ){
          posvector <- c(posvector,which(dn$index==Countries))
        } else {
          for(i in 1:pp){
            posvector <- c(posvector,which(dn$index==Countries[1,i]))
          }
        }
        
        X0t <- as.matrix(dataprep.out$X0[,posvector])%*% as.matrix(Cweights)
        table2store[,(4:1)[pp]] <- X0t
        
        fig7store[,(4:1)[pp]] <- dataprep.out$Y0[,posvector]%*%as.matrix(Cweights)
        
      } # close loop over number of countries
      
      # Add full sample to table3
      synth.tables.sparse <- synth.tab(
        dataprep.res = dataprep.out,
        synth.res = synth.out
      )
      synth.tables.sparse$tab.w[,1] <- round(synth.tables.sparse$tab.w[,1],2)
      table3[[5]] <- synth.tables.sparse$tab.w[order(-1*synth.tables.sparse$tab.w[,1]),2:1][1:5,]
      
      # Save Table 3
      capture.output(table3, file = paste0("table3_", version_name, ".txt"))
      save(table3, file = paste0("table3_", version_name, ".RData"))
      
      # Table 4
      synth.tables.sparse$tab.pred[,3] <- c(8021.1,31.9,7.4,34.2,44.1,25.9)
      table4 <- round(
        cbind(synth.tables.sparse$tab.pred[,1:2],
              table2store,
              synth.tables.sparse$tab.pred[,3]),1)
      rownames(table4) <- c("GDP per-capita","Trade openness",
                            "Inflation rate","Industry share",
                            "Schooling","Investment rate")
      colnames(table4)[2:7] <- c(5:1,"OECD Sample")
      
      # Save Table 4
      write.csv(table4, paste0("table4_", version_name, ".csv"))
      
      # Calculate y-axis limits for sparse controls plot - consistent for original and constant2015
      all_sparse_data <- as.vector(fig7store)
      if (version_name == "original" || version_name == "constant2015") {
        ylim_fig7 <- c(0, 160)  # 0 to 150 with 10 padding above
        use_custom_axes <- TRUE
      } else if (version_name == "usa100") {
        ylim_fig7 <- c(60, 105)  # Upper limit 100 with padding, increments of 10
        use_custom_axes <- TRUE
      } else {
        ylim_fig7 <- calculate_ylim(list(dataprep.out$Y1, all_sparse_data))
        use_custom_axes <- FALSE
      }
      
      ## Figure 7: Per-Capita GDP Gaps Between West Germany and Sparse Synthetic Controls
      pdf(file = paste0("sparse_synthetic_controls_", version_name, ".pdf"), width = 11, height = 8.0, family = "Times", pointsize = 12)
      par(mfrow=c(2,2), mar=c(5.1,4.1,3,2.1))  # Remove top margin but keep space for subplot titles and same plot box size
      for(pp in 4:1){
        plot(1960:2003,dataprep.out$Y1,
             type="l",ylim=ylim_fig7,col="black",lty="solid",
             ylab = y_label,
             xlab ="year",
             xaxs = "i", yaxs = "i",
             lwd=2,
             main=paste("No. of control countries: ",pp,sep=""),
             yaxt=if(use_custom_axes) "n" else "s"
        )
        if (use_custom_axes) {
          if (version_name == "usa100") {
            axis(2, at=seq(60, 100, 10), las=1)
          } else if (version_name == "original" || version_name == "constant2015") {
            axis(2, at=seq(0, 150, 50), las=1)  # Numbers every 50
          }
        }
        lines(1960:2003,fig7store[,c(4:1)[pp]],col="black",lty="dashed",lwd=2)
        abline(v=1990,lty="dotted")
        legend(x="bottomright",
               legend=c("West Germany","synthetic West Germany")
               ,lty=c("solid","dashed"),col=c("black","black")
               ,cex=.8,bg="white",lwd=c(2,2))
        arrows(1987,ylim_fig7[2]*0.7,1989,ylim_fig7[2]*0.7,col="black",length=.1)
        text(1982.5,ylim_fig7[2]*0.7,"reunification",cex=.8)
      }
      dev.off()
    }
    
    cat("Analysis completed for:", version_name, "\n")
    
  }, error = function(e) {
    cat("Error in SCM analysis for", version_name, ":", e$message, "\n")
  }, finally = {
    setwd(old_dir)
  })
}

# Main execution function
main <- function() {
  # Create output directories
  create_directories()
  
  # Load original data
  cat("Loading original data...\n")
  d_original <- read.dta("Data/repgermany.dta")
  
  # 1. Run original analysis with normalization (West Germany 1990 = 100)
  cat("\n=== Running Original Analysis (West Germany 1990 = 100) ===\n")
  d_original_normalized <- normalize_gdp(d_original)
  
  # Save database for original normalized version
  save_database(d_original_normalized, "database_original_normalized.csv", 
                "Original data normalized (West Germany 1990 = 100)")
  
  run_scm_analysis(d_original_normalized, "Results/Original_normalized", "original")
  
  # 2. Run original analysis with USA = 100 each year
  cat("\n=== Running Original Analysis (USA = 100 each year) ===\n")
  d_original_usa100 <- normalize_gdp_usa_100_each_year(d_original)
  
  # Save database for USA = 100 each year version
  save_database(d_original_usa100, "database_original_usa100.csv", 
                "Original data normalized (USA = 100 each year)")
  
  run_scm_analysis(d_original_usa100, "Results/Original_USA_100", "usa100")
  
  # 3. Run WDI analyses with normalization (West Germany 1990 = 100)
  wdi_indicators <- list(
    list(code = "NY.GDP.PCAP.KD", name = "GDP per capita (constant 2015 US$)", 
         folder = "Results/WDI_constant", version = "constant2015")
  )
  
  for (indicator in wdi_indicators) {
    cat("\n=== Running WDI Analysis:", indicator$name, "===\n")
    
    tryCatch({
      # Download and prepare WDI data
      wdi_result <- prepare_wdi_data(indicator$code, indicator$name)
      
      # Merge with original data
      d_merged <- merge_wdi_with_original(d_original, wdi_result)
      
      # Normalize GDP 
      d_merged_normalized <- normalize_gdp(d_merged)
      
      # Save database for WDI version
      save_database(d_merged_normalized, "database_wdi_constant2015.csv", 
                    "WDI constant 2015 data normalized (West Germany 1990 = 100)")
      
      # Run SCM analysis
      run_scm_analysis(d_merged_normalized, indicator$folder, indicator$version)
      
    }, error = function(e) {
      cat("Failed to complete analysis for", indicator$name, ":", e$message, "\n")
    })
  }
  
  cat("\n=== All analyses completed ===\n")
  cat("Results saved in separate folders:\n")
  cat("- Results/Original_normalized/ (West Germany 1990 = 100)\n")
  cat("- Results/Original_USA_100/ (USA = 100 each year)\n")
  cat("- Results/WDI_constant/ (WDI constant 2015 data, West Germany 1990 = 100)\n")
  cat("\nDatabases saved in Data folder:\n")
  cat("- Data/database_original_normalized.csv\n")
  cat("- Data/database_original_usa100.csv\n")
  cat("- Data/database_wdi_constant2015.csv\n")
  cat("\nNote: Each folder contains all 15 output files from the SCM analysis\n")
}

# Run the main function
main()