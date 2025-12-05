# Set the working directory to the script's location
setwd(getSrcDirectory(function(dummy) {dummy}))

## Modified Replication Code for Multiple Datasets
# A. Abadie, A. Diamond, and J. Hainmueller. 2014.
# Comparative Politics and the Synthetic Control Method
# American Journal of Political Science.
# Updated to:
# 1. Process multiple datasets (original, WDI)
# 2. Use dynamic y-axis labels for all plots
# 3. Set y-axis to start at 0 for key plots
# 4. Enhanced error handling and robustness
# 5. Include pre and post RMSPEs in output

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
if (!require(gtools, quietly = TRUE)) {
  install.packages("gtools")
  library(gtools)
}
if (!require(kernlab, quietly = TRUE)) {
  install.packages("kernlab")
  library(kernlab)
}


# Function to run complete analysis for a single dataset
run_analysis <- function(data_file, output_dir) {
  
  cat(paste("Processing:", data_file, "-> Results:", output_dir, "\n"))
  
  # Create output directory
  if (!dir.exists(paste0("Results/", output_dir))) {
    dir.create(paste0("Results/", output_dir), recursive = TRUE)
  }
  
  # --- Dynamic Y-Axis Label Generation ---
  if (output_dir == "Replication") {
    y_axis_label <- "per-capita GDP (PPP, current USD)"
  } else if (startsWith(output_dir, "wdi_")) {
    year <- sub("wdi_", "", output_dir)
    y_axis_label <- paste0("per-capita GDP (PPP, ", year, " USD)")
  } else {
    y_axis_label <- "per-capita GDP (PPP, 2002 USD)" # Default fallback
  }
  gap_y_axis_label <- paste("gap in", y_axis_label)
  
  # Load DTA data
  d <- read.dta(paste0("Data/", data_file))
  
  ## Table 1 & 2, Figure 1, 2, & 3
  
  ## pick v by cross-validation
  # data setup for training model
  dataprep.out <-
    dataprep(
      foo = d,
      predictors    = c("gdp","trade","infrate"),
      dependent     = "gdp",
      unit.variable = 1,
      time.variable = 3,
      special.predictors = list(
        list("industry", 1971:1980, c("mean")),
        list("schooling",c(1970,1975), c("mean")),
        list("invest70" ,1980, c("mean"))
      ),
      treatment.identifier = 7,
      controls.identifier = unique(d$index)[-7],
      time.predictors.prior = 1971:1980,
      time.optimize.ssr = 1981:1990,
      unit.names.variable = 2,
      time.plot = 1960:2003
    )
  
  # fit training model
  synth.out <- 
    synth(
      data.prep.obj=dataprep.out,
      Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
    )
  
  # data prep for main model
  dataprep.out <-
    dataprep(
      foo = d,
      predictors    = c("gdp","trade","infrate"),
      dependent     = "gdp",
      unit.variable = 1,
      time.variable = 3,
      special.predictors = list(
        list("industry" ,1981:1990, c("mean")),
        list("schooling",c(1980,1985), c("mean")),
        list("invest80" ,1980, c("mean"))
      ),
      treatment.identifier = 7,
      controls.identifier = unique(d$index)[-7],
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
  ); synth.tables
  
  # Replace means for OECD sample (computed externally using proper pop weighting)
  synth.tables$tab.pred[,3]          <- c(8021.1,31.9,7.4,34.2,44.1,25.9)
  colnames(synth.tables$tab.pred)[3] <- "Rest of OECD Sample"
  rownames(synth.tables$tab.pred) <- c("GDP per-capita","Trade openness",
                                       "Inflation rate","Industry share",
                                       "Schooling","Investment rate")
  
  # Save Table 2
  table2_output <- xtable(round(synth.tables$tab.pred,1),digits=1)
  print(table2_output, file=paste0("Results/", output_dir, "/table2.txt"), type="latex")
  write.csv(round(synth.tables$tab.pred,1), paste0("Results/", output_dir, "/table2.csv"))
  
  #### Table 1
  tab1 <- data.frame(synth.tables$tab.w)
  tab1[,1] <- round(tab1[,1],2) 
  X0 <- cbind(1,t(dataprep.out$X0))
  X1 <- as.matrix(c(1,dataprep.out$X1))
  W     <- X0%*%solve(t(X0)%*%X0)%*%X1
  Wdat  <- data.frame(unit.numbers=as.numeric(rownames(X0)),
                      regression.w=round(W,2))
  tab1  <- merge(tab1,Wdat,by="unit.numbers")
  tab1  <- tab1[order(tab1[,3]),]
  
  table1_output <- xtable(cbind(tab1[1:9,c(3,2,4)],
                                tab1[10:18,c(3,2,4)]
  ))
  print(table1_output, file=paste0("Results/", output_dir, "/table1.txt"), type="latex")
  write.csv(tab1, paste0("Results/", output_dir, "/table1.csv"))
  
  #### Figure 1: Trends in Per-Capita GDP: West Germany vs. Rest of the OECD Sample
  oecd_mean <- aggregate(d[,c("gdp")],by=list(d$year),mean,na.rm=T)[,2]
  all_data_fig1 <- c(dataprep.out$Y1plot, oecd_mean)
  y_min_fig1 <- 0 
  y_max_fig1 <- max(all_data_fig1, na.rm=T) * 1.05
  text_height_fig1 <- y_max_fig1 * 0.7
  
  pdf(file = paste0("Results/", output_dir, "/ger_vs_oecd.pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
  par(mar=c(5.1,4.1,1,2.1))
  plot(1960:2003, dataprep.out$Y1plot,
       type="l", ylim=c(y_min_fig1, y_max_fig1), col="black", lty="solid",
       ylab = y_axis_label,
       xlab ="year",
       xaxs = "i", yaxs = "i",
       lwd=2)
  lines(1960:2003, oecd_mean, col="black", lty="dashed", lwd=2)
  abline(v=1990, lty="dotted")
  legend(x="bottomright",
         legend=c("West Germany","rest of the OECD sample"),
         lty=c("solid","dashed"), col=c("black","black"),
         cex=.8, bg="white", lwd=c(2,2))
  dev.off()
  
  #### Figure 2: Trends in Per-Capita GDP: West Germany vs. Synthetic West Germany
  synthY0 <- (dataprep.out$Y0%*%synth.out$solution.w)
  all_data_fig2 <- c(dataprep.out$Y1plot, as.vector(synthY0))
  y_min_fig2 <- 0
  y_max_fig2 <- max(all_data_fig2, na.rm=T) * 1.05
  text_height_fig2 <- y_max_fig2 * 0.7
  
  pdf(file = paste0("Results/", output_dir, "/ger_vs_synthger.pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
  par(mar=c(5.1,4.1,1,2.1))
  plot(1960:2003, dataprep.out$Y1plot,
       type="l", ylim=c(y_min_fig2, y_max_fig2), col="black", lty="solid",
       ylab = y_axis_label,
       xlab ="year",
       xaxs = "i", yaxs = "i",
       lwd=2)
  lines(1960:2003, synthY0, col="black", lty="dashed", lwd=2)
  abline(v=1990, lty="dotted")
  legend(x="bottomright",
         legend=c("West Germany","synthetic West Germany"),
         lty=c("solid","dashed"), col=c("black","black"),
         cex=.8, bg="white", lwd=c(2,2))
  dev.off()
  
  ### Figure 3: Per-Capita GDP Gap Between West Germany and Synthetic West Germany
  gap <- dataprep.out$Y1-(dataprep.out$Y0%*%synth.out$solution.w)
  gap_range <- max(abs(range(as.vector(gap), na.rm=T)))
  y_min_fig3 <- -gap_range * 1.1
  y_max_fig3 <- gap_range * 1.1
  text_height_fig3 <- y_max_fig3 * 0.25
  
  pdf(file = paste0("Results/", output_dir, "/ger_vs_synthger_gaps.pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
  par(mar=c(5.1,4.1,1,2.1))
  plot(1960:2003, gap,
       type="l", ylim=c(y_min_fig3, y_max_fig3), col="black", lty="solid",
       ylab = gap_y_axis_label,
       xlab ="year",
       xaxs = "i", yaxs = "i",
       lwd=2)
  abline(v=1990, lty="dotted")
  abline(h=0, lty="dotted")
  dev.off()
  
  gap_data <- data.frame(
    year = 1960:2003, 
    actual_west_germany = as.numeric(dataprep.out$Y1plot),
    synthetic_west_germany = as.numeric(synthY0),
    gap = as.numeric(gap)
  )
  write.csv(gap_data, paste0("Results/", output_dir, "/gap.csv"), row.names = FALSE)
  
  ### Figure 4: Placebo Reunification 1975
  tryCatch({
    dataprep_placebo_train <-
      dataprep(
        foo = d, predictors=c("gdp","trade","infrate"), dependent="gdp", unit.variable=1, time.variable=3,
        special.predictors = list(list("industry",1971,c("mean")), list("schooling",c(1960,1965),c("mean")), list("invest60" ,1980,c("mean"))),
        treatment.identifier=7, controls.identifier=unique(d$index)[-7], time.predictors.prior=1960:1964, time.optimize.ssr=1965:1975,
        unit.names.variable=2, time.plot=1960:1990
      )
    synth_placebo_train <- synth(data.prep.obj=dataprep_placebo_train, Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6)
    dataprep_placebo <-
      dataprep(
        foo = d, predictors=c("gdp","trade","infrate"), dependent="gdp", unit.variable=1, time.variable=3,
        special.predictors = list(list("industry" ,1971:1975,c("mean")), list("schooling",c(1970,1975),c("mean")), list("invest70" ,1980,c("mean"))),
        treatment.identifier=7, controls.identifier=unique(d$index)[-7], time.predictors.prior=1965:1975, time.optimize.ssr=1960:1975,
        unit.names.variable=2, time.plot=1960:1990
      )
    synth_placebo <- synth(data.prep.obj=dataprep_placebo, custom.v=as.numeric(synth_placebo_train$solution.v))
    
    synth_placebo_Y0 <- (dataprep_placebo$Y0%*%synth_placebo$solution.w)
    all_data_placebo <- c(dataprep_placebo$Y1plot, as.vector(synth_placebo_Y0))
    y_min_placebo <- 0
    y_max_placebo <- max(all_data_placebo, na.rm=T) * 1.05
    text_height_placebo <- y_max_placebo * 0.6
    
    pdf(file = paste0("Results/", output_dir, "/placebo1975.pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
    par(mar=c(5.1,4.1,1,2.1))
    plot(1960:1990, dataprep_placebo$Y1plot,
         type="l", ylim=c(y_min_placebo, y_max_placebo), col="black", lty="solid",
         ylab = y_axis_label,
         xlab ="year",
         xaxs = "i", yaxs = "i",
         lwd=2)
    lines(1960:1990, synth_placebo_Y0, col="black", lty="dashed", lwd=2)
    abline(v=1975, lty="dotted")
    legend(x="bottomright", legend=c("West Germany","synthetic West Germany"), lty=c("solid","dashed"), col=c("black","black"), cex=.8, bg="white", lwd=c(2,2))
    dev.off()
  }, error = function(e) {
    cat(paste("Warning: Placebo 1975 analysis failed for", output_dir, ":", e$message, "\n"))
    writeLines("Placebo 1975 analysis failed", paste0("Results/", output_dir, "/placebo1975_failed.txt"))
  })
  
  ### Figure 5: Ratio of post-reunification RMSPE to pre-reunification RMSPE
  tryCatch({
    dataprep.out <-
      dataprep(
        foo = d, predictors=c("gdp","trade","infrate"), dependent="gdp", unit.variable=1, time.variable=3,
        special.predictors = list(list("industry" ,1981:1990,c("mean")), list("schooling",c(1980,1985),c("mean")), list("invest80" ,1980,c("mean"))),
        treatment.identifier=7, controls.identifier=unique(d$index)[-7], time.predictors.prior=1981:1990, time.optimize.ssr=1960:1989,
        unit.names.variable=2, time.plot=1960:2003
      )
    
    control_units <- unique(d$index)[-7]
    n_controls <- length(control_units)
    storegaps <- matrix(NA, length(1960:2003), n_controls)
    rownames(storegaps) <- 1960:2003
    
    i <- 1
    co <- unique(d$index)
    failed_units <- c()
    successful_units <- c()
    
    for(k in control_units){
      tryCatch({
        dataprep_control_train <- dataprep(
          foo=d, predictors=c("gdp","trade","infrate"), dependent="gdp", unit.variable=1, time.variable=3,
          special.predictors=list(list("industry",1971:1980,c("mean")), list("schooling",c(1970,1975),c("mean")), list("invest70",1980,c("mean"))),
          treatment.identifier=k, controls.identifier=co[-which(co==k)], time.predictors.prior=1971:1980, time.optimize.ssr=1981:1990,
          unit.names.variable=2, time.plot=1960:2003
        )
        synth_control_train <- synth(data.prep.obj=dataprep_control_train, Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6)
        dataprep_control <- dataprep(
          foo=d, predictors=c("gdp","trade","infrate"), dependent="gdp", unit.variable=1, time.variable=3,
          special.predictors=list(list("industry",1981:1990,c("mean")), list("schooling",c(1980,1985),c("mean")), list("invest80",1980,c("mean"))),
          treatment.identifier=k, controls.identifier=co[-which(co==k)], time.predictors.prior=1981:1990, time.optimize.ssr=1960:1989,
          unit.names.variable=2, time.plot=1960:2003
        )
        synth_control <- synth(data.prep.obj=dataprep_control, custom.v=as.numeric(synth_control_train$solution.v))
        storegaps[,i] <- dataprep_control$Y1 - (dataprep_control$Y0%*%synth_control$solution.w)
        successful_units <- c(successful_units, k)
        i <- i + 1
      }, error = function(e) {
        cat(paste("Warning: Error processing control unit", k, ":", e$message, "\n"))
        failed_units <- c(failed_units, k)
      })
    } 
    
    n_successful <- length(successful_units)
    if (n_successful == 0) {
      stop("No control units could be processed successfully")
    }
    
    storegaps <- storegaps[, 1:n_successful, drop=FALSE]
    
    d_sorted <- d[order(d$index,d$year),]
    country_mapping <- unique(d_sorted[,c("index", "country")])
    successful_countries <- country_mapping[country_mapping$index %in% successful_units, "country"]
    colnames(storegaps) <- successful_countries
    
    # Main West Germany analysis
    dataprep_main_train <- dataprep(
      foo=d, predictors=c("gdp","trade","infrate"), dependent="gdp", unit.variable=1, time.variable=3,
      special.predictors=list(list("industry",1971:1980,c("mean")), list("schooling",c(1970,1975),c("mean")), list("invest70",1980,c("mean"))),
      treatment.identifier=7, controls.identifier=unique(d$index)[-7], time.predictors.prior=1971:1980, time.optimize.ssr=1981:1990,
      unit.names.variable=2, time.plot=1960:2003
    )
    synth_main_train <- synth(data.prep.obj=dataprep_main_train, Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6)
    dataprep_main <- dataprep(
      foo=d, predictors=c("gdp","trade","infrate"), dependent="gdp", unit.variable=1, time.variable=3,
      special.predictors=list(list("industry",1981:1990,c("mean")), list("schooling",c(1980,1985),c("mean")), list("invest80",1980,c("mean"))),
      treatment.identifier=7, controls.identifier=unique(d$index)[-7], time.predictors.prior=1981:1990, time.optimize.ssr=1960:1989,
      unit.names.variable=2, time.plot=1960:2003
    )
    synth_main <- synth(data.prep.obj=dataprep_main, custom.v=as.numeric(synth_main_train$solution.v))
    
    gap_main <- dataprep_main$Y1-(dataprep_main$Y0%*%synth_main$solution.w)
    storegaps <- cbind(gap_main, storegaps)
    colnames(storegaps)[1] <- c("West Germany")
    
    rmse <- function(x){sqrt(mean(x^2, na.rm=T))}
    preloss <- apply(storegaps[1:30,], 2, rmse)
    postloss <- apply(storegaps[31:44,], 2, rmse)
    ratio_values <- postloss/preloss
    
    # Create comprehensive data frame with all RMSPE information
    rmspe_data <- data.frame(
      country = names(ratio_values),
      pre_rmspe = preloss,
      post_rmspe = postloss,
      ratio = ratio_values,
      stringsAsFactors = FALSE
    )
    
    # Filter out invalid ratios and sort
    valid_rows <- !is.infinite(rmspe_data$ratio) & !is.na(rmspe_data$ratio)
    rmspe_data <- rmspe_data[valid_rows, ]
    rmspe_data <- rmspe_data[order(rmspe_data$ratio), ]
    
    # Save comprehensive RMSPE data
    write.csv(rmspe_data, paste0("Results/", output_dir, "/rmspe_ratios.csv"), row.names = FALSE)
    
    # Extract sorted ratio values for plotting (maintain existing plot functionality)
    ratio_values <- rmspe_data$ratio
    names(ratio_values) <- rmspe_data$country
    
    if (length(ratio_values) > 0) {
      pdf(paste0("Results/", output_dir, "/ratio_post_to_preperiod_rmse.pdf"), width = 7, height = 6.5, family = "Times", pointsize = 12)
      par(mar=c(5.1,4.1,1,2.1))
      dotchart(ratio_values, xlim=c(0, max(ratio_values, na.rm=T)*1.1), xlab="Post-Period RMSE / Pre-Period RMSE", pch=19)
      dev.off()
    } else {
      cat(paste("Warning: No valid RMSPE ratios calculated for", output_dir, "\n"))
    }
    
  }, error = function(e) {
    cat(paste("Warning: RMSPE analysis failed for", output_dir, ":", e$message, "\n"))
    writeLines("RMSPE analysis failed", paste0("Results/", output_dir, "/rmspe_failed.txt"))
  })
  
  ### Figure 6: Leave-one-out distribution 
  tryCatch({
    storegaps_jackknife <- matrix(NA, length(1960:2003), 5)
    colnames(storegaps_jackknife) <- c(1,3,9,12,14)
    co <- unique(d$index)[-7]
    
    for(k in 1:5){
      tryCatch({
        omit <- c(1,3,9,12,14)[k]  
        dataprep_jack_train <- dataprep(
          foo=d, predictors=c("gdp","trade","infrate"), dependent="gdp", unit.variable=1, time.variable=3,
          special.predictors=list(list("industry",1971:1980,c("mean")), list("schooling",c(1970,1975),c("mean")), list("invest70",1980,c("mean"))),
          treatment.identifier=7, controls.identifier=co[-which(co==omit)], time.predictors.prior=1971:1980, time.optimize.ssr=1981:1990,
          unit.names.variable=2, time.plot=1960:2003
        )
        synth_jack_train <- synth(data.prep.obj=dataprep_jack_train, Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6)
        dataprep_jack <- dataprep(
          foo=d, predictors=c("gdp","trade","infrate"), dependent="gdp", unit.variable=1, time.variable=3,
          special.predictors=list(list("industry",1981:1990,c("mean")), list("schooling",c(1980,1985),c("mean")), list("invest80",1980,c("mean"))),
          treatment.identifier=7, controls.identifier=co[-which(co==omit)], time.predictors.prior=1981:1990, time.optimize.ssr=1960:1989,
          unit.names.variable=2, time.plot=1960:2003
        )
        synth_jack <- synth(data.prep.obj=dataprep_jack, custom.v=as.numeric(synth_jack_train$solution.v))
        storegaps_jackknife[,k] <- (dataprep_jack$Y0%*%synth_jack$solution.w)
      }, error = function(e) {
        cat(paste("Warning: Jackknife", k, "failed for", output_dir, ":", e$message, "\n"))
      })
    }
    
    if (any(!is.na(storegaps_jackknife))) {
      synthY0_main <- (dataprep_main$Y0%*%synth_main$solution.w)
      all_jackknife_data <- c(dataprep_main$Y1plot, as.vector(synthY0_main), as.vector(storegaps_jackknife))
      y_min_jackknife <- 0
      y_max_jackknife <- max(all_jackknife_data, na.rm=T) * 1.05
      text_height_jackknife <- y_max_jackknife * 0.7
      
      pdf(file = paste0("Results/", output_dir, "/jackknife.pdf"), width = 5.5, height = 5.0, family = "Times", pointsize = 12)
      par(mar=c(5.1,4.1,1,2.1))
      plot(1960:2003, dataprep_main$Y1plot,
           type="l", ylim=c(y_min_jackknife, y_max_jackknife), col="black", lty="solid",
           ylab = y_axis_label,
           xlab ="year",
           xaxs = "i", yaxs = "i", lwd=2)
      
      abline(v=1990, lty="dotted")
      for(i in 1:5){ 
        if (!all(is.na(storegaps_jackknife[,i]))) {
          lines(1960:2003, storegaps_jackknife[,i], col="darkgrey", lty="solid") 
        }
      }
      lines(1960:2003, synthY0_main, col="black", lty="dashed", lwd=2)
      lines(1960:2003, dataprep_main$Y1plot, col="black", lty="solid", lwd=2)
      legend(x="bottomright", legend=c("West Germany", "synthetic West Germany", "synthetic West Germany (leave-one-out)"),
             lty=c("solid","dashed","solid"), col=c("black","black","darkgrey"), cex=.8, bg="white", lwd=c(2,2,1))
      dev.off()
    } else {
      cat(paste("Warning: All jackknife analyses failed for", output_dir, "\n"))
    }
    
  }, error = function(e) {
    cat(paste("Warning: Jackknife analysis failed for", output_dir, ":", e$message, "\n"))
    writeLines("Jackknife analysis failed", paste0("Results/", output_dir, "/jackknife_failed.txt"))
  })
  
  ### Table 3 & 4, Figure 7: Sparse SCM
  tryCatch({
    synth.tables <- synth.tab(dataprep.res=dataprep_main, synth.res=synth_main)
    table3 <- list()
    synth.tables$tab.w[,1] <- round(synth.tables$tab.w[,1],2)
    table3[[5]] <-synth.tables$tab.w[order(-1*synth.tables$tab.w[,1]),2:1][1:5,]
    
    V <- diag(as.numeric(synth_main$solution.v))
    big.dataframe <- cbind(dataprep_main$X0, dataprep_main$X1)
    divisor <- sqrt(apply(big.dataframe, 1, var))
    scaled.matrix <- t(t(big.dataframe) %*% (1/(divisor) * diag(rep(dim(big.dataframe)[1], 1))))
    X0.scaled <- scaled.matrix[,1:(dim(dataprep_main$X0)[2])]
    X1.scaled <- as.matrix(scaled.matrix[,dim(scaled.matrix)[2]])
    dn <- d[d$year==1970,c("country","index")]
    dn <- dn[order(dn$index),][-7,]
    
    table2store <- matrix(NA,nrow(dataprep_main$X1),4)
    fig7store   <- matrix(NA,length(1960:2003),4)  
    
    for(pp in 4:1){
      store <- combinations(length(unique(d$index)[-7]), r=pp, v=unique(d$index)[-7])
      store.loss <- matrix(NA,nrow=nrow(store),1)
      store.w <- matrix(NA,nrow=nrow(store),pp)
      
      for(k in 1:nrow(store)){
        posvector <- which(dn$index %in% store[k,])
        X0temp <- X0.scaled[, posvector]
        H <- t(X0temp) %*% V %*% (X0temp)
        c_val <- -1*c(t(X1.scaled) %*% V %*% (X0temp))
        
        if(pp==1){ solution.w <- matrix(1) } 
        else { res <- ipop(c=c_val, H=H, A=t(rep(1,pp)), b=1, l=rep(0,pp), u=rep(1,pp), r=0); solution.w <- as.matrix(primal(res)) }
        
        store.loss[k] <- t(X1.scaled - X0temp %*% solution.w) %*% V %*% (X1.scaled - X0temp %*% solution.w)
        store.w[k,] <- t(solution.w)
      }
      
      best_fit_idx <- which.min(store.loss)
      Countries <- dn$country[which(dn$index %in% store[best_fit_idx,])]
      Cweights <- as.numeric(store.w[best_fit_idx,])
      table3[[pp]] <- data.frame(unit.names=Countries, w.weights=round(Cweights,2))[order(-Cweights),]
      
      posvector <- which(dn$index %in% store[best_fit_idx,])
      table2store[,(4:1)[pp]] <- as.matrix(dataprep_main$X0[,posvector]) %*% as.matrix(Cweights)
      fig7store[,(4:1)[pp]] <- dataprep_main$Y0[,posvector] %*% as.matrix(Cweights)
    }
    
    capture.output(table3, file = paste0("Results/", output_dir, "/table3.txt"))
    
    synth.tables$tab.pred[,3] <- c(8021.1,31.9,7.4,34.2,44.1,25.9)
    table4 <- round(cbind(synth.tables$tab.pred[,1:2], table2store, synth.tables$tab.pred[,3]), 1)
    rownames(table4) <- c("GDP per-capita","Trade openness", "Inflation rate","Industry share", "Schooling","Investment rate")
    colnames(table4)[2:7] <- c(5:1,"OECD Sample")
    write.csv(table4, paste0("Results/", output_dir, "/table4.csv"))
    
    all_sparse_data <- c(dataprep_main$Y1, as.vector(fig7store))
    y_max_sparse <- max(all_sparse_data, na.rm=T) * 1.05
    text_height_sparse <- y_max_sparse * 0.7
    
    pdf(file = paste0("Results/", output_dir, "/sparse_synthetic_controls.pdf"), width = 11, height = 8.0, family = "Times", pointsize = 12)
    par(mfrow=c(2,2), mar=c(5.1,4.1,3,2.1))
    for(pp in 4:1){
      plot(1960:2003, dataprep_main$Y1, type="l", ylim=c(0, y_max_sparse), col="black", lty="solid",
           ylab = y_axis_label, xlab="year", xaxs="i", yaxs="i", lwd=2, main=paste("No. of control countries:", pp))
      lines(1960:2003, fig7store[,c(4:1)[pp]], col="black", lty="dashed", lwd=2)
      abline(v=1990, lty="dotted")
      legend(x="bottomright", legend=c("West Germany","synthetic West Germany"), lty=c("solid","dashed"), col=c("black","black"), cex=.8, bg="white", lwd=c(2,2))
    }
    dev.off()
    
  }, error = function(e) {
    cat(paste("Warning: Sparse SCM analysis failed for", output_dir, ":", e$message, "\n"))
    writeLines("Sparse SCM analysis failed", paste0("Results/", output_dir, "/sparse_scm_failed.txt"))
  })
  
  cat(paste("Completed analysis for:", data_file, "\n"))
}

# --- Main Execution Block ---

# Original analysis sets (Replication and WDI)
datasets_orig <- c("repgermany.dta")
output_dirs_orig <- c("Replication")
for (year in 1960:2003) {
  datasets_orig <- c(datasets_orig, paste0("repgermany_wdi", year, ".dta"))
  output_dirs_orig <- c(output_dirs_orig, paste0("wdi_", year))
}

for (i in 1:length(datasets_orig)) {
  tryCatch({
    run_analysis(datasets_orig[i], output_dirs_orig[i])
  }, error = function(e) {
    cat(paste("Error processing", datasets_orig[i], ":", e$message, "\n"))
  })
}

cat("-------------------------------------------\n")
cat("Original and WDI analysis completed!\n")
cat("Results saved in the 'Results/' directory.\n")
cat("-------------------------------------------\n")