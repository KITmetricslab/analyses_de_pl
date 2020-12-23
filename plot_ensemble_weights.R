# Generating plots showing ensemble weights (inverse-WIS) over time

# setwd("/home/johannes/Documents/Projects/intermediate_results/R")

library(here)
library(plotrix)

# set language to English (Unix systems)
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# source functions and colour definitions:
source("functions_paper.R")
source("define_colors.R")

# introdue densities to show KIT-baseline as shaded:
col_dens <- rep(NA, length(cols)); names(col_dens) <- names(cols)
col_dens[names(col_dens) != "KIT-baseline"] <- NA
col_dens[names(col_dens) == "KIT-baseline"] <- 20

# get inverse WIS ensemble weights
path_hub <- "/home/johannes/Documents/Projects/covid19-forecast-hub-de"
sub_path_weights <- "code/ensemble/inverse_wis_weights"

location <- "GM"
target <- "inc death"

# rn over locations and targets to plot:
for(location in c("GM", "PL")){
  for(target in c("inc case", "cum case", "inc death", "cum death")){
    files <- list.files(paste0(path_hub, "/", sub_path_weights))
    forecast_dates <- as.Date(gsub(".csv", "", gsub("inverse_wis_weights-", "", files)))
    
    # restrict to study period:
    first_forecast_date <- as.Date("2020-10-12")
    last_forecast_date <- as.Date("2020-12-14")
    files <- files[forecast_dates >= first_forecast_date & forecast_dates <= last_forecast_date]
    forecast_dates <- forecast_dates[forecast_dates >= first_forecast_date & forecast_dates <= last_forecast_date]
    
    
    wgts <- NULL
    for(i in seq_along(files)){
      wgts_temp <- read.csv(paste0(path_hub, "/", sub_path_weights, "/", files[i]))
      sub_temp <- wgts_temp[wgts_temp$location == location & wgts_temp$target == target, ]
      sub_temp$forecast_date <- forecast_dates[i]
      
      if(is.null(wgts)){
        wgts <- sub_temp
      }else{
        wgts <- rbind(wgts, sub_temp)
      }
    }
    
    wgts_wide <- reshape(wgts, direction = "wide", timevar = "forecast_date", idvar = c("model", "location", "target"))
    wgts_wide[is.na(wgts_wide)] <- 0
    
    matrix_to_plot <- as.matrix(wgts_wide[, grepl("inverse_wis", colnames(wgts_wide))])
    rownames(matrix_to_plot) <- wgts_wide$model
    
    pdf(paste0("../figures/inverse_wis_weights_", gsub(" ", "_", target), "_", location, ".pdf"),
        width = 8, height = 3)
    layout(matrix(1:2, ncol = 2), widths = c(3, 1))
    par(mar = c(4, 4, 2.5, 1))
    barplot(matrix_to_plot, col = cols[rownames(matrix_to_plot)], names.arg = forecast_dates,
            main = paste0("Inverse WIS weights, ", target, ", ", 
                          switch(location, "PL" = "Poland", "GM" = "Germany")), legend.text = FALSE,
            density = col_dens[rownames(matrix_to_plot)])
    par(mar = c(0, 0, 0, 0))
    plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
    legend("center", legend = (wgts_wide$model), col = cols[rownames(matrix_to_plot)],
           cex = 0.8, pch = ifelse(is.na(col_dens[rownames(matrix_to_plot)]), 15, 7), pt.cex = 1)
    dev.off()
  }
}

