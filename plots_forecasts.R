# Generate plots of forecasts issued by teams and ensembles

# setwd("/home/johannes/Documents/Projects/intermediate_results/R")

library(here)
library(plotrix)
library(colorspace)

# set language to English (Unix systems)
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# source functions and colour definitions:
source("functions_paper.R")
source("define_colors.R")
source("define_models.R")

# get evaluation + truth data and plotting functions from the Hub:
path_hub <- "/home/johannes/Documents/Projects/covid19-forecast-hub-de"
source(paste0(path_hub, "/code/R/plot_functions.R"))

dat_eval <- read.csv(paste0(path_hub, "/evaluation/evaluation-ECDC.csv"),
                     colClasses = list("target_end_date" = "Date", "forecast_date" = "Date", "timezero" = "Date"),
                     stringsAsFactors = FALSE)
dat_eval <- subset(dat_eval, timezero >= as.Date("2020-10-12") & target_end_date <= as.Date("2020-12-19"))

dat_truth <- read.csv(paste0(path_hub, "/app_forecasts_de/data/truth_to_plot_ecdc.csv"),
                      colClasses = list(date = "Date"))
dat_truth_gm <- subset(dat_truth, location == "GM")
dat_truth_pl <- subset(dat_truth, location == "PL")


# define shifts for bands of multple foreasts in the same plot:
shifts <- c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)
x_start <- as.Date("2020-10-12")
x_end <- as.Date("2020-12-30")
# suppress scientific notation for plot labels:
options(scipen = 1000)


par(las = 1)

# separate plots for "main" and "other" models (main manuscript and appendix)
for(model_cat in c("main", "others")){
  # run over horizons
  for(h in 1:4){
    # needed to plot coverage:
    dat_eval_list <- list(ECDC = dat_eval)
    
    pdf(paste0("../figures/plot_forecasts_", h, "wk_", model_cat,".pdf"), width = 8, height = 10)
    par(mfrow = c(4, 1))
    
    layout(matrix(1:8, ncol = 2, byrow = TRUE), widths = c(4, 1))
    
    # cases, DE
    plot_from_eval(dat_eval = dat_eval,
                   model = models_gm[[model_cat]],
                   location = "GM", target_type = "case",
                   inc_or_cum = "inc", horizon = h, 
                   width = 0.4, col = cols[models_gm[[model_cat]]],
                   pch = pchs[models_gm[[model_cat]]],
                   start = x_start, end = x_end, 
                   shifts = shifts, legend = TRUE, ylim = c(0, 300000))
    legend("topright", col = c("darkgrey", "lightgrey"), legend = c("50% PI", "95% PI"), pch = 15, bty = "n")
    legend("bottomright", col = "black", lty = 1, pch = 15, legend = "observed\n (ECDC)", cex = 0.9, bty = "n")
    lines(dat_truth_gm$date, dat_truth_gm$inc_case, type = "o", pch = 15)
    title(paste("Weekly incident cases, Germany,", h, "wk ahead"))
    
    plot_scores(scores = dat_eval_list,
                target = "inc case",
                horizon = paste(h, "wk ahead"),
                selected_truth = "ECDC", models = models_gm[[model_cat]],
                location = "GM",
                start = as.Date("2020-10-12"),
                end = as.Date("2020-12-19"),
                cols = cols[models_gm[[model_cat]]], display = "coverage", shift.coverage = 0)
    # legend("topleft", col = c("black", "grey"), legend = c("coverage 50% PI", "coverage 95% PI"), lwd = 4, bty = "n")
    
    # coverage:
    plot_from_eval(dat_eval = dat_eval,
                   model = models_gm[[model_cat]],
                   location = "GM", target_type = "death",
                   inc_or_cum = "inc", horizon = h, 
                   width = 0.4, col = cols[models_gm[[model_cat]]], pch = pchs[models_gm[[model_cat]]],
                   start = x_start, end = x_end, 
                   shifts = shifts, legend = FALSE, ylim = c(0, 5000))
    title(paste("Weekly incident deaths, Germany,", h, "wk ahead"))
    lines(dat_truth_gm$date, dat_truth_gm$inc_death, type = "o", pch = 15)
    
    # deaths, DE:
    plot_scores(scores = dat_eval_list,
                target = "inc death",
                horizon = paste(h, "wk ahead"),
                selected_truth = "ECDC", models = models_gm[[model_cat]],
                location = "GM",
                start = as.Date("2020-10-12"),
                end = as.Date("2020-12-19"),
                cols = cols[models_gm[[model_cat]]], display = "coverage", shift.coverage = 0)
    
    plot_from_eval(dat_eval = dat_eval,
                   model = models_pl[[model_cat]],
                   location = "PL", target_type = "case",
                   inc_or_cum = "inc", horizon = h, 
                   width = 0.4, col = cols[models_pl[[model_cat]]], pch = pchs[models_pl[[model_cat]]],
                   start = x_start, end = x_end, 
                   shifts = shifts, legend = TRUE, ylim = c(0, 400000))
    title(paste("Weekly incident cases, Poland,", h, "wk ahead"))
    lines(dat_truth_pl$date, dat_truth_pl$inc_case, type = "o", pch = 15)
    
    # coverage
    plot_scores(scores = dat_eval_list,
                target = "inc case",
                horizon = paste(h, "wk ahead"),
                selected_truth = "ECDC", models = models_pl[[model_cat]],
                location = "PL",
                start = as.Date("2020-10-12"),
                end = as.Date("2020-12-19"),
                cols = cols[models_pl[[model_cat]]], display = "coverage", shift.coverage = 0)
    
    # deaths, PL
    plot_from_eval(dat_eval = dat_eval,
                   model = models_pl[[model_cat]],
                   location = "PL", target_type = "death",
                   inc_or_cum = "inc", horizon = h, 
                   width = 0.4, col = cols[models_pl[[model_cat]]], pch = pchs[models_pl[[model_cat]]],
                   start = x_start, end = x_end, 
                   shifts = shifts, legend = FALSE, ylim = c(0, 6000))
    title(paste("Weekly incident deaths, Poland,", h, "wk ahead"))
    lines(dat_truth_pl$date, dat_truth_pl$inc_death, type = "o", pch = 15)
    
    # coverage:
    plot_scores(scores = dat_eval_list,
                target = "inc death",
                horizon = paste(h, "wk ahead"),
                selected_truth = "ECDC", models = models_pl[[model_cat]],
                location = "PL",
                start = as.Date("2020-10-12"),
                end = as.Date("2020-12-19"),
                cols = cols[models_pl[[model_cat]]], display = "coverage", shift.coverage = 0)
    # legend("topleft", col = c("black", "grey"), legend = c("coverage 50% PI", "coverage 95% PI"), lwd = 4, bty = "n")
    dev.off()
  }
}

# Plot to exemplify forecast heterogeneity for cases in DE:
pdf(paste0("../figures/plot_point_forecasts_gm.pdf"), width = 12, height = 4)
par(mfrow = 1:2, las = 1)

models_gm_long <- c(models_gm$main,
                    "LeipzigIMISE-SECIR", "SDSC_ISG-TrendModel",
                    "UCLA-SuEIR", "USC-SIkJalpha")
models_gm_long <- models_gm_long[models_gm_long != "LANL-GrowthRate"] # LANL not available

plot_from_eval(dat_eval = dat_eval,
               model = models_gm_long,
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2020-10-19",
               width = 0.4, col = cols[models_gm_long], pch = pchs[models_gm_long],
               start = as.Date("2020-09-14"), end = as.Date("2020-12-19"), 
               shifts = seq(from = 0, to = 0, length.out = length(models_gm_long)),
               legend = FALSE, 
               ylim = c(0, 420000), show_intervals = FALSE)

letter_in_circle(as.Date("2020-11-02"), 400000, "a") # DE: semi-lockdown
letter_in_circle(as.Date("2020-11-03"), 360000, "b") # DE: new test strategy
letter_in_circle(as.Date("2020-12-01"), 400000, "c") # DE: reinforcement of contact restrictions https://www.tagesschau.de/inland/corona-plan-bundeslaender-beschluss-103.html
letter_in_circle(as.Date("2020-12-16"), 400000, "d") # DE: full lockdown (incl school closure)

lines(dat_truth_gm$date, dat_truth_gm$inc_case, type = "o", pch = 15)
legend("topleft", col = cols[models_gm_long], pch = pchs[models_gm_long],
       legend = models_gm_long, bty = "n", cex = 0.75)

title(paste("Incident cases, Germany, forecasts from 2020-10-19"))
legend("bottomright", col = "black", lty = 1, pch = 15, legend = "observed\n (ECDC)", cex = 0.9, bty = "n")


plot_from_eval(dat_eval = dat_eval,
               model = models_gm_long,
               location = "GM", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2020-11-09",
               width = 0.4, col = cols[models_gm_long], pch = pchs[models_gm_long],
               start = as.Date("2020-09-14"), end = as.Date("2020-12-19"), 
               shifts = seq(from = 0, to = 0, length.out = length(models_gm_long)),
               legend = FALSE, 
               ylim = c(0, 420000), show_intervals = FALSE)
lines(dat_truth_gm$date, dat_truth_gm$inc_case, type = "o", pch = 15)
title(paste("Incident cases, Germany, forecasts from 2020-11-09"))

letter_in_circle(as.Date("2020-11-02"), 400000, "a") # DE: semi-lockdown
letter_in_circle(as.Date("2020-11-03"), 360000, "b") # DE: new test strategy
letter_in_circle(as.Date("2020-12-01"), 400000, "c") # DE: reinforcement of contact restrictions https://www.tagesschau.de/inland/corona-plan-bundeslaender-beschluss-103.html
letter_in_circle(as.Date("2020-12-16"), 400000, "d") # DE: full lockdown (incl school closure)


dev.off()

# Plot showing ensemble mechanisms for Poland:
pdf(paste0("../figures/plot_ensembles_pl.pdf"), width = 9, height = 3.3)
par(mfrow = c(1, 4), las = 1)
dat_eval_2wk <- subset(dat_eval, grepl("1 wk", target) | grepl("2 wk", target))

models_pl_ensemble2 <- c("MIT_CovidAnalytics-DELPHI",
                         "epiforecasts-EpiExpert",
                         "epiforecasts-EpiNow2",
                         "ICM-agentModel", 
                         "LANL-GrowthRate",
                         "MOCOS-agent1")

plot_from_eval(dat_eval = dat_eval_2wk,
               model = models_pl_ensemble2,
               location = "PL", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2020-11-30",
               width = 0.4, col = cols[models_pl_ensemble2], pch = pchs[models_pl_ensemble2],
               start = as.Date("2020-11-19"), end = as.Date("2020-12-14"), 
               shifts = seq(from = -1.5, to = 1.5, length.out = length(models_pl_ensemble2)),
               legend = TRUE,
               ylim = c(0, 11000), show_intervals = TRUE, separate_intervals = TRUE)
mtext(side = 3, "Incident deaths, Poland, forecasts issued\n on 30 Nov 2020", 
      at = as.Date("2020-12-20"), line = 1, font = 2)
legend("bottomright", col = "black", lty = 1, pch = 15, legend = "observed\n (ECDC)", cex = 0.9, bty = "n")


plot_from_eval(dat_eval = dat_eval_2wk,
               model = c("KITCOVIDhub-median_ensemble",
                         "KITCOVIDhub-mean_ensemble"),
               location = "PL", target_type = "death",
               inc_or_cum = "inc", forecast_date = "2020-11-30",
               width = 0.4,
               col = cols[c("KITCOVIDhub-median_ensemble",
                                         "KITCOVIDhub-mean_ensemble")],
               pch = pchs[c("KITCOVIDhub-median_ensemble",
                            "KITCOVIDhub-mean_ensemble")],
               start = as.Date("2020-11-19"), end = as.Date("2020-12-14"), 
               shifts = c(-0.5, 0.5),
               legend = TRUE, 
               ylim = c(0, 11000), show_intervals = TRUE, separate_intervals = TRUE)


models_pl_ensemble <- c("MIT_CovidAnalytics-DELPHI",
                        "epiforecasts-EpiExpert",
                        "epiforecasts-EpiNow2",
                        "ITWW-county_repro", 
                        "MOCOS-agent1")

plot_from_eval(dat_eval = dat_eval_2wk,
               model = models_pl_ensemble,
               location = "PL", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2020-11-02",
               width = 0.4, col = cols[models_pl_ensemble], pch = pchs[models_pl_ensemble],
               start = as.Date("2020-10-23"), end = as.Date("2020-11-18"), 
               shifts = seq(from = -1.5, to = 1.5, length.out = length(models_pl_ensemble)),
               legend = TRUE,
               ylim = c(0, 360000), show_intervals = TRUE, separate_intervals = TRUE)
mtext(side = 3, "Incident cases, Poland, forecasts issued\n on 02 Nov 2020", 
      at = as.Date("2020-11-26"), line = 1, font = 2)

plot_from_eval(dat_eval = dat_eval_2wk,
               model = c("KITCOVIDhub-median_ensemble",
                         "KITCOVIDhub-mean_ensemble"),
               location = "PL", target_type = "case",
               inc_or_cum = "inc", forecast_date = "2020-11-02",
               width = 0.4, col = cols[c("KITCOVIDhub-median_ensemble",
                                         "KITCOVIDhub-mean_ensemble")],
               pch = pchs[c("KITCOVIDhub-median_ensemble",
                            "KITCOVIDhub-mean_ensemble")],
               start = as.Date("2020-10-23"), end = as.Date("2020-11-18"), 
               shifts = c(-0.5, 0.5),
               legend = TRUE, 
               ylim = c(0, 360000), show_intervals = TRUE, separate_intervals = TRUE)


dev.off()

