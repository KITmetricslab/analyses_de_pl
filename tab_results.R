# Generate summary tables on results

library(xtable)
library(here)
# setwd("/home/johannes/Documents/Projects/intermediate_results/R")
source("define_models.R")
source("define_colors.R")



path_hub <- "../../covid19-forecast-hub-de"

# specify truth data (needs to be run once with "ECDC" and once with "JHU"):
truth <- "ECDC"

dat_evaluation <- read.csv(paste0(path_hub, "/evaluation/evaluation-", truth, ".csv"),
                           colClasses = list("target_end_date" = "Date", "forecast_date" = "Date", "timezero" = "Date"),
                           stringsAsFactors = FALSE)

dat_evaluation <- subset(dat_evaluation, !(model %in% c("Karlen-pypm",
                                                        "Imperial-ensemble1",
                                                        "epiforecasts-EpiNow2_secondary",
                                                        "IHME-CurveFit")))

# function to create summary table for horizons 1 through 4:
generate_summary <- function(dat_eval, target_type, inc_or_cum, location, first_forecast_date, last_observation_date){
  # restrict to relevant entries:
  tab <- dat_eval[dat_eval$timezero >= first_forecast_date &
                    dat_eval$target_end_date <= last_observation_date &
                    grepl(paste(inc_or_cum, target_type), dat_eval$target) &
                    !grepl("0 wk", dat_eval$target) &
                    !grepl("-1 wk", dat_eval$target) &
                    dat_eval$location == location, ]
  
  # compute coverages:
  tab$coverage0.5 <- (tab$truth >= tab$value.0.25 &
                        tab$truth <= tab$value.0.75)
  tab$coverage0.95 <- (tab$truth >= tab$value.0.025 &
                         tab$truth <= tab$value.0.975)
  # extra variable needed for re-formatting:
  tab$horizon <- substr(tab$target, start = 1, stop = 1)
  
  # select relevant columns:
  tab <- tab[, c("timezero", "model", "ae", "wis", "coverage0.5", "coverage0.95", "horizon")]
  
  # bring to wide format with separate variables for horizons:
  tab_wide <- reshape(tab, direction = "wide", timevar = "horizon",
                      idvar = c("timezero", "model"))
  
  
  # some vectors to handle column names:
  cols_imputation <- c("ae.1", "ae.2", "ae.3", "ae.4",
                       "wis.1", "wis.2", "wis.3", "wis.4")
  cols_numbers <- c(cols_imputation,
                    paste0("coverage0.5.", 1:4),
                    paste0("coverage0.95.", 1:4))
  
  # identify NA values:
  tab_wide_is_available <- tab_wide; tab_wide_is_available[, cols_numbers] <- !is.na(tab_wide_is_available[, cols_numbers])
  tab_wide_is_available$timezero <- NULL
  
  # count number of available scores per model:
  available_per_model <- aggregate(. ~ model, data = tab_wide_is_available, FUN = sum)
  # what is the possible maximum?
  available_per_model_max <- apply(available_per_model[, cols_numbers], MARGIN = 2, FUN = max)
  available_per_model_rel <- available_per_model
  # compute relative share of covered targets/dates:
  available_per_model_rel[, cols_numbers] <- available_per_model[, cols_numbers]/
    matrix(available_per_model_max, ncol = length(available_per_model_max), nrow = nrow(available_per_model), byrow = TRUE)
  
  # generate version where worst scores are filled in where NA
  timezeros <- sort(unique(tab_wide$timezero))
  replace_na <- function(vect){
    if(all(is.na(vect))) return(NA)
    vect[is.na(vect)] <- max(vect, na.rm = TRUE)
    return(vect)
  }
  
  tab_wide_imputed <- tab_wide
  for(tz in timezeros){
    inds <- which(tab_wide$timezero == tz)
    for(co in cols_imputation){
      tab_wide_imputed[inds, co] <- replace_na(tab_wide[inds, co])
    }
  }
  
  # aggregate including NA values ("raw"):
  tab_wide$timezero <- NULL
  summary_tab_raw <- aggregate(. ~ model, data = tab_wide,
                               FUN = mean, na.rm = TRUE, na.action = na.pass)
  
  # aggregate with imputation:
  tab_wide_imputed$timezero <- NULL
  summary_tab_imputed <- aggregate(. ~ model, data = tab_wide_imputed,
                                   FUN = mean, na.rm = TRUE, na.action = na.pass)
  
  # set to NA where not enough observations available
  for(co in cols_imputation){
    summary_tab_raw[which(available_per_model_rel[, co] < 1), co] <- NA
    summary_tab_imputed[which(available_per_model_rel[, co] < 2/3), co] <- NA
  }
  # and remove NaN:
  for(co in cols_numbers){
    summary_tab_raw[is.nan(summary_tab_raw[, co]), co] <- NA
    summary_tab_imputed[is.nan(summary_tab_imputed[, co]), co] <- NA
  }
  
  return(list(summary_tab_raw = summary_tab_raw,
              summary_tab_imputed = summary_tab_imputed,
              available_per_model = available_per_model,
              available_per_model_rel = available_per_model_rel,
              available_per_model_max = available_per_model_max))
}

# helper function to restrict summary to some horizons:
restrict_summary <- function(summ, horizons = 1:2){
  columns_to_keep <- sapply(horizons, function(x) paste0(c("ae.", "wis.", "coverage0.5.", "coverage0.95."), x))
  
  for(el in c("summary_tab_raw", "summary_tab_imputed", "available_per_model")){
    summ[[el]] <- summ[[el]][, c("model", columns_to_keep)]
  }
  summ$available_per_model_max <- summ$available_per_model_max[columns_to_keep]
  return(summ)
}

# helper function to merge summaries of incident and cumulative results:
merge_inc_cum_summaries <- function(summ_inc, summ_cum){
  summ <- list()
  for(el in c("summary_tab_raw", "summary_tab_imputed", "available_per_model")){
    summ[[el]] <- merge(summ_inc[[el]], summ_cum[[el]], by = "model", all.x = TRUE, all.y = TRUE,
                        suffixes = c(".inc", ".cum"))
  }
  
  names(summ_inc$available_per_model_max) <- paste0(names(summ_inc$available_per_model_max), ".inc")
  names(summ_cum$available_per_model_max) <- paste0(names(summ_cum$available_per_model_max), ".cum")
  summ$available_per_model_max <- c(summ_inc$available_per_model_max,
                                    summ_cum$available_per_model_max)
  
  return(summ)
}

# function for printing:
xtable_summary_tab <- function(summary_tab){
  tab <- summary_tab$summary_tab_imputed
  
  # re-format proportions:
  columns_coverage <- colnames(tab)[grepl("coverage", colnames(tab))]
  for(co in columns_coverage){
    inds <- which(!is.na(tab[, co]))
    tab[inds, co] <- round(tab[inds, co]*summary_tab$available_per_model[inds, co])
    tab[inds, co] <- paste0(tab[inds, co], "/", summary_tab$available_per_model[inds, co])
  }
  
  # add stars:
  columns_scores <- colnames(tab)[grepl("ae", colnames(tab)) | grepl("wis", colnames(tab))]
  
  for(co in columns_scores){
    inds <- which(summary_tab$available_per_model[, co] < summary_tab$available_per_model_max[co] &
                    !is.na(tab[, co]))
    tab[, co] <- format(tab[, co], digits = 0, scientific = FALSE, big.mark = ",")
    tab[grepl("NA", tab[, co]), co] <- "" # remove NA
    tab[inds, co] <- paste0(tab[inds, co], "*")
  }
  
  tab <- tab[order(tab$model), ]
  
  tab_baselines <- tab[grepl("baseline", tab$model), ]
  tab_baselines <- tab_baselines[order(tab_baselines$model), ]
  
  tab_ensembles <- tab[grepl("KITCOVIDhub", tab$model), ]
  tab_ensembles <- tab_ensembles[order(tab_ensembles$model), ]
  
  tab_members <- tab[!grepl("KITCOVIDhub", tab$model) & !grepl("baseline", tab$model), ]
  tab_members <- tab_members[order(tab_members$model), ]
  
  tab_to_print <- rbind(tab_members, tab_baselines, tab_ensembles)
  
  hline.after <- if(nrow(tab_members) != 0) nrow(tab_members) + c(0, nrow(tab_baselines)) else NULL
  
  print(xtable(tab_to_print), hline.after = hline.after,
        include.rownames=FALSE, only.contents = TRUE, include.colnames = FALSE,
        format.args = list(big.mark = ","))
}


# Germany, cases:

## 1 + 2 wk
summary_gm_inc_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "inc", location = "GM",
                                        first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))

summary_gm_cum_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "cum", location = "GM",
                                        first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))


summary_gm_inc_case_12 <- restrict_summary(summary_gm_inc_case)
summary_gm_cum_case_12 <- restrict_summary(summary_gm_cum_case)

summary_gm_case_12 <- merge_inc_cum_summaries(summary_gm_inc_case_12,
                                              summary_gm_cum_case_12)

xtable_summary_tab(summary_gm_case_12)

summary_gm_inc_case_34 <- restrict_summary(summary_gm_inc_case, horizons = 3:4)
summary_gm_cum_case_34 <- restrict_summary(summary_gm_cum_case, horizons = 3:4)

summary_gm_case_34 <- merge_inc_cum_summaries(summary_gm_inc_case_34,
                                              summary_gm_cum_case_34)

## Germany, deaths:

## 1 + 2 wk
summary_gm_inc_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "inc", location = "GM",
                                         first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))

summary_gm_cum_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "cum", location = "GM",
                                         first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))


summary_gm_inc_death_12 <- restrict_summary(summary_gm_inc_death)
summary_gm_cum_death_12 <- restrict_summary(summary_gm_cum_death)

summary_gm_death_12 <- merge_inc_cum_summaries(summary_gm_inc_death_12,
                                               summary_gm_cum_death_12)


summary_gm_inc_death_34 <- restrict_summary(summary_gm_inc_death, horizons = 3:4)
summary_gm_cum_death_34 <- restrict_summary(summary_gm_cum_death, horizons = 3:4)

summary_gm_death_34 <- merge_inc_cum_summaries(summary_gm_inc_death_34,
                                               summary_gm_cum_death_34)



# Poland, cases:

## 1 + 2 wk
summary_pl_inc_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "inc", location = "PL",
                                        first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))

summary_pl_cum_case <- generate_summary(dat_eval = dat_evaluation, target_type = "case", inc_or_cum = "cum", location = "PL",
                                        first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))


summary_pl_inc_case_12 <- restrict_summary(summary_pl_inc_case)
summary_pl_cum_case_12 <- restrict_summary(summary_pl_cum_case)

summary_pl_case_12 <- merge_inc_cum_summaries(summary_pl_inc_case_12,
                                              summary_pl_cum_case_12)


summary_pl_inc_case_34 <- restrict_summary(summary_pl_inc_case, horizons = 3:4)
summary_pl_cum_case_34 <- restrict_summary(summary_pl_cum_case, horizons = 3:4)

summary_pl_case_34 <- merge_inc_cum_summaries(summary_pl_inc_case_34,
                                              summary_pl_cum_case_34)

## Poland, deaths:

## 1 + 2 wk
summary_pl_inc_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "inc", location = "PL",
                                         first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))

summary_pl_cum_death <- generate_summary(dat_eval = dat_evaluation, target_type = "death", inc_or_cum = "cum", location = "PL",
                                         first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))


summary_pl_inc_death_12 <- restrict_summary(summary_pl_inc_death)
summary_pl_cum_death_12 <- restrict_summary(summary_pl_cum_death)

summary_pl_death_12 <- merge_inc_cum_summaries(summary_pl_inc_death_12,
                                               summary_pl_cum_death_12)


summary_pl_inc_death_34 <- restrict_summary(summary_pl_inc_death, horizons = 3:4)
summary_pl_cum_death_34 <- restrict_summary(summary_pl_cum_death, horizons = 3:4)

summary_pl_death_34 <- merge_inc_cum_summaries(summary_pl_inc_death_34,
                                               summary_pl_cum_death_34)

for(fil in c("summary_gm_case_12",
             "summary_gm_death_12",
             "summary_gm_case_34",
             "summary_gm_death_34",
             "summary_pl_case_12",
             "summary_pl_death_12",
             "summary_pl_case_34",
             "summary_pl_death_34")){
  writeLines(xtable_summary_tab(get(fil)), con = paste0("../input/", fil, "_", truth, ".tex"))
}

source("define_colors.R")

plot_performance_decay <- function(summ, models, score_type = "wis", imputed = TRUE, col, log = TRUE, legend = FALSE, 
                                   max_horizon = 4, add_baseline = TRUE){
  if(imputed){
    tab <- summ$summary_tab_imputed
  }else{
    tab <- tab <- summ$summary_tab_raw
  }
  
  yl <- range(c(tab$wis.1, tab[, paste0("ae.", max_horizon)]), na.rm = TRUE)
  # yl[1] <- 1
  
  plot(NULL, xlim = c(0.9, ifelse(legend, 7, 4.2)), ylim = yl,
       log = ifelse(log, "y", ""), xlab = "horizon", ylab = "mean WIS or AE", axes = FALSE)
  axis(1, at = 1:max_horizon); axis(2); box()
  
  if(add_baseline){
    scores_baseline <- unlist(subset(tab, model == "KIT-baseline")[, paste0(score_type, ".", 1:max_horizon)])
    polygon(c(1:max_horizon, max_horizon:1),
            c(scores_baseline, rep(yl[2], max_horizon)), col = "lightgrey", border = NA)
    # if(score_type == "wis"){
    #   ae_baseline <- unlist(subset(tab, model == "KIT-baseline")[, paste0("ae.", 1:max_horizon)])
    #   polygon(c(1:max_horizon, max_horizon:1),
    #           c(ae_baseline, rep(yl[2], max_horizon)), col = "grey90", border = NA)
    # }
  }
  abline(v = 1:max_horizon, lty = "dotted")
  
  ltys <- rep(1, length(models))
  for(m in seq_along(models)){
    scores <- unlist(subset(tab, model == models[m])[, paste0(score_type, ".", 1:max_horizon)])
    if(score_type == "wis" & any(!is.na(scores))) lines(seq_along(scores), scores, col = col[m])
    if(all(is.na(scores)) | score_type == "ae"){
      scores <- unlist(subset(tab, model == models[m])[, paste0("ae.", 1:max_horizon)])
      lines(seq_along(scores), scores, col = col[m], lty = "dashed")
      ltys[m] <- 2
    }
    
  }
  if(legend) legend("right", legend = models, col = col, lty = ltys, bty = "n", cex = 0.7)
}

# suppress scientific notation for plot labels:
options(scipen = 1000); par(las = 1)

models_horizons_germany <- c(models_gm$main, "KIT-extrapolation_baseline", "LeipzigIMISE-SECIR", "UCLA-SuEIR", "USC-SIkJalpha")
models_horizons_poland <- c(models_pl$main, "KIT-extrapolation_baseline", "USC-SIkJalpha")

# remove LANL as it is NA:
models_horizons_germany <- models_horizons_germany[models_horizons_germany != "LANL-GrowthRate"]

pdf("../figures/performance_horizons.pdf", width = 9, height = 6)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 0.5))
plot_performance_decay(summary_gm_inc_case, models = models_horizons_germany,
                       col = cols[models_horizons_germany], log = TRUE, max_horizon = 4)
title("Incident cases, Germany")

plot_performance_decay(summary_gm_inc_death, models = models_horizons_germany,
                       col = cols[models_horizons_germany], log = TRUE)
title("Incident deaths, Germany")

plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
legend("center", legend = c(models_horizons_germany, "KIT-baseline"),
       col = c(cols[models_horizons_germany], "lightgrey"),
       lty = c(rep(1, length(models_gm$main)), 2, 2, 2, NA),
       pch = c(rep(NA, length(models_horizons_germany)), 15))

plot_performance_decay(summary_pl_inc_case, models = models_horizons_poland,
                       col = cols[models_horizons_poland], log = TRUE)
title("Incident cases, Poland")

plot_performance_decay(summary_pl_inc_death, models = models_horizons_poland,
                       col = cols[models_horizons_poland], log = TRUE)
title("Incident deaths, Poland")


plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
legend("center", legend = c(models_horizons_poland, "KIT-baseline"),
       col = c(cols[models_horizons_poland], "lightgrey"),
       lty = c(rep(1, length(models_pl$main) + 1), 2, NA),
       pch = c(rep(NA, length(models_horizons_poland)), 15))
dev.off()




# alternative plot with model categories as colours:
model_categories <- c(
  "KIT-baseline" = "baseline",
  "KIT-time_series_baseline" = "baseline",
  "KIT-extrapolation_baseline" = "baseline",
  "epiforecasts-EpiExpert" = "human_judgement",
  "epiforecasts-EpiNow2" = "renewal_eq",
  "FIAS_FZJ-Epi1Ger" = "compartmental",
  "SDSC_ISG-TrendModel" = "renewal_eq",
  "ICM-agentModel" = "micro_sim",
  "IHME-CurveFit" = "compartmental",
  "Imperial-ensemble2" = "ensemble",
  "ITWW-county_repro" = "renewal_eq",
  "LANL-GrowthRate" = "reneqal_eq",
  "LeipzigIMISE-SECIR" = "compartmental",
  "MIMUW-StochSEIR" = "compartmental",
  "MIT_CovidAnalytics-DELPHI" = "compartmental",
  "MOCOS-agent1" = "micro_sim",
  "UCLA-SuEIR" = "compartmental",
  "USC-SIkJalpha" = "compartmental",
  "Karlen-pypm" = "compartmental",
  "KITCOVIDhub-mean_ensemble" = "ensemble",
  "KITCOVIDhub-median_ensemble" = "ensemble",
  "KITCOVIDhub-inverse_wis_ensemble" = "ensemble"
)
cols_categories <- c("compartmental" = "blue",
                     "micro_sim" = "darkorange",
                     "renewal_eq" = "red",
                     "human_judgement" = "green",
                     "ensemble" = "black",
                     "baseline" = "darkgrey")


pdf("../figures/performance_horizons_by_type.pdf", width = 9, height = 6)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 0.5))
plot_performance_decay(summary_gm_inc_case, models = models_horizons_germany,
                       col = cols_categories[model_categories[models_horizons_germany]],
                       log = TRUE, max_horizon = 4)
title("Incident cases, Germany")

plot_performance_decay(summary_gm_inc_death, models = models_horizons_germany,
                       col = cols_categories[model_categories[models_horizons_germany]],
                       log = TRUE)
title("Incident deaths, Germany")

plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
legend("center", legend = c("compartmental", "microsimulation", "renewal equation",
                            "human judg.", "ensemble", "baseline"),
       col = cols_categories,
       lty = c(rep(1, length(models_pl$main) + 1), 2, NA),
       pch = c(rep(NA, length(models_horizons_poland)), 15))

plot_performance_decay(summary_pl_inc_case, models = models_horizons_poland,
                       col = cols_categories[model_categories[models_horizons_poland]], log = TRUE)
title("Incident cases, Poland")

plot_performance_decay(summary_pl_inc_death, models = models_horizons_poland,
                       col = cols_categories[model_categories[models_horizons_poland]], log = TRUE)
title("Incident deaths, Poland")


plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
legend("center", legend = c("compartmental", "microsimulation", "renewal equation",
                            "human judgement", "ensemble", "baseline"),
       col = cols_categories,
       lty = c(rep(1, length(models_pl$main) + 1), 2, NA),
       pch = c(rep(NA, length(models_horizons_poland)), 15))
dev.off()


### compute tables for additional ensembles (i.e. with all models included):

dat_evaluation_additional_ensembles <- read.csv("additional_ensembles/evaluation_additional_ensembles_ECDC.csv",
                                                colClasses = list("target_end_date" = "Date", "forecast_date" = "Date", "timezero" = "Date"),
                                                stringsAsFactors = FALSE)


# Germany, cases:

## 1 + 2 wk
summary_gm_inc_case <- generate_summary(dat_eval = dat_evaluation_additional_ensembles, target_type = "case", inc_or_cum = "inc", location = "GM",
                                        first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))

summary_gm_cum_case <- generate_summary(dat_eval = dat_evaluation_additional_ensembles, target_type = "case", inc_or_cum = "cum", location = "GM",
                                        first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))


summary_gm_inc_case_12 <- restrict_summary(summary_gm_inc_case)
summary_gm_cum_case_12 <- restrict_summary(summary_gm_cum_case)

summary_gm_case_12 <- merge_inc_cum_summaries(summary_gm_inc_case_12,
                                              summary_gm_cum_case_12)

xtable_summary_tab(summary_gm_case_12)

summary_gm_inc_case_34 <- restrict_summary(summary_gm_inc_case, horizons = 3:4)
summary_gm_cum_case_34 <- restrict_summary(summary_gm_cum_case, horizons = 3:4)

summary_gm_case_34 <- merge_inc_cum_summaries(summary_gm_inc_case_34,
                                              summary_gm_cum_case_34)

## Germany, deaths:

## 1 + 2 wk
summary_gm_inc_death <- generate_summary(dat_eval = dat_evaluation_additional_ensembles, target_type = "death", inc_or_cum = "inc", location = "GM",
                                         first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))

summary_gm_cum_death <- generate_summary(dat_eval = dat_evaluation_additional_ensembles, target_type = "death", inc_or_cum = "cum", location = "GM",
                                         first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))


summary_gm_inc_death_12 <- restrict_summary(summary_gm_inc_death)
summary_gm_cum_death_12 <- restrict_summary(summary_gm_cum_death)

summary_gm_death_12 <- merge_inc_cum_summaries(summary_gm_inc_death_12,
                                               summary_gm_cum_death_12)


summary_gm_inc_death_34 <- restrict_summary(summary_gm_inc_death, horizons = 3:4)
summary_gm_cum_death_34 <- restrict_summary(summary_gm_cum_death, horizons = 3:4)

summary_gm_death_34 <- merge_inc_cum_summaries(summary_gm_inc_death_34,
                                               summary_gm_cum_death_34)



# Poland, cases:

## 1 + 2 wk
summary_pl_inc_case <- generate_summary(dat_eval = dat_evaluation_additional_ensembles, target_type = "case", inc_or_cum = "inc", location = "PL",
                                        first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))

summary_pl_cum_case <- generate_summary(dat_eval = dat_evaluation_additional_ensembles, target_type = "case", inc_or_cum = "cum", location = "PL",
                                        first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))


summary_pl_inc_case_12 <- restrict_summary(summary_pl_inc_case)
summary_pl_cum_case_12 <- restrict_summary(summary_pl_cum_case)

summary_pl_case_12 <- merge_inc_cum_summaries(summary_pl_inc_case_12,
                                              summary_pl_cum_case_12)


summary_pl_inc_case_34 <- restrict_summary(summary_pl_inc_case, horizons = 3:4)
summary_pl_cum_case_34 <- restrict_summary(summary_pl_cum_case, horizons = 3:4)

summary_pl_case_34 <- merge_inc_cum_summaries(summary_pl_inc_case_34,
                                              summary_pl_cum_case_34)

## Poland, deaths:

## 1 + 2 wk
summary_pl_inc_death <- generate_summary(dat_eval = dat_evaluation_additional_ensembles, target_type = "death", inc_or_cum = "inc", location = "PL",
                                         first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))

summary_pl_cum_death <- generate_summary(dat_eval = dat_evaluation_additional_ensembles, target_type = "death", inc_or_cum = "cum", location = "PL",
                                         first_forecast_date = as.Date("2020-10-12"), last_observation_date = as.Date("2020-12-19"))


summary_pl_inc_death_12 <- restrict_summary(summary_pl_inc_death)
summary_pl_cum_death_12 <- restrict_summary(summary_pl_cum_death)

summary_pl_death_12 <- merge_inc_cum_summaries(summary_pl_inc_death_12,
                                               summary_pl_cum_death_12)


summary_pl_inc_death_34 <- restrict_summary(summary_pl_inc_death, horizons = 3:4)
summary_pl_cum_death_34 <- restrict_summary(summary_pl_cum_death, horizons = 3:4)

summary_pl_death_34 <- merge_inc_cum_summaries(summary_pl_inc_death_34,
                                               summary_pl_cum_death_34)

for(fil in c("summary_gm_case_12",
             "summary_gm_death_12",
             "summary_gm_case_34",
             "summary_gm_death_34",
             "summary_pl_case_12",
             "summary_pl_death_12",
             "summary_pl_case_34",
             "summary_pl_death_34")){
  writeLines(xtable_summary_tab(get(fil)), con = paste0("../input/", fil, "_additional_ensembles_", truth, ".tex"))
}

