# Defining colours, line and point types for other scripts.

library(pals)

cols_raw <- kelly(12)[-c(1, 2, 9)]

cols_baseline <- c("KIT-baseline" = "darkgrey",
                   "KIT-time_series_baseline" = "darkgrey",
                   "KIT-extrapolation_baseline" = cols_raw[8])

pch_baseline <- c("KIT-baseline" = 21,
                  "KIT-time_series_baseline" = 22, 
                  "KIT-extrapolation_baseline" = 24)

cols_ensemble <- c("KITCOVIDhub-mean_ensemble" = "black",
                   "KITCOVIDhub-median_ensemble" = "black",
                   "KITCOVIDhub-inverse_wis_ensemble" = "black")

pch_ensemble <- c("KITCOVIDhub-mean_ensemble" = 24,
                  "KITCOVIDhub-median_ensemble" = 21,
                  "KITCOVIDhub-inverse_wis_ensemble" = 22)

cols_submitted <- c("epiforecasts-EpiExpert" = cols_raw[1],
                    "epiforecasts-EpiNow2" = cols_raw[2],
                    "FIAS_FZJ-Epi1Ger" = cols_raw[3],
                    "SDSC_ISG-TrendModel" = cols_raw[4],
                    "ICM-agentModel" = cols_raw[3],
                    "IHME-CurveFit" = cols_raw[5],
                    "Imperial-ensemble2" = cols_raw[8],
                    "ITWW-county_repro" = cols_raw[4],
                    "LANL-GrowthRate" = cols_raw[5],
                    "LeipzigIMISE-SECIR" = cols_raw[6],
                    "MIMUW-StochSEIR" = cols_raw[9],
                    "MIT_CovidAnalytics-DELPHI" = cols_raw[7],
                    "MOCOS-agent1" = cols_raw[6],
                    "UCLA-SuEIR" = cols_raw[8],
                    "USC-SIkJalpha" = cols_raw[9],
                    "Karlen-pypm" = "white")

pch_submitted <- c("epiforecasts-EpiExpert" = 21,
                    "epiforecasts-EpiNow2" = 21,
                    "FIAS_FZJ-Epi1Ger" = 22,
                    "SDSC_ISG-TrendModel" = 23,
                    "ICM-agentModel" = 24,
                    "IHME-CurveFit" = 24,
                    "Imperial-ensemble2" = 24,
                    "ITWW-county_repro" = 24,
                    "LANL-GrowthRate" = 21,
                    "LeipzigIMISE-SECIR" = 21,
                    "MIMUW-StochSEIR" = 21,
                    "MIT_CovidAnalytics-DELPHI" = 21,
                    "MOCOS-agent1" = 24,
                    "UCLA-SuEIR" = 21,
                    "USC-SIkJalpha" = 22,
                    "Karlen-pypm" = 22)

cols <- c(cols_baseline, cols_submitted, cols_ensemble)
pchs <- c(pch_baseline, pch_submitted, pch_ensemble)
