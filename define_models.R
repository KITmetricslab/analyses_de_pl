# Defining sets of models as used in the different other scripts.

# select which models to show in main plots:
models_gm <- list(main = c("epiforecasts-EpiExpert",
                           "epiforecasts-EpiNow2",
                           "FIAS_FZJ-Epi1Ger",
                           "ITWW-county_repro",
                           "LANL-GrowthRate",
                           "MIT_CovidAnalytics-DELPHI",
                           "KIT-time_series_baseline",
                           "KITCOVIDhub-median_ensemble"),
                  others = c("SDSC_ISG-TrendModel", 
                             "Imperial-ensemble2", 
                             "LeipzigIMISE-SECIR",
                             "UCLA-SuEIR", 
                             "USC-SIkJalpha",
                             "KIT-extrapolation_baseline",
                             "KIT-baseline",
                             "KITCOVIDhub-inverse_wis_ensemble",
                             "KITCOVIDhub-mean_ensemble"))

models_pl <- list(main = c("epiforecasts-EpiExpert",
                           "epiforecasts-EpiNow2",
                           "ICM-agentModel",
                           "ITWW-county_repro",
                           "MIT_CovidAnalytics-DELPHI",
                           "MOCOS-agent1",
                           "KIT-time_series_baseline",
                           "KITCOVIDhub-median_ensemble"),
                  others = c("SDSC_ISG-TrendModel", 
                             "Imperial-ensemble2", 
                             "LANL-GrowthRate", 
                             "MIMUW-StochSEIR", 
                             "USC-SIkJalpha",
                             "KIT-extrapolation_baseline",
                             "KIT-baseline",
                             "KITCOVIDhub-inverse_wis_ensemble", 
                             "KITCOVIDhub-mean_ensemble"))