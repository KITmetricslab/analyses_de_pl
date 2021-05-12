# Some plots showing the time series of cases and deaths in DE + PL

library(here)
library(plotrix)

setwd("/home/johannes/Documents/Projects/intermediate_results/R")
source("functions_paper.R")


Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

path_hub <- "/home/johannes/Documents/Projects/covid19-forecast-hub-de"
source(paste0(path_hub, "/code/R/auxiliary_functions.R"))

# read in data:
dat_ecdc <- read.csv(paste0(path_hub, "/app_forecasts_de/data/truth_to_plot_ecdc.csv"), colClasses = list(date = "Date"))
dat_jhu <- read.csv(paste0(path_hub, "/app_forecasts_de/data/truth_to_plot_jhu.csv"), colClasses = list(date = "Date"))



par(las = 1)
options(scipen=5)


# custom function to plot time series:
plot_ts <- function(dat_ecdc, dat_jhu, variable, main = "", col_gm = "black", col_pl = "red", points = FALSE,
                    cex.points = 0.9, highlight = NULL, col_highlight = "lightgrey", ylim = NULL, add = FALSE, ...){
  dat_ecdc_gm <- subset(dat_ecdc, location == "GM")
  dat_ecdc_pl <- subset(dat_ecdc, location == "PL")
  dat_jhu_gm <- subset(dat_jhu, location == "GM")
  dat_jhu_pl <- subset(dat_jhu, location == "PL")
  
  if(!add){
    if(is.null(ylim)) ylim <- c(0, 1.2*max(c(dat_ecdc_gm[, variable],
                                             dat_jhu_gm[, variable],
                                             dat_ecdc_pl[, variable],
                                             dat_jhu_pl[, variable]), na.rm = TRUE))
    
    plot(dat_ecdc_gm$date, dat_ecdc_gm[, variable], type = "l", xlab = "", ylab = "", col = "white", ylim = ylim, ...)
    if(!is.null(highlight)) rect(xleft = highlight[1], ybottom = 0, xright = highlight[2], ytop = 10^7, col = col_highlight, border = NA)
  }
  
  lines(dat_ecdc_gm$date, dat_ecdc_gm[, variable], type = "l", col = col_gm)
  lines(dat_jhu_gm$date, dat_jhu_gm[, variable], type = "l", lty = 2, col = col_gm)
  lines(dat_ecdc_pl$date, dat_ecdc_pl[, variable], type = "l", col = col_pl, cex = 0.7)
  lines(dat_jhu_pl$date, dat_jhu_pl[, variable], type = "l", lty = 2, col = col_pl)

  if(points){
    points(dat_ecdc_gm$date, dat_ecdc_gm[, variable], pch = 15, xlab = "", ylab = "", cex = cex.points, col = col_gm)
    points(dat_jhu_gm$date, dat_jhu_gm[, variable], pch = 17, xlab = "", ylab = "", col = col_gm, cex = cex.points)
    points(dat_ecdc_pl$date, dat_ecdc_pl[, variable], pch = 15, xlab = "", ylab = "", cex = cex.points, col = col_pl)
    points(dat_jhu_pl$date, dat_jhu_pl[, variable], pch = 17, xlab = "", ylab = "", cex = cex.points, col = col_pl)
  }

  mtext(side = 3, main, cex = 0.8, line = 0.3)
}

# helper function to add axes:
add_axes <- function(xticks, xticks_labelled){
  axis(1, at = xticks, labels = rep("", length(xticks)), tck=-0.015)
  axis(1, at = xticks_labelled, labels = xticks_labelled, tck = -0.03)
  axis(2); box()
}
# ticks:
xticks <- seq(from = as.Date("2020-09-12"), by = 7, length.out = 16)
xticks_labelled <- xticks[c(1, 5, 9, 13)]

# define area to highlight:
highlight <- c(as.Date("2020-10-12"), as.Date("2020-12-19"))


pdf("../figures/time_series.pdf", width = 8.5, height = 5.5)

# structure plot area
layout(matrix(1:6, ncol = 2), heights = c(1.4, 2.6, 2.1))

# Cases:

# small plot showing also first wave:
par(mar = c(2.5, 4, 2, 2), las = 1)
plot_ts(dat_ecdc, dat_jhu, "inc_case", highlight = highlight, points = TRUE,
        main = "Weekly incident cases", cex.points = 0.7)

# large time series plot:
par(las = 1, mar = c(2.5, 4, 1, 2))
plot_ts(dat_ecdc, dat_jhu, "inc_case", highlight = highlight,
        xlim = highlight + c(-38, 14), axes = FALSE,
        points = TRUE)
add_axes(xticks = xticks, xticks_labelled = xticks_labelled)

# Add letters/numbers for events:
letter_in_circle(as.Date("2020-09-03"), 200000, "1", col = "red", col_line = "lightgrey") # PL: strict test criteria
letter_in_circle(as.Date("2020-10-10"), 200000, "2", col = "red", col_line = "lightgrey") # PL: entire country classified as "yellow zone"
letter_in_circle(as.Date("2020-10-24"), 200000, "3", col = "red") # PL: entire country classified as "red zone" (partial school closure, restaurants closed, gatherings restrictied)
letter_in_circle(as.Date("2020-10-31"), 180000, "4", col = "red") # PL: relaxed test criteria
letter_in_circle(as.Date("2020-11-02"), 200000, "a") # DE: semi-lockdown
letter_in_circle(as.Date("2020-11-03"), 180000, "b") # DE: new test strategy
letter_in_circle(as.Date("2020-11-07"), 200000, "5", col = "red") # PL: reinforced restrictions (extension of school closures, shooping malls closed)
letter_in_circle(as.Date("2020-11-25"), 200000, "6", col = "red") # PL: bulk reporting in PL
letter_in_circle(as.Date("2020-11-28"), 180000, "7", col = "red") # PL: Malls reopened
letter_in_circle(as.Date("2020-12-01"), 200000, "c") # DE: reinforcement of contact restrictions https://www.tagesschau.de/inland/corona-plan-bundeslaender-beschluss-103.html
letter_in_circle(as.Date("2020-12-16"), 200000, "d") # DE: full lockdown (incl school closure)

# avoid overplotting with vertival dashed lines
plot_ts(dat_ecdc, dat_jhu, "inc_case", add = TRUE, points = TRUE)

# explanation of events:
plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")

legend("center", legend = c("Measures in Germany",
                            "a - semi-lockdown (restaurants closed, work f. home encouraged,",
                            "   no mass gatherings, inter-househ. contacts restr.; 2020-11-02)",
                            "b - new testing strategy announced (stricter criteria on symptoms;",
                            "  2020-11-03)",
                            "c - reinforced restrictions on inter-househ. contacts (2020-12-01)",
                            "d - start of full lockdown (schools and non-essential shops;",
                            "  closed; 2020-12-16)"), bty = "n")

# Deaths:

# small plot showing also first wave:
par(mar = c(2.5, 4, 2, 2), las = 1)
plot_ts(dat_ecdc, dat_jhu, "inc_death", highlight = highlight, points = TRUE,
        main = "Weekly incident deaths", cex.points = 0.7)

# large time series plot:
par(las = 1, mar = c(2.5, 4, 1, 2))
plot_ts(dat_ecdc, dat_jhu, "inc_death", highlight = highlight,
        xlim = highlight + c(-38, 14), ylim = c(0, 5200),  axes = FALSE,
        points = TRUE)
add_axes(xticks = xticks, xticks_labelled = xticks_labelled)

# events:
letter_in_circle(as.Date("2020-09-03"), 5000, "1", col = "red", col_line = "lightgrey") # PL: stricter testing criteria
letter_in_circle(as.Date("2020-10-10"), 5000, "2", col = "red", col_line = "lightgrey") # PL: entire country classified as "yellow zone"
letter_in_circle(as.Date("2020-10-24"), 5000, "3", col = "red") # PL: entire country classified as "red zone" (partial school closure, restaurants closed, gatherings restrictied)
letter_in_circle(as.Date("2020-10-31"), 4500, "4", col = "red") # PL: relaxed test criteria
letter_in_circle(as.Date("2020-11-02"), 5000, "a") # DE: semi-lockdown
letter_in_circle(as.Date("2020-11-03"), 4500, "b") # DE: new test strategy
letter_in_circle(as.Date("2020-11-07"), 5000, "5", col = "red") # PL: reinforced restrictions (extension of school closures, shooping malls closed)
letter_in_circle(as.Date("2020-11-25"), 5000, "6", col = "red") # PL: bulk reporting in PL
letter_in_circle(as.Date("2020-11-28"), 4500, "7", col = "red") # PL: Malls reopened
letter_in_circle(as.Date("2020-12-01"), 5000, "c") # DE: reinforcement of contact restrictions https://www.tagesschau.de/inland/corona-plan-bundeslaender-beschluss-103.html
letter_in_circle(as.Date("2020-12-16"), 5000, "d") # DE: full lockdown (incl school closure)

legend("bottomright", legend = c("Germany", "Poland", "", "ECDC", "JHU"),
       lty = c(1, 1, NA, 1, 2), col = c("black", "red", NA, "black", "black"), bty = "n",
       pch = c(15, 15, NA, 15, 17))

plot_ts(dat_ecdc, dat_jhu, "inc_death", add = TRUE, points = TRUE)

# Explanation of measures in Poland:
plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")

legend("center", legend = c("Measures in Poland",
                            "1 - stricter testing criteria (four symptoms needed; 2020-09-03)",
                            "2 - entire country classified yellow zone (mask wearing and", 
                            "  limitations on gatherings; 2020-10-10)",
                            "3 - entire country classified red zone (partial school closure,",
                            "  restaurants closed, gatherings restricted; 2020-10-24)",
                            "4 - eased testing criteria (one symptom sufficient; 2020-10-31)",
                            "5 - reinforced restrictions (extension of school closure, ",
                            "  shopping malls closed; 2020-11-07)",
                            "6 - bulk reporting of 22,000 cases (2020-11-24)",
                            "7 - shopping malls re-opened (2020-11-28)"), 
       bty = "n", text.col = "red")

dev.off()

# plot by age, DE (for supplement):
# read in age-stratified data from Hub
dat_ecdc_age_case <- read.csv(paste0(path_hub, "/data-truth/RKI/by_age/truth_RKI-Incident Cases by Age_Germany.csv"), colClasses = list(date = "Date"))
dat_ecdc_age_death <- read.csv(paste0(path_hub, "/data-truth/RKI/by_age/truth_RKI-Incident Deaths by Age_Germany.csv"), colClasses = list(date = "Date"))

# transform to weekly:
dat_ecdc_case_all <- inc_truth_to_weekly(subset(dat_ecdc_age_case, location == "GM"))
dat_ecdc_case_80 <- inc_truth_to_weekly(subset(dat_ecdc_age_case, age_group == "A80+" &
                                                 location == "GM"))
dat_ecdc_case_others <- inc_truth_to_weekly(subset(dat_ecdc_age_case, age_group != "A80+" &
                                                         location == "GM"))


dat_ecdc_death_all <- inc_truth_to_weekly(subset(dat_ecdc_age_death, location == "GM"))
dat_ecdc_death_80 <- inc_truth_to_weekly(subset(dat_ecdc_age_death, age_group == "A80+" &
                                                 location == "GM"))
dat_ecdc_death_others <- inc_truth_to_weekly(subset(dat_ecdc_age_death, age_group != "A80+" &
                                                     location == "GM"))

# Plot:

pdf("../figures/time_series_age.pdf", width = 12.5, height = 6)

# Cases:
par(mfrow = 1:2, las = 1, mar = c(4, 4, 1.5, 1))
plot(dat_ecdc_case_all$date, dat_ecdc_case_all$value, xlab = "", ylab = "", 
     xlim = highlight + c(-28, 14), type = "o", pch = 15,
     ylim = c(0, 200000), axes = FALSE)
add_axes(xticks = xticks, xticks_labelled = xticks_labelled)

mtext("Weekly incident cases by age, Germany", line = 0.3, cex = 1.7)
rect(xleft = highlight[1], ybottom = 0, xright = highlight[2], ytop = 10^7, col = "lightgrey", border = NA)

letter_in_circle(as.Date("2020-11-02"), 200000, "a") # DE: semi-lockdown
letter_in_circle(as.Date("2020-11-03"), 180000, "b") # DE: new test strategy
letter_in_circle(as.Date("2020-12-01"), 200000, "c") # DE: reinforcement of contact restrictions https://www.tagesschau.de/inland/corona-plan-bundeslaender-beschluss-103.html
letter_in_circle(as.Date("2020-12-16"), 200000, "d") # DE: full lockdown (incl school closure)

lines(dat_ecdc_case_all$date, dat_ecdc_case_all$value, type = "o", pch = 15)
lines(dat_ecdc_case_80$date, dat_ecdc_case_80$value, type = "o", lty = 3)
lines(dat_ecdc_case_others$date, dat_ecdc_case_others$value, type = "o", lty = 4, pch = 4)

legend("topleft", lty = c(1, 4, 3), legend = c("total", "younger than 80", "80 or older"), 
       bty = "n", pch = c(15, 3, 1))

# Deaths:
plot(dat_ecdc_death_all$date, dat_ecdc_death_all$value, type = "l", xlab = "", ylab = "", col = "white", 
     xlim = highlight + c(-28, 14), ylim = c(0, 5000), axes = FALSE)
add_axes(xticks = xticks, xticks_labelled = xticks_labelled)
mtext("Weekly incident deaths by age, Germany", line = 0.3, cex = 1.7)
rect(xleft = highlight[1], ybottom = 0, xright = highlight[2], ytop = 10^7, col = "lightgrey", border = NA)

letter_in_circle(as.Date("2020-11-02"), 5000, "a") # DE: semi-lockdown
letter_in_circle(as.Date("2020-11-03"), 4500, "b") # DE: new test strategy
letter_in_circle(as.Date("2020-12-01"), 5000, "c") # DE: reinforcement of contact restrictions https://www.tagesschau.de/inland/corona-plan-bundeslaender-beschluss-103.html
letter_in_circle(as.Date("2020-12-16"), 5000, "d") # DE: full lockdown (incl school closure)

lines(dat_ecdc_death_all$date, dat_ecdc_death_all$value, type = "o", pch = 15)
lines(dat_ecdc_death_80$date, dat_ecdc_death_80$value, type = "o", lty = 3)
lines(dat_ecdc_death_others$date, dat_ecdc_death_others$value, type = "o", lty = 4, pch = 4)

dev.off()
