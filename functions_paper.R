# Some auxiliary functions used in the other scripts.

# function for time series plot based on evaluation data:
plot_from_eval <- function(dat_eval, models, location,
                           target_type, inc_or_cum, horizon = NULL, forecast_date = NULL,
                           ylim = NULL, start = NULL, end = NULL,
                           col = "steelblue", pch = 21, alpha.col = 0.3, add = FALSE,
                           shifts = c(0, 1, -1, -0.5, -0.5), width = 0.5,
                           separate_intervals = TRUE, legend = FALSE,
                           show_intervals = TRUE){
  
  if(!is.null(forecast_date)) forecast_date <- as.Date(forecast_date)
  
  # restrict to relevant rows:
  dat_eval <- dat_eval[  grepl(target_type, dat_eval$target) &
                           grepl(inc_or_cum, dat_eval$target) &
                           dat_eval$location == location &
                           !grepl("0", dat_eval$target) &
                           !grepl("-1", dat_eval$target), ]
  
  # extract data to plot truth:
  all_dates <- dat_eval$target_end_date[!duplicated(dat_eval$target_end_date)]
  all_truths <- dat_eval$truth[!duplicated(dat_eval$target_end_date)]
  if(!is.null(forecast_date)){
    last_date <- all_dates[(all_dates > forecast_date - 7) & (forecast_date > all_dates)]
    last_truth <- all_truths[all_dates == last_date]
  }else{
    last_date <- last_truth <- NULL
  }
  
  # restrict to models and forecast date or horizon:
  dat_eval <- dat_eval[dat_eval$model %in% models, ]
  if(!is.null(forecast_date)) dat_eval <- dat_eval[dat_eval$timezero == forecast_date, ]
  if(!is.null(horizon)){
    dat_eval <- dat_eval[grepl(horizon, dat_eval$target), ]
  }
  
  # catch in case of only missings:
  if(nrow(dat_eval) == 0 & !add){
    plot(NULL, xlim = 0:1, ylim = 0:1, axes = FALSE, xlab = "", ylab = "")
    text(0.5, 0.5, labels = "No forecasts available.")
    return(invisible(list(ylim = NULL)))
  }else{
    
    # choose start, end and ylim if not specified:
    if(is.null(start)) start <- ifelse(is.null(horizon), min(dat_eval$forecast_date) - 35, min(dat_eval$forecast_date) - 21)
    if(is.null(end)) end <- ifelse(is.null(horizon), max(dat_eval$forecast_date) + 63, max(dat_eval$forecast_date) + 35)
    if(is.null(ylim)) ylim <- c(ifelse(inc_or_cum == "inc",
                                       0,
                                       0.75*min(c(dat_eval$value.0.025,
                                                  dat_eval$value.point,
                                                  dat_eval$truth), na.rm = TRUE)),
                                max(c(dat_eval$value.0.975,
                                      dat_eval$value.point,
                                      dat_eval$truth), na.rm = TRUE))
    
    # initialize plot if necessary:
    if(!add){
      plot(dat_eval$target_end_date, dat_eval$truth, ylim = ylim, xlim = c(start, end),
           xlab = "time", ylab = "", # paste(inc_or_cum, target_type), 
           col  ="white")
      # horizontal ablines:
      abline(h = axTicks(2), col = "grey")
    }
    # create transparent color:
    col_transp <- modify_alpha(col, alpha.col)
    
    for(i in seq_along(models)){
      dat_eval_m <- dat_eval[dat_eval$model == models[i], ]
      # add forecasts:
      if(show_intervals){
        plot_weekly_bands(dates = c(last_date, dat_eval_m$target_end_date), 
                          lower = c(last_truth, dat_eval_m$value.0.025),
                          upper = c(last_truth, dat_eval_m$value.0.975),
                          separate_all = separate_intervals,
                          col = lighten(col[i], 0.5), border = NA, width = width, shift = shifts[i])
        plot_weekly_bands(dates = c(last_date, dat_eval_m$target_end_date),
                          lower = c(last_truth, dat_eval_m$value.0.25),
                          upper = c(last_truth, dat_eval_m$value.0.75),
                          separate_all = separate_intervals,
                          col = lighten(col[i], 0), border = NA, width = width, shift = shifts[i])
      }
      if(!is.null(forecast_date)) lines(c(last_date, dat_eval_m$target_end_date + shifts[i]), 
                                        c(last_truth, dat_eval_m$value.point), 
                                        col = col[i], lty = "dotted")
      points(dat_eval_m$target_end_date + shifts[i], dat_eval_m$value.point, pch = pch[i], col = col[i], bg = "white")
    }
    
    points(all_dates, all_truths, pch = 15, cex = 0.9)
    lines(all_dates[order(all_dates)], all_truths[order(all_dates)])
    
    
    # mark forecast date if necessary:
    if(!is.null(forecast_date)) abline(v = forecast_date, lty = 2)
    
    if(legend) legend("topleft", col = col, legend = models, pch = pch, bty = "n", cex = 0.8)
    
    # if(!add){
    #   title(paste(horizon, inc_or_cum, target_type, "-", location, "-", model,
    #               ifelse(!is.null(forecast_date), "- Forecast from", ""), forecast_date))
    # }
    
    # return ylim so it can be used in second plot:
    return(invisible(list(ylim = ylim)))
  }
}

# identify extreme forecasts
extreme_forecasts <- function(dat_eval){
  
  col_ordering <- colnames(dat_eval)
  
  dat_eval_max_lower <- aggregate(value.0.025 ~ target_end_date + target + location,
                                  data = dat_eval, FUN = max)
  colnames(dat_eval_max_lower)[4] <- "max.value.0.025"
  
  dat_eval_min_upper <- aggregate(value.0.975 ~ target_end_date + target + location,
                                  data = dat_eval, FUN = min)
  colnames(dat_eval_min_upper)[4] <- "min.value.0.975"
  
  dat_eval <- merge(dat_eval, dat_eval_min_upper, by = c("target_end_date", "location", "target"))
  dat_eval <- merge(dat_eval, dat_eval_max_lower, by = c("target_end_date", "location", "target"))
  
  dat_eval_min_upper <- subset(dat_eval, value.0.975 == min.value.0.975)
  dat_eval_min_upper$model <- "min_upper"
  dat_eval_max_lower <- subset(dat_eval, value.0.025 == max.value.0.025)
  dat_eval_max_lower$model <- "max_lower"
  
  dat_eval_min_upper <- dat_eval_min_upper[, col_ordering]
  dat_eval_max_lower <- dat_eval_max_lower[, col_ordering]
  
  return(list(min_upper = dat_eval_min_upper,
              max_lower = dat_eval_max_lower))
}


letter_in_circle <- function(x, y, letter, col = "black", col_line = "white", cex = 0.8){
  abline(v = x, lty = "dashed", col = col_line)
  draw.circle(x = x, y = y, radius = strwidth("aa", cex = cex)/1.7, border = col, col = "white")
  text(x, y, letter, col = col, cex = cex)
}
