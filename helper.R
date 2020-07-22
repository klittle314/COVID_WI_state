# function to force monotonocity on nominally monotone vector and return differenced series correct with initial value
make_vec <- function(x) {
  #function to take a nominally monotone vector and force monotonicity by back propagation
    index_fix <-  which(diff(x)< 0)
    len_if <- length(index_fix)
    while (len_if > 0){
      index_use <- max(index_fix)
      x[index_use] <- x[index_use + 1]
      index_fix <- which(diff(x) < 0)
      len_if <- length(index_fix)
    }
  
  x_out <- x -lag(x)
  
  x_out[1] <- x[1]
  
  return(x_out)
}

make_vec1 <- function(x){
  x_out <- x -lag(x)
  
  x_out[1] <- x[1]
  
  return(x_out)
}


#make basic count plot

count_plot <- function(dfx,location,date_calc_end, date_calc_start,agg_weekly = FALSE){
      #distinguish baseline from most recent 14 days
      dfx <- dfx %>% filter(NAME == location) %>% select(-GEOID)
      #cut_date <- max(dfx$Date_reported) - 14
      
      if(agg_weekly) {
        dfx <- daily_to_weekly(dfx) %>% 
                  filter(!is.na(week_num)) %>% 
                  rename(Date_reported = week_end)
      }
      
      dfx$phase <- ifelse(dfx$Date_reported < date_calc_start,
                           "history",
                           ifelse(dfx$Date_reported >= date_calc_start & 
                                    dfx$Date_reported <= date_calc_end,
                                  "baseline","post_baseline")
                          )
      
      dfx$phase <-  factor(dfx$phase, levels = c("history","baseline","post_baseline"))
     
      if(!agg_weekly){
          dfx_longer <- dfx %>% 
            select(GEO,NAME,Date_reported,POSITIVE_daily,NEGATIVE_daily,Total_daily_tests,POS_pct_daily,phase) %>%
            pivot_longer(cols = -c("GEO","NAME","Date_reported","phase"),
                         names_to = "Measure",
                         values_to = "value")
          
          dfA <- dfx_longer %>% filter(Measure %in% c('NEGATIVE_daily','POS_pct_daily','POSITIVE_daily','Total_daily_tests'))
          dfA$Measure <- factor(dfA$Measure, levels=c("POSITIVE_daily", 'NEGATIVE_daily','Total_daily_tests','POS_pct_daily'))
          
          #problem on March 30 for positive tests state level, the back check method leads to 100% positive testing.  Replace for plotting.
          if(location == "WI") {
              dfA$value[dfA$Measure == "POS_pct_daily" & dfA$value > 25] <-  NA
          }
      } else {
         dfx_longer <- dfx %>% 
           select(GEO,NAME,Date_reported,week_num,POS_week,NEG_week,TOT_week,POS_pct_week,phase) %>%
           pivot_longer(cols = -c("GEO","NAME","Date_reported","week_num","phase"),
                        names_to = "Measure",
                        values_to = "value")
         dfA <- dfx_longer %>% filter(Measure %in% c("POS_week","NEG_week","TOT_week","POS_pct_week"))
         dfA$Measure <- factor(dfA$Measure, levels=c("POS_week","NEG_week","TOT_week","POS_pct_week"))
                 
      }
     
      
      #create medians
      dfA1 <- dfA %>% filter(phase == 'baseline')
      med_A1 <- as.vector(tapply(dfA1$value,dfA1$Measure, median, na.rm=TRUE))
      Measure <- factor(levels(dfA1$Measure), levels = levels(dfA1$Measure))
      df.hlines <- data.frame(Measure, med_A1)
      
      
       p1 <- ggplot(data=dfA,aes(x=Date_reported,y=value))+
        theme_bw()+
        geom_point(aes(shape=phase),size=rel(2))+
        facet_wrap(~Measure,
                   ncol = 1,
                   scales = 'free_y')+
        labs(title=paste0("Test counts for ", location),
             subtitle=paste0("Dashed lines are medians for baseline period in each series: ",as.character(date_calc_start)," to ",as.character(date_calc_end)))+
             
        geom_hline(aes(yintercept=med_A1),data=df.hlines,lty=2)+
        
        ylab("")+
        xlab("Date Reported")+
        annotate("rect", fill = "blue", alpha = 0.1, 
                 xmin = cut_date + .5, xmax = max(dfA$Date_reported)+.5,
                 ymin = -Inf, ymax = Inf)+
        scale_shape_discrete(na.translate=FALSE)+
        theme(legend.position = c(0.1, 0.99),
              legend.justification = c("right", "top"),
              legend.text = element_text(size = 6),
              legend.title= element_text(size = 8))
      
      #problem on March 30 for positive tests, the back check method leads to 100% positive testing.
      if(location=="WI" & !agg_weekly) {
        p1 <- p1 + labs(caption = "Set aside Pct Positive on 30 March:  cleaning method gives 100% positive")
      }
      
      p1 
}

#functions to make control chart plots



#function to calculate slope, intercept and confidence interval for a regression on 14 days of data
slope_pars <- function(dfx,daycode=daycode0) {
  list_out <- list()
  
  lm1 <- lm(dfx$POS_pct_daily ~ daycode)
  
  list_out$slope <- lm1$coefficients[2]
  
  list_out$intercept <- lm1$coefficients[1]
  
  list_out$conf_int <- confint(lm1,'daycode',level= 0.95)
  
  return(list_out)
}

#sequence of dates:  allow for change in dates
seq_dates <- function(dfx) {
  
  list_out <- list()
 
  list_out$start_date <- min((dfx$Date_reported[!is.na(dfx$POS_pct_daily)]), na.rm=TRUE)
  
  list_out$end_date <- max(dfx$Date_reported) - 13
  
  if(list_out$start_date <= list_out$end_date) {
      list_out$date_seq <- seq.Date(from = list_out$start_date, to = list_out$end_date, by = "day")
  
      } else list_out$date_seq <- vector() #a vector of length zero
  
  return(list_out)
}

#function to return the linear regression parameters as a 1 row dataframe
get_slope_pars <- function(dfx,date_start,daycode) {
  
  
  df_use <- dfx %>% filter(Date_reported >= date_start & Date_reported <= date_start + 13)
  
  ls <- slope_pars(dfx = df_use,daycode = daycode)
  
  df_slope_pars <- data.frame(ls$intercept,
                              ls$slope,
                              ls$conf_int[1],
                              ls$conf_int[2])
  
  names(df_slope_pars) <- c("intercept","slope", "LCI","UCI")
  
  row.names(df_slope_pars) <- NULL
  
  return(df_slope_pars)
}

#function to create the slopes plot
slopes_plot <- function(dfA,location) {
  
  list_slopes <- list()
  
  list_slopes$message <- "Insuffficient data to calculate slopes chart"
  
  list_slopes$plot <- list()
  
if(!(location %in% counties_small_counts)){
  df1 <- dfA %>% filter(NAME == location)
  
  #problem on March 30 for positive tests state level, the back check method leads to 100% positive testing.  Replace for calculating slopes.
  if(location == "WI") {
    df1$POS_pct_daily[df1$POS_pct_daily> 99] <-  NA
  }
  
  list_dates <- seq_dates(df1)
  
  #compute how many positive tests have been seen in the series; we use an arbitrary cut point of 100 on 5-10-2020
  #also set aside the counties with low daily positive counts
  sum_pos_tests <- sum(df1$POSITIVE_daily, na.rm=TRUE)
  
  if(length(list_dates$date_seq)>0 & sum_pos_tests >= min_n_pos_tests_slope_chart) {
  
      list_df_slope_pars <- lapply(list_dates$date_seq,get_slope_pars,dfx = df1, daycode = daycode0)
  
      df_slopes <- do.call(rbind, list_df_slope_pars)
  
      df_slopes$Date_end <- list_dates$date_seq + 13
      
      caption1 <- "95 percent confidence interval limits for each slope marked by *"
      
      if(location=="WI") {
        caption1 <- paste0(caption1,"; Pct Pos = 100% omitted for 3/30/2020")
      }  
  
      p_slopes <- ggplot(data=df_slopes,aes(x=Date_end,y=slope))+
        theme_bw()+
        geom_point()+
        labs(title = paste0('Slope of Pct Positive Tests: 14 day windows for ',location),
             subtitle = "The slope plotted for each day is the slope from the linear fit of \n pct positive daily tests in a 14 day window ending at the day's date",
             caption = caption1)+
        geom_point(aes(x=Date_end,y=LCI),shape=8)+
        geom_point(aes(x=Date_end,y=UCI),shape=8)+
        geom_hline(yintercept=0) +
        #ylim(-1,1)+
        xlab("")+
        ylab("")
      
        list_slopes$message <- "Sufficient data to calculate at least one slope"
        
        list_slopes$plot <- p_slopes
  }
  #browser()
 }  
 return(list_slopes)
}


#function to create the per cent control charts.  Function as of 5/10/2020 is specific to % positive daily tests parameter.

p_control_chart_plots <- function(dfA,location, date_calc_end, date_calc_start) {
      #browser()
      list_control_charts <- list()
      
      list_control_charts$message <- "Insufficient data to create % control charts"
      
      list_control_charts$plots <- list()
      
      list_control_charts$n_records <- integer()
      
      
  if(!(location %in% counties_small_counts)) {  
      df1 <- dfA %>% filter(NAME == location)
      
      #problem on March 30 for positive tests state level, the back check method leads to 100% positive testing.  Use NA for plotting.
      if(location == "WI"){
        df1$POS_pct_daily[df1$POS_pct_daily > 25] <- NA
      }  
      
      
      #Note that the start_date is specified as of 5/10/2020 as function of pct positive tests  
      
      #list_dates <- seq_dates(df1)
      
      df1$phase <- ifelse(df1$Date_reported < date_calc_start,
                          "history",
                          ifelse(df1$Date_reported >= date_calc_start & 
                                   df1$Date_reported <= date_calc_end,
                                 "baseline","post_baseline")
      )
      
      df1$phase <-  factor(df1$phase, levels = c("history","baseline","post_baseline"))
      
      df1$pi <- df1$POS_pct_daily/100
      
      df0 <- df1 %>% filter(phase != "history")
      
      #compute how many positive tests have been seen in the series; we use an arbitrary cut point of 100 on 5-10-2020
      sum_pos_tests <- sum(df0$POSITIVE_daily, na.rm=TRUE)
      
      #cut_date <- max(df0$Date_reported) - 14
      
      if(nrow(df0) >= min_n_control_charts & sum_pos_tests >= min_n_pos_tests){
          list_control_charts$message <- paste0("Sufficient data to display control charts: ",
                                                    nrow(df0)," records.")
          
          list_control_charts$n_records <- nrow(df0)
          
          df0_baseline <- df0 %>% filter(phase == "baseline")
          
          #compute binomial limits
          df1$qi <- 1 - df1$pi
          
          p_bar <-sum(df0_baseline$POSITIVE_daily)/sum(df0_baseline$Total_daily_tests)
          
          df0$sigma_pi <- sqrt(p_bar*(1-p_bar)/df0$Total_daily_tests)
          
          df0$p_UCL <- p_bar + 3*df0$sigma_pi
          
          df0$p_LCL <- p_bar - 3*df0$sigma_pi
          
          #update df0_baseline with the control limits for p chart
          df0_baseline <- df0 %>% filter(phase == "baseline")
          
          #compute z scores and assess variation in z scores for baseline period
          df0_baseline$zi <- (df0_baseline$pi - p_bar)/df0_baseline$sigma_pi
          
          Rzi <- abs(diff(df0_baseline$zi))
          #could change to median calculation
          sigma_zi <- mean(Rzi)/1.128
          
          df0$Laney_sigma <- df0$sigma_pi*sigma_zi
          
          df0$pprime_LCL <- p_bar - 3*df0$Laney_sigma
          
          df0$pprime_UCL <- p_bar + 3*df0$Laney_sigma
          
          #compute I chart values
          #Issue:   is the mean of the chart the unweighted average of the daily pi's or weighted for sample size?
          #see Laney:  p. 533 who uses the weighted average for the center line of the I chart.  
          #justification is that the weighted average will have less bias than unweighted average.  We adopt this here.
          p_barI <- mean(df0_baseline$pi)  #assumes equal weighting of the Lot proportions. In general this will differ from p_bar
          
          Rpi <- abs(diff(df0_baseline$pi))
          
          sigma_aver_Rpi <- mean(Rpi)/1.128
          
          sigma_median_Rpi <- median(Rpi)/0.9554
          
          pchart <- ggplot(data=df1,aes(x=Date_reported,y=100*pi)) +
            theme_bw()+
            geom_point(aes(shape=phase),size=rel(2))+
            geom_line()+
            labs(title="p control chart for Pct Positive tests",
                 x=" ",
                 y="%",
                 subtitle=paste0("Limits based on binomial variation using p-bar in baseline period, ", as.character(date_calc_start),":",as.character(date_calc_end))) +
            geom_hline(yintercept=100*p_bar)+
            geom_line(data = df0, aes(x=Date_reported,y=100*p_LCL),linetype='dashed')+
            geom_line(data = df0, aes(x=Date_reported,y=100*p_UCL),linetype='dashed')+
            geom_hline(yintercept= 5, colour="blue",lty="dotted")+
            ylim(0,NA)+
            annotate("rect", fill = "blue", alpha = 0.1, 
                     xmin = cut_date + .5, xmax = max(df0$Date_reported)+.5,
                     ymin = -Inf, ymax = Inf)+
            scale_shape_discrete(na.translate=FALSE)+
            theme(legend.position = "none") #+
            # theme(legend.position = c(0.99, 0.99),
            #       legend.justification = c("right", "top"),
            #       legend.text = element_text(size = 6),
            #       legend.title= element_text(size = 8))
          
          #pchart
          
          #ichart
          ichart <-ggplot(data=df1,aes(x=Date_reported,y=100*pi)) +
            theme_bw()+
            geom_point(aes(shape=phase),size=rel(2))+
            geom_line()+
            labs(title="Individuals control chart for Pct Positive tests",
                 x="",
                 y="%",
                 subtitle="Limits based between day variation ignoring test counts during baseline period") +
            #Laney suggests the center line will be p_bar not p_barI
            geom_hline(yintercept=100*p_bar)+
            geom_hline(yintercept= 5, colour="blue",lty="dotted")+
            ylim(0,NA)+
            annotate("rect", fill = "blue", alpha = 0.1, 
                     xmin = cut_date + .5, xmax = max(df0$Date_reported)+.5,
                     ymin = -Inf, ymax = Inf)+
            scale_shape_discrete(na.translate=FALSE)+
            # theme(legend.position = c(0.99, 0.99),
            #       legend.justification = c("right", "top"),
            #       legend.text = element_text(size = 6),
            #       legend.title= element_text(size = 8))
            theme(legend.position = "none")
          
          ichart_Median <- ichart+ geom_line(data = df0,aes(x=Date_reported,y=100*(p_bar - 3*sigma_median_Rpi)),linetype='dashed')+
            geom_line(data = df0, aes(x=Date_reported,y=100*(p_bar + 3*sigma_median_Rpi)),linetype='dashed')+
            labs(caption="Limits based on median moving range")
          #ichart_Median
          
          ichart_Mean <- ichart+ geom_line(data = df0, aes(x=Date_reported,y=100*(p_bar - 3*sigma_aver_Rpi)),linetype='dashed')+
            geom_line(data = df0, aes(x=Date_reported,y=100*(p_bar + 3*sigma_aver_Rpi)),linetype='dashed') +
            labs(caption="Limits use median moving range; option is to use average moving range")
          #ichart_Mean
          
          #make p prime chart
          pprime_chart <-ggplot(data=df1,aes(x=Date_reported,y=100*pi)) +
            theme_bw()+
            geom_point(aes(shape=phase),size=rel(2))+
            geom_line()+
            labs(title="p' control chart for Pct Positive",
                 x="",
                 y="%",
                 subtitle="Limits based on Laney calculations from baseline period (within and between day variation)") +
            geom_hline(yintercept=100*p_bar)+
            geom_line(data = df0, aes(x=Date_reported,y=100*pprime_LCL),linetype='dashed')+
            geom_line(data = df0, aes(x=Date_reported,y=100*pprime_UCL),linetype='dashed')+
            geom_hline(yintercept= 5, colour="blue",lty="dotted")+
            ylim(0,NA)+
            scale_shape_discrete(na.translate=FALSE)+
            theme(legend.position = "none")+
            # theme(legend.position = c(0.99, 0.99),
            #       legend.justification = c("right", "top"),
            #       legend.text = element_text(size = 6),
            #       legend.title= element_text(size = 8))+
            annotate("rect", fill = "blue", alpha = 0.1, 
                     xmin = cut_date + .5, xmax = max(df0$Date_reported)+.5,
                     ymin = -Inf, ymax = Inf)
          
          #pprime_chart
          
          #number of tests per day
          ptests <- ggplot(data=df1,aes(x=Date_reported,y=Total_daily_tests))+
            theme_bw()+
            geom_point(aes(shape=phase),size=rel(2))+
            geom_line()+
            labs(title="Total Daily Tests",
                 subtitle="Baseline Median Daily Tests, dashed line",
                 x="",
                 y="")+
            geom_hline(yintercept=median(df1$Total_daily_tests[df1$phase == "baseline"]),lty="dashed")+
            annotate("rect", fill = "blue", alpha = 0.1, 
                     xmin = cut_date + .5, xmax = max(df0$Date_reported)+.5,
                     ymin = -Inf, ymax = Inf)+
            scale_shape_discrete(na.translate=FALSE) #+
            # theme(legend.position = c(0.05, 0.99),
            #       legend.justification = c("left", "top"),
            #       legend.text = element_text(size = 6),
            #       legend.title= element_text(size = 8))
          
          #ptests
         
          list_control_charts$plot  <- list(pchart, ichart_Median, pprime_chart)
          
      }
  }  
  return(list_control_charts)
}

#function to create a c-chart for counties with low counts
c_control_chart_plot <- function(dfA,location,date_calc_end,date_calc_start, agg_weekly) {
  #Date_limit_small_counts <- as.Date("2020-05-11") will serve as initial cut point for control limits
  #requre at least 10 positive cases else return message 'insufficient data to make control chart'
  
  df0 <- dfA %>% filter(NAME == location)
  
  if(agg_weekly) {
    df0 <- daily_to_weekly(df0) %>% 
      filter(!is.na(week_num)) %>% 
      rename(Date_reported = week_end)
  }
  
  df0$phase <- ifelse(df0$Date_reported < date_calc_start,
                      "history",
                      ifelse(df0$Date_reported >= date_calc_start & 
                               df0$Date_reported <= date_calc_end,
                             "baseline","post_baseline")
  )
  
  df0$phase <-  factor(df0$phase, levels = c("history","baseline","post_baseline"))
  
  df0_baseline <- df0 %>% filter(phase=="baseline")
  
  if(!agg_weekly){
  
        cchart_mean <- mean(df0_baseline$POSITIVE_daily)
        
        df0$value <- df0$POSITIVE_daily
        
        title1 <- paste0('c-control chart of daily positive cases for ', location,' County')
        
  } else {
        cchart_mean <- mean(df0_baseline$POS_week)
        
        df0$value <- df0$POS_week
        
        title1 <- paste0('c-control chart of weekly positive cases for ', location,' County')
  }
  
        cchart_UCL <- cchart_mean + 3*sqrt(cchart_mean)
  
        cchart_LCL <- max(0,cchart_mean - 3*sqrt(cchart_mean), na.rm=TRUE)
  
  p_c_chart <- ggplot(data=df0,aes(x=Date_reported, y=value))+
                theme_bw()+
                geom_point(aes(shape=phase),size=rel(3))+
                geom_line()+
                labs(title = title1,
                     subtitle = paste0("mean and control limit(s) based on baseline period, ", as.character(date_calc_start),":",as.character(date_calc_end)))+
                ylab("")+
                xlab("Date Reported")+
                geom_hline(yintercept = cchart_mean)+
                geom_hline(yintercept = cchart_UCL, linetype = "dashed")+
                annotate("rect", fill = "grey", alpha = 0.1, 
                         xmin = Date_limit_small_counts + .5, xmax = max(dfA$Date_reported)+.5,
                         ymin = -Inf, ymax = Inf)+
                scale_shape_discrete(na.translate=FALSE)+
                theme(legend.position = c(0.1, 0.99),
                      legend.justification = c("right", "top"),
                      legend.text = element_text(size = 6),
                      legend.title= element_text(size = 8))
  
 return(p_c_chart)
  
}

#######################daily to weekly functions

rolling_week <- function(date_vector, end_date = as.Date(Sys.Date())){
  #Coerce to dates
  date_vector <- as.Date(date_vector)
  end_date = as.Date(end_date)
  
  min_date <- min(date_vector)
  
  period_length <- as.numeric(difftime(end_date, min_date))
  period_weeks <- floor(period_length / 7)
  
  week_ends <-  c(end_date + lubridate::days(1), end_date - lubridate::weeks(1:period_weeks))
  
  out <- period_weeks - cut(date_vector, breaks = week_ends, include_lowest = TRUE, right = TRUE, label = FALSE) + 1
  
  return(out)
}


#function accepts df = df1_small structure, specific to a location
daily_to_weekly <- function(df) {
  last_date <- max(df$Date_reported)  
  df1 <- df %>% 
    mutate(week_num = rolling_week(date_vector = Date_reported,
                                   end_date = last_date)
    ) %>% 
    group_by(GEO,NAME,week_num) %>% 
    summarize(
      NEG_week = sum(NEGATIVE_daily),
      POS_week = sum(POSITIVE_daily),
      TOT_week = sum(Total_daily_tests),
      POS_pct_week = 100*POS_week/TOT_week,
      week_end = max(Date_reported)
    )
}
###################################################