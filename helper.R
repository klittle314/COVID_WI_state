


#make basic count plot

count_plot <- function(dfx,location){
      #distinguish baseline from most recent 14 days
      dfx <- dfx %>% filter(NAME == location)
      cut_date <- max(dfx$Date_reported) - 14
      
      dfx$phase <- ifelse(dfx$Date_reported <= cut_date,"baseline","last 14 days")
     
      dfx_longer <- dfx %>% 
        select(GEO,NAME,Date_reported,POSITIVE_daily,NEGATIVE_daily,Total_daily_tests,POS_pct_daily,phase) %>%
        pivot_longer(cols = -c("GEO","NAME","Date_reported","phase"),
                     names_to = "Measure",
                     values_to = "value")
      
      dfA <- dfx_longer %>% filter(Measure %in% c('NEGATIVE_daily','POS_pct_daily','POSITIVE_daily','Total_daily_tests'))
      dfA$Measure <- factor(dfA$Measure, levels=c("POSITIVE_daily", 'NEGATIVE_daily','Total_daily_tests','POS_pct_daily'))
      
      #create medians
      med_A <- as.vector(tapply(dfA$value,dfA$Measure, median, na.rm=TRUE))
      Measure <- levels(dfA$Measure)
      df.hlines <- data.frame(Measure, med_A)
      
      p1 <- ggplot(data=dfA,aes(x=Date_reported,y=value))+
        theme_bw()+
        geom_point(aes(shape=phase))+
        facet_wrap(~Measure,
                   ncol = 1,
                   scales = 'free_y')+
        labs(title=paste0("Test counts for ", location),
             subtitle="Dashed lines are medians for entire series")+
             
        geom_hline(aes(yintercept=med_A),data=df.hlines,lty=2)+
        
        ylab("")+
        xlab("Date Reported")+
        annotate("rect", fill = "blue", alpha = 0.1, 
                 xmin = cut_date + .5, xmax = max(dfA$Date_reported)+.5,
                 ymin = -Inf, ymax = Inf)+
        scale_shape_discrete(na.translate=FALSE)+
        theme(legend.position = c(0.15, 0.99),
              legend.justification = c("right", "top"),
              legend.text = element_text(size = 6),
              legend.title= element_text(size = 8))
      
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
  
  list_out$start_date <- min(dfx$Date_reported)
  
  list_out$end_date <- max(dfx$Date_reported) - 13
  
  list_out$date_seq <- seq.Date(from = list_out$start_date, to = list_out$end_date, by = "day")
  
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
  df1 <- dfA %>% filter(NAME == location)
  
  list_dates <- seq_dates(df1)
  
  list_df_slope_pars <- lapply(list_dates$date_seq,get_slope_pars,dfx = df1, daycode = daycode0)
  
  df_slopes <- do.call(rbind, list_df_slope_pars)
  
  df_slopes$Date_end <- list_dates$date_seq + 13
  
  p_slopes <- ggplot(data=df_slopes,aes(x=Date_end,y=slope))+
    theme_bw()+
    geom_point()+
    labs(title = paste0('Slope of Pct Positive Tests: 14 day windows for ',location),
         subtitle = "The slope plotted for each day is the slope from the linear fit of \n pct positive daily tests in a 14 day window ending at the day's date",
         caption = '95 percent confidence interval limits for each slope marked by *')+
    geom_point(aes(x=Date_end,y=LCI),shape=8)+
    geom_point(aes(x=Date_end,y=UCI),shape=8)+
    geom_hline(yintercept=0) +
    #ylim(-1,1)+
    xlab("")+
    ylab("")
  
  return(p_slopes)
}