library(readr)
library(tidyverse)
library(GGally)
library(gridExtra)

#Symptoms 	Downward trajectory of influenza-like illnesses (ILI) reported within a 14-day period. 	Red indicator = no progress
#Symptoms 	Downward trajectory of COVID-like syndromic cases reported within a 14-day period. 	Red indicator = no progress
#Cases 	Downward trajectory of positive tests as a percent of total tests within a 14-day period. 	Red indicator = no progress
#Hospitals 	Criteria are currently under development in partnership with Wisconsin healthcare stakeholders.


## Example of Control Chart applied to percent positive tests
State_data_4_29_2020 <- read_csv("State_data_4-29-2020_1.csv")
View(State_data_4_29_2020)

head(State_data_4_29_2020$LoadDttm)

df1 <- State_data_4_29_2020

df1 <- df1 %>% filter(GEO %in% c('State','County'))

df1$Date_Time <- df1$LoadDttm/1000

class(df1$Date_Time) <- c('POSIXt','POSIXct')
summary(df1$Date_Time)

df1$Date_reported <- as.Date(df1$Date_Time)
summary(df1$Date_reported)

summary(df1)
#most of the variables are blank

#negative value on 30 March  check
df_test <- df1 %>% filter(Date_reported > as.Date("2020-03-27") & Date_reported < as.Date("2020-04-02")) %>%
              select(GEO,NAME,Date_reported,NEGATIVE,POSITIVE) %>% filter(GEO == "State")

#appears that negative tests on 29 and 30 March were flipped:  29 Mar value of 16550 and 30 Mar value of 15856.
#

df1_small_state <- df1 %>% select(GEO,NAME,Date_reported,NEGATIVE,POSITIVE) %>% filter(GEO == 'State')
df1_small_state$NEGATIVE[df1_small_state$Date_reported == as.Date("2020-03-29")] <- 15856
df1_small_state$NEGATIVE[df1_small_state$Date_reported == as.Date("2020-03-30")] <- 16550
#now pull only the key variables for initial analysis


df1_small_state <- df1_small_state %>%
                  group_by(NAME) %>% 
  mutate(lag_POSITIVE=lag(POSITIVE)) %>% mutate(POSITIVE_daily = POSITIVE-lag_POSITIVE) %>%
  mutate(lag_NEGATIVE=lag(NEGATIVE)) %>% mutate(NEGATIVE_daily = NEGATIVE - lag_NEGATIVE) %>%
  mutate(Total_daily_tests = POSITIVE_daily + NEGATIVE_daily) %>%
  mutate(POS_pct_daily = 100*(POSITIVE_daily/Total_daily_tests)) 


df1_small_state_longer <- df1_small_state %>% pivot_longer(cols = -c("GEO","NAME","Date_reported"),
                                               names_to = "Measure",
                                               values_to = "value")

dfA <- df1_small_state_longer %>% filter(Measure %in% c('NEGATIVE_daily','POS_pct_daily','POSITIVE_daily','Total_daily_tests'))
dfA$Measure <- factor(dfA$Measure, levels=c("POSITIVE_daily", 'NEGATIVE_daily','Total_daily_tests','POS_pct_daily'))

#create medians
med_A <- as.vector(tapply(dfA$value,dfA$Measure, median, na.rm=TRUE))
Measure <- levels(dfA$Measure)
df.hlines <- data.frame(Measure, med_A)


p1 <- ggplot(data=dfA,aes(x=Date_reported,y=value))+
        theme_bw()+
        geom_point()+
        facet_wrap(~Measure,
                   ncol = 1,
                   scales = 'free_y')+
        ggtitle("Test counts")+
        geom_hline(aes(yintercept=med_A),data=df.hlines,lty=2)

p1  
####################set up the charts
df0 <- df1_small_state %>% filter(Date_reported >= as.Date("2020-03-25"))

df0$pi <- df0$POS_pct_daily/100

df0_baseline <- df0 %>% filter(Date_reported <= max(Date_reported) - 14)

#compute binomial limits
df0$qi <- 1 - df0$pi
p_bar <-sum(df0_baseline$POSITIVE_daily)/sum(df0_baseline$Total_daily_tests)
df0$sigma_pi <- sqrt(p_bar*(1-p_bar)/df0$Total_daily_tests)
df0$p_UCL <- p_bar + 3*df0$sigma_pi
df0$p_LCL <- p_bar - 3*df0$sigma_pi

#compute z scores and assess variation in z scores
df0$zi <- (df0$pi - p_bar)/df0$sigma_pi
Rzi <- abs(diff(df0$zi[df0$Date_reported <= max(df0$Date_reported)-14]))
sigma_zi <- mean(Rzi)/1.128
df0$Laney_sigma <- df0$sigma_pi*sigma_zi
df0$pprime_LCL <- p_bar - 3*df0$Laney_sigma
df0$pprime_UCL <- p_bar + 3*df0$Laney_sigma

#compute I chart values
p_barI <- mean(df0_baseline$pi)  #assumes equal weighting of the Lot proportions
Rpi <- abs(diff(df0_baseline$pi))
sigma_aver_Rpi <- mean(Rpi)/1.128
sigma_median_Rpi <- median(Rpi)/0.9554

pchart <- ggplot(data=df0,aes(x=Date_reported,y=100*pi)) +
  theme_bw()+
  geom_point(size=rel(2))+
  geom_line()+
  labs(title="p control chart for Pct Positive tests",
       x="Date",
       y="Per cent Positive",
       subtitle="Limits based on binomial variation, scaled by n each day; baseline 14 days prior to end of series") +
  geom_hline(yintercept=100*p_bar)+
  geom_line(aes(x=Date_reported,y=100*p_LCL),linetype='dashed')+
  geom_line(aes(x=Date_reported,y=100*p_UCL),linetype='dashed')+
  ylim(0,20)

pchart

#ichart
ichart <-ggplot(data=df0,aes(x=Date_reported,y=100*pi)) +
  theme_bw()+
  geom_point(size=rel(2))+
  geom_line()+
  labs(title="Individuals control chart for Pct Positive tests",
       x="Date",
       y="Per cent Positive",
       subtitle="Limits based on day to day variation ignoring number of tests; baseline 14 days prior to end of series") +
  geom_hline(yintercept=100*p_barI)+
  ylim(0,20)

ichart_Median <- ichart+ geom_line(aes(x=Date_reported,y=100*(p_barI - 3*sigma_median_Rpi)),linetype='dashed')+
  geom_line(aes(x=Date_reported,y=100*(p_barI + 3*sigma_median_Rpi)),linetype='dashed')+
  labs(caption="Limits based on median moving range")
ichart_Median

ichart_Mean <- ichart+ geom_line(aes(x=Date_reported,y=100*(p_barI - 3*sigma_aver_Rpi)),linetype='dashed')+
  geom_line(aes(x=Date_reported,y=100*(p_barI + 3*sigma_aver_Rpi)),linetype='dashed') +
  labs(caption="Limits based on average moving range")
ichart_Mean

#make p prime chart
pprime_chart <-ggplot(data=df0,aes(x=Date_reported,y=100*pi)) +
  theme_bw()+
  geom_point(size=rel(2))+
  geom_line()+
  labs(title="p' control chart for Pct Positive",
       x="Date",
       y="Per cent Positive",
       subtitle="Limits based on Laney calculations; baseline 14 days prior to end of series") +
  geom_hline(yintercept=100*p_bar)+
  geom_line(aes(x=Date_reported,y=100*pprime_LCL),linetype='dashed')+
  geom_line(aes(x=Date_reported,y=100*pprime_UCL),linetype='dashed')+
  ylim(0,20)

pprime_chart

#number of tests per day
ptests <- ggplot(data=df0,aes(x=Date_reported,y=Total_daily_tests))+
          theme_bw()+
          geom_point(size=rel(2))+
          geom_line()+
          labs(title="Total Daily Tests",
            subtitle="Median Daily Tests, dashed line",
            x="Date",
            y="Test Counts")+
      geom_hline(yintercept=median(df0$Total_daily_tests),lty="dashed")
 
ptests

grid.arrange(pprime_chart,ptests)

#Method notes:  Establish baseline center line and limits using Laney, up to N - 14.  Extend the average line
#and the limits based on the sample size of the new points and the previous average.   Look for evidence of
#shift relative to the limits.  What is the 'algorithm' for updating the chart?  Do you need human intervention?


