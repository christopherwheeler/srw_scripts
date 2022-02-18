### Getting Area Normalized Flow for The Three gages for all sampling Dates

### 02203700 - Intrenchment at Constitution Rd

###02203871 - Doolittle at Flat Shoals

###02203900 - SR at Flakes Mill

library(tidyverse)

library(dataRetrieval)


### Below we are getting 2020 discharge at the 3 gages


# Intrenchment at Constitution Rd

siteNumber <- "02203700" 
IntrenchmentInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Raw daily data:
rawDailyDataIntrench <- readNWISdv(siteNumber,parameterCd,
                                   "2020-01-01","2021-01-01")

names(rawDailyDataIntrench)

rawDailyDataIntrench <- rawDailyDataIntrench %>% 
  rename(discharge = X_00060_00003)

rawDailyDataIntrench <- select(rawDailyDataIntrench, -c(X_00060_00003_cd))


ggplot(rawDailyDataIntrench, aes(x = Date, y = discharge)) + 
  geom_path()

### Doolittle Creek at Flat Shoals Road

siteNumber <- "02203831" 
IntrenchmentInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Raw daily data:
rawDailyDataDoolittle <- readNWISdv(siteNumber,parameterCd,
                                    "2020-01-01","2021-01-01")

names(rawDailyDataDoolittle)

rawDailyDataDoolittle <- rawDailyDataDoolittle %>% 
  rename(discharge = X_00060_00003)

rawDailyDataDoolittle <- select(rawDailyDataDoolittle, -c(X_00060_00003_cd))


ggplot(rawDailyDataDoolittle, aes(x = Date, y = discharge)) + 
  geom_path()


### South River at Flakes Mill Road

siteNumber <- "02203900" 
IntrenchmentInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Raw daily data:

rawDailyDataSR <- readNWISdv(siteNumber,parameterCd,
                             "2020-01-01","2021-01-01")

names(rawDailyDataSR)

rawDailyDataSR <- rawDailyDataSR %>% 
  rename(discharge = X_00060_00003)

rawDailyDataSR <- select(rawDailyDataSR, -c(X_00060_00003_cd))


ggplot(rawDailyDataSR, aes(x = Date, y = discharge)) + 
  geom_path()

### Area normalize the discharge column to mm per day
### 1.  First multiply by 86400 and 28320000 to change to mm3 per day
### 2. Then the last number is dividing by the watershed area in 


rawDailyDataIntrench <- rawDailyDataIntrench %>% 
  mutate(discharge = (discharge * 86400 * 28320000) / 27454000000000)

rawDailyDataDoolittle <- rawDailyDataDoolittle %>% 
  mutate(discharge = (discharge * 86400 * 28320000) / 11007000000000)

rawDailyDataSR <- rawDailyDataSR %>% 
  mutate(discharge = (discharge * 86400 * 28320000) / 256400000000000)


### Put The 3 gages into one data frame and check with a graph


sr_normalized_discharge <- bind_rows(rawDailyDataIntrench, rawDailyDataDoolittle, rawDailyDataSR)

ggplot(sr_normalized_discharge, aes(x = Date, y = discharge, color = site_no)) + 
  geom_path()

### Select Sampling Dates out of overall Data frame for 2020

sampling_date_discharge <- sr_normalized_discharge %>% 
  filter (Date == "2020-06-22" | Date == "2020-06-27"| Date == "2020-07-02"| Date == "2020-07-09"| Date == "2020-07-12"| Date == "2020-07-17"| Date == "2020-07-22"| Date == "2020-07-29"| Date == "2020-08-04"| Date == "2020-08-15"| Date == "2020-08-29"| Date == "2020-09-02"| Date == "2020-09-08"| Date == "2020-09-19"| Date == "2020-09-30")

ggplot(sampling_date_discharge, aes(x = Date, y = discharge, color = site_no)) + 
  geom_path() + 
  geom_point() + 
  theme_bw() + 
  ggtitle("SRW Area Normalized Discharge at Gages on Sampling Dates") + 
  ylab("Dishcarge (mm/day)") + 
  theme(plot.title = element_text(hjust = 0.5))

### Arrange by date to make comparing the discharge between the gages easier, then write to csv

sampling_date_discharge_arr <- sampling_date_discharge %>% 
  arrange(Date)

write.csv(sampling_date_discharge_arr,"sampling_date_dicharge.csv", row.names = FALSE)




