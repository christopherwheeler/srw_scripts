### CTW: script for flow duration curve (from USGS gage data)

install.packages("tidyverse")
install.packages("dataRetrieval")

library(tidyselect)
library(dataRetrieval)


# Put in Site number for USGS gage of interest
siteNumber <- "02203900" 
GageInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Raw daily data:
USGSDailyDischarge <- readNWISdv(siteNumber,parameterCd,
                                 "2019-10-01","2020-09-30")

USGSDailyDischarge <- USGSDailyDischarge %>% 
  rename(Discharge = X_00060_00003) %>% 
  dplyr::select(site_no, Date, Discharge)

USGSDailyDischarge <- USGSDailyDischarge %>%  
  arrange(desc(Discharge))

USGSDailyDischarge$rank <- c(1:366)

USGSDailyDischarge <- USGSDailyDischarge %>%  
  mutate(exceedance_prob = (rank/367) * 100)

### Plot Flow Duration Curve
ggplot(USGSDailyDischarge, aes(x = exceedance_prob, y = Discharge)) + 
  geom_point() + 
  geom_path() + 
  theme_bw() + 
  ggtitle("USGS Gage Flow Duration") + 
  xlab("Exceedance Probability") + 
  ylab("Mean Daily Discharge (cfs)") + 
  theme(plot.title = element_text(hjust = 0.5))
