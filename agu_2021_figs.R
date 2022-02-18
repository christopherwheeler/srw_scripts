#### AGU 2021 Poster Script

write_csv(leveragem, "posterleverage.csv")



library(tidyverse)
library(cowplot)

leverage <- read_csv("Leverage_R.csv")

### get rid of BS-1
leveragem <- leverage[-c(1, 18),]

leveragem <- leveragem %>% 
  mutate(Na_CV = Dev_Na/Mean_Na) %>% 
  mutate(NO3_CV = Dev_NO3/Mean_NO3)

### N03 - Area and Leverage
a <- ggplot(posterleverage, aes(x = Watershed_Area, y = NO3_leverage, color = Dependence)) + 
  geom_point(size = 3.5) + 
  theme_bw() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("NO3 Leverage")

### NO3 - Area and Concentration
b <- ggplot(posterleverage, aes(x = Watershed_Area, y = Mean_NO3, color = Dependence)) + 
  geom_point(size = 3.5) + 
  theme_bw() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Mean NO3 (mg/L)")

### stack them
plot_grid(a, b, labels=c("A", "B"), ncol = 1, nrow = 2, scale = 1)

############################################################################################################################################
############################################################################################################################################

### Cl - Area and Leverage
c <- ggplot(leveragem, aes(x = Watershed_Area, y = Cl_leverage)) + 
  geom_point(size = 3.5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Cl Leverage")

### Cl - Area and Concentration
d <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_Cl)) + 
  geom_point(size = 3.5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Mean Cl (mg/L)")

### stack them
plot_grid(c, d, labels=c("A", "B"), ncol = 1, nrow = 2, scale = 1)

############################################################################################################################################
############################################################################################################################################

### Na - Area and Leverage
e <- ggplot(posterleverage, aes(x = Watershed_Area, y = Na_leverage, color = Dependence)) + 
  geom_point(size = 3.5) + 
  theme_bw() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Na Leverage")

### Na - Area and Concentration
f <- ggplot(posterleverage, aes(x = Watershed_Area, y = Mean_Na, color = Dependence)) + 
  geom_point(size = 3.5) + 
  theme_bw() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Mean Na (mg/L)")

### stack them
plot_grid(e, f, labels=c("A", "B"), ncol = 1, nrow = 2, scale = 1)


############################################################################################################################################
############################################################################################################################################

### K - Area and Leverage
g <- ggplot(leveragem, aes(x = Watershed_Area, y = K_leverage)) + 
  geom_point(size = 3.5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("K Leverage")

### K - Area and Concentration
h <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_K)) + 
  geom_point(size = 3.5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Mean K (mg/L)")

### stack them
plot_grid(g, h, labels=c("A", "B"), ncol = 1, nrow = 2, scale = 1)




############################################################################################################################################
############################################################################################################################################

### SO4 - Area and Concentration

i <- ggplot(leveragem, aes(x = Watershed_Area, y = SO4_leverage)) + 
  geom_point(size = 3.5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("SO4 Leverage")

### K - Area and Concentration
j <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_SO4)) + 
  geom_point(size = 3.5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Mean SO4(mg/L)")

### stack them
plot_grid(i, j, labels=c("A", "B"), ncol = 1, nrow = 2, scale = 1)




############################################################################################################################################
############################################################################################################################################

### Ca - Area and Concentration

k <- ggplot(leveragem, aes(x = Watershed_Area, y = Ca_leverage)) + 
  geom_point(size = 3.5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Ca Leverage")

### K - Area and Concentration
l <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_SO4)) + 
  geom_point(size = 3.5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Mean Ca (mg/L)")

### stack them
plot_grid(k, l, labels=c("A", "B"), ncol = 1, nrow = 2, scale = 1)


############################################################################################################################################
############################################################################################################################################
#### Hydrograph with Sampling Dates 

library(dataRetrieval)
vignette("dataRetrieval")

# South River at Flakes Mill Road near Atlanta, GA
siteNumber <- "02203700" 
SRInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Raw daily data:
SR_Daily_Discharge <- readNWISdv(siteNumber,parameterCd,
                                 "2020-06-22","2020-09-30")
names(SR_Daily_Discharge)


SR_Daily_Discharge <- SR_Daily_Discharge %>% 
  rename(discharge = X_00060_00003) %>% 
  select(Date, discharge)


ggplot(SR_Daily_Discharge, aes(x = Date, y = discharge)) + 
  geom_line() + 
  theme_classic()

write_csv(SR_Daily_Discharge, "sr_hydro_dates.csv")

sr_hydro_dates <- read_csv("sr_hydro_dates.csv", 
                           col_types = cols(Date = col_date(format = "%m/%d/%Y")))

stability_time <- read_csv("stability_time.csv", 
                           col_types = cols(Date = col_date(format = "%m/%d/%Y")))

hydro_stab <- left_join(sr_hydro_dates, stability_time, by = "Date")


aa<- ggplot(hydro_stab) + 
  geom_line(aes(x = Date, y = discharge), size = 1) + 
  geom_point(aes(x = Date, y = samp),color = "red", size = 3.5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Date") + 
  ylab("Mean Daily Discharge (cfs)")


############################################################################################################################################
############################################################################################################################################


### Average Solute Stability 

rm(stability)
stability <- read_csv("ave_stability_r.csv")

ggplot(stability, aes(x = Temp_Variance, y = Spatial_Stability, shape = Solute)) + 
  geom_point(size = 6) + 
  theme_bw() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Temporal Variance") + 
  ylab("Spatial Stability")

ggplot(stability, aes(x = Temp_Variance, y = Spatial_Stability, color = Solute)) + 
  geom_point(size = 5) + 
  theme_classic() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Temporal Variance") + 
  ylab("Spatial Stability")

###################################################################################################################################################
############################################################################################################################################

### plot grid for NO3 and Na

plot_grid(a, b, e, f,y, z, ncol = 2, nrow = 3, scale = 1)

### scaled cv for No3 and NA

y <- ggplot(leveragem, aes(x = Watershed_Area, y = NO3_CV, color = Dependence)) + 
  geom_point(size = 3.5) + 
  theme_bw() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("NO3 Scaled CV")


z <- ggplot(leveragem, aes(x = Watershed_Area, y = Na_CV, color = Dependence)) + 
  geom_point(size = 3.5) + 
  theme_bw() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Subcatchment Area (km2)") + 
  ylab("Na Scaled CV")

write_csv(leveragem, "leveragem.csv")

###################################################################################################################################################
############################################################################################################################################

### Revise Independent Sites PCA Figure


### Here I'm gonna try to import the original base chem data set, then filter our just the independent sites, then do PCA

library(readr)
library(readxl)
library(tidyverse)

### import base chem 

setwd("C:/Users/cwhee/Desktop/REVISIONS_091121")

basechem <- read_excel("basechem.xlsx")

### Filter basechem for just independent sites

basechemind <- basechem %>% 
  filter(Site == "BS02" | Site == "BS07" | Site == "BS08" | Site == "BS09" | Site == "BS10" |Site == "BS11" |Site == "BS13" |Site == "BS14" |Site == "BS16" |Site == "BS18")

### Perform PCA and graph

SR_ind_Pr <- prcomp(basechemind[1:7], scale = TRUE)

SR_ind_Pr
summary(SR_ind_Pr)

plot(SR_ind_Pr, type = "l")

biplot(SR_ind_Pr)

biplot(SR_ind_Pr, scale = 0)

str(SR_ind_Pr)

SR_ind_Pr$x

basechemind2 <- cbind(basechemind, SR_ind_Pr$x)

ggplot(basechemind2, aes(PC1, PC2, col = Site, fill = Site)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  theme_bw() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Principal Component 1") + 
  ylab("Principal Component 2")


###################################################################################################################################################
############################################################################################################################################

#### Redo of hysteresis loops


#### SR 76
SR76 <- read_csv("SR76.csv", col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

o <- ggplot(SR76, aes(x = Discharge, y = SPC, color = Hour)) + 
  geom_point(size = 3.5) + 
  scale_color_viridis_c() + 
  geom_path() + 
  theme_bw() + 
  xlab("Discharge (cfs)") + 
  ylab("SPC (uS/cm)") + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17))



### Int 72
Int72 <- read_csv("Int72.csv", col_types = cols(DateTime = col_datetime(format = "%m/%d/%Y %H:%M")))

p <- ggplot(Int72, aes(x = Q, y = Nitrate, color = Hour)) + 
  geom_point(size = 3.5) + 
  scale_color_viridis_c() + 
  geom_path() + 
  theme_bw() + 
  xlab("Discharge (cfs)") + 
  ylab("Nitrate (mg N/L)") +
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17))

### Combined
plot_grid(o, p, labels=c("A", "B"), ncol = 1, nrow = 2, scale = 1)

###################################################################################################################################################
############################################################################################################################################


