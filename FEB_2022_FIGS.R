###################################################################################################################
### Finalized SRW FIGS (Feb 19 2022)###########################################################

rm(list=ls())

library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(corrgram)
library(dataRetrieval)
library(GGally)
library(plotly)
library(lares)
library(WaveletComp)
library(cowplot)
library(patchwork)

setwd("C:/Users/Christopher/Desktop/R_Directory/REVISION_DATA_2022")

### List of baseflow figs

#### Figure  - Map in ArcMap (ISC as gradient) - DONE!



########################################################################################################
########################################################################################################

### Figure spatial stability below discharge and sampling dates 

### First plot dicharge and sampling dates

rm(sr_hydro_dates)

sr_hydro_dates <- read_csv("sr_hydro_dates.csv", 
                           col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                            discharge = col_number(), samp = col_number()))

sr_hydro_dates <- sr_hydro_dates %>% 
  mutate(discharge = discharge* 0.028316846592) %>% 
  mutate(samp = samp* 0.028316846592)

a<- ggplot(sr_hydro_dates) + 
  geom_line(aes(x = Date, y = discharge), size =0.75) + 
  geom_point(aes(x = Date, y = samp),color = "red", size = 2.1) + 
  theme_cowplot() + 
  ylab(bquote('Discharge '(m^3/s))) 

a

### Now do spatal stability with points and connected lines

rm(line_graph)

line_graph <- read_csv("sptial_stability_line_graph.csv", 
                       col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                        spatial_stability = col_number()))



line_graph$Solute <- as.factor(line_graph$Solute)



levels(line_graph$Solute) <- list(Sulfate = "S04^2-", 
                                  Calcium = "Ca^2+", 
                                  Potassium = "K^+", 
                                  Nitrate = "NO3^-" ,
                                  Sodium = "Na^+", 
                                  Chloride = "Cl^-")



b <- ggplot(line_graph, aes(x = Date, y = spatial_stability, color = Solute)) + 
  geom_point() + 
  geom_path(size=1) + 
  theme_cowplot() + 
  ylab("Spatial Stability") +
  scale_color_brewer(palette = "BrBG",
                     name  ="Solute",
                     breaks=c("Sulfate", "Calcium", "Potassium", "Nitrate", "Sodium","Chloride"),
                     labels=c(bquote(SO[4]^"2-"), bquote(Ca^"2+"), bquote(K^"+"),bquote(NO[3]^" -"),  bquote(Na^"+"), bquote(Cl^" -"))) +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank())


b


#### two panel

b / a




########################################################################################################
########################################################################################################

### Figure - temporal variance versus spatial stability (DONE)

#### Figure  - Final independent sites PCA (DONE)


### Here I'm gonna try to import the original base chem data set, then filter our just the independent sites, then do PCA

library(readr)

library(tidyverse)

library(readxl)

library(RColorBrewer)

### import base chem 

setwd("C:/Users/Christopher/Desktop/R_Directory/REVISION_DATA_2022")

basechem <- read_csv("basechem.csv")

### Filter basechem for just independent sites

basechemind <- basechem %>% 
  filter(Site == "Trib9" | Site == "Trib8" | Site == "Trib7" | Site == "Trib6" | Site == "Trib5a" |Site == "Trib1" |Site == "Trib3a" |Site == "Trib2" |Site == "Trib4a" |Site == "SR1")

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
  theme_cowplot() + 
  scale_fill_brewer(palette = "Paired")



############################################################################################################
############################################################################################################



#### Figure  - Mean, leverage, and scaled CV for all solutes


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

averages <- read_csv("SRW_Solute_Averages.csv", 
                     col_types = cols(Temp_Variance = col_number(), 
                                      Spatial_Stability = col_number(), 
                                      subcatchment_synchrony = col_number()))


averages$Solute <- as.factor(averages$Solute)


levels(averages$Solute) 



y <- ggplot(averages, aes(x = Temp_Variance, y = Spatial_Stability, shape = Solute)) + 
  geom_point(size = 6, alpha = 0.8) + 
  theme_cowplot() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Temporal Variance") + 
  ylab("Spatial Stability") + 
  theme(legend.position="none") 


h <- ggplot(averages, aes(x = Spatial_Stability, y = subcatchment_synchrony, shape = Solute)) + 
  geom_point(size = 6, alpha = 0.8) + 
  theme_cowplot() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Temporal Variance") + 
  ylab("Spatial Stability") + 
  theme(legend.position="none")

h


z <- ggplot(averages, aes(x = Temp_Variance, y = subcatchment_synchrony, shape = Solute)) + 
  geom_point(size = 6, alpha = 0.8) + 
  theme_cowplot() + 
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17)) +
  xlab("Temporal Variance") + 
  ylab("Subcatchment Synchrony") + 
  scale_shape_discrete(name  ="Solute",
                       breaks=c("Ca", "Cl", "K", "N03", "Na","S04"),
                       labels=c(bquote(Ca^"2+"), bquote(Cl^" -"), bquote(K^"+"), bquote(NO[3]^" -"), bquote(Na^"+"),bquote(SO[4]^"2-"))) 



y + z
