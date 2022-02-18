#### SRW spatial stability figure


#### Figure  - Mean, leverage, and scaled CV for all solutes

library(tidyverse)
library(cowplot)
library(patchwork)

leveragem <- read_csv("leveragem.csv")

#### Nitrate

a <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_NO3, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot() + 
  theme(legend.position="none")


b <- ggplot(leveragem, aes(x = Watershed_Area, y = NO3_leverage, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

c <- ggplot(leveragem, aes(x = Watershed_Area, y = NO3_CV, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

aa <- a + b + c


### Chloride


d <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_Cl, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")



e <- ggplot(leveragem, aes(x = Watershed_Area, y = Cl_leverage, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

f <- ggplot(leveragem, aes(x = Watershed_Area, y = Cl_CV, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

bb <- d + e + f


### Potassium


g <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_K, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")



h <- ggplot(leveragem, aes(x = Watershed_Area, y = K_leverage, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

i <- ggplot(leveragem, aes(x = Watershed_Area, y = K_CV, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

cc <- g + h + i

cc

### Calcium


j <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_Ca, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")



k <- ggplot(leveragem, aes(x = Watershed_Area, y = Ca_leverage, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

l <- ggplot(leveragem, aes(x = Watershed_Area, y = Ca_CV, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

dd <- j + k + l

dd


### Sulfate


m <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_SO4, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")



n <- ggplot(leveragem, aes(x = Watershed_Area, y = SO4_leverage, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

o <- ggplot(leveragem, aes(x = Watershed_Area, y = S04_CV, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

ee <- m + n + o

ee


### Sodium


p <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_Na, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")



q <- ggplot(leveragem, aes(x = Watershed_Area, y = Na_leverage, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

r <- ggplot(leveragem, aes(x = Watershed_Area, y = Na_CV, color = Dependence)) + 
  geom_point(size = 2.5) + 
  theme_cowplot()+ 
  theme(legend.position="none")

ff <- p + q + r

ff

################################################################################################

### Now combining all together


aa/bb/cc/dd/ee/ff
