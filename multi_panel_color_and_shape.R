### Multi-panel color and shape


#### SRW spatial stability figure - Shapes version

rm(list=ls())

#### Figure  - Mean, leverage, and scaled CV for all solutes

library(tidyverse)
library(cowplot)
library(patchwork)

leveragem <- read_csv("data/leveragem.csv")

#### Nitrate

a <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_NO3, shape = Dependence)) + 
  geom_point(size = 3, alpha = 0.65) + 
  theme_cowplot() + 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank()) + 
  ylab(bquote(NO[3]^" -")) + 
  ggtitle("Mean") +
  theme(plot.title = element_text(hjust = 0.5))



a



b <- ggplot(leveragem, aes(x = Watershed_Area, y = NO3_leverage, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) + 
  ggtitle("Leverage") +
  theme(plot.title = element_text(hjust = 0.5))



b

c <- ggplot(leveragem, aes(x = Watershed_Area, y = NO3_CV, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) + 
  ggtitle("CV") +
  theme(plot.title = element_text(hjust = 0.5))

aa <- a + b + c

aa

### Chloride


d <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_Cl, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank()) + 
  ylab(bquote(Cl^" -"))



e <- ggplot(leveragem, aes(x = Watershed_Area, y = Cl_leverage, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())

f <- ggplot(leveragem, aes(x = Watershed_Area, y = Cl_CV, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())

bb <- d + e + f

bb

### Potassium


g <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_K, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank())+ 
  ylab(bquote(K^"+"))

h <- ggplot(leveragem, aes(x = Watershed_Area, y = K_leverage, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())

i <- ggplot(leveragem, aes(x = Watershed_Area, y = K_CV, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())

cc <- g + h + i

cc

### Calcium


j <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_Ca, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank())+ 
  ylab(bquote(Ca^"2+"))



k <- ggplot(leveragem, aes(x = Watershed_Area, y = Ca_leverage, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())

l <- ggplot(leveragem, aes(x = Watershed_Area, y = Ca_CV, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())

dd <- j + k + l

dd


### Sulfate


m <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_SO4, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank())+ 
  ylab(bquote(SO[4]^"2-"))

n <- ggplot(leveragem, aes(x = Watershed_Area, y = SO4_leverage, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())


o <- ggplot(leveragem, aes(x = Watershed_Area, y = S04_CV, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank())


ee <- m + n + o

ee


### Sodium


p <- ggplot(leveragem, aes(x = Watershed_Area, y = Mean_Na, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(legend.position="none")+ 
  ylab(bquote(Na^"+")) + 
  theme(axis.title.x=element_blank())

q <- ggplot(leveragem, aes(x = Watershed_Area, y = Na_leverage, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot()+ 
  theme(axis.title.y=element_blank())+ 
  theme(legend.position="none") +
  theme(axis.title.x=element_blank())

r <- ggplot(leveragem, aes(x = Watershed_Area, y = Na_CV, shape = Dependence)) + 
  geom_point(size = 2.5, alpha = 0.65) + 
  theme_cowplot() +
  theme(axis.title.x=element_blank())

ff <- p + q + r

ff

### Now combining all together


aa/bb/cc/dd/ee/ff


aa/bb/cc/dd/ee/ff +
  plot_annotation(
    caption = 'Subcatchment Area',
    theme = theme(plot.caption = element_text(size = 18, hjust = 0.5), plot.caption.position = "plot")
  )






