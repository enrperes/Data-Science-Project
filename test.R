## Libraries ## ----
library(tidyverse)
library(dplyr)
library(ggplot2)
library(png)
library(ggpubr)
library(cowplot)


## IMPORT ## ----
data <- read.csv("2022RG_final.csv") 
court = readPNG("clay-court.png")
half_court = readPNG("half-clay-court.png")
court_test = read.csv("courtdimensions.csv") # test dataset to calibrate the court image



## DATA ## ----
serves_djoko <- data %>%
  filter(Player == "Player1" & Shot_Type %in% c("1st Serve", "2nd Serve")) %>%
  filter(Y2 > -6.4 & Y2 < 6.4 & (Y2< -1 | Y2 > 1)) %>%
  select(Player, Shot_Side, Shot_Type, X2, Y2, Serve_Speed) %>%
  mutate(X2 = ifelse(Y2 < 0, -X2, X2))

serves_nadal <- data %>%
  filter(Player == "Player2" & Shot_Type %in% c("1st Serve", "2nd Serve")) %>%
  filter(Y2 > -6.4 & Y2 < 6.4 & (Y2< -1 | Y2 > 1)) %>%
  select(Player, Shot_Side, Shot_Type, X2, Y2, Serve_Speed) %>%
  mutate(X2 = ifelse(Y2 < 0, -X2, X2))



## Nadal Serve Plot ## ----
nadal_serve_plot = ggplot(serves_nadal, aes(x=X2, y= abs(Y2)))+
  stat_density_2d(aes(fill = after_stat(density)),
                  geom = "raster",
                  contour = FALSE, 
                  h = c(1, 2) # control x and y bandwidths of the estimate.
  )+
  scale_fill_gradientn(colors = c("transparent", # scale that is transparent on the smallest end of the range
                                  viridis::viridis_pal(alpha = 0.5, 
                                                       option = "turbo")(10)))+  
  geom_point(
    colour = "#2e1d2c", alpha = 0.5
  ) + 
  theme_void()+
  theme(aspect.ratio = 1, legend.position = "none")+
  xlim(c(-6, 6)) + 
  ylim(c(0, 12))


## Djokovic Serve plot ## ----
djoko_serve_plot = ggplot(serves_djoko, aes(x=X2, y=Y2))+
  stat_density_2d(aes(fill = after_stat(density)),
                  geom = "raster",
                  contour = FALSE,
                  h = c(1, 2)
  )+
  scale_fill_gradientn(colors = c("transparent", 
                                  viridis::viridis_pal(alpha = 0.5, 
                                                       option = "turbo")(10)))+ 
  geom_point(
    color = "#5d7294"
  )+
  theme_void()+
  theme(aspect.ratio = 1, legend.position = "none")+
  xlim(c(-6, 6)) + 
  ylim(c(0, 12))
  #ggtitle("Djokovic service position", )



## Court Calibration ## ----
test_Hcourt_plot = ggplot(court_test, aes(x=X2, y=Y2))+
  geom_point() + 
  theme_void()+
  theme(aspect.ratio = 1) + 
  xlim(c(-6, 6)) + 
  ylim(c(0, 12))

test_Fcourt_plot = ggplot(court_test, aes(x=X2, y=Y2))+
  geom_point() + 
  theme_void()+
  theme(aspect.ratio = 2) + 
  xlim(c(-6, 6)) + 
  ylim(c(-12, 12))


## DRAW PLOT ## ----
ggdraw()+
  draw_image(half_court, scale = 1)+
  draw_plot(djoko_serve_plot, x=0, y=-0.055, scale=0.85) 
#draw_plot(djoko_serve_plot, scale = 0.85)

