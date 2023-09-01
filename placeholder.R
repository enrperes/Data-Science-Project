
ggdraw()+
  draw_image(court, scale = 1)+
  draw_plot(nadal_serve_plot, x=0, y=0, scale=1)


nadal_serve_plot <- serves_nadal %>%
  ggplot(aes(x = X2, y = Y2)) +
  geom_point() +
  theme_dark()+
  theme(aspect.ratio = 2)


  xlim(c(-5, 5)) + 
  ylim(c(-12, 12))
  
   scale_fill_distiller(palette = "Set1", direction = -1)
   
   
   
   
   
   # ----------------------------------------
   
   library(tidyverse)
   library(dplyr)
   library(ggplot2)
   library(png)
   library(ggpubr)
   library(cowplot)
   
   data <- read.csv("2022RG_final.csv") 
   court = readPNG("clay-court.png")
   half_court = readPNG("half-clay-court.png")
   court_test = read.csv("courtdimensions.csv") # test dataset to calibrate the court image
   
   
   ## DATA ##
   serves_djoko <- data %>%
     filter(Player == "Player1" & Shot_Type %in% c("1st Serve", "2nd Serve")) %>%
     filter(Y2 > -6.4 & Y2 < 6.4 & (Y2< -1 | Y2 > 1)) %>%
     select(Player, Shot_Side, Shot_Type, X2, Y2, Serve_Speed)
   
   serves_nadal <- data %>%
     filter(Player == "Player2" & Shot_Type %in% c("1st Serve", "2nd Serve")) %>%
     filter(Y2 > -6.4 & Y2 < 6.4 & (Y2< -1 | Y2 > 1)) %>%
     select(Player, Shot_Side, Shot_Type, X2, Y2, Serve_Speed) 
   
   
   ## Nadal Serve Plot ##
   nadal_serve_plot = ggplot(serves_nadal, aes(x=X2, y=Y2))+
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
     theme(aspect.ratio = 2, legend.position = "none")+
     xlim(c(-6, 6)) + 
     ylim(c(-12, 12))
   
   ## Djokovic Serve plot ##
   djoko_serve_plot = ggplot(serves_djoko, aes(x=X2, y=Y2))+
     stat_density_2d(aes(fill = after_stat(density)),
                     geom = "raster",
                     contour = FALSE,
                     alpha = 0.5, 
                     h = c(1, 2)
     )+
     scale_fill_distiller(palette = "Set1", direction = -1)+
     geom_point(
       color = "#5d7294"
     )+
     theme_void()+
     theme(aspect.ratio = 2, legend.position = "none")+
     xlim(c(-6, 6)) + 
     ylim(c(-12, 12))
   
   ## Court Calibration ##
   test_serve_plot = ggplot(court_test, aes(x=X2, y=Y2))+
     geom_point() + 
     theme_void()+
     theme(aspect.ratio = 2) + 
     scale_fill_distiller(palette = 4, direction = -1)+
     xlim(c(-6, 6)) + 
     ylim(c(-12, 12))
   
   ## DRAW ##
   ggdraw()+
     draw_image(court, scale = 1)+
     draw_plot(nadal_serve_plot, x=0, y=0, scale=0.85) 
   #draw_plot(djoko_serve_plot, scale = 0.85)
   
   ----------------
     
     
     
     ggtitle("Djokovic serve speed")+
     theme(
       plot.title = element_text(hjust = .5,
                                 vjust = 10, 
                                 face = "bold", 
                                 family = "mono", 
                                 size = 20)
       
       
       
       
       
       
       
       "In-Play" = "#1D1D1D3B",
       "Unforced-Error" = "#8A2113C4",
       "Forced-Error" = "#8A3D33C4",
       "Winner" = "#FFE600C0"),