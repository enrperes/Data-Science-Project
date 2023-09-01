## Libraries ## ----
library(tidyverse)
library(dplyr)
library(ggplot2)
library(png)
library(ggpubr)
library(cowplot)


## IMPORT ## ----
data <- read.csv("2022RG_final.csv")
court_test <- read.csv("courtdimensions.csv") # test dataset to calibrate the court image # nolint: line_length_linter.
court <- readPNG("clay-court.png")
half_court <- readPNG("half-clay-court.png")

## DATA ## ----
serves_djoko <- data %>%
  filter(Player == "Player1" & Shot_Type %in% c("1st Serve", "2nd Serve")) %>%
  filter(Y2 > -6.4 & Y2 < 6.4 & (Y2 < -1 | Y2 > 1) & (X2 < 3.5 | X2 > 3.5)) %>% # filter out faults
  select(Player, Shot_Type, X2, Y2) %>%
  mutate(X2 = ifelse(Y2 < 0, -X2, X2))

serves_nadal <- data %>%
  filter(Player == "Player2" & Shot_Type %in% c("1st Serve", "2nd Serve")) %>%
  filter(Y2 > -6.4 & Y2 < 6.4 & (Y2 < -1 | Y2 > 1) & (X2 < 4.1 | X2 > 4.1)) %>%
  select(Player, Shot_Side, Shot_Type, X2, Y2, Serve_Speed) %>%
  mutate(X2 = ifelse(Y2 < 0, -X2, X2))

serve_speed_djoko <- data %>% 
  filter(Player == "Player1" & Shot_Type %in% c("1st Serve", "2nd Serve")) %>% 
  select(Serve_Speed, Set, Shot_Type, Point_Winner, Stroke_Number, Outcome) %>% 
  mutate(isAce = ifelse((Point_Winner == 2) & Stroke_Number == 1 & Outcome == "Winner", TRUE, FALSE)) %>% 
  filter(Serve_Speed != "Unknown")

serve_speed_nadal <- data %>% # sbagliato
  filter(Player == "Player2" & Shot_Type %in% c("1st Serve", "2nd Serve")) %>% 
  select(Serve_Speed, Set, Shot_Type, Point_Winner, Stroke_Number, Outcome) %>% 
  mutate(isAce = ifelse((Point_Winner == 1) & Stroke_Number == 1 & Outcome == "Winner", TRUE, FALSE)) %>% 
  filter(Serve_Speed != "Unknown")

## Nadal Serve position Plot ## ----
nadal_serve_pos_plot <- ggplot(serves_nadal, aes(x = X2, y = abs(Y2))) +
  stat_density_2d(aes(fill = after_stat(density)),
    geom = "raster",
    contour = FALSE,
    h = c(1, 2) # control x and y bandwidths of the estimate.
  ) +
  scale_fill_gradientn(colors = c(
    "transparent", # scale that is transparent on the smallest end of the range
    viridis::viridis_pal(
      alpha = 0.5,
      option = "turbo"
    )(10) # number of colors in the scale
  )) +
  geom_point(
    aes(colour = factor(Shot_Type))
  ) +
  scale_colour_manual(values = c("1st Serve" = "#00000077", "2nd Serve" = "#FFFFFF77"))+
  theme_void() +
  theme(aspect.ratio = 1, legend.position = "none") +
  xlim(c(-6, 6)) +
  ylim(c(0, 12)) +
  ggtitle("Nadal serve position") +
  theme(plot.title = element_text(hjust = .5,
                                  vjust = 10,
                                  face = "bold",
                                  family = "mono",
                                  size = 20)
        )
# Use this below for full court scale! #
# theme(aspect.ratio = 2, legend.position = "none")+
# xlim(c(-6, 6)) +
# ylim(c(-12, 12))



## Djokovic Serve position plot ## ----
djoko_serve_pos_plot <- ggplot(serves_djoko, aes(x = X2, y = abs(Y2))) +
  stat_density_2d(aes(fill = after_stat(density)),
    geom = "raster",
    contour = FALSE,
    h = c(1, 2)
  ) +
  scale_fill_gradientn(colors = c(
    "transparent",
    viridis::viridis_pal(
      alpha = 0.5,
      option = "turbo"
    )(10)
  )) +
  geom_point(
    aes(colour = factor(Shot_Type))
  ) +
  scale_colour_manual(values = c("1st Serve" = "#00000077", "2nd Serve" = "#FFFFFF77"))+
  theme_void() +
  theme(aspect.ratio = 1, legend.position = "none") +
  xlim(c(-6, 6)) +
  ylim(c(0, 12)) +
  ggtitle("Djokovic serve position") +
  theme(plot.title = element_text(hjust = .5, 
                                  vjust = 10,
                                  face = "bold",
                                  family = "mono",
                                  size = 20)
        )

## Nadal Serve Speed Plot

mean_nadal_serve_speed <- serve_speed_nadal %>% # Create a new dataframe with the mean serve speed for each combination of Set and Shot_Type
  group_by(Shot_Type, Set) %>% 
  summarise(mean_serve_speed = round(mean(as.numeric(Serve_Speed))),
            .groups = "drop") # Remove the grouping

nadal_serve_speed_plot <- ggplot(serve_speed_nadal, 
                                 aes(x = as.numeric(Serve_Speed),
                                     y = reorder(Set, desc(Set))))+
  geom_point(
    size = 3, 
    aes(colour = factor(Shot_Type), shape = factor(isAce)), 
    position = position_jitter(height = 0.14), # Add random jitter to the points
    na.rm = TRUE # missing values are removed without warning
  )+
  geom_text(
    data = mean_nadal_serve_speed, # Use the new dataframe
    aes(label= mean_serve_speed, x = mean_serve_speed), 
    vjust = 3.5,
    size = 2.5
  )+
  stat_summary(
    na.rm = TRUE,
    fun = mean,
    aes(group = interaction(Set, Shot_Type)),  # Group by Set and Shot_Type
    shape = 3,
    colour = "#6f302a",
    show.legend = TRUE
  )+
  scale_color_manual(values = c("1st Serve"="#4E80DB", "2nd Serve" = "#DAB157"))+
  scale_shape_manual(values = c("FALSE" = 20, "TRUE" = 0))+
  theme_minimal()+
  ylab("Set")+
  scale_x_continuous(name = "Serve speed (Km/h)", breaks = c(150, 175, 200))+
  guides(color=FALSE, shape = FALSE)

## Djokovic Serve Speed Plot ## ----

mean_djoko_serve_speed <- serve_speed_djoko %>% 
  group_by(Shot_Type, Set) %>% 
  summarise(mean_serve_speed = round(mean(as.numeric(Serve_Speed))),
            .groups = "drop")

serve_speed_djoko <- data %>% 
  filter(Player == "Player1" & Shot_Type %in% c("1st Serve", "2nd Serve")) %>% 
  select(Serve_Speed, Set, Shot_Type, Point_Winner, Stroke_Number, Outcome) %>% 
  mutate(isAce = ifelse((Point_Winner == 2) & Stroke_Number == 1 & Outcome == "Winner", TRUE, FALSE)) %>% 
  filter(Serve_Speed != "Unknown")

mean_djoko_serve_speed <- serve_speed_djoko %>% 
  group_by(Shot_Type, Set) %>% 
  summarise(mean_serve_speed = round(mean(as.numeric(Serve_Speed))),
            .groups = "drop")

pp2 = ggdraw()+
  draw_plot(
    djoko_serve_speed_plot <- ggplot(serve_speed_djoko,
                                     aes(x = as.numeric(Serve_Speed),
                                         y = reorder(Set, desc(Set))))+
      geom_point(
        size = 3, 
        aes(colour = factor(Shot_Type), shape = factor(isAce)), 
        position = position_jitter(height = 0.14), 
        na.rm = TRUE 
      ) + 
      geom_text(
        data = mean_djoko_serve_speed, 
        aes(label= mean_serve_speed, x = mean_serve_speed), 
        vjust = 3.5,
        size = 2.5
      )+
      stat_summary(
        na.rm = TRUE,
        fun = mean,
        aes(group = interaction(Set, Shot_Type)),  # Group by Set and Shot_Type
        shape = "|",
        colour = "#000B1CB5",
        size = 2.14,
        show.legend = TRUE
      )+
      ggtitle(
        "Djokovic"
      )+
      theme(plot.title = element_text(hjust = .5,
                                      vjust = 10,
                                      face = "bold",
                                      family = "mono",
                                      size = 20,
                                      color = "#37230e")
      )+
      scale_color_manual(values = c("1st Serve"="#4E80DB", "2nd Serve" = "#DAB157"))+
      scale_shape_manual(values = c("FALSE" = 20, "TRUE" = 2))+
      theme_minimal()+
      ylab("Set")+
      scale_x_continuous(name = "Serve speed (Km/h)", breaks = c(125, 150, 175, 200))+
      guides(color=FALSE, shape = FALSE)
    
  )
#pp2
  
## Court Calibration ## ----
test_Hcourt_plot <- ggplot(court_test, aes(x = X2, y = Y2)) +
  geom_point() +
  theme_void() +
  theme(aspect.ratio = 1) +
  xlim(c(-6, 6)) +
  ylim(c(0, 12))



test_Fcourt_plot <- ggplot(court_test, aes(x = X2, y = Y2)) +
  geom_point() +
  theme_void() +
  theme(aspect.ratio = 2) +
  xlim(c(-6, 6)) +
  ylim(c(-12, 12))

## DRAW PLOT ## ----
ggdraw() +
  #draw_image(half_court, scale = 1) +
  # draw_plot(djoko_serve_pos_plot, x=0, y=-0.05, scale=0.9)
  # draw_plot(nadal_serve_pos_plot, x=0, y=-0.05, scale=0.9)
  # draw_plot(test_Hcourt_plot, x = 0, y = -0.06, scale = 0.85)
   draw_plot(nadal_serve_speed_plot)
  # draw_plot(djoko_serve_speed_plot)
