# libraries:
library(tidyverse)
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)
library(here)
library(directlabels)
library(lubridate)
# install.packages("devtools")
devtools::install_github("hadley/emo")s
library(emojifont)
library(ggimage)
library(ggrepel)
library(here)
library(cowplot)
library(magick)
 

flags <- tibble(names = c("sweden","norway", "finland", "denmark","iceland"),
              emoji = map_chr(names, emo::ji),
              flag_path = paste0(rep(".../data/", 5), c("SE","NO", "FI", "DK","IS"), 
                                rep(".png", 5)),
              geo = c("SE","NO", "FI", "DK","IS"),
              UNICODE=c("U+1F1F8 U+1F1EA", "U+1F1F3 U+1F1F1", "U+1F1F8 U+1F1EA", "U+1F1F3 U+1F1F1",
                       "U+1F1F8 U+1F1EA")
              ) 
flags




excess_mortality <- read_csv("data/Excess_mortality_by_month", 
                                      col_types = cols(TIME_PERIOD = col_date(format = "%Y-%m"))) %>% 
  left_join(flags)
max_x <- max(unique(excess_mortality$TIME_PERIOD)) 
min_x <- min(unique(excess_mortality$TIME_PERIOD))

area_excess_mortality <- excess_mortality %>% 
  filter(geo == "geo")

cbbPalette <- c("#929299", "#929299","#929299", "#929299", "#FECC02")





# Plot
ggplot(data = excess_mortality, 
         aes(x=TIME_PERIOD, y=OBS_VALUE, group=geo, color=geo)) +
    geom_segment(aes(x=min_x, xend=max_x,
                     y=0,  yend=0),
                 col = "#929299",
                 size = 0.35,
                 linetype=3) +
    geom_area(data = subset(excess_mortality, geo == 'SE'), 
              aes(color = geo, fill = geo), alpha = 0.6) +
    scale_fill_manual(values=c("#FECC02")) +
  geom_line(size=1.1) +
  geom_point() +
  scale_color_manual(values=cbbPalette)  +
  #ggtitle("Excess mortality in the Nordic Countries") +
  labs(title = "Excess mortality in the Nordic Countries",
       subtitle = "In Memory of Our Father, who lost his battle against \n Covid-19  on 11th January 2021",
       caption = "Deaths compared to average of previous five years \nSource data: Data are collected by Eurostat from the national statistical institutes. \n@leynu | Jan 2022"
  ) +
  theme_ft_rc() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major.x=element_blank(), 
        legend.position="none",
        plot.title = element_text(size = 21),
        plot.subtitle = element_text(size = 17),
        plot.caption=element_text(hjust = 0,
                                  size = 11),
    axis.text.x = element_text(angle = 45),
    axis.title.x = element_blank(),
    axis.title.y = element_blank() 
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "2 month",
               limits=c(min_x, max_x)) +
  scale_y_continuous(limits=c(-40,40)) +
  geom_dl(aes(label = geo), 
          method = list(dl.trans(x = x + 0.3), 
                        "last.points", 
                        cex = 1.25)) +
  transition_reveal(TIME_PERIOD) +
  shadow_wake(wake_length = 0.05) 
  



# Save at gif:
#
anim_save("viejo.gif")
