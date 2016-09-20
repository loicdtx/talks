library(ggplot2)
library(dplyr)
library(zoo)

df0 <- read.table('https://www.frames.gov/files/2113/5353/5837/willow.txt', header = T) 

df <- df0 %>%
  filter(mean < 0.75) %>%
  filter(Wavelength < 1760 | Wavelength > 1950) %>%
  filter(Wavelength < 2400) %>%
  mutate(mean_smooth = rollapply(mean, width = 7, FUN = median, fill = NA))


rect <- data.frame(xmin = c(390, 490, 590), xmax = c(490, 590, 700), ymin = rep(-Inf, 3), ymax = rep(Inf, 3), color = c('blue', 'green', 'red'), stringsAsFactors = F)

ggplot(df, aes(Wavelength, mean_smooth)) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill = color), alpha=0.5, inherit.aes = FALSE) +
  scale_fill_identity() +
  geom_line(color = 'green') +
  ylab('Reflectance') +
  theme_classic() +
  theme(axis.line.x = element_line(colour = 'lightgrey', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'lightgrey', size=0.5, linetype='solid'),
        axis.title = element_text(color = 'lightgrey'),
        axis.text = element_text(color = 'lightgrey'),
        axis.ticks = element_line(color = 'lightgrey'),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(filename = '~/git/talks/phd_defense/img/reflectance.png', bg = 'transparent')
