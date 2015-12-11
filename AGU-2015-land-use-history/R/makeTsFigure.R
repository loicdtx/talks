library(zoo)
library(strucchange)
library(bfast)
library(ggplot2)
library(dplyr)
library(lubridate)

ts <- readRDS('data/Au_Tum_SR_Zoo.rds')
str(ts)
names(ts) <- c('b1', 'b2', 'b3', 'b4', 'b5', 'b7')
# Data frame
df <- ts %>%
  data.frame() %>%
  na.omit() %>%
  add_rownames('time') %>%
  mutate(time = as.Date(time)) %>%
  mutate(NDMI = (b4 - b5)/(b4 + b5))

# Variables
order <- 1
formula <- response ~ trend + harmon

# pp dataframe
pp <- bfastts(df$NDMI, df$time, type = 'irregular') %>%
  bfastpp(order = order)

# pp dataframe for adding model fit
ppFull <- bfastts(df$NDMI, df$time, type = 'irregular') %>%
  bfastpp(order = 1, na.action = na.pass)

model <- lm(data = ppFull, formula = formula)
ppFull$predicted <- predict(model, newdata = ppFull)


### FIrst ggplot without model and break
gg1 <- ggplot(df) +
  geom_line(aes(time, NDMI)) +
  geom_point(aes(time, NDMI), color = 'darkgreen') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
gg1

# Second plot with simple model with no breaks
gg2 <- ggplot(ppFull) +
  geom_line(aes(time, predicted), color = 'blue') +
  geom_line(data = na.omit(ppFull), aes(time, response)) +
  geom_point(aes(time, response), color = 'darkgreen') +
  ylab('NDMI') +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
gg2

### plot with model and breakpoints
# pp dataframe with breakpoints
bp1 <- breakpoints(formula = formula, data = pp, h = 0.2)
df2 <- pp
df2$breaks <- NA
df2$breaks[bp1$breakpoints] <- 1
xIntercept <- df2$time[df2$breaks == 1]
segments <- c(df2$time[c(1,bp1$breakpoints, nrow(df2))])


gg3 <- ggplot(df2) +
  geom_line(aes(time, response)) +
  geom_point(aes(time, response), color = 'darkgreen') +
  geom_vline(xintercept = xIntercept, color = 'red', linetype = 'dashed') +
  ylab('NDMI') +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
for(i in seq_along(segments[-1])) {
  predTs <- bfastts(rep(NA, nrow(df2)), date_decimal(df2$time), type = 'irregular')
  predDf <- bfastpp(predTs, order = order, na.action = na.pass)
  predDfSub <- subset(predDf, time <= segments[i + 1] & time >= segments[i])
  trainDfSub <- subset(df2, time <= segments[i + 1] & time >= segments[i])
  model <- lm(formula = formula, data = trainDfSub)
  predDfSub$pred <- predict(model, newdata = predDfSub)
  
  gg3 <- gg3 + geom_line(data = predDfSub, aes(x = time, y = pred), color = 'blue')
}
gg3

ggsave(gg1, filename = '~/sandbox/gifs/bp1.png', width = 9, height = 4)
ggsave(gg2, filename = '~/sandbox/gifs/bp2.png', width = 9, height = 4)
ggsave(gg3, filename = '~/sandbox/gifs/bp3.png', width = 9, height = 4)
