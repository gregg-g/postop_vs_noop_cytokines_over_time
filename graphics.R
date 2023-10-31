### Graphics ###
library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggpubr)

### Will need to read in models from analysis prior to visualizing ###

p1 <- ggplot(data = data, aes(x = time, y = il.18, group = horse, color = surgery))
p1 <- p1 + geom_point() +
  geom_line()

p2 <- ggplot(data = data, aes(x = time, y = il.10, group = horse, color = surgery))
p2 <- p2 + geom_point() +
  geom_line()

p3 <- ggplot(data = data, aes(x = time, y = il.2, group = horse, color = surgery))
p3 <- p3 + geom_point() +
  geom_line()

p4 <- ggplot(data = data, aes(x = time, y = ip.10, group = horse, color = surgery))
p4 <- p4 + geom_point() +
  geom_line()

p5 <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

p5

### Without the highest outlier ###

p1_2 <- ggplot(data = data2, aes(x = time, y = il.18, group = horse, color = surgery))
p1_2 <- p1_2 + geom_point() +
  geom_line()

p2_2 <- ggplot(data = data2, aes(x = time, y = il.10, group = horse, color = surgery))
p2_2 <- p2_2 + geom_point() +
  geom_line()

p3_2 <- ggplot(data = data2, aes(x = time, y = il.2, group = horse, color = surgery))
p3_2 <- p3_2 + geom_point() +
  geom_line()

p4_2 <- ggplot(data = data2, aes(x = time, y = ip.10, group = horse, color = surgery))
p4_2 <- p4_2 + geom_point() +
  geom_line()

p5_2 <- ggarrange(p1_2, p2_2, p3_2, p4_2, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

p5_2

### Without the second outlier and log transformed ###
p1_3 <- ggplot(data = data3, aes(x = time, y = il.18, group = horse, color = surgery))
p1_3 <- p1_3 + geom_point() +
  geom_line()

#FOCUS ON THIS ONE  - seems to be the only one with differences
p2_3 <- ggplot(data = data3, aes(x = time, y = il.10, group = horse, color = surgery))
p2_3 <- p2_3 + geom_point() +
  geom_line()
p2_3

p3_3 <- ggplot(data = data3, aes(x = time, y = il.2, group = horse, color = surgery))
p3_3 <- p3_3 + geom_point() +
  geom_line()

p4_3 <- ggplot(data = data3, aes(x = time, y = ip.10, group = horse, color = surgery))
p4_3 <- p4_3 + geom_point() +
  geom_line()

p5_3 <- ggarrange(p1_3, p2_3, p3_3, p4_3, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

p5_3