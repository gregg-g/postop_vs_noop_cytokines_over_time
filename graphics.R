### Graphics ###
library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggpubr)

### Will need to read in data from analysis prior to visualizing ###

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

### Without the second outlier ###
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
ggsave("cytokines_all.png", width = 8, height = 8, units = "in", dpi = 300)

### focus on IL-10 ###
p2_3_final <- ggplot(data = data3, aes(x = time, y = il.10))
p2_3_final <- p2_3_final + geom_point(aes(shape = surgery, group = horse)) +
  geom_line(aes(linetype = surgery, group = horse)) +
  stat_summary(aes(group = surgery, shape = surgery), geom = "point", position = position_dodge(width = 2), size = 4) +
  stat_summary(aes(group = surgery), linetype = 'solid', geom = "line") +
  stat_summary(aes(group = surgery), geom = "errorbar", width = 2, position = position_dodge(width = 2)) +
  scale_linetype_manual(values = c(2,3)) +
  scale_x_continuous(breaks = c(0, 24, 48)) +
  theme_bw()
p2_3_final
ggsave("IL10.png", width = 4, height = 4, units = "in", dpi = 300)

### Plot other data ###

p1_4 <- ggplot(data = data, aes(x = time, y = temp, group = horse, color = surgery))
p1_4 <- p1_4 + geom_point() +
  geom_line()

p2_4 <- ggplot(data = data, aes(x = time, y = wbc, group = horse, color = surgery))
p2_4 <- p2_4 + geom_point() +
  geom_line()

p3_4 <- ggplot(data = data, aes(x = time, y = neut, group = horse, color = surgery))
p3_4 <- p3_4 + geom_point() +
  geom_line()

p4_4 <- ggplot(data = data, aes(x = time, y = lymph, group = horse, color = surgery))
p4_4 <- p4_4 + geom_point() +
  geom_line()

p5_4 <- ggplot(data = data, aes(x = time, y = fibr, group = horse, color = surgery))
p5_4 <- p5_4 + geom_point(position = position_dodge(width = 2)) +
  geom_line()

p6_4 <- ggarrange(p1_4, p2_4, p3_4, p4_4, p5_4, ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")

p6_4

### Neutrophil comparison ###
p_neut_final <- ggplot(data = data, aes(x = time, y = neut))
p_neut_final <- p_neut_final + geom_point(aes(shape = surgery, group = horse)) +
  geom_line(aes(linetype = surgery, group = horse)) +
  stat_summary(aes(group = surgery, shape = surgery), geom = "point", position = position_dodge(width = 2), size = 4) +
  stat_summary(aes(group = surgery), linetype = 'solid', geom = "line") +
  stat_summary(aes(group = surgery), geom = "errorbar", width = 2, position = position_dodge(width = 2)) +
  scale_linetype_manual(values = c(2,3)) +
  scale_x_continuous(breaks = c(0, 24, 48)) +
  theme_bw()
p_neut_final
ggsave("Neutrophils.png", width = 4, height = 4, units = "in", dpi = 300)

### Lymphocyte comparison ###
p_lymph_final <- ggplot(data = data, aes(x = time, y = lymph))
p_lymph_final <- p_lymph_final + geom_point(aes(shape = surgery, group = horse)) +
  geom_line(aes(linetype = surgery, group = horse)) +
  stat_summary(aes(group = surgery, shape = surgery), geom = "point", position = position_dodge(width = 2), size = 4) +
  stat_summary(aes(group = surgery), linetype = 'solid', geom = "line") +
  stat_summary(aes(group = surgery), geom = "errorbar", width = 2, position = position_dodge(width = 2)) +
  scale_linetype_manual(values = c(2,3)) +
  scale_x_continuous(breaks = c(0, 24, 48)) +
  theme_bw()
p_lymph_final
ggsave("lymphocytes.png", width = 4, height = 4, units = "in", dpi = 300)