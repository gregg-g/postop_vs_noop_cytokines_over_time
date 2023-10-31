library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggpubr)

data <- read.csv('postop_cytok_over_time.csv')
head(data)
str(data)
data$surgery <- as.factor(data$surgery)
data$horse <- as.factor(data$horse)

data[,c(1,3:7)] %>% group_by(surgery, time) %>% summarise('mean' = mean(),
                                                          'sd' = sd())

model_il.18 <- lmer(il.18 ~ surgery + time + (1|horse), data = data)
summary(model_il.18)
anova(model_il.18)

model_il.2 <- lmer(il.2 ~ surgery + time + (1|horse), data = data)
summary(model_il.2)
anova(model_il.2)

model_ip.10 <- lmer(ip.10 ~ surgery*time + (1|horse), data = data)
summary(model_ip.10)
anova(model_ip.10)

model_il.10 <- lmer(il.10 ~ surgery*time + (1|horse), data = data)
summary(model_il.10)
anova(model_il.10)

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

# Need to remove the top outlier based on graphics - maybe the second highest as well??
# start with the top

data2 <- data %>% filter(horse != 13)

model2_il.18 <- lmer(il.18 ~ surgery*time + (1|horse), data = data2)
summary(model2_il.18)
anova(model2_il.18)

model2_il.2 <- lmer(il.2 ~ surgery*time + (1|horse), data = data2)
summary(model2_il.2)
anova(model2_il.2)

model2_ip.10 <- lmer(ip.10 ~ surgery*time + (1|horse), data = data2)
summary(model2_ip.10)
anova(model2_ip.10)

model2_il.10 <- lmer(il.10 ~ surgery*time + (1|horse), data = data2)
summary(model2_il.10)
anova(model2_il.10)

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

#and removing the next one - still an outlier

data3 <- data2 %>% filter(horse != 2)

model3_il.18 <- lmer(il.18 ~ surgery + (1|horse), data = data3)
summary(model3_il.18)
anova(model3_il.18)

model3_il.2 <- lmer(il.2 ~ surgery*time + (1|horse), data = data3)
summary(model3_il.2)
anova(model3_il.2)

model3_ip.10 <- lmer(ip.10 ~ surgery*time + (1|horse), data = data3)
summary(model3_ip.10)
anova(model3_ip.10)

model3_il.10 <- lmer(il.10 ~ surgery*time + (1|horse), data = data3)
summary(model3_il.10)
anova(model3_il.10)

shapiro.test(data3$il.10)
plot_norm1 <- plot(model3_il.10)
plot_norm2 <- ggplot(data = data3, aes(x = il.10)) +
  geom_histogram()

### Doesn't appear to be distributed well, although not too bad...###
### Let's log transform without the outliers ###

data4 <- data3 %>% mutate(log_il.10 = if_else(il.10 <= 0, 0, log(il.10)))
shapiro.test(data4$log_il.10)

### IL10 seems to be the one significant factor here ###

model4_il.10 <- lmer(log_il.10 ~ surgery*time + (1|horse), data = data4)
summary(model4_il.10)
anova(model4_il.10)

plot_norm3 <- plot(model4_il.10)
plot_norm4 <- ggplot(data = data4, aes(x = log_il.10)) +
  geom_histogram()
plot_norm_2 <- ggarrange(plot_norm3, plot_norm4, nrow = 1)
plot_norm_2

### Looks much better ###

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