library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggpubr)
library(magrittr)

data <- read.csv('postop_cytok_over_time.csv')
head(data)
str(data)
data$surgery <- as.factor(data$surgery)
data$horse <- as.factor(data$horse)

data %>% group_by(time, surgery) %>%
  summarise(across(c(il.18, il.2, ip.10, il.10, temp, wbc, neut, lymph, fibr),
                   c(mean = ~mean(.x), sd = ~sd(.x))))

     # Plot cytokine data #

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
# start with the top - horse #13

data2 <- data %>% filter(horse != 13)

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

#and removing the next one - #2 still an outlier

data3 <- data2 %>% filter(horse != 2)

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

### Check distribution before modeling ###

shapiro.test(data3$il.18);shapiro.test(data3$il.2);shapiro.test(data3$ip.10); shapiro.test(data3$il.10)
#plot_norm1_1 <- plot(model3_il.18)
#plot_norm1_2 <- plot(model3_il.2)
#plot_norm1_3 <- plot(model3_il.10)
plot_norm2_1 <- ggplot(data = data3, aes(x = il.18)) +
  geom_histogram(bins = 30)
plot_norm2_2 <- ggplot(data = data3, aes(x = il.2)) +
  geom_histogram(bins = 30)
plot_norm2_3 <- ggplot(data = data3, aes(x = ip.10)) +
  geom_histogram(bins = 30)
plot_norm2_4 <- ggplot(data = data3, aes(x = il.10)) +
  geom_histogram(bins = 30)

#plot_norm1_all <- ggarrange(plot_norm1_1, plot_norm1_2, plot_norm1_3,
#                            ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
plot_norm2_all <- ggarrange(plot_norm2_1, plot_norm2_2, plot_norm2_3, plot_norm2_4,
                            ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

plot_norm2_all

### Will need to evaluate residuals ###
### Model as-is ###
cytokine_vars <- list('il.18', 'il.2', 'ip.10', 'il.10')
for (i in cytokine_vars){
  model <- lmer(paste(i[[1]], "~ surgery*time + (1|horse)"), data = data3)
  print(anova(model))
  plot(resid(model, type = "pearson") ~ fitted(model))
  qqnorm(resid(model, type = "pearson"))
}

### No real differences in the change over time (interaction term) ###
### All of the interleukins could show a significant effect of surgery, however ###


### Doesn't appear to be distributed well, although not too bad...###
### Let's log transform without the outliers ###

data4 <- data3 %>% mutate(log_il.18 = if_else(il.18 <= 0, 0, log(il.18)),
                          log_il.2 = if_else(il.2 <= 0, 0, log(il.2)),
                          log_ip.10 = if_else(ip.10 <= 0, 0, log(ip.10)),
                          log_il.10 = if_else(il.10 <= 0, 0, log(il.10)))

log_cytokine_vars <- list('log_il.18', 'log_il.2', 'log_ip.10', 'log_il.10')
for (i in log_cytokine_vars){
  model <- lmer(paste(i[[1]], "~ surgery*time + (1|horse)"), data = data4)
  print(anova(model))
  plot(resid(model, type = "pearson") ~ fitted(model))
  qqnorm(resid(model, type = "pearson"))
}
### IL10 seems to be the one significant factor here ###
### Can also try square-root transformation ###

data5 <- data3 %>% mutate(sqrt_il.18 = if_else(il.18 <= 0, 0, sqrt(il.18)),
                          sqrt_il.2 = if_else(il.2 <= 0, 0, sqrt(il.2)),
                          sqrt_ip.10 = if_else(ip.10 <= 0, 0, sqrt(ip.10)),
                          sqrt_il.10 = if_else(il.10 <= 0, 0, sqrt(il.10)))

sqrt_cytokine_vars <- list('sqrt_il.18', 'sqrt_il.2', 'sqrt_ip.10', 'sqrt_il.10')
for (i in sqrt_cytokine_vars){
  model <- lmer(paste(i[[1]], "~ surgery*time + (1|horse)"), data = data5)
  print(anova(model))
  plot(resid(model, type = "pearson") ~ fitted(model))
  qqnorm(resid(model, type = "pearson"))
}

### not really better - stick with log transformation ###

model4_il.10 <- lmer(log_il.10 ~ surgery*factor(time) + (1|horse), data = data4) #factor time to allow for individual comparisons
summary(model4_il.10)
anova(model4_il.10)

### Check contreasts
emm_model1 <- emmeans(model4_il.10, ~surgery|time, at = list(time = c(0, 24, 48)))
contrast(emm_model1, "trt.vs.ctrl")
emm_model2 <- emmeans(model4_il.10, ~time|surgery, at = list(time = c(0, 24, 48)))
contrast(emm_model2, "pairwise")

### Print graphic for IL-10 ###
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

### nothing stands out as a significant outlier ###
### model all as before ###

phys_vars <- list('temp', 'wbc', 'neut', 'lymph', 'fibr')
for (i in phys_vars){
  model <- lmer(paste(i[[1]], "~ surgery*time + (1|horse)"), data = data)
  print(anova(model))
  plot(resid(model, type = "pearson") ~ fitted(model))
  qqnorm(resid(model, type = "pearson"))
}

### Looks like neut and lymph have some significant differences ###
### resids look fine, let's check the contrasts for both ###

model_neut <- lmer(neut ~ surgery*factor(time) + (1|horse), data = data) #factor time to allow for individual comparisons
summary(model_neut)
### Check contrasts ###
emmeans(model_neut, ~surgery|time, at = list(time = c(0, 24, 48))) %>%
  contrast("trt.vs.ctrl")
emmeans(model_neut, ~time|surgery, at = list(time = c(0, 24, 48))) %>%
  contrast("pairwise")

model_lymph <- lmer(lymph ~ surgery*factor(time) + (1|horse), data = data) #factor time to allow for individual comparisons
summary(model4_il.10)
### Check contrasts ###
emmeans(model_lymph, ~surgery|time, at = list(time = c(0, 24, 48))) %>%
  contrast("trt.vs.ctrl")
emmeans(model_lymph, ~time|surgery, at = list(time = c(0, 24, 48))) %>%
  contrast("pairwise")

### Visualize significant variables - neutrophils and lymphocytes ###

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