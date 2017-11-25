#' ---
#' Data Analysis script for Exp 02
#' ---

#' Libraries Used
library(dplyr)
library(ggplot2)

#' # Day 1
#' Reading Data and Cleanup
raw <- read.csv("./day1/well_data.csv", stringsAsFactors = FALSE)

dat <- raw %>%
  rowwise() %>%
  mutate(std_Mean = mean(c(std1, std2, std3)), std_Sd = sd(c(std1, std2, std3)))

#' Plots
plot_data <- dat %>%
  select(-c(desc, gst_only, gst_nfat)) %>%
  mutate(Mean = std_Mean, Sd = std_Sd)

ggplot(plot_data, aes(conc, Mean))+
  geom_point()+
  geom_smooth(span = .6, method = "loess")+
  ggtitle("Concentration vs Intensity of known BSA solutions")+
  xlab("Concentration of BSA (mg/mL)")+
  ylab("Average Intensity")+
  ggsave("../results/day1/smooth_curve.png")

#' Linear Model
m <- summary(lm(Mean ~ conc, plot_data))$coef[[2]]
b <- summary(lm(Mean ~ conc, plot_data))$coef[[1]]

eqn <- paste0("Y = ", round(m, 2), "X + ", round(b, 2))

ggplot(plot_data, aes(conc, Mean))+
  geom_point()+
  geom_smooth(span = .6, method = "lm", se = FALSE)+
  geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd), width = .1)+
  geom_text(x = .25, y = .3, label = eqn)+
  ggtitle("Concentration vs Intensity of known BSA solutions")+
  xlab("Concentration of BSA (mg/mL)")+
  ylab("Average Intensity")+
  ggsave("../results/day1/linear_model.png")





