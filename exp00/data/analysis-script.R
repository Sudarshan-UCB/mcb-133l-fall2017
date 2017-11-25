#' ---
#' Data Analysis script for Exp 00
#' ---

#' Libraries Used
library(dplyr)
library(ggplot2)

#' Reading Data and Cleanup
raw <- read.csv("./well_data.csv", stringsAsFactors = FALSE)

conc <- c(0, .1, 0.2, 0.4, 0.7, 1, 1.5, 2)

dat <- raw %>% rbind(c(NA, conc, NA, NA, NA, NA)) %>% slice(c(4, 1:3))


#' Plots
temp <- dat %>% slice(-1) %>% select(-X)
plot_data <- as.data.frame(t(as.matrix((temp)))) %>%
  slice(1:8) %>%
  rowwise() %>%
  mutate(Mean = mean(c(V1, V2, V3)), Sd = sd(c(V1, V2, V3))) %>%
  cbind(conc)

ggplot(plot_data, aes(conc, Mean))+
  geom_point()+
  geom_smooth(span = .6, method = "loess")+
  ggtitle("Concentration vs Intensity of known BSA solutions")+
  xlab("Concentration of BSA (mg/mL)")+
  ylab("Average Intensity")+
  ggsave("../results/smooth_curve.png")

#' Linear Model
m <- summary(lm(Mean ~ conc, plot_data[3:7,]))$coef[[2]]
b <- summary(lm(Mean ~ conc, plot_data[3:7,]))$coef[[1]]

eqn <- paste0("Y = ", round(m, 2), "X + ", round(b, 2))

ggplot(plot_data[3:7,], aes(conc, Mean))+
  geom_point()+
  geom_smooth(span = .6, method = "lm", se = FALSE)+
  geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd), width = .1)+
  geom_text(x = .4, y = .4, label = eqn)+
  ggtitle("Concentration vs Intensity of known BSA solutions")+
  xlab("Concentration of BSA (mg/mL)")+
  ylab("Average Intensity")+
  ggsave("../results/linear_model.png")



