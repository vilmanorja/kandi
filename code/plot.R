#Visulaizations for temperature data

setwd("/m/home/home3/33/norjav2/data/Documents/kandi_local/kandi/code")
source("rcode.R")
#read data
original_data <- read.table(file = "anjala.csv", header = TRUE, sep = ",",
                            col.names = c("Year", "Month", "Day", "Hour",
                                          "Time zone", "Celcius"))

library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)
library(lintr)

#center text in figures
theme_update(plot.title = element_text(hjust = 0.5))

#remove and edit data
original_data$Date <- as.Date(with(original_data, paste(Year, Month, Day,
                                                        sep = "-")), "%Y-%m-%d")
data_july <- original_data[original_data$Month == "7", ]
data_july$Group <- c(rep(1959, 31), rep(1960, 10 * 31), rep(1970, 10 * 31),
                     rep(1980, 10 * 31), rep(1990, 10 * 31), rep(2000, 10 * 31),
                     rep(2010, 10 * 31), rep(2020, 2 * 31))

#one year of each decade
data_10 <- filter(data_july, Year == "1961" | Year == "1971" | Year == "1981" |
  Year == "1991" | Year == "2001" | Year == "2011" | Year == "2021")
mean_10 <- data_10 %>%
  group_by(Year) %>%
  summarise(mean_val_10 = mean(Celcius))
ggplot(data_10, aes(Day, Celcius)) + geom_point(color = "darkblue") +
  geom_hline(data = mean_10, aes(yintercept = mean_val_10)) +
  facet_grid(~Year, scales = "free_x")  +
  labs(title = "Maximum daily temperatures in July in Anjala",
  x = "Day (July)", y = "Temperature (\u00B0C)")

#Compare two different decades
comp_1 <- filter(data_july, Group == "1970")
mean_1 <- comp_1 %>%
  group_by(Year) %>%
  summarise(mean_val_1 = mean(Celcius))
ggp1 <- ggplot(comp_1, aes(Day, Celcius)) + geom_point(colour = "darkred") +
  geom_hline(data = mean_1, aes(yintercept = mean_val_1)) +
  facet_grid(~Year, scales = "free_x")  +
  labs(x = "Day (July)", y = "Temperature (\u00B0C)") + ylim(5, 35)

comp_2 <- filter(data_july, Group == "2010")
mean_2 <- comp_2 %>%
  group_by(Year) %>%
  summarise(mean_val_2 = mean(Celcius))
ggp2 <- ggplot(comp_2, aes(Day, Celcius)) + geom_point(colour = "darkblue") +
  geom_hline(data = mean_2, aes(yintercept = mean_val_2)) +
  facet_grid(~Year, scales = "free_x")  +
  labs(x = "Day (July)", y = "Temperature (\u00B0C)") + ylim(5, 35)

grid.arrange(ggp1, ggp2, ncol = 2,
             top = "Maximum daily temperatures in July in Anjala")

#Decades
data_july_dec <- filter(data_july, Group == "1960" | Group == "1970" |
                          Group == "1980" | Group == "1990" | Group == "2000" |
                          Group == "2010")
mean_by_decade <- data_july_dec %>%
  group_by(Group) %>%
  summarise(mean_val_dec = mean(Celcius))

x2 <- mean_by_decade$mean_val_dec
d2 <- mean(x2[2:6] - x2[1:5])
data_july_dec$h2 <- c(rep(x2[1:1], 10 * 31), rep(x2[1:1] + d2, 10 * 31),
                      rep(x2[1:1] + 2 * d2, 10 * 31),
                      rep(x2[1:1] + 3 * d2, 10 * 31),
                      rep(x2[1:1] + 4 * d2, 10 * 31),
                      rep(x2[1:1] + 5 * d2, 10 * 31))
ggplot(data_july_dec, aes(Date, Celcius)) + geom_point(aes(colour = Year)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  geom_hline(data = mean_by_decade, aes(yintercept = mean_val_dec), size = 1) +
  geom_line(aes(Date, h2), size = 1) +
  facet_grid(~Group, scales = "free_x", labeller =
               labeller(Group = c("1960" = "1960s", "1970" = "1970s",
                                  "1980" = "1980s", "1990" = "1990s",
                                  "2000" = "2000s", "2010" = "2010s"))) +
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Date",
       y = "Temperature (\u00B0C)")

y_centered_d <- as.numeric(data_july_dec$Celcius -
                             c(rep(0, 10 * 31), rep(d2, 10 * 31),
                               rep(2 * d2, 10 * 31), rep(3 * d2, 10 * 31),
                               rep(4 * d2, 10 * 31), rep(5 * d2, 10 * 31)))
hist(y_centered_d)

# 10 year periods from 1961
data_july$Group2 <- c(rep(0, 3 * 31), rep(1962, 10 * 31), rep(1972, 10 * 31),
                      rep(1982, 10 * 31), rep(1992, 10 * 31),
                      rep(2002, 10 * 31), rep(2012, 10 * 31))
data_jul_10 <- filter(data_july, Group2 == "1962" | Group2 == "1972" |
                        Group2 == "1982" | Group2 == "1992" |
                        Group2 == "2002" | Group2 == "2012")
mean_by_10 <- data_jul_10 %>%
  group_by(Group2) %>%
  summarise(mean_val_10 = mean(Celcius))
x <- mean_by_10$mean_val_10
d <- mean(x[2:6] - x[1:5])
data_jul_10$h <- c(rep(x[1:1], 10 * 31), rep(x[1:1] + d, 10 * 31),
                   rep(x[1:1] + 2 * d, 10 * 31),
                   rep(x[1:1] + 3 * d, 10 * 31),
                   rep(x[1:1] + 4 * d, 10 * 31),
                   rep(x[1:1] + 5 * d, 10 * 31))
ggplot(data_jul_10, aes(Date, Celcius)) + geom_point(aes(colour = Year)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  geom_hline(data = mean_by_10, aes(yintercept = mean_val_10), size = 1) +
  geom_line(aes(Date, h), col = "red", size = 1) +
  facet_grid(~Group2, scales = "free_x") +
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Date",
       y = "Temperature (\u00B0C)")

# center the observations on the level of the first mean
y_centered <- as.numeric(data_jul_10$Celcius -
                           c(rep(0, 10 * 31), rep(d, 10 * 31),
                             rep(2 * d, 10 * 31), rep(3 * d, 10 * 31),
                             rep(4 * d, 10 * 31), rep(5 * d, 10 * 31)))

# ## test
# obs <- y_centered_d
# l <- length(obs)
# p <- 10^-30
# values_gamma_m <- rep(0,400)
# for (k in 2:(400)) {
#   values_gamma_m[k] <- gamma_m(obs, l, k)
# }
# plot(values_gamma_m, type = 'l')
#
# values_p_q <- rep(0,400)
# for (k in 2:(400)) {
#   values_p_q[k] <- p_q(obs, l, k, p)
# }
# plot(values_p_q, type = 'l')
# median(values_p_q)
# max(values_p_q)
# ##

l <- length(y_centered)

values_gamma_m <- rep(0, l - 1)
for (k in 1:(l - 1)) {
  values_gamma_m[k] <- gamma_m(y_centered, l, k)
}
plot(values_gamma_m, type = "l", xlab = "k", ylab = "moment estimator")

values_sigma_m <- rep(0, l - 1)
for (k in 1:(l - 1)) {
  values_sigma_m[k] <- sigma_m(y_centered, l, k)
}

plot(values_sigma_m, type = "l", xlab = "k", ylab = "scale estimator (mom.)")

values_median <- rep(0, 10)
values_mean <- rep(0, 10)
values_min <- rep(0, 10)
values_max <- rep(0, 10)
k_range <- c(1096, 919, 734, 566, 437, 317, 214, 124, 73, 31)
temp <- 33:42

for (i in 1:10) {
  values_p_estim <- rep(0, k_range[i] - 1)
  for (k in 1:(k_range[i] - 1)) {
    values_p_estim[k] <- p_estim(y_centered, l, 1000, k, temp[i], 6 * d)
  }
  hist(values_p_estim[10:(k_range[i] - 1)])
  # plot(values_p_estim, type = "l", xlab = "k", ylab = "p estim.", main =
  #        paste("Probability for T = ", temp[i], " \u00B0C"))
  # values_median[i] <- p_10(median(values_p_estim[10:(k_range[i] - 1)]))
  # values_mean[i] <- p_10(mean(values_p_estim[10:(k_range[i] - 1)]))
  # values_min[i] <- p_10(min(values_p_estim[10:(k_range[i] - 1)]))
  # values_max[i] <- p_10(max(values_p_estim[10:(k_range[i] - 1)]))
}

table <- data.frame(temp, values_min, values_median, values_mean, values_max)
names(table) <- c("Temperature", "Min", "Median", "Max")

#T = 33, n <- 1096, median 0.02369829
#T = 34, n <- 919, median 0.0156573
#T = 35, n <- 734, median 0.01072835
#T = 36, n <- 566, median 0.006619595
#T = 37, n <- 437, median 0.003813095
#T = 38, n <- 317, median 0.001848534
#T = 39, n <- 214, median 0.0008450508
#T = 40, n <- 124, median 0.0004226507
#T = 41, n <- 73, median 7.087633e-05
#T = 42, n <- 31, median 2.085661e-05
