#Visulaizations for temperature data

setwd("/m/home/home3/33/norjav2/data/Documents/kandi_local/kandi/code")
source("rcode.R")
#read data
data <- read.table(file = "anjala.csv", header = TRUE, sep = ",", col.names = 
                     c('Year', 'Month', 'Day', 'Hour', 'Time zone', 'Celcius'))
#setwd("C:/Users/vilma/OneDrive - Aalto University/Yliopisto/Kandi")
#data <- read.table(file = "anjaladata.csv", header = TRUE, sep= ",", col.names = c('Year','Month', "Day", "hour", "aikavyohyke", 'Celcius'))

library(ggplot2)
library(dplyr)
#library(forecast)
#library(carData)
#library(car)
#library(lmtest)
#library(tseries)
library(gridExtra)
library(scales)

#center text in figures
theme_update(plot.title = element_text(hjust = 0.5))

#remove and edit data
data$Date <- as.Date(with(data, paste(Year,Month,Day,sep="-")),"%Y-%m-%d")
data_july <- data[data$Month == "7",]
data_july$Group <- c(rep(1959,31), rep(1960,10*31), rep(1970,10*31), rep(1980,10*31), rep(1990,10*31), rep(2000,10*31), rep(2010,10*31), rep(2020,2*31))

#one year of each decade
data_10 <- filter(data_july, Year == "1961" | Year == "1971" | Year == "1981" | Year == "1991" | Year == "2001" | Year == "2011" | Year == "2021")
mean_10 <- data_10%>% group_by(Year)%>%summarise(mean_val_10=mean(Celcius))
ggplot(data_10, aes(Day, Celcius)) + geom_point(color = 'darkblue') + 
  geom_hline(data = mean_10, aes(yintercept = mean_val_10)) + 
  facet_grid(~Year, scales = "free_x")  + 
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Day (July)", y = "Temperature (\u00B0C)")

#suom. one year of each decade
data_10 <- filter(data_july, Year == "1961" | Year == "1971" | Year == "1981" | Year == "1991" | Year == "2001" | Year == "2011" | Year == "2021")
mean_10 <- data_10%>% group_by(Year)%>%summarise(mean_val_10=mean(Celcius))
ggplot(data_10, aes(Day, Celcius)) + geom_point(color = 'darkgreen') + 
  geom_hline(data = mean_10, aes(yintercept = mean_val_10)) + 
  facet_grid(~Year, scales = "free_x")  + 
  labs(title = "Vuorokauden ylin lämpötila Anjalassa", x = "Päivä (heinäkuu)", y = "Lämpötila (\u00B0C)")

#Compare two different decades
comp_1 <- filter(data_july, Group == "1970")
mean_1 <- comp_1%>% group_by(Year)%>%summarise(mean_val_1=mean(Celcius))
ggp1 <- ggplot(comp_1, aes(Day, Celcius)) + geom_point(colour= 'darkred') + 
  geom_hline(data = mean_1, aes(yintercept = mean_val_1)) + 
  facet_grid(~Year, scales = "free_x")  + 
  labs(x = "Day (July)", y = "Temperature (\u00B0C)") + ylim(5,35)

comp_2 <- filter(data_july, Group == "2010")
mean_2 <- comp_2%>% group_by(Year)%>%summarise(mean_val_2=mean(Celcius))
ggp2 <- ggplot(comp_2, aes(Day, Celcius)) + geom_point(colour= 'darkblue') + 
  geom_hline(data = mean_2, aes(yintercept = mean_val_2)) + 
  facet_grid(~Year, scales = "free_x")  + 
  labs(x = "Day (July)", y = "Temperature (\u00B0C)") + ylim(5,35)

grid.arrange(ggp1, ggp2, ncol = 2, top = "Maximum daily temperatures in July in Anjala")

#Decades
data_july_dec <- filter(data_july, Group == "1960" | Group == "1970" | Group == "1980" | Group == "1990" | Group == "2000" |Group == "2010")
mean_byDecade <- data_july_dec%>% group_by(Group)%>%summarise(mean_val_dec=mean(Celcius))
x2 <- mean_byDecade$mean_val_dec
d2 <- mean(x2[2:6]-x2[1:5])
data_july_dec$h2 <- c(rep(x2[1:1],10*31),rep(x2[1:1]+d2,10*31),rep(x2[1:1]+2*d2,10*31),rep(x2[1:1]+3*d2,10*31),rep(x2[1:1]+4*d2,10*31),rep(x2[1:1]+5*d2,10*31))
ggplot(data_july_dec, aes(Date, Celcius)) + geom_point(aes(colour=Year)) + 
  scale_x_date(date_breaks = "3 years" , date_labels = "%Y") +
  geom_hline(data = mean_byDecade, aes(yintercept = mean_val_dec), size=1) + 
  geom_line(aes(Date, h2), size=1) + 
  facet_grid(~Group, scales = "free_x", labeller = labeller(Group = c('1960'="1960s",'1970'="1970s",'1980'="1980s",'1990'="1990s",'2000'="2000s", '2010'="2010s"))) + 
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Date", y = "Temperature (\u00B0C)")

#y_centered_10 <- data_july_dec$Celcius-as.numeric(data_july_dec$h2)
y_centered_10 <- as.numeric(data_july_dec$Celcius-c(rep(0,10*31),rep(d2,10*31),rep(2*d2,10*31),rep(3*d2,10*31),rep(4*d2,10*31),rep(5*d2,10*31)))
hist(y_centered_10)

#suom. Decades
data_july_dec <- filter(data_july, Group == "1960" | Group == "1970" | Group == "1980" | Group == "1990" | Group == "2000" |Group == "2010")
mean_byDecade <- data_july_dec%>% group_by(Group)%>%summarise(mean_val_dec=mean(Celcius))
ggplot(data_july_dec, aes(Date, Celcius)) + geom_point(aes(colour=Year)) + 
  scale_x_date(date_breaks = "4 years" , date_labels = "%Y") +
  geom_hline(data = mean_byDecade, aes(yintercept = mean_val_dec), size=1) + 
  facet_grid(~Group, scales = "free_x", labeller = labeller(Group = c('1960'="1960-luku",'1970'="1970-luku",'1980'="1980-luku",'1990'="1990-luku",'2000'="2000-luku", '2010'="2010-luku"))) + 
  labs(title = "Heinäkuun vuorokausien ylin lämpötila Anjalassa vuodesta 1960 alkaen", x = "Päivämäärä", y = "Lämpötila (\u00B0C)")


#10 year periods from 1961
data_july$Group2 <- c(rep(0,3*31), rep(1962,10*31), rep(1972,10*31), rep(1982,10*31), rep(1992,10*31), rep(2002,10*31), rep(2012,10*31))
data_jul_10 <- filter(data_july, Group2 == "1962" | Group2 == "1972" | Group2 == "1982" | Group2 == "1992" | Group2 == "2002" |Group2 == "2012")
mean_by_10 <- data_jul_10%>% group_by(Group2)%>%summarise(mean_val_10=mean(Celcius))
x <- mean_by_10$mean_val_10
d <- mean(x[2:6]-x[1:5])
data_jul_10$h <- c(rep(x[1:1],10*31),rep(x[1:1]+d,10*31),rep(x[1:1]+2*d,10*31),rep(x[1:1]+3*d,10*31),rep(x[1:1]+4*d,10*31),rep(x[1:1]+5*d,10*31))
ggplot(data_jul_10, aes(Date, Celcius)) + geom_point(aes(colour=Year)) + 
  scale_x_date(date_breaks = "3 years" , date_labels = "%Y") +
  geom_hline(data = mean_by_10, aes(yintercept = mean_val_10), size=1) + 
  geom_line(aes(Date, h), size=1) + 
  facet_grid(~Group2, scales = "free_x") + 
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Date", y = "Temperature (\u00B0C)")

y_centered <- as.numeric(data_jul_10$Celcius-c(rep(0,10*31),rep(d,10*31),rep(2*d,10*31),rep(3*d,10*31),rep(4*d,10*31),rep(5*d,10*31)))
#y_centered <- data_jul_10$Celcius-as.numeric(data_jul_10$h)

## test
obs <- y_centered_10
l <- length(obs)
p <- 10^-30
values_gamma_m <- rep(0,400)
for (k in 2:(400)) {
  values_gamma_m[k] <- gamma_m(obs, l, k)
}
plot(values_gamma_m, type = 'l')

values_p_q <- rep(0,400)
for (k in 2:(400)) {
  values_p_q[k] <- p_q(obs, l, k, p)
}
plot(values_p_q, type = 'l')
median(values_p_q)
max(values_p_q)
##

n <- length(data_jul_10$Celcius)

values_gamma_m <- rep(0,n-1)
for (k in 1:(n-1)) {
  values_gamma_m[k] <- gamma_m(y_centered, n, k)
}
plot(values_gamma_m, type = 'l')

values_sigma_m <- rep(0,n-1)
for (k in 1:(n-1)) {
  values_sigma_m[k] <- sigma_m(y_centered, n, k)
}

plot(values_sigma_m, type = 'l')

values_p_estim <- rep(0,800)
for (k in 1:(800)) {
  values_p_estim[k] <- p_estim(y_centered, n, k, 35, 6*d)
}
plot(values_p_estim, type = 'l')
hist(values_p_estim)

#use decades instead of xxx2-xxx1 
n <- length(y_centered_10)

values_sigma_m <- rep(0,n-1)
for (k in 1:(n-1)) {
  values_sigma_m[k] <- sigma_m(y_centered_10, n, k)
}

plot(values_sigma_m, type = 'l')

values_p_estim2 <- rep(0,800)
for (k in 1:(800)) {
  values_p_estim2[k] <- p_estim(y_centered_10, length(y_centered_10), k, 45, 6*d2)
}
plot(values_p_estim2, type = 'l')

#assume gamma=0
values_p_estim <- rep(0,1000)
for (k in 1:(1000)) {
  values_p_estim[k] <- p_estim_zero(y_centered, n, k, 45, 6*d)
}
plot(values_p_estim, type = 'l')
hist(values_p_estim, breaks = 1000)

values_p_estim <- rep(0,1000)
for (k in 1:(1000)) {
  values_p_estim[k] <- p_estim_zero(y_centered_10, length(y_centered_10), k, 45, 6*d2)
}
plot(values_p_estim, type = 'l')
