#setwd("/m/home/home3/33/norjav2/unix/Documents/kandi_local/kandi")

setwd("C:/Users/vilma/OneDrive - Aalto University/Yliopisto/Kandi")

library(ggplot2)
library(dplyr)
library(forecast)
library(carData)
library(car)
library(lmtest)
library(tseries)
library(gridExtra)

data <- read.table(file = "anjaladata.csv", header = TRUE, sep= ",", col.names = c('Year','Month', "Day", "hour", "aikavyohyke", 'Celcius'))
data$Date <- as.Date(with(data, paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

data_july <- data[data$Month == "7",]
data_july$Group <- c(rep(1959,31), rep(1960,10*31), rep(1970,10*31), rep(1980,10*31), rep(1990,10*31), rep(2000,10*31), rep(2010,10*31), rep(2020,2*31))

temp = ts(data_july$Celcius, start = 1959, frequency = 31)

ts.plot(temp, xlab = "Year",
        ylab = "Celcius")

#mean_temp <- aggregate(data_july$Celcius, list(data_july$Year), FUN=mean)
mean <- data_july%>% group_by(Year)%>%summarise(mean_val=mean(Celcius))
ggplot(data_july, aes(Date, Celcius)) + geom_point(aes(colour=Year)) + geom_hline(data = mean, aes(yintercept = mean_val)) + facet_grid(~Year, scales = "free_x") + labs(y = "Temperature (\u00B0C)")

ts.plot(ts(mean$mean_val, start = 1959, frequency = 1))
#one year of each decade
data_10 <- filter(data_july, Year == "1961" | Year == "1971" | Year == "1981" | Year == "1991" | Year == "2001" | Year == "2011" | Year == "2021")
mean_10 <- data_10%>% group_by(Year)%>%summarise(mean_val_10=mean(Celcius))
ggplot(data_10, aes(Day, Celcius)) + geom_point(color = 'darkrblue') + geom_hline(data = mean_10, aes(yintercept = mean_val_10)) + facet_grid(~Year, scales = "free_x")  + labs(y = "Temperature (\u00B0C)")

#Compare two different decades
data_compare1 <- filter(data_july, Group == "1970" | Group == "2010")
mean_compare1 <- data_compare1%>% group_by(Year)%>%summarise(mean_val_comp1=mean(Celcius))
ggplot(data_compare1, aes(Day, Celcius)) + geom_point(aes(colour=Year)) + geom_hline(data = mean_compare1, aes(yintercept = mean_val_comp1, col=Year)) + facet_grid(~Year, scales = "free_x")  + labs(y = "Temperature (\u00B0C)", colour = "Year")


comp_1 <- filter(data_july, Group == "1970")
mean_1 <- comp_1%>% group_by(Year)%>%summarise(mean_val_1=mean(Celcius))
ggp1 <- ggplot(comp_1, aes(Day, Celcius)) + geom_point(colour= 'darkred') + geom_hline(data = mean_1, aes(yintercept = mean_val_1)) + facet_grid(~Year, scales = "free_x")  + labs(y = "Temperature (\u00B0C)") + ylim(5,35)


comp_2 <- filter(data_july, Group == "2010")
mean_2 <- comp_2%>% group_by(Year)%>%summarise(mean_val_2=mean(Celcius))
ggp2 <- ggplot(comp_2, aes(Day, Celcius)) + geom_point(colour= 'darkblue') + geom_hline(data = mean_2, aes(yintercept = mean_val_2)) + facet_grid(~Year, scales = "free_x")  + labs(y = "Temperature (\u00B0C)") + ylim(5,35)


grid.arrange(ggp1, ggp2, ncol = 2)

data_compare2 <- filter(data_july, Group == "1960" | Group == "2010")
mean_compare2 <- data_compare2%>% group_by(Group)%>%summarise(mean_val_comp2=mean(Celcius))
ggplot(data_compare2, aes(Date, Celcius, colour=Group)) + geom_point() + geom_hline(data = mean_compare2, aes(yintercept = mean_val_comp2, col=Group)) + facet_grid(~Group, scales = "free_x") + labs(x = "Year", y = "Temperature (\u00B0C)", colour = "Decade")

#Decades
mean_byDecade <- data_july%>% group_by(Group)%>%summarise(mean_val_dec=mean(Celcius))
ggplot(data_july, aes(Date, Celcius)) + geom_point(aes(colour=Group)) + geom_hline(data = mean_byDecade, aes(yintercept = mean_val_dec, col=Group), size=1) + facet_grid(~Group, scales = "free_x") + labs(x = "Date", y = "Temperature (\u00B0C)", colour = "Decades")

#Stationarize
acf(temp, lag.max=168)
acf(temp, lag.max=168, type = "partial")
#2.5806
d = 1
S = 0 
D = 0 
dtemp = temp
if (d > 0) {
  dtemp = diff(dtemp, lag = 1, differences = d)
}
if (D > 0) {
  dtemp = diff(dtemp, lag = S, differences = D)
}

acf(dtemp, lag.max=168) #2.5484
acf(dtemp, lag.max=168, type = "partial")

ts.plot(dtemp,
        xlab = "year, dtemp")
