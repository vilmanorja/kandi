#old version

setwd("/m/home/home3/33/norjav2/unix/Documents/kandi_local/kandi")

#read data
data <- read.table(file = "anjala.csv", header = TRUE, sep = ",", col.names = 
                     c('Year', 'Month', 'Day', 'Hour', 'Time zone', 'Celcius'))
#setwd("C:/Users/vilma/OneDrive - Aalto University/Yliopisto/Kandi")

library(ggplot2)
library(dplyr)
#library(forecast)
#library(carData)
#library(car)
#library(lmtest)
#library(tseries)
library(gridExtra)
library(scales)

theme_update(plot.title = element_text(hjust = 0.5))
#data <- read.table(file = "anjaladata.csv", header = TRUE, sep= ",", col.names = c('Year','Month', "Day", "hour", "aikavyohyke", 'Celcius'))
data$Date <- as.Date(with(data, paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

#remove and edit data
data_july <- data[data$Month == "7",]
data_july$Group <- c(rep(1959,31), rep(1960,10*31), rep(1970,10*31), rep(1980,10*31), rep(1990,10*31), rep(2000,10*31), rep(2010,10*31), rep(2020,2*31))

temp = ts(data_july$Celcius, start = 1959, frequency = 31)

ts.plot(temp, xlab = "Year",
        ylab = "Celcius")

#mean_temp <- aggregate(data_july$Celcius, list(data_july$Year), FUN=mean)
mean <- data_july%>% group_by(Year)%>%summarise(mean_val=mean(Celcius))
ggplot(data_july, aes(Date, Celcius)) + geom_point(aes(colour=Year)) + 
  geom_hline(data = mean, aes(yintercept = mean_val)) + 
  facet_grid(~Year, scales = "free_x") + 
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Day (July)", y = "Temperature (\u00B0C)")

ts.plot(ts(mean$mean_val, start = 1959, frequency = 1))
#one year of each decade
data_10 <- filter(data_july, Year == "1961" | Year == "1971" | Year == "1981" | Year == "1991" | Year == "2001" | Year == "2011" | Year == "2021")
mean_10 <- data_10%>% group_by(Year)%>%summarise(mean_val_10=mean(Celcius))
ggplot(data_10, aes(Day, Celcius)) + geom_point(color = 'darkblue') + 
  geom_hline(data = mean_10, aes(yintercept = mean_val_10)) + 
  facet_grid(~Year, scales = "free_x")  + 
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Day (July)", y = "Temperature (\u00B0C)")

#suomeksi
data_10 <- filter(data_july, Year == "1961" | Year == "1971" | Year == "1981" | Year == "1991" | Year == "2001" | Year == "2011" | Year == "2021")
mean_10 <- data_10%>% group_by(Year)%>%summarise(mean_val_10=mean(Celcius))
ggplot(data_10, aes(Day, Celcius)) + geom_point(color = 'darkgreen') + 
  geom_hline(data = mean_10, aes(yintercept = mean_val_10)) + 
  facet_grid(~Year, scales = "free_x")  + 
  labs(title = "Vuorokauden ylin lämpötila Anjalassa", x = "Päivä (heinäkuu)", y = "Lämpötila (\u00B0C)")

#Compare two different decades
data_compare1 <- filter(data_july, Group == "1970" | Group == "2010")
mean_compare1 <- data_compare1%>% group_by(Year)%>%summarise(mean_val_comp1=mean(Celcius))
ggplot(data_compare1, aes(Day, Celcius)) + 
  geom_point(aes(colour=Year)) + 
  geom_hline(data = mean_compare1, aes(yintercept = mean_val_comp1, col=Year)) + 
  facet_grid(~Year, scales = "free_x")  + 
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Day (July)", y = "Temperature (\u00B0C)", colour = "Year")


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

data_compare2 <- filter(data_july, Group == "1960" |Group == "1980" | Group == "2000")
mean_compare2 <- data_compare2%>% group_by(Group)%>%summarise(mean_val_comp2=mean(Celcius))
ggplot(data_compare2, aes(Date, Celcius)) + geom_point() + 
geom_hline(data = mean_compare2, aes(yintercept = mean_val_comp2)) + 
facet_grid(~Group, scales = "free_x", labeller = labeller(Group = c('1960'="1960s",'1980'="1980s",'2000'="2000s"))) + labs(x = "Year", y = "Temperature (\u00B0C)")

data_july2 <- data.frame(data_july$Year, data_july$Day, data_july$Temperature)

#Decades
data_july_dec <- filter(data_july, Group == "1960" | Group == "1970" | Group == "1980" | Group == "1990" | Group == "2000" |Group == "2010")
mean_byDecade <- data_july_dec%>% group_by(Group)%>%summarise(mean_val_dec=mean(Celcius))
ggplot(data_july_dec, aes(Date, Celcius)) + geom_point(aes(colour=Year)) + 
  scale_x_date(date_breaks = "3 years" , date_labels = "%Y") +
  geom_hline(data = mean_byDecade, aes(yintercept = mean_val_dec), size=1) + 
  facet_grid(~Group, scales = "free_x", labeller = labeller(Group = c('1960'="1960s",'1970'="1970s",'1980'="1980s",'1990'="1990s",'2000'="2000s", '2010'="2010s"))) + 
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Date", y = "Temperature (\u00B0C)")
x2 <- mean_byDecade[2]
h2 <- sum(x2[2:6,]-x2[1:5,])*1/5
data_july_dec$h2 <- c(rep(x2[1,1],10*31),rep(x2[1,1]+h2,10*31),rep(x2[1,1]+2*h2,10*31),rep(x2[1,1]+3*h2,10*31),rep(x2[1,1]+4*h2,10*31),rep(x2[1,1]+5*h2,10*31))
y_centered2 <- data_july_dec$Celcius-as.numeric(data_july_dec$h2)

#Decades suom.
data_july_dec <- filter(data_july, Group == "1960" | Group == "1970" | Group == "1980" | Group == "1990" | Group == "2000" |Group == "2010")
mean_byDecade <- data_july_dec%>% group_by(Group)%>%summarise(mean_val_dec=mean(Celcius))
ggplot(data_july_dec, aes(Date, Celcius)) + geom_point(aes(colour=Year)) + 
  scale_x_date(date_breaks = "4 years" , date_labels = "%Y") +
  geom_hline(data = mean_byDecade, aes(yintercept = mean_val_dec), size=1) + 
  facet_grid(~Group, scales = "free_x", labeller = labeller(Group = c('1960'="1960-luku",'1970'="1970-luku",'1980'="1980-luku",'1990'="1990-luku",'2000'="2000-luku", '2010'="2010-luku"))) + 
  labs(title = "Heinäkuun vuorokausien ylin lämpötila Anjalassa vuodesta 1960 alkaen", x = "Päivämäärä", y = "Lämpötila (\u00B0C)")


#mean_temp <- aggregate(data_july$Temperature, list(data_july$Year), FUN = mean)
#plot(mean_temp)

#10 year periods from 1961
data_july$Group2 <- c(rep(0,3*31), rep(1962,10*31), rep(1972,10*31), rep(1982,10*31), rep(1992,10*31), rep(2002,10*31), rep(2012,10*31))
data_jul_dec <- filter(data_july, Group2 == "1962" | Group2 == "1972" | Group2 == "1982" | Group2 == "1992" | Group2 == "2002" |Group2 == "2012")
mean_by_dec <- data_jul_dec%>% group_by(Group2)%>%summarise(mean_val_dec=mean(Celcius))
x <- mean_by_dec[2]
h <- sum(x[2:6,]-x[1:5,])*1/5
data_jul_dec$h <- c(rep(x[1,1],10*31),rep(x[1,1]+h,10*31),rep(x[1,1]+2*h,10*31),rep(x[1,1]+3*h,10*31),rep(x[1,1]+4*h,10*31),rep(x[1,1]+5*h,10*31))
ggplot(data_jul_dec, aes(Date, Celcius)) + geom_point(aes(colour=Year)) + 
  scale_x_date(date_breaks = "3 years" , date_labels = "%Y") +
  geom_hline(data = mean_by_dec, aes(yintercept = mean_val_dec), size=1) + 
  facet_grid(~Group2, scales = "free_x", labeller = labeller(Group2 = c('1960'="1960s",'1970'="1970s",'1980'="1980s",'1990'="1990s",'2000'="2000s", '2010'="2010s"))) + 
  labs(title = "Maximum daily temperatures in July in Anjala", x = "Date", y = "Temperature (\u00B0C)")

plot(data_jul_dec$Date, data_jul_dec$Celciu)
lines(data_jul_dec$Date, data_jul_dec$h, lwd=2.0)

#y_centered <- data_jul_dec$Celcius-as.numeric(data_jul_dec$h)
y_centered <- as.numeric(data_jul_dec$Celcius-c(rep(0,10*31),rep(h,10*31),rep(2*h,10*31),rep(3*h,10*31),rep(4*h,10*31),rep(5*h,10*31)))
hist(y_centered)

#moment estimator
n <- length(data_jul_dec$Celcius)
values <- rep(0,n-1)

for(k in 1:n-1){
  y_ordered <- sort(y_centered,decreasing = FALSE)[k:n]
  m_n_1 <- 1/k*sum((log(y_ordered[2:length(y_ordered)])-log(y_ordered[1])))
  m_n_2 <- 1/k*sum((log(y_ordered[2:length(y_ordered)])-log(y_ordered[1]))^2)
  values[k] <- m_n_1+1-(1/2)*(1-(m_n_1^2/m_n_2))^(-1)
}
