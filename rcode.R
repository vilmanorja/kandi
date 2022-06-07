setwd("/m/home/home3/33/norjav2/unix/Documents/kandi_local/kandi")

data <- read.table(file = "anjala.csv", header = TRUE, sep = ",", col.names = 
                     c('Year', 'Month', 'Day', 'Hour', 'Time zone', 'Temperature'))

data_july <- data[data$Month == "7",]

data_july2 <- data.frame(data_july$Year, data_july$Temperature)

temp_ts = ts(data_july$Temperature, start = 1959, frequency = 31)

ts.plot(temp_ts, xlab = 'Year', ylab = 'top temperature/Celcius')

mean_temp <- aggregate(data_july2$data_july.Temperature, list(data_july2$data_july.Year), FUN = mean)
group <- aggregate(data_july2$data_july.Temperature, list(data_july2$data_july.Year))

plot(mean_temp)
