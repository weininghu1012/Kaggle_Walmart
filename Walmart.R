# Walmart Kaggle competition
train = read.csv(file = "train.csv")
weather = read.csv(file = "weather.csv")
# key is to map the store number with the station number so linked back to weather
key = read.csv(file = "key.csv")
test = read.csv(file = "test.csv")
names(weather)
# [1] "station_nbr" "date"        "tmax"        "tmin"        "tavg"        "depart" (departure from normal)     
# "dewpoint"    "wetbulb"     "heat"(seasons begins with July)       
# [10] "cool" (seasons begins with January)       "sunrise"     "sunset"      "codesum"     "snowfall"    "preciptotal" 
# "stnpressure" "sealevel"    "resultspeed"
# [19] "resultdir"   "avgspeed"  
# Get sub of the train data that items are not 0
install.packages("dplyr")
library(dplyr)
# Merge the data in key.csv into train
merged_train = merge(train,key, by = "store_nbr")
write.csv(merged_train, file = "merged_train.csv")
pos_train = filter(train,units > 0)
# Noticing that item#5 occurs quite often, choose it as an example to check relationship between data
train_1_5 = filter(merged_train, store_nbr == 1,item_nbr == 5)
train_2_5 = filter(merged_train, store_nbr == 2,item_nbr == 5)
merged_weather_1_5 = merge(weather,train_1_5, by = c("date","station_nbr"))
merged_weather_2_5 = merge(weather,train_2_5, by = c("date","station_nbr"))

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}






