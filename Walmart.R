# Walmart Kaggle competition
# This package is to get the day of the week from date 
install.packages("lubridate")
library("lubridate")
# This package is for manipulating data
install.packages("dplyr")
library(dplyr)
install.packages("varSelRF")
library(varSelRF)
# Read into dateset
train = read.csv(file = "train.csv")
weather = read.csv(file = "weather.csv")
merged_train = read.csv(file = "merged_train.csv")
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

#Create a function featureEngineer to perform same operations on train and test data
featureEngineer = function(df){
  date = df$date
  df$day = wday(as.Date(date))
  merged_df = merge(df,key, by = "store_nbr")
}

# Write the merged files into disk
merged_train = featureEngineer(train)
merged_test = featureEngineer(test)
write.csv(merged_train, file = "merged_train.csv")
write.csv(merged_test, file = "merged_test.csv")
merged_train = read.csv(file = "merged_train.csv")
# Noticing that item#5 occurs quite often, choose it as an example to check relationship between data

m_2_5 = merged_train[merged_train$store_nbr == 2 & merged_train$item_nbr == 5,]
merged_weather_1_5 = merge(weather,train_1_5, by = c("date","station_nbr"))
m_w_2_5 = merge(weather,m_2_5, by = c("date","station_nbr"))
rf.vs1 = varSelRF(m_w_2_5, units, ntree = 500, ntreeIterat = 300,vars.drop.frac = 0.2)
fit_2_5 = randomForest(units ~ day+tavg+preciptotal, data=m_w_2_5, ntree=myNtree, mtry=5, importance=myImportance)
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
# install.packages("leaps")
# library(leaps)
install.packages("AUCRF")
library(AUCRF)
fit = AUCRF(units~.,data = merged_weather_2_5)
# out.exh = regsubsets(units~., data = merged_weather_2_5)
# There are 45 stores and 111 items, training 45*111 models for each item at each store
for (i in 1:45){
  for (j in 1:111){
    mydata = merged_train[merged_train$store_nbr == i & merged_train$item_nbr == j,]
    merged_weather = merge(weather,mydata, by = c("date","station_nbr"))
    fit = AUCRF(units~.,)
    
  }
}

