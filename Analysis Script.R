data <- read.csv("activity.csv",header=TRUE)
library(lubridate)
data$date <- ymd(data$date)
data$isnull <- is.na(data$steps)

library(ggplot2)
plotData <- aggregate(isnull ~ date, data=data,sum)
qplot(date,isnull,data=aggregate(isnull ~ date, data=data,sum),color="steelblue")
?qplot
