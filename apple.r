###--------------------------------------------------
### Apple quarterly sales data.
###--------------------------------------------------


library(dplyr)
library(ggplot2)
library(tidyr)
library(splines)
library(scales)

theme_set(theme_minimal())

data <- read.csv("data/apple-all-products-quarterly-sales.csv", header=TRUE)
data$Date <- seq(as.Date("1998/12/31"), as.Date("2015/4/1"), by = "quarter")


data.m <- gather(data, Product, Sales, iPhone:Mac)

## Sales trends with a loess smoother
p <- ggplot(subset(data.m, Product!="iPod" & Period>30), aes(x=Date, y=Sales, color=Product, fill=Product))
p + geom_point(size=1.3) + geom_smooth(size=0.8, se=FALSE) + theme(legend.position="top") + scale_x_date(labels = date_format("%Y"), breaks=date_breaks("year")) + xlab("") + ylab("Sales (millions)")

## Strong Mac growth gets swamped compared to the others
p <- ggplot(subset(data.m, Product=="Mac"), aes(x=Date, y=Sales, color=Product, fill=Product))
p + geom_point(size=0.8) + geom_smooth(size=0.8, se=FALSE) + theme(legend.position="top") + scale_x_date(labels = date_format("%Y"), breaks=date_breaks("year")) + xlab("") + ylab("Sales (millions)")


### quick time series decompositions
ipad <- ts(data[47:66, "iPad"], start=c(2010, 2), frequency = 4)
plot(stl(ipad, s.window = 5, t.jump = 1))
title("Loess Decomposition of iPad Sales")

iphone <- ts(data[35:66, "iPhone"], start=c(2007, 2), frequency = 4)
plot(stl(iphone, s.window = 5, t.jump = 1))
title("Loess Decomposition of iPhone Sales")
