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
pdf(file="figures/apple-sales-trends.pdf", height=4, width=8)
p <- ggplot(subset(data.m, Product!="iPod" & Period>30), aes(x=Date, y=Sales, color=Product, fill=Product))
p0 <- p + geom_point(size=1.3) + geom_smooth(size=0.8, se=FALSE) + theme(legend.position="top") + scale_x_date(labels = date_format("%Y"), breaks=date_breaks("year")) + xlab("") + ylab("Sales (millions)")
print(p0)
dev.off()

ggsave("figures/apple-sales-trends.png", p0, height=4, width=8, dpi=300)


## Strong Mac growth gets swamped compared to the others
pdf(file="figures/apple-sales-trends-mac.pdf", height=4, width=8)
p <- ggplot(subset(data.m, Product=="Mac"), aes(x=Date, y=Sales, color=Product, fill=Product))
p0 <- p + geom_point(size=0.8) + geom_smooth(size=0.8, se=FALSE) + theme(legend.position="top") + scale_x_date(labels = date_format("%Y"), breaks=date_breaks("year")) + xlab("") + ylab("Sales (millions)")
print(p0)
dev.off()

ggsave("figures/apple-sales-trends-mac.png", p0, height=4, width=8, dpi=300)



### quick time series decompositions
ipad <- data.m %>% group_by(Product) %>% filter(Product=="iPad") %>% na.omit() %>% data.frame(.)
ipad.ts <- ts(ipad$Sales, start=c(2010, 2), frequency = 4)

pdf(file="figures/apple-ipad-decomposition.pdf", height=4, width=8)
plot(stl(ipad.ts, s.window = "periodic", t.jump = 1))
title("Loess Decomposition of iPad Sales")
dev.off()

iphone <- data.m %>% group_by(Product) %>% filter(Product=="iPhone") %>%
    na.omit() %>% data.frame(.)
iphone.ts <- ts(iphone$Sales, start=c(2007, 2), frequency = 4)
pdf(file="figures/apple-iphone-decomposition.pdf", height=4, width=8)
plot(stl(iphone.ts, s.window = "periodic", t.jump = 1))
title("Loess Decomposition of iPhone Sales")
dev.off()

mac <- data.m %>% group_by(Product) %>% filter(Product=="Mac") %>%
    na.omit() %>% data.frame(.)
mac.ts <- ts(mac$Sales, start=c(1998, 4), frequency = 4)
pdf(file="figures/apple-mac-decomposition.pdf", height=4, width=8)
plot(stl(mac.ts, s.window = "periodic", t.jump = 1))
title("Loess Decomposition of Mac Sales")
dev.off()
