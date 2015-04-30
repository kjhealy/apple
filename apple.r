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
p0 <- p + geom_point(size=1.3) + geom_smooth(size=0.8, se=FALSE) + theme(legend.position="top") + scale_x_date(labels = date_format("%Y"), breaks=date_breaks("year")) + xlab("") + ylab("Sales (millions)") + scale_colour_manual(values=my.colors()) + scale_fill_manual(values=my.colors())
print(p0)
dev.off()

ggsave("figures/apple-sales-trends.png", p0, height=4, width=8, dpi=300)


## Strong Mac growth gets swamped compared to the others
pdf(file="figures/apple-sales-trends-mac.pdf", height=4, width=8)
p <- ggplot(subset(data.m, Product=="Mac"), aes(x=Date, y=Sales, color=Product, fill=Product))
p0 <- p + geom_point(size=0.8) + geom_smooth(size=0.8, se=FALSE) + theme(legend.position="top") + scale_x_date(labels = date_format("%Y"), breaks=date_breaks("year")) + xlab("") + ylab("Sales (millions)") + scale_color_manual(values=my.colors()[3])
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
mac.stl <- stl(mac.ts, s.window = "periodic", t.jump = 1)

pdf(file="figures/apple-mac-decomposition.pdf", height=4, width=10)
plot(stl(mac.ts, s.window = "periodic", t.jump = 1))
title("Loess Decomposition of Mac Sales")
dev.off()


### Look again
mac.stl2 <- stl(mac.ts, s.window = 11, t.jump = 1)
ggmac.stl <- data.frame(mac.stl2$time.series)
ggmac.stl$sales <- data$Mac
ggmac.stl$Date <- data$Date
ggmac.stl$Product <- "Mac"


p <- ggplot(ggmac.stl, aes(x=Date, y=sales))
p1 <- p + geom_line() + ylab("Data")

p <- ggplot(ggmac.stl, aes(x=Date, y=trend))
p2 <- p + geom_line() + ylab("Trend")

p <- ggplot(ggmac.stl, aes(x=Date, y=seasonal))
p3 <- p + geom_line() + ylab("Seasonal")

p <- ggplot(ggmac.stl, aes(x=Date, y=remainder))
p4 <- p + geom_bar(stat="identity", position="dodge") + ylab("Remainder")

p <- ggplot(ggmac.stl, aes(x=Date, y=(seasonal/trend)*100))
p5 <- p + geom_line(stat="identity", position="dodge") + ylab("Seasonal/\nTrend (pct)")

pdf(file="figures/apple-mac-decomposition-gg.pdf", height=8, width=12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(5, 1)))
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(3, 1))
print(p4, vp = vplayout(4, 1))
print(p5, vp = vplayout(5, 1))
dev.off()

ipad.stl2 <- stl(ipad.ts, s.window = 11, t.jump = 1)
ggipad.stl <- data.frame(ipad.stl2$time.series)
ggipad.stl$sales <- data$iPad %>% na.omit()
ind <- is.na(data$iPad)
ggipad.stl$Date <- data$Date[!ind]
ggipad.stl$Product <- "iPad"


p <- ggplot(ggipad.stl, aes(x=Date, y=sales))
p1 <- p + geom_line() + ylab("Data")

p <- ggplot(ggipad.stl, aes(x=Date, y=trend))
p2 <- p + geom_line() + ylab("Trend")

p <- ggplot(ggipad.stl, aes(x=Date, y=seasonal))
p3 <- p + geom_line() + ylab("Seasonal")

p <- ggplot(ggipad.stl, aes(x=Date, y=remainder))
p4 <- p + geom_bar(stat="identity", position="dodge") + ylab("Remainder")

p <- ggplot(ggipad.stl, aes(x=Date, y=(seasonal/trend)*100))
p5 <- p + geom_line(stat="identity", position="dodge") + ylab("Seasonal/\nTrend (pct)")

pdf(file="figures/apple-ipad-decomposition-gg.pdf", height=8, width=10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(5, 1)))
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(3, 1))
print(p4, vp = vplayout(4, 1))
print(p5, vp = vplayout(5, 1))
dev.off()

iphone.stl2 <- stl(iphone.ts, s.window = 11, t.jump = 1)
ggiphone.stl <- data.frame(iphone.stl2$time.series)
ggiphone.stl$sales <- data$iPhone %>% na.omit()
ind <- is.na(data$iPhone)
ggiphone.stl$Date <- data$Date[!ind]
ggiphone.stl$Product <- "iPhone"

p <- ggplot(ggiphone.stl, aes(x=Date, y=sales))
p1 <- p + geom_line() + ylab("Data")

p <- ggplot(ggiphone.stl, aes(x=Date, y=trend))
p2 <- p + geom_line() + ylab("Trend")

p <- ggplot(ggiphone.stl, aes(x=Date, y=seasonal))
p3 <- p + geom_line() + ylab("Seasonal")

p <- ggplot(ggiphone.stl, aes(x=Date, y=remainder))
p4 <- p + geom_bar(stat="identity", position="dodge") + ylab("Remainder")

p <- ggplot(ggiphone.stl, aes(x=Date, y=(seasonal/trend)*100))
p5 <- p + geom_line(stat="identity", position="dodge") + ylab("Seasonal/\nTrend (pct)")

pdf(file="figures/apple-iphone-decomposition-gg.pdf", height=8, width=10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(5, 1)))
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(3, 1))
print(p4, vp = vplayout(4, 1))
print(p5, vp = vplayout(5, 1))
dev.off()


stl.comb <- rbind(ggmac.stl, ggiphone.stl, ggipad.stl)
stl.comb$Ratio <- (stl.comb$seasonal/stl.comb$trend)*100

pdf(file="figures/apple-three-season-gg.pdf", height=6, width=10)
p <- ggplot(stl.comb, aes(x=Date, y=Ratio, color=Product))
p + geom_line(size=1.1) + ylab("Seasonal/Trend (pct)") +  scale_colour_manual(values=my.colors()) + theme(legend.position="top")
dev.off()
