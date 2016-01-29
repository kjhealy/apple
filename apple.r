### --------------------------------------------------
### Apple quarterly sales data.
### --------------------------------------------------


library(grid)
library(ggplot2)
library(gtable)
library(dplyr)
library(tidyr)
library(lubridate)
library(splines)
library(scales)


### Nice alignment solution from Baptiste Auguié
### http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot/22984913
rbind_gtable_max <- function(...) {

    gtl <- list(...)
    stopifnot(all(sapply(gtl, is.gtable)))
    bind2 <- function(x, y) {
        stopifnot(ncol(x) == ncol(y))
        if (nrow(x) == 0)
            return(y)
        if (nrow(y) == 0)
            return(x)
        y$layout$t <- y$layout$t + nrow(x)
        y$layout$b <- y$layout$b + nrow(x)
        x$layout <- rbind(x$layout, y$layout)
        x$heights <- gtable:::insert.unit(x$heights, y$heights)
        x$rownames <- c(x$rownames, y$rownames)
        x$widths <- grid::unit.pmax(x$widths, y$widths)
        x$grobs <- append(x$grobs, y$grobs)
        x
    }

    Reduce(bind2, gtl)
}

##' Color-blind friendly palette
##' From \url{http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/}
##' @title Color-blind friendly palette
##' @param palette Choose 'cb', 'rcb', or 'bly'. cb is the Winston
##' Chang color blind palette; rcb is that palette in reverse; bly
##' puts the yellow and blue in the palette first.
##' @return Variations on an eight-color color-blind friendly palette.
##' @author Winston Chang / Kieran Healy
##' @export
my.colors <- function(palette = "cb") {
    ### The palette with grey:
    cb.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
        "#D55E00", "#CC79A7")
    ## Same one Reversed
    rcb.palette <- rev(cb.palette)
    ## Blue and yellow first choices
    bly.palette <- c("#E69F00", "#0072B2", "#999999", "#56B4E9", "#009E73", "#F0E442",
        "#D55E00", "#CC79A7")
    if (palette == "cb")
        return(cb.palette) else if (palette == "rcb")
        return(rcb.palette) else if (palette == "bly")
        return(bly.palette) else stop("Choose cb, rcb, or bly ony.")
}


### Convenience functions to draw the plots

get.stl <- function(data=data.m, prod.name = "iPhone", start.yr = 2007, start.q = 2){

    data.ts <- data %>% group_by(Product) %>% filter(Product == prod.name) %>%
        na.omit() %>% data.frame(.)

    prod.ts <- ts(data.ts$Sales, start = c(start.yr, start.q), frequency = 4)

    data.stl2 <- stl(prod.ts, s.window = 11, t.jump = 1)
    ggdata.stl <- data.frame(data.stl2$time.series)
    ggdata.stl$sales <- data.ts$Sales
    ggdata.stl$Date <- data.ts$Date
    ggdata.stl$Product <- data.ts$Product
    return(ggdata.stl)
}

draw.stl <- function(data.stl = iphone.stl,
                     prod.name = "iPhone",
                     bar.width = 4) {

    theme_set(theme_minimal())

    p <- ggplot(data.stl, aes(x = Date, y = sales))
    p1 <- p + geom_line() + ylab("Data") + xlab("") + theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)), plot.title = element_text(size = rel(1))) +
        ggtitle(paste(prod.name, "Units Sold (Millions)"))

    p <- ggplot(data.stl, aes(x = Date, y = trend))
    p2 <- p + geom_line() + ylab("Trend") + xlab("") + theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)))

    p <- ggplot(data.stl, aes(x = Date, y = seasonal))
    p3 <- p + geom_line() + ylab("Seasonal") + xlab("") + theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)))

    p <- ggplot(data.stl, aes(x = Date, ymax = remainder, ymin = 0))
    p4 <- p + geom_linerange(size = bar.width) + ylab("Remainder") + xlab("") + theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)))

    p <- ggplot(data.stl, aes(x = Date, y = (seasonal/trend) * 100))
    p5 <- p + geom_line(stat = "identity") + ylab("Seasonal/Trend (%)") + theme(axis.title.y = element_text(size = rel(0.8)))

    g1 <- ggplotGrob(p1)
    g2 <- ggplotGrob(p2)
    g3 <- ggplotGrob(p3)
    g4 <- ggplotGrob(p4)
    g5 <- ggplotGrob(p5)
    out <- rbind_gtable_max(g1, g2, g3, g4, g5)
    return(out)
}


theme_set(theme_minimal())

data <- read.csv("data/apple-all-products-quarterly-sales.csv", header = TRUE)
data$Date <- seq(as.Date("1998/12/31"), as.Date("2015/12/31"), by = "quarter")

data.m <- gather(data, Product, Sales, iPhone:Mac)

## Sales trends with a loess smoother
pdf(file = "figures/apple-sales-trends.pdf", height = 4, width = 8)
p <- ggplot(subset(data.m, Product != "iPod" & Period > 30), aes(x = Date, y = Sales,
    color = Product, fill = Product))
p0 <- p + geom_point(size = 1.3) + geom_smooth(size = 0.8, se = FALSE) + theme(legend.position = "top") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + xlab("") +
    ylab("Sales (millions)") + scale_colour_manual(values = my.colors()) + scale_fill_manual(values = my.colors())
print(p0)
dev.off()

ggsave("figures/apple-sales-trends.png", p0, height = 4, width = 8, dpi = 300)


## Strong Mac growth gets swamped compared to the others
pdf(file = "figures/apple-sales-trends-mac.pdf", height = 4, width = 8)
p <- ggplot(subset(data.m, Product == "Mac"), aes(x = Date, y = Sales, color = Product,
    fill = Product))
p0 <- p + geom_point(size = 0.8) + geom_smooth(size = 0.8, se = FALSE) + theme(legend.position = "top") +
    scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + xlab("") +
    ylab("Sales (millions)") + scale_color_manual(values = my.colors()[3])
print(p0)
dev.off()

ggsave("figures/apple-sales-trends-mac.png", p0, height = 4, width = 8, dpi = 300)


### Some other requests---from @siracusa Showing iPod as well. Smoothers don't work
### so well because the iPod seasonality is nuts. Use a moving average instead

library(TTR)

mac.ma <- SMA(data$Mac, 4)
ipod.ma <- c(SMA(data$iPod[1:64], 4), rep(NA, 5))  # SMA doesn't like non-leading NAs
iphone.ma <- SMA(data$iPhone, 4)
ipad.ma <- SMA(data$iPad, 4)

data.ma <- data.frame(data$Date, mac.ma, ipod.ma, iphone.ma, ipad.ma)
colnames(data.ma) <- c("Date", "Mac", "iPod", "iPhone", "iPad")

data.mam <- gather(data.ma, Product, Sales, iPad:Mac)

## Sales trends with a 4-period moving average
pdf(file = "figures/apple-sales-trends-siracusa-ma.pdf", height = 8, width = 10)
p <- ggplot(data.mam, aes(x = Date, y = Sales, color = Product, fill = Product))
p0 <- p + geom_line(size = 1) + theme(legend.position = "top") + scale_x_date(labels = date_format("%Y"),
    breaks = date_breaks("year")) + xlab("") + ylab("Moving Average of Sales (millions)") +
    scale_colour_manual(values = my.colors()) + scale_fill_manual(values = my.colors())
print(p0)
dev.off()

ggsave("figures/apple-sales-trends-siracusa-ma.png", p0, height = 8, width = 10, dpi = 300)

## Raw trends for all products
pdf(file = "figures/apple-sales-trends-raw-siracusa.pdf", height = 8, width = 10)
p <- ggplot(data.m, aes(x = Date, y = Sales, color = Product, fill = Product))
p0 <- p + geom_line(size = 1) + theme(legend.position = "top") + scale_x_date(labels = date_format("%Y"),
    breaks = date_breaks("year")) + xlab("") + ylab("Raw Sales (millions)") + scale_colour_manual(values = my.colors()) +
    scale_fill_manual(values = my.colors())
print(p0)
dev.off()

ggsave("figures/apple-sales-trends-raw-siracusa.png", p0, height = 8, width = 10,
    dpi = 300)





### Loess Decomposition

pdf(file = "figures/apple-mac-decomposition-gg.pdf", height = 8, width = 12)

mac.stl.df <- get.stl(data=data.m,
                      prod.name = "Mac",
                      start.yr = 1998,
                      start.q = 2)

mac.stl <- draw.stl(data.stl = mac.stl.df,
                    prod.name = "Mac",
                    bar.width = 4)
grid.newpage()
grid.draw(mac.stl)

dev.off()


pdf(file = "figures/apple-ipad-decomposition-gg.pdf", height = 8.2, width = 10)

ipad.stl.df <- get.stl(data = data.m,
                        prod.name = "iPad",
                        start.yr = 2010,
                        start.q = 2)

ipad.stl <- draw.stl(data = ipad.stl.df,
                    prod.name = "iPad",
                    bar.width = 8)


grid.newpage()
grid.draw(ipad.stl)

dev.off()


pdf(file = "figures/apple-iphone-decomposition-gg.pdf", height = 8, width = 10)

iphone.stl.df <- get.stl(data = data.m,
                         prod.name = "iPhone",
                         start.yr = 2007,
                         start.q = 2)

iphone.stl <- draw.stl(data.stl = iphone.stl.df,
                    prod.name = "iPhone",
                    bar.width = 6)


grid.newpage()
grid.draw(iphone.stl)


dev.off()


### With the LOESS decomposition, redraw the moving average plot
ipod.stl.df <- get.stl(data = data.m,
                       prod.name = "iPod",
                       start.yr = 2001,
                       start.q = 4)

data.stl.all <- rbind(mac.stl.df, ipod.stl.df, iphone.stl.df, ipad.stl.df)

## Sales trends with a 4-period moving average
pdf(file = "figures/apple-sales-trends-siracusa.pdf", height = 8, width = 10)
p <- ggplot(data.stl.all, aes(x = Date, y = trend, color = Product, fill = Product))
p0 <- p + geom_line(size = 1) +
    theme(legend.position = "top") +
    scale_x_date(labels = date_format("%Y"),
                 breaks = date_breaks("year")) +
    xlab("Year") +
    ylab("Decomposed Sales Trend (millions)") +
    scale_colour_manual(values = my.colors()) +
    scale_fill_manual(values = my.colors())
print(p0)
dev.off()

ggsave("figures/apple-sales-trends-siracusa.png", p0, height = 8, width = 10, dpi = 300)



### Volatility comparison
years <- "2007"
stl.comb <- rbind(ggmac.stl, ggiphone.stl, ggipad.stl)
stl.comb$Ratio <- (stl.comb$seasonal/stl.comb$trend) * 100

stl.comb.recent <- stl.comb %>% filter(is.null(years) | year(Date) > years)

pdf(file = "figures/apple-three-season-gg.pdf", height = 3, width = 10)
p <- ggplot(stl.comb.recent, aes(x = Date, y = Ratio, color = Product))
p + geom_line(size = 0.9) + ylab("Seasonal/Trend (pct)") + scale_colour_manual(values = my.colors()) +
    theme(legend.position = "top")
dev.off()


pdf(file = "figures/apple-three-season-gg.pdf", height = 3, width = 10)
p <- ggplot(stl.comb, aes(x = Date, y = Ratio, color = Product))
p + geom_line(size = 0.9) + ylab("Seasonal/Trend (pct)") + scale_colour_manual(values = my.colors()) +
    theme(legend.position = "top")
dev.off()

### Banking illustration iphone sales
p <- ggplot(ggiphone.stl, aes(x = Date, y = sales))
p2 <- p + geom_line() + ylab("Millions")

## calculate banking aspect ratio
library(ggthemes)
ar <- bank_slopes(as.numeric(ggiphone.stl$Date), ggiphone.stl$sales)

pdf(file = "figures/apple-iphone-banked45.pdf", height = 3, width = 15)
p3 <- p2 + coord_fixed(ratio = ar)
print(p3)
dev.off()

ggsave("figures/apple-iphone-banked45.png", p3, height = 4, width = 15, dpi = 300)


pdf(file = "figures/apple-iphone-banked-modest.pdf", height = 2.5, width = 10)
p3 <- p2
print(p3)
dev.off()

ggsave("figures/apple-iphone-banked-modest.png", p3, height = 4, width = 10, dpi = 300)



pdf(file = "figures/apple-iphone-2to1.pdf", height = 4, width = 8)
p3 <- p2 + ggtitle("iPhone Quarterly Trend")
print(p3)
dev.off()

ggsave("figures/apple-iphone-2to1.png", p3, height = 4, width = 8, dpi = 300)


pdf(file = "figures/apple-iphone-square.pdf", height = 4, width = 3, pointsize = 8)
p3 <- p2 + theme(plot.margin = unit(c(5, 15, 5, 15), "mm"), axis.text.x = element_text(size = 6))
print(p3)
dev.off()
ggsave("figures/apple-iphone-square.png", p3, height = 4, width = 3, dpi = 300)
