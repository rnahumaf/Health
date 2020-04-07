# Libraries
library(minpack.lm)
library(ggplot2)
library(data.table)

# Functions
prepare <- function(mortes, column = 1, prim = 1, date = F){
  df.mortes <- data.frame(mortes)
  red.mortes <- df.mortes[which(df.mortes[,column]>=prim), column]
  date.mortes <- df.mortes[which(df.mortes[,column]>=prim), date]
  day.mortes <- seq(0, length(red.mortes)-1, 1)
  if(date!=F){
    df.mortes <<- data.frame(date = date.mortes, day = day.mortes, mortes = red.mortes)
  } else {
    df.mortes <<- data.frame(day = day.mortes, mortes = red.mortes)
  }
}

Cgeneral <- function(c0, r, m, proj){
  return((r*(0:(proj-1))/m + c0^(1/m))^m)
}

# prepare(df, death col, start, date col)
prepare(mortes, 3, 1, 1) 

# Dataset
mortes <- fread("Data	Casos	Óbitos
2020-02-26	1	0
2020-02-27	1	0
2020-02-28	1	0
2020-02-29	2	0
2020-03-01	2	0
2020-03-02	2	0
2020-03-03	2	0
2020-03-04	3	0
2020-03-05	8	0
2020-03-06	13	0
2020-03-07	19	0
2020-03-08	25	0
2020-03-09	30	0
2020-03-10	34	0
2020-03-11	52	0
2020-03-12	77	0
2020-03-13	98	0
2020-03-14	121	0
2020-03-15	200	0
2020-03-16	234	0
2020-03-17	291	1
2020-03-18	428	4
2020-03-19	621	7
2020-03-20	978	11
2020-03-21	1178	18
2020-03-22	1604	25
2020-03-23	1960	34
2020-03-24	2271	47
2020-03-25	2555	59
2020-03-26	2915	77
2020-03-27	3417	92
2020-03-28	3904	114
2020-03-29	4256	136
2020-03-30	4579	159
2020-03-31	5717	201
2020-04-01	6836	240
2020-04-02	7910	299")
mortes$Data <- as.Date(mortes$Data, "%Y-%m-%d")

# day zero
c0 <- df.mortes[1, 3]

# create model
mod.mortes <- nlsLM(mortes ~ (r*day/m + c0^(1/m))^m, 
                    data = df.mortes,
                    start = list(r = 0.5, m = 0.5))

# assign confidence intervals
lower.r <- confint(mod.mortes)[1,1]
higher.r <- confint(mod.mortes)[1,2]
lower.m <- confint(mod.mortes)[2,1]
higher.m <- confint(mod.mortes)[2,2]

####### Plot using base R (problem: can't plot dates)
# plot confidence intervals + original data
x <- c(1:nrow(df.mortes))
y1 <- Cgeneral(1, lower.r, lower.m, nrow(df.mortes))
y2 <- Cgeneral(1, higher.r, higher.m, nrow(df.mortes))
plot(x, y1, type="l", col=rgb(0,0,0, alpha=0.5), xlim = c(0, 20), ylim = c(0, 1000))
lines(x, y2, type="l", col=rgb(0,0,0, alpha=0.5))
polygon(c(x, rev(x)), c(y1,rev(y2)),col="skyblue")

# plot real data
lines(df.mortes[, 3], type="l", lwd=5, col=rgb(1,0,0, alpha=0.5))

####### Plot using ggplot2 (problem: more complicated)
# plot with dates
library(ggplot2)
lower <- Cgeneral(1, lower.r, lower.m, nrow(df.mortes)+4)
higher <- Cgeneral(1, higher.r, higher.m, nrow(df.mortes)+4)
real <- df.mortes[, 3]
forecast <- Cgeneral(1, coef(mod.mortes)[1], coef(mod.mortes)[2], nrow(df.mortes)+4)
max.len = max(length(higher), length(lower), length(real))
mydata = data.frame(date = seq(as.Date("2020-03-17"), as.Date("2020-03-17")+max.len-1, by="days"),
                    lower = c(lower, rep(NA, max.len - length(lower))),
                    higher = c(higher, rep(NA, max.len - length(higher))),
                    real = c(real, rep(NA, max.len-length(real))),
                    forecast = c(forecast, rep(NA, max.len-length(forecast))))
ggplot(mydata, aes(x=date, y = real)) +
  geom_line(aes(y = lower)) + 
  geom_line(aes(y = higher)) + 
  geom_ribbon(aes(ymin = lower, ymax = higher), fill = "gray", alpha = .5) +
  geom_line(aes(y = forecast), color="darkgray", size = 2) +
  geom_point(aes(y = real),color="black", size = 3) +
  ylim(0, 1250) + xlim(as.Date("2020-03-17"), as.Date("2020-03-17")+max.len-1) +
  theme_bw()+
  ylab("Previsão de mortes") + xlab("Data")
