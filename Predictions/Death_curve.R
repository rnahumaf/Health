library(minpack.lm)
library(data.table)

# Daily Deaths, data from Brazil
d <- fread("3
3
4
7
7
9
13
12
18
15
22
22
23
42
39
59
60
73
54
67
114
133
141
115
68
")
df <- data.frame(Dia = c(1:25), New = unlist(d))

# Model
mod.df <- nlsLM(New ~ a*(exp(b*(Dia+t)))/((exp(b*t) + exp(b*Dia))^2), 
                data = df,
                start = list(t = 30, b = 0.01, a = 0.01))

# Plot
list.death <- c()
for(Dia in 1:44){
  list.death[Dia] <- (a*(exp(b*(Dia+t)))/((exp(b*t) + exp(b*Dia))^2))
}
plot(list.death, type = "l", ylab = "Novas mortes", xlab = "Dias passados")
points(unlist(d))
text(labels = "Previsão", x = 30, y = 100)
text(labels = "Dados do \n Brasil", x = 5, y = 20)
