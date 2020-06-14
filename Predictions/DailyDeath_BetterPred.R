#According to https://www.medrxiv.org/content/10.1101/2020.04.07.20056937v2.full.pdf+html

library(minpack.lm)
library(data.table)
library(pracma)



# Daily Deaths, data from France
d <- fread("3
2
1
9
11
3
15
13
18
12
36
21
27
69
128
78
112
112
186
240
231
365
299
319
292
418
499
509
471
2004
1053
518
833
1417
541
1341
987
635
561
574
762
1438
753
761
642
395
547
531
544
516
389
369
242
437
367
427
289
218
166
135
306
330
278
178
243
80
70
263
348
83
351
130
88
68
186
125
110
83
74
43
35
65
98
66
66
52
57
31
31
107
81
44
46
31
13
54
87
23
")
df <- data.frame(Dia = c(1:98), New = unlist(d))

df$New <- movavg(df$New, n = 7, type="s")

# Model
mod.df <- nlsLM(New ~ a*b*(exp(b*t0)*exp(b*Dia))/((exp(b*t0) + exp(b*Dia))^2) + b*c*(exp(b*Dia)/(exp(b*t0)+exp(b*Dia))), 
                data = df,
                start = list(a = 150000, b = 0.2, c = 1000, t0 = 60))
a <- coef(mod.df)[1]
b <- coef(mod.df)[2]
c <- coef(mod.df)[3]
t0 <- coef(mod.df)[4]

# Plot
list.death <- c()
for(Dia in 1:130){
  list.death[Dia] <- a*b*(exp(b*t0)*exp(b*Dia))/((exp(b*t0) + exp(b*Dia))^2) + b*c*(exp(b*Dia)/(exp(b*t0)+exp(b*Dia)))
}
plot(list.death, type = "l", ylab = "Novas mortes", xlab = "Dias passados", ylim = c(0,2200))
points(unlist(d))

# Area under curve
AUC = trapz(seq(list.death),list.death)

# Text
text(15, 800, labels = paste("France\nAUC=", round(AUC)))


























# Daily Deaths, data from Brazil

br <- fread("0
1
3
2
5
7
7
9
12
11
20
15
22
22
23
42
40
58
60
73
54
67
114
133
141
115
68
99
105
204
204
188
217
206
115
113
166
165
407
357
346
189
338
474
449
435
428
421
275
296
600
615
610
751
730
496
396
881
749
844
824
816
485
674
1179
888
1188
1001
965
653
807
1039
1086
1156
1124
956
480
623
1262
1349
1473
1005
904
525
679
1272
1274
")

df.br <- data.frame(Dia = c(1:87), New = unlist(br))
df.br$New <- movavg(df.br$New, n = 7, type="s")

# Model
mod.df <- nlsLM(New ~ a*b*(exp(b*t0)*exp(b*Dia))/((exp(b*t0) + exp(b*Dia))^2) + b*c*(exp(b*Dia)/(exp(b*t0)+exp(b*Dia))), 
                data = df.br,
                start = list(a = 150000, b = 0.2, c = 1000, t0 = 60))
a <- coef(mod.df)[1]
b <- coef(mod.df)[2]
c <- coef(mod.df)[3]
t0 <- coef(mod.df)[4]

# Plot
list.death <- c()
for(Dia in 1:130){
  list.death[Dia] <- a*b*(exp(b*t0)*exp(b*Dia))/((exp(b*t0) + exp(b*Dia))^2) + b*c*(exp(b*Dia)/(exp(b*t0)+exp(b*Dia)))
}
lines(list.death, type = "l", ylab = "Novas mortes", xlab = "Dias passados", col="red")
points(unlist(br), col="red")

# Area under curve
AUC = trapz(seq(list.death),list.death)

text(110, 1250, label=paste("Brazil\nAUC =", round(AUC)), col="red")



