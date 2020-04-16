# Time delay from death to notification
tempo_morte <- c(rep(0, 13), rep(1, 28), rep(2, 10), rep(3, 8), rep(4, 3), rep(5, 6), rep(6, 2), rep(7, 6), rep(8, 5), rep(9, 3), rep(10, 1), rep(11, 2), rep(12, 3), rep(13, 1), rep(14, 3), rep(15, 0), rep(16, 0), rep(17, 0), rep(18, 0), rep(19, 0), rep(20, 0), rep(21, 0), rep(22, 0), rep(23, 0), rep(24, 0), rep(25, 0), rep(26, 0), rep(27, 0),
                 rep(0, 3), rep(1, 22), rep(2, 42), rep(3, 25), rep(4, 18), rep(5, 18), rep(6, 16), rep(7, 17), rep(8, 3), rep(9, 7), rep(10, 6), rep(11, 1), rep(12, 0), rep(13, 2), rep(14, 1), rep(15, 2), rep(16, 0), rep(17, 1), rep(18, 3), rep(19, 0), rep(20, 1), rep(21, 1), rep(22, 0), rep(23, 1), rep(24, 1), rep(25, 0), rep(26, 0),
                 rep(0, 10), rep(1, 20), rep(2, 30), rep(3, 21), rep(4, 15), rep(5, 9), rep(6, 10), rep(7, 9), rep(8, 10), rep(9, 7), rep(10, 4), rep(11, 5), rep(12, 6), rep(13, 4), rep(14, 4), rep(15, 5), rep(16, 1), rep(17, 2), rep(18, 3), rep(19, 3), rep(20, 0), rep(21, 1), rep(22, 1), rep(23, 0), rep(24, 0), rep(25, 1), rep(26, 1), rep(27, 0), rep(28, 0), rep(29, 1), rep(30, 0), rep(31, 0))
delay <- data.frame(dias = tempo_morte)
delay <- data.frame(cbind(titulo = c(1), delay))
d.median <- median(delay$dias)
d.sup <- quantile(delay$dias, 0.9)
d.inf <- quantile(delay$dias, 0.1)

# Plot
library(ggplot2)

ggplot(delay, aes(x = titulo, y = dias)) +
  annotate(geom = "rect", xmin=-Inf, xmax=Inf, ymin=d.inf, ymax=d.sup,
           fill = "palegreen", colour = "black", alpha = 0.2) +
  geom_dotplot(binaxis='y', stackdir='center',
               fill = "black", stackgroups=T, binpositions="all", position = "dodge",
               stackratio=0.5, dotsize=0.5, alpha = 0.6,
               binwidth = 0.5) +
  geom_hline(yintercept=d.median, linetype=1, color = "red", size=2, alpha=0.5) +
  geom_hline(yintercept=d.sup, linetype=1, color = "black", size=2, alpha=0.5) +
  geom_hline(yintercept=d.inf, linetype=1, color = "black", size=2, alpha=0.5) +
  annotate("text", label = paste0("Mediana: ", d.median, " dias"), 
           hjust =0, x = 0.5, y = 4.5, size = 15, colour = "red") +
  annotate("text", label = paste0("Percentil 90: ", d.sup, " dias"), 
           hjust =0, x = 0.5, y = 12.5, size = 15, colour = "black") +
  annotate("text", label = paste0("Percentil 10: ", d.inf, " dia"), 
           hjust =0, x = 0.5, y = 0.5, size = 15, colour = "black") +
  ylab("Tempo entre a data de morte por COVID-19 e a data de divulgação (dias)") +
  xlab("Baseado em 468 mortes divulgadas \n nos dias 10, 11 e 15 de abril") +
  scale_x_continuous(labels = rep("", 21), breaks = seq(0, 2, 0.1)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  theme_bw() +
  theme(axis.text.y = element_text(size=40),
        axis.title=element_text(size=40))


