# Time delay from death to notification
tempo_morte <- c(rep(0, 10), rep(1, 20), rep(2, 30), rep(3, 21), rep(4, 15), 
                rep(5, 9), rep(6, 10), rep(7, 9), rep(8, 10), rep(9, 7), 
                rep(10, 4), rep(11, 5), rep(12, 6), rep(13, 4), rep(14, 4), 
                rep(15, 5), rep(16, 1), rep(17, 2), rep(18, 3), rep(19, 3), 
                rep(20, 0), rep(21, 1), rep(22, 1), rep(23, 0), rep(24, 0), 
                rep(25, 1), rep(26, 1), rep(27, 0), rep(28, 0), rep(29, 1), 
                rep(30, 0), rep(31, 0))
delay <- data.frame(dias = tempo_morte)
delay <- data.frame(cbind(titulo = c("Delay"), delay))
d.median <- median(delay$dias)

# Plot
library(ggplot2)

ggplot(delay, aes(x = titulo, y = dias)) +
  geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=0.5, alpha = 0.2) +
  geom_hline(yintercept=d.median, linetype="dashed", color = "red", size=1) +
  annotate("text", label = paste0("mediana: ", d.median, " dias"), x = 0.6, y = 5, size = 5, colour = "red") +
  ylab("Dias de atraso na divulgação de óbito por COVID-19") +
  xlab("Dados do dia 15 de abril") +
  theme_bw()
