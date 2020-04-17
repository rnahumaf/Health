library(readxl)

InfoGripe <- read_xlsx("InfoGripe.xlsx")
head(InfoGripe)
colnames(InfoGripe) <- c("UF", "casos", "ano", "semana")
InfoGripe <- data.frame(InfoGripe)
InfoGripe$UF <- factor(InfoGripe$UF)
InfoGripe$ano <- factor(InfoGripe$ano)
InfoGripe$semana <- factor(InfoGripe$semana)

library(ggplot2)
ggplot(InfoGripe[which(InfoGripe$UF == "Brasil"),], 
       aes(x = semana, y = casos,  color = ano)) +
  geom_point(aes(size = casos, alpha = casos)) +
  scale_colour_manual(values=c("red", rep('black', 10), "blue")) +
  annotate("Text", x = "37", y = 9000, label = "H1N1 pandêmico \n 2009", size = 5) +
  annotate("Text", x = "7", y = 8000, label = "SARS-CoV-2 \n 2020", size = 5) +
  ggtitle("Casos respiratórios agudos graves no Brasil \n entre 2009 e 2020 (fonte: InfoGripe)") +
  ylab("Número de casos") +
  xlab("Semana do ano") +
  labs(size="Casos", 
       col="Anos",
       alpha="Casos") +
  scale_x_discrete(labels = seq(1, 52, 3), breaks = seq(1, 52, 3)) +
  scale_y_continuous(breaks = seq(0, 12000, 1000)) +
  guides(color = guide_legend(override.aes = list(size=5))) +
  theme_bw() +
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title = element_text(size=15),
        plot.title = element_text(size=20, hjust = 0.5),
        legend.title = element_text(size=15),
        legend.text=element_text(size=15))

ggsave("InfoGripe.png", width = 20, height = 20, units = "cm", dpi = 300)
