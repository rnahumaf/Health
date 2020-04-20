# Adjust file in Excel beforehand. Save in xlsx and keep it in R Working Directory.
library(readxl)
Mortalidade <- read_xlsx("Mortality190420.xlsx")

Mortalidade <- data.frame(Mortalidade)

# You need to make sure the location is set at USA, because dates in OurWorldInData files are in such format.
Sys.getlocale()
Sys.setlocale("LC_TIME", "English")
Mortalidade$Date <- as.Date(Mortalidade$Date, "%b %d, %Y")

# We want to plot files starting at day 0, when first death was accounted.
# Therefore we delete records with zero cumulative deaths.
Positive_mortality <- Mortalidade[which(Mortalidade$Mortality != 0),]
Positive_mortality$Country <- as.factor(Positive_mortality$Country)
Countries <- levels(Positive_mortality$Country)

# We need to come up with some way to accomodate different records overlayed with respect to x axis
# I decided to make a new column to count days, restarting the count for each new country in the dataframe.
dias <- c()
for(i in Countries){
  dias <- c(dias, c(1:nrow(Positive_mortality[which(Positive_mortality$Country==i),])))
}
Positive_mortality <- cbind(Positive_mortality, Days = dias)

# GGPLOT
library(ggplot2)
p <- ggplot(Positive_mortality, aes(x = Days, y = Mortality)) +
      geom_point(alpha = 0.2, cex = 0.5) +
      geom_line(data = Positive_mortality[which(Positive_mortality$Country=="Brazil"),], 
                aes(x = Days, y = Mortality, colour="pink"), lwd = 1, lineend = "round") +
      xlim(1, 60) +
      scale_y_log10() +
      theme_classic() +
  xlab("Dias após a primeira morte") + ylab("Mortalidade") +
  ggtitle("Mortalidade cumulativa por 1 milhão de habitantes\n(dados de 166 países, Brasil destacado em rosa)") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
ggsave("Mortality.png", plot = p, width = 5, height = 5, dpi = 300)
