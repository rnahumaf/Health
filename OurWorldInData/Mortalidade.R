# Adjust file in Excel beforehand. Save in xlsx and keep it in R Working Directory.
library(readxl)
Mortalidade <- read_xlsx("Mortality200420.xlsx")
Mortalidade <- data.frame(Mortalidade)
Mortalidade <- Mortalidade[which(Mortalidade$Country!="Europe"),]
colnames(Mortalidade) <- c("Country", "Date", "Mortality")

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


## PLOTS ##
# Simple dot plot
library(ggplot2)
ggplot(Positive_mortality, aes(x = Days, y = Mortality)) +
      geom_point(alpha = 0.2, cex = 0.5) +
  # Brazil
      geom_line(data = Positive_mortality[which(Positive_mortality$Country=="Brazil"),],
                aes(x = Days, y = Mortality, colour="pink"), lwd = 1, lineend = "round") +
  xlim(1, 60) +
  scale_y_log10() +
  theme_classic() +
  xlab("Dias após a primeira morte") + ylab("Mortalidade") +
  ggtitle("Mortalidade cumulativa por 1 milhão de habitantes\n(dados de 166 países, Brasil destacado em rosa)") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))




# Plot with SD
ggplot(Positive_mortality, aes(x = Days, y = Mortality, group = Country)) +
      geom_line(alpha = 0.2) +
      geom_ribbon(data = add_df, aes(ymin = lowest, ymax = lower, x = Days), 
                  fill = "black", alpha = 0.5, inherit.aes = FALSE) + 
      geom_ribbon(data = add_df, aes(ymin = lower, ymax = median, x = Days), 
                  fill = "gray", alpha = 0.5, inherit.aes = FALSE) + 
      geom_ribbon(data = add_df, aes(ymin = median, ymax = upper, x = Days), 
                  fill = "gray", alpha = 0.5, inherit.aes = FALSE) + 
      geom_ribbon(data = add_df, aes(ymin = upper, ymax = highest, x = Days), 
                  fill = "black", alpha = 0.5, inherit.aes = FALSE) + 
      geom_line(data = add_df, aes(x = Days, y = median), 
                colour="black", linejoin = "mitre", inherit.aes = FALSE) +
      geom_vline(aes(xintercept = 40)) +
  # Brazil
      geom_line(data = Positive_mortality[which(Positive_mortality$Country=="Brazil"),], 
                aes(x = Days, y = Mortality), colour="black", lineend = "round", alpha = 0.3, lwd = 1) +
      geom_point(data = Positive_mortality[which(Positive_mortality$Country=="Brazil"),], 
               aes(x = Days, y = Mortality), fill='red', colour="black", pch=21, size=2) +
  xlim(1, 60) +
  scale_y_log10() +
  theme_classic() +
  xlab("Dias após a primeira morte") + ylab("Mortalidade") +
  ggtitle("Mortalidade cumulativa por 1 milhão de habitantes\n(dados de 168 países, Brasil destacado em rosa)") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 35, y = 20, label = "Brasil", col = "red", fontface =2) +
  annotate("text", x = 60, y = 1000, label = "z +2", col = "black", fontface =2) +
  annotate("text", x = 60, y = 30, label = "z +1", col = "black", fontface =2) +
  annotate("text", x = 59, y = 3, label = "média", col = "black", fontface =2) +
  annotate("text", x = 60, y = 0.6, label = "z -1", col = "black", fontface =2) +
  annotate("text", x = 60, y = 0.02, label = "z -2", col = "black", fontface =2) +
  annotate("text", x = 5, y = 0.0001, label = "Dia 1: 18/03", col = "black", fontface =2) +
  annotate("text", x = 46, y = 0.0001, label = "Dia 40: 27/04", col = "black", fontface =2)

