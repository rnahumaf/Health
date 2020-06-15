library(readxl)
library("pracma")

#Read dataset from outworldindata.com
covid <- read_xlsx("covid.xlsx")

new.covid <- covid[which(covid$new_deaths>0),]
new.covid <- data.frame(new.covid)
new.covid$location <- as.factor(new.covid$location)


computation <- function(column = "new_deaths", len = 50, numb = 5, lg = F) {

  # N days after first death
  for(i in 1:nrow(new.covid)){
    new.covid$counts[i] <- sum(new.covid$location==new.covid[i, "location"])
  }
  
  # Select only above 50 days
  new.covid <- new.covid[which(new.covid$counts>=len),]
  new.covid$location <- as.character(new.covid$location)
  new.covid$location <- as.factor(new.covid$location) 
  
  # Moving averages
  for(i in levels(new.covid$location)){
    new.covid[which(new.covid$location==i),column] <- movavg(new.covid[which(new.covid$location==i),column], n = 7, type="s")
  }
  
  # Compute correlations
  cor.countries <- data.frame(country = NA, cor = NA)
  j = 0
  for(i in levels(new.covid$location)){
    j = j+1
    cor.countries[j, "country"] <- i
    cor.countries[j, "cor"] <- cor(new.covid[which(new.covid$location=="Brazil"),column][1:len],
                                new.covid[which(new.covid$location==i),column][1:len])
  }
  
  if(lg == T){
    # Plot top 5
    plot(log(new.covid[which(new.covid$location=="Brazil"),column]), 
         ylim=c(0, max(log(new.covid[which(new.covid$location=="Brazil"),column])*2)), 
         xlim=c(0,120),
         xlab = "Dias passados",
         ylab = column)
    for(i in 1:numb){
      lines(log(new.covid[which(new.covid$location==cor.countries[order(cor.countries$cor, decreasing = T),"country"][i]),column]))
    }
    for(i in 1:numb){
      text(sum(new.covid$location==cor.countries[order(cor.countries$cor, decreasing = T),"country"][i]),
           log(new.covid[which(new.covid$location==cor.countries[order(cor.countries$cor, decreasing = T),"country"][i]),column][length(new.covid[which(new.covid$location==cor.countries[order(cor.countries$cor, decreasing = T),"country"][i]),column])]),
           cor.countries[order(cor.countries$cor, decreasing = T),"country"][i],
           pos = 4)
    }
  } else {
    # Plot top 5
    plot(new.covid[which(new.covid$location=="Brazil"),column], 
         ylim=c(0, max(new.covid[which(new.covid$location=="Brazil"),column])*2), 
         xlim=c(0,120),
         xlab = "Dias passados",
         ylab = column)
    for(i in 1:numb){
      lines(new.covid[which(new.covid$location==cor.countries[order(cor.countries$cor, decreasing = T),"country"][i]),column])
    }
    for(i in 1:numb){
      text(sum(new.covid$location==cor.countries[order(cor.countries$cor, decreasing = T),"country"][i]),
           new.covid[which(new.covid$location==cor.countries[order(cor.countries$cor, decreasing = T),"country"][i]),column][length(new.covid[which(new.covid$location==cor.countries[order(cor.countries$cor, decreasing = T),"country"][i]),column])],
           cor.countries[order(cor.countries$cor, decreasing = T),"country"][i],
           pos = 4)
    }
  }
  return(print(cor.countries[order(cor.countries$cor, decreasing = T),"country"][1:numb]))
}
computation("new_deaths", 60, 5, T)
computation("new_deaths_per_million", 60, 5, F)
