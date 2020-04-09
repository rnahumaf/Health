
library(readxl)
library(writexl)
my_data <- read_excel("death.covid.xlsx") #World Bank data
head(my_data)
my_data_nonzero <- my_data[which(my_data$Death>0),]
# my_data_nonzero <- my_data[which(is.na(my_data$Death)==F),]
my_data_nonzero$Country <- as.factor(my_data_nonzero$Country)
df1 <- data.frame(my_data_nonzero)
colnames(df1) <- c("Country", "Date", "Death")
ggplot(df1[which(df1[,1]=="Italy"),], aes(x = Date, y = Death)) +
          geom_line(aes(y = Death))

# Objective: get all maximum values of countries
# Get the length of each country and select the highest length
len.days <- c()
for(i in levels(df1$Country)){
  len.days <- c(len.days, length(df1[which(df1[,1]==i),3]))
}
max.len <- max(len.days)
# rbind all with same length
covid.death <- c()
more.len <- c()
for(i in levels(df1$Country)){
  more.len <- df1[which(df1[,1]==i),3]
  length(more.len) <- max.len
  covid.death <- rbind(covid.death, more.len)
}
rownames(covid.death) <- (levels(df1$Country))




# Objective: make 3 rows (upper, lower, average)
covid.average <- matrix(ncol = max.len, nrow = 3)
for(i in 1:ncol(covid.death)){
  covid.average[1, i] <- quantile(covid.death[,i], .75, na.rm = T) # Upper
  covid.average[2, i] <- quantile(covid.death[,i], .25, na.rm = T) # Lower
  covid.average[3, i] <- median(covid.death[,i], na.rm = T) # Median
}
covid.average[, c(1:10)]

# Objective: save excel file
covid.average <- data.frame(covid.average)
write_xlsx(covid.average,"covid_average.xlsx")




# Objective: make 3 rows (upper, lower, average) for new deaths
covid.new <- matrix(ncol = max.len-1, nrow = nrow(covid.death))
for(i in 1:nrow(covid.death)){
  covid.new[i, ] <- covid.death[i, 2:ncol(covid.death)] - covid.death[i, 1:(ncol(covid.death)-1)]
}
covid.new.average <- matrix(ncol = max.len-1, nrow = 3)
for(i in 1:ncol(covid.new)){
  covid.new.average[1, i] <- quantile(covid.new[,i], .75, na.rm = T) # Upper
  covid.new.average[2, i] <- quantile(covid.new[,i], .25, na.rm = T) # Lower
  covid.new.average[3, i] <- median(covid.new[,i], na.rm = T) # Median
}

# Objective: save excel file
covid.new.average <- data.frame(covid.new.average)
write_xlsx(covid.new.average,"covid.new.average.xlsx")




# Objective: make 3 rows (upper, lower, average) for new deaths
covid.new.sd <- matrix(ncol = max.len-1, nrow = 3)
for(i in 1:ncol(covid.new)){
  covid.new.sd[1, i] <- mean(covid.new[,i], na.rm = T) + sd(covid.new[,i], na.rm = T) # Upper
  covid.new.sd[2, i] <- mean(covid.new[,i], na.rm = T) + 2*sd(covid.new[,i], na.rm = T) # Lower
  covid.new.sd[3, i] <- mean(covid.new[,i], na.rm = T) # Mean
}

# Objective: save excel file
covid.new.sd <- data.frame(covid.new.sd)[,c(1:60)]
write_xlsx(covid.new.sd,"covid.new.sd.xlsx") # Use in DataWrapper
