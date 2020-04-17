library(jsonlite)
library(writexl)

# Prepare variables
URL <- c()
TBL <- data.frame(data.territory_name = NA, 
                  data.epiweek = NA, 
                  data.situation_name = NA,
                  data.value = NA,
                  ano = NA)

# Add data to TBL
for(ano in 2009:2020){
  for(semana in 1:52){
    URL <- fromJSON(paste0('http://info.gripe.fiocruz.br/data/detailed/1/1/', ano, '/', semana, '/1/Brasil/data-table'))
    URL <- data.frame(URL)
    URL$ano <- ano
    TBL <- rbind(TBL, URL)
    print(paste0("semana: ", semana, "/ ano: ", ano))
  }
}

# Export to Excel file
write_xlsx(TBL, "InfoGripe_2020_14.xlsx")
