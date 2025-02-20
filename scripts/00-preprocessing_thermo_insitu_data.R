## Preparing input thermo data - download in situ
# Last update: 2024-08-23


## Libraries


library(tidyverse)
#Dados horários em UTC

temp <- list.files(path = "./data/sensores_thermo",
                   pattern = "*.csv") # listar arquivos .csv do diretório



# aplicar leitura das planilhas contidas na listagem temp
dir <- "./data/sensores_thermo"
temp.qualified <- paste(dir, temp, sep = "/")
myfiles <- lapply(temp.qualified,
                  read_delim,
                  col_select = c(1:21))

# checking if all data frames have the same column names
my_func <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same')
      break
    }
    else if(i==tail(names(y),n=1)) {
      print('Names are identical')
    }
  }
}

my_func(myfiles[[1]], myfiles[[2]])


# unificar planilhas de dados
data_thermo <- do.call("rbind", myfiles)
data_thermo <- data_thermo[, c(1:21)]

data_thermo <- data_thermo %>%
  mutate(localDate = as.Date(localDate, tz = "America/Sao_Paulo"),
         Local = recode((instrumentName),
                         "thermo-grid-883f4a344cbe" = "Rio Branco do Sul", #02/08/23 15:50
                         "thermo-grid-f4e11e8e1321" = "Almirante Tamandaré")) %>% #15/08/2023 11:45
  filter(Local == "Rio Branco do Sul" | Local == "Almirante Tamandaré") %>%
  subset(localDate >= as.Date("2023-08-02", tz = "America/Sao_Paulo")) %>%#& localDate <= '2023-12-21'
  unique() %>%
  mutate(localDateTime = ymd_hms(paste0(localDate, " ", localTime), tz = "America/Sao_Paulo")) %>%
  select(Local, localDateTime, so2, no2, o3, co, pm2p5, pm10,  rh)

colnames(data_thermo) <- c('Cidade','date','SO2', 'NO2', 'O3', 'CO', 'PM2.5','PM10', 'rh_sensor')



# Local time

data_thermo <- data_thermo %>%
  mutate(date = force_tz(date, tz = "America/Sao_Paulo"))


# Saving
saveRDS(data_thermo, "data/data_insitu_thermo.rds")



#################### TO COMPARE WITH WHO, 2021 AQG

# https://www.breeze-technologies.de/blog/air-pollution-how-to-convert-between-mgm3-%C2%B5gm3-ppm-ppb/

data_thermo_converted <- data_thermo %>%
  mutate(CO = CO*1.15, #from ppm to mg/m³
         O3 = O3*1.96, #from ppb to ug/m³
         NO2 = NO2*1.88, #from ppb to ug/m³
         SO2 = SO2*2.62, #from ppb to ug/m³
         PM2.5 = PM2.5, # ug/m³
         PM10 = PM10) #from ppb to ug/m³
