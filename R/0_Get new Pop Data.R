rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)

setwd("C:/Users/jmaburto/Documents/GitHub/Violence-against-women-in-Mexico")

# Program to read new data from CONAPO

# Population
Pop.data <- read.table('C:/Users/jmaburto/Documents/Data Mexico CONAPO New/pob_mit_proyecciones.csv',header = T,sep = ',')
names(Pop.data) <- c('P', 'year', 'state.name','state','age','sex','population')
Pop.data <- Pop.data[,-1]
Pop.data$state.name <- as.character(Pop.data$state.name)

Pop.data$sex <- as.character(Pop.data$sex)
Pop.data$sex <- ifelse(Pop.data$sex == 'Hombres',1,2)
Pop.data     <- data.table(Pop.data)
Pop.data     <- Pop.data[order(year,state,sex,age),]

#### sex=1 <- males

# Mortality
Mort.data <- read.table('C:/Users/jmaburto/Documents/Data Mexico CONAPO New/def_edad_proyecciones.csv',header = T,sep = ',')
names(Mort.data) <- c('P', 'year', 'state.name','state','sex','age','deaths')
Mort.data <- Mort.data[,-1]
Mort.data$state.name <- as.character(Mort.data$state.name)

Mort.data$sex <- as.character(Mort.data$sex)
Mort.data$sex <- ifelse(Mort.data$sex == 'Hombres',1,2)
Mort.data     <- data.table(Mort.data)
Mort.data     <- Mort.data[order(year,state,sex,age),]

CONAPO.Data <- merge(Mort.data,Pop.data)

save(CONAPO.Data, file = 'Data/New_CONAPO_Data.RData')

save(CONAPO.Data, file = 'C:/Users/jmaburto/Documents/Data Mexico CONAPO New/New_CONAPO_Data.RData')
