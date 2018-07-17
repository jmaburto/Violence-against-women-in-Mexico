

rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Desktop/Women homicides/")

#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

# These data comes from INEGI data micro files
load('Data/Counts&Rates_1990-2016Mex_.RData')
Data_Counts <- Data_Counts[Data_Counts$year > 1994 & Data_Counts$year < 2017,]

Homicides           <- Data_Counts[,c(1:5,17)]
colnames(Homicides) <- c(colnames(Homicides)[1:5],'Homicide')
Homicides[,sum(Homicide), by = list(year,sex)]



Homicides[,su]
# Get total for the country
National       <- Data_Counts[, lapply(.SD, sum, na.rm=TRUE), by= list(year,sex,age), 
                        .SDcols=colnames(Data_Counts)[5:length(colnames(Data_Counts))]] 
National$state <- 0
National       <- National[,c(1:2,22,3:21)]
Data_Counts    <- rbind(Data_Counts,National)
Data_Counts    <- Data_Counts[with(Data_Counts,order(year,sex,state,age)),]
Data_Counts    <- Data_Counts[,c(1:19,21)]
gdata::keep(Data_Counts,sure = T)

#Keep females
females           <- Data_Counts[Data_Counts$sex == 2,c(1:5,17)]
colnames(females) <- c(colnames(females)[1:5],'Homicides')

tot_homicides <- females[, lapply(.SD,sum,na.rm =TRUE), by = list(year),.SDcols = c('Pop','Homicides')]