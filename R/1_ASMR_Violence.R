rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)

setwd("C:/Users/jmaburto/Documents/GitHub/Violence-against-women-in-Mexico")

#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

#### violence
####1 Hubo violencia familiar
####2 Hubo violencia no familiar
####8 No aplica cuando no es homicidio
####9 No especificado
####999 No había opción en Certificado, a partir del 2000

# These data comes from INEGI data micro files up to 2017
load('Data/Deaths_Counts-1990-2017.RData')
load('Data/New_CONAPO_Data.RData')

Pop_Data <- CONAPO.Data[CONAPO.Data$year %in% 1992:2017 & CONAPO.Data$state %in% 1:32,]
names(Pop_Data)[6:7] <- c('conapo_deaths', 'conapo_pop')

# Subset data to 11 years before and after 2005
Data_Counts <- Death_Counts[Death_Counts$year >= 1992 & Death_Counts$year <= 2017,1:20]

gdata::keep(Data_Counts,Pop_Data, sure = T)


# get deaths by age (without) the violence variable
Data_Counts       <- melt.data.table(Data_Counts, id.vars = c('year','sex','state','age'),variable.name = 'cause',value.name = 'deaths',value.factor = F,variable.factor = F)
Data_Counts$cause <- as.numeric(Data_Counts$cause)
Data_Counts       <- Data_Counts[,deaths.prop:=deaths/sum(deaths,na.rm = T), by = list(year,sex,state,age)]
Data_Counts[is.na(deaths.prop) & cause == 15,]$deaths.prop <- 1
Data_Counts[is.na(deaths.prop),]$deaths.prop <- 0

Data_Counts <- merge(x = Pop_Data,y = Data_Counts, by = c('year','sex','state','age'),sort = T)

gdata::keep(Data_Counts, sure =T)

source('R/Functions.R')

# I want to find the ages that maximize the increase in homicides rates. 
# First find the max proportion in mid-life years

age1     <- 15 # this parameter will be used trhoughout the exercise as the initial age
age2     <- 65 # this parameter will be used trhoughout the exercise as the final age
std.year <- 2005  #year of standard population

# standard population female population in 2005 (mid period)
pop.std <-  Data_Counts[Data_Counts$year==std.year & Data_Counts$sex == 2,]
pop.std <- pop.std[,sum(conapo_pop), by = list(age)]
pop.std <- pop.std[,pop.std:=V1/sum(V1),]
pop.std <- pop.std[,c(1,3)]
sum(pop.std$pop.std)
# add pop standard to women
Data_Counts <- merge(Data_Counts,pop.std,by = 'age')

# get age-specific mortality rates
Data_Counts[Data_Counts$conapo_deaths > Data_Counts$conapo_pop,]$conapo_pop <- Data_Counts[Data_Counts$conapo_deaths > Data_Counts$conapo_pop,]$conapo_deaths
Data_Counts[Data_Counts$conapo_pop == 0 ,]$conapo_deaths <- 0

Data_Counts  <- Data_Counts[, age.cause.MX :=  deaths.prop*conapo_deaths/conapo_pop,]
Data_Counts  <- Data_Counts[order(year,sex,state,state.name,cause,age),]

Data_Counts[is.na(age.cause.MX),]  

#get the standardized rates with age1 and age2
ASDR <- Data_Counts[,list(ASDR = sum(age.cause.MX[(age1 + 1):(age2+1)]*pop.std[(age1 + 1):(age2+1)])*100000), by = list(year,sex,state,state.name,cause)]

#get some average value from 1994-1999 2002-2007  2011-2017
Labels.periods   <- c('1992-1997','2002-2007','2012-2017')
years.avg        <- c(1992:1997,2002:2007,2012:2017)
ASDR.avg         <- ASDR[year %in% years.avg & cause == 12,]
ASDR.avg$Period  <- cut(ASDR.avg$year+1, breaks=c(1992,2002,2012,Inf),labels=Labels.periods)
ASDR.avg         <- ASDR.avg[,list(ASDR.avg= mean(ASDR)), by = list(sex,state,state.name,cause,Period)]

# adding region and state name columns is then easy
ASDR.avg         <- cbind(region=region.recvec[as.character(ASDR.avg$state)],ASDR.avg)
ASDR.avg$region  <- factor(ASDR.avg$region, levels = 1:3, labels = c('South', 'Central', 'North'))
ASDR.avg$region  <- factor(ASDR.avg$region,levels = rev(levels(ASDR.avg$region)))
ASDR.avg         <- ASDR.avg[order(state,Period,cause),]

#get the difference in the periods
labels.period <- c('From 1992-97 to 2002-07', 'From 2002-07 to 2012-17')
ASDR.avg.diff               <- ASDR.avg[,list(Change=diff(ASDR.avg),Period=1:2), by = list(region,state,state.name,sex,cause)]
ASDR.avg.diff$Period        <- as.factor(ASDR.avg.diff$Period)
levels(ASDR.avg.diff$Period)<- labels.period

t1 <- cbind(ASDR.avg[sex==2 & cause==12 & Period == '2002-2007',],ASDR.avg[sex==2 & cause==12 & Period == '2012-2017',])

write.csv(t1,'Manuscript/PAA abstract/info.csv')
#make some preliminary graphs

#trends graph
# p <- ggplot(ASDR.avg[Cause == 'Homicide',], aes(x = Period,y = ASDR.avg,colour=(state))) +
#   ggtitle(paste('State-specific avergae standardized mortality rates by 100 thousand women between ages ',age1, ' and ',age2,' in Mexico')) +
#   geom_line(aes(group = state), size= 1) +
#   theme_light()+
#   theme(text = element_text(size=14),
#         strip.text.x = element_text(size = 14, colour = "black"))
# p
# 
# 
  # q <- ggplot() +
  #  ggtitle(paste('State-specific standardized mortality rates by 100 between ages ',age1, ' and ',age2,' in Mexico')) +
  #  geom_line(data=ASDR[Cause == 'Homicide' & sex ==2,], aes(x = year,y = ASDR, group = state),col='red',alpha=.3) +
  #  geom_line(data=ASDR[Cause == 'Homicide' & sex ==1,], aes(x = year,y = ASDR, group = state),col='blue',alpha=.3) +
  #  theme_light()+
  #  theme(text = element_text(size=14),
  #        strip.text.x = element_text(size = 14, colour = "black"))
  # q

 r.data            <- ASDR.avg.diff[ASDR.avg.diff$cause==12 & ASDR.avg.diff$sex==2,]
 r.data            <- r.data[order(Period,state.name),]
 r.data$ref.order  <-rep(r.data[r.data$Period==labels.period[2],]$Change,2)
 r.data$state.name <- reorder(r.data$state.name,r.data$ref.order)
 r <- ggplot(r.data, aes(Change, state.name)) +
   ggtitle(paste('Change in standardized homicide rates by 100 '), 
           subtitle = paste('thousand women between ages ',age1, ' and ',age2))+
   geom_vline(xintercept = 0)+
   geom_point(aes(Change, state.name,col=Period, shape=Period),size = 3) +
   facet_grid(region+sex ~., scales = "free", space = "free") +
   theme_light()+
   theme(plot.title = element_text(size=14, face = 'bold'))+
   theme(plot.subtitle = element_text(size=14, face = 'bold'))+
   scale_color_manual(values=c('blue','red'))+
   theme(axis.title.y=element_blank())+
   theme(axis.title.x = element_text(size = 12, angle = 00))+
   theme(text = element_text(size=14),
         strip.text.x = element_text(size = 14, colour = "black"))+
   theme(strip.text.y = element_text(colour = "black"))+
   theme(legend.position = 'bottom')
 r


pdf(file = 'Manuscript/Exhibit1.pdf',width = 6.3,height = 10,useDingbats = F)
r
dev.off()


## Check if in some state the rates are higher for women than men
ASDR.check       <- ASDR[sex==1,c(1:4)]
ASDR.check$men   <- ASDR[sex==1,c(5)]
ASDR.check$women <- ASDR[sex==2,c(5)]
ASDR.check$diff  <- ASDR.check$men - ASDR.check$women
ASDR.check$rel.dif<- (ASDR.check$men - ASDR.check$women)/ASDR.check$men
ASDR.check       <- ASDR.check[ASDR.check$Cause=='Homicide',]
ASDR.check       <- cbind(state.name=state.name.vec[as.character(ASDR.check$state)],ASDR.check)
ASDR.check[ASDR.check$diff < 0,]

check.years <- c(2005,2011,2016)
ASDR.check <- ASDR.check[year %in% check.years,]
ASDR.check[ASDR.check$year==2016 & order(rel.dif/ASDR.check$men),]

