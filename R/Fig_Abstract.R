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

# These data comes from INEGI data micro files up to 2016
load('Data/Counts&Rates_1990-2016Mex.RData')

# Subset data to 11 years before and after 2005
Data_Counts <- Data_Counts[Data_Counts$year >= 1994 & Data_Counts$year < 2017,]
Data_Counts <- Data_Counts[order(year,sex,state,age),]
gdata::keep(Data_Counts, sure = T)
source('R/Functions.R')

# I want to find the ages that maximize the increase in homicides rates. 
# First find the max proportion in mid-life years

age1     <- 15 # this parameter will be used trhoughout the exercise as the initial age
age2     <- 65 # this parameter will be used trhoughout the exercise as the final age
std.year <- 2005  #year of standard population

Deaths       <- Data_Counts
names(Deaths)<- c(names(Deaths)[1:5],cause.name.vec,'Total')

# standard population female population in 2005 (mid period)
pop.std <-  Deaths[Deaths$year==std.year,]
pop.std <- pop.std[,sum(Pop), by = list(age,sex)]
pop.std <- pop.std[,prop:=V1/sum(V1), by = list(sex)]
pop.std <- pop.std[pop.std$sex==2,]
pop.std <- pop.std[,c(1,4)]
sum(pop.std$prop)
# add pop standard to women
Deaths <- merge(Deaths,pop.std,by = 'age')
Deaths <- Deaths[order(year,sex,state,age),]

# get age-specific mortality rates
Deaths.asdr        <- Deaths
Deaths.asdr[,6:22] <- (Deaths.asdr[,6:22]/Deaths.asdr$Pop)*Deaths.asdr$prop
Deaths.asdr.melt   <- melt(data=Deaths.asdr[,c(1,2,3,4,6:22)],id.vars = c('age','year','sex','state'),variable.name = 'Cause',value.name = 'asdr')

#get the standardized rates with age1 and age2
ASDR <- Deaths.asdr.melt[,list(ASDR = sum(asdr[(age1 + 1):(age2+1)])*100000), by = list(year,sex,state,Cause)]

#get some avergae value from 1994-1999 2002-2007  2011-2016
Labels.periods   <- c('1994-1999','2002-2007','2011-2016')
years.avg        <- c(1994:1999,2002:2007,2011:2016)
ASDR.avg         <- ASDR[year %in% years.avg,]
ASDR.avg$Period  <- cut(ASDR.avg$year+1, breaks=c(1994,2002,2011,Inf),labels=Labels.periods)
ASDR.avg         <- ASDR.avg[,list(ASDR.avg= mean(ASDR)), by = list(sex,state,Cause,Period)]

# adding region and state name columns is then easy
ASDR.avg         <- cbind(region=region.recvec[as.character(ASDR.avg$state)],ASDR.avg)
ASDR.avg$region  <- factor(ASDR.avg$region, levels = 1:3, labels = c('South', 'Central', 'North'))
ASDR.avg$region  <- factor(ASDR.avg$region,levels = rev(levels(ASDR.avg$region)))
ASDR.avg         <- cbind(state.name=state.name.vec[as.character(ASDR.avg$state)],ASDR.avg)
ASDR.avg         <- ASDR.avg[order(Period,region,state,sex,Cause),]
ASDR.avg         <- ASDR.avg[order(state,Period,Cause),]

#get the difference in the periods
labels.period <- c('From 1994-99 to 2002-07', 'From 2002-07 to 2011-16')
ASDR.avg.diff               <- ASDR.avg[,list(Change=diff(ASDR.avg),Period=1:2), by = list(region,state,state.name,sex,Cause)]
ASDR.avg.diff$Period        <- as.factor(ASDR.avg.diff$Period)
levels(ASDR.avg.diff$Period)<- labels.period


 r.data            <- ASDR.avg.diff[ASDR.avg.diff$Cause=='Homicide' & ASDR.avg.diff$sex==2,]
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

###
Perception <- read.csv('Data/ENSI_2005.csv',header = T,sep = ';')
Perception <- Perception[,1:3]
Change <- Perception[1:32,1]
Change <- data.table(Change)
Change$Perception <- Perception[33:64,]$Proportion - Perception[1:32,]$Proportion

fig.data <- r.data[r.data$Period == 'From 2002-07 to 2011-16',]
fig.data <- fig.data[order(state),]
fig.data$Perception <- Change$Perception

fig.data$Perception1 <-Perception[1:32,]$Proportion
  fig.data$Perception2 <- Perception[33:64,]$Proportion

cols      <-  c("#DDDD77", "#44AA77", "#CC6677","#4477AA", 'red')

fig.abs<- ggplot(fig.data, aes(Change, Perception,color = region)) +
  ggtitle(paste('Change in standardized homicide rates (15-65) and vulnerability perception '),
          subtitle = 'from 2002-07 to 2011-16')+
  geom_point(aes(Change, Perception),size = 3,show.legend = T) +
  geom_text(aes(label=state.name),hjust=1.3, vjust=1.3,show.legend = F)+
  scale_colour_manual('Region',values=cols[2:4])+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  scale_x_continuous("Change in homicide rates", limits=c(-8,8))+
  scale_y_continuous("Change in perception (%)", limits=c(-55,55))+
  theme_light()+
  theme(plot.title = element_text(size=14, face = 'bold'))+
  theme(plot.subtitle = element_text(size=14, face = 'bold'))
  
pdf(file = 'Output/Exhibit1_2.pdf',width = 8,height = 7,useDingbats = F)
fig.abs
dev.off()

fig.data[order(Perception2),]

fig.data
