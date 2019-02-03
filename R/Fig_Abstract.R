rm(list=ls(all=TRUE))
library(data.table)
library(reshape2)
library(ggplot2)

setwd("C:/Users/jmaburto/Documents/GitHub/Violence-against-women-in-Mexico")

source('R/1_ASMR.R')

gdata::keep(r.data,sure=T)

#### sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 
#### 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer,
#### 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories


###  Start processing data from ensi and envipe 2005 and 2017

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
