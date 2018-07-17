###### Code to process data from INEGI original mortality files 1990-2015
###### The classification is based on Aburto et al 2016
###### The outcome data is the input for the paper with Tim Riffe
library(reshape)
library(gdata)
library(latticeExtra)

rm(list=ls(all=TRUE))


setwd("C:/Users/jmaburto/Desktop/Women homicides/")


# File to store no specified deaths
Deaths.NoSpecified <- NULL

initial_year    <- 2000
Nspecified_year <- 9999
Deaths1         <-NULL

#i <- 2016
for(i in 2000:2016){  
  j<-paste("Data/Original INEGI data 90-2016/DEF",i,".RData",sep="")
  load(j)  
  DEF<-get(paste("DEF",i,sep=""))    

  if (i==1998){DEF<- subset(DEF,select=c(ENT_RES,CAUSA,SEXO,EDAD_L,EDAD_N,ANO_D))
               DEF$ANO_D<-as.integer(as.character(DEF$ANO_D))}
  if (i==1999){DEF<- subset(DEF,select=c(ent_res,causa,sexo,edad_l,edad_n,ano_d))
               DEF<-rename.vars(DEF,c("ent_res","causa","sexo","edad_l","edad_n","ano_d"),
                                c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i>=2000 & i<=2001){DEF<- subset(DEF,select=c(entres,causa,sexo,cveedad,edad,anodef))
               DEF<-rename.vars(DEF,c("entres","causa","sexo","cveedad","edad","anodef"),
                                c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i>=2002 & i<=2003){DEF<- subset(DEF,select=c(ENTRES,CAUSA,SEXO,CVEEDAD,EDAD,ANODEF))
                         DEF<-rename.vars(DEF,c("ENTRES","CAUSA","SEXO","CVEEDAD","EDAD","ANODEF"),
                                          c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)
                         DEF$ANO_D<-as.integer(as.character(DEF$ANO_D))}
  if (i==2004){DEF<- subset(DEF,select=c(entres,causab,causad,sexo,cveedad,edad,anodef))
               DEF$causa<-paste(DEF$causab,DEF$causad,sep="")
               DEF<-subset(DEF,select=c(entres,causa,sexo,cveedad,edad,anodef))
               DEF<-rename.vars(DEF,c("entres","causa","sexo","cveedad","edad","anodef"),
                               c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i==2005){DEF<- subset(DEF,select=c(entres,causab,causad,sexo,cveedad,edad,aniodef))
               DEF$causa<-paste(DEF$causab,DEF$causad,sep="")
               DEF<-subset(DEF,select=c(entres,causa,sexo,cveedad,edad,aniodef))
               DEF<-rename.vars(DEF,c("entres","causa","sexo","cveedad","edad","aniodef"),
                                c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i==2006){DEF$edad_l<-0
               DEF<- subset(DEF,select=c(ent_resid,causa_def,sexo,edad_l,edad,anio_ocur))                              
               DEF<-rename.vars(DEF,c("ent_resid","causa_def","sexo","edad_l","edad","anio_ocur"),
                                c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i>=2007 & i<=2008){DEF<- subset(DEF,select=c(ENTRH,CAUSADEF,DESDOBLA,SEXO,EDADUNI,EDADVALOR,ANIODEF))
               DEF$causa<-paste0(DEF$CAUSADEF,DEF$DESDOBLA)
               DEF<-subset(DEF,select=c(ENTRH,causa,SEXO,EDADUNI,EDADVALOR,ANIODEF))
               DEF<-rename.vars(DEF,c("ENTRH","causa","SEXO","EDADUNI","EDADVALOR","ANIODEF"),
                                c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)
               
               DEF$SEXO<-as.numeric(as.character(DEF$SEXO))
               DEF$ANO_D<-as.numeric(as.character(DEF$ANO_D))}
  if (i>=2009 & i<=2011){DEF$edad_l<-0
               DEF<- subset(DEF,select=c(ent_resid,causa_def,sexo,edad_l,edad,anio_ocur))                              
               DEF<-rename.vars(DEF,c("ent_resid","causa_def","sexo","edad_l","edad","anio_ocur"),
                                c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i>=2012 & i<=2016){DEF$edad_l<-0
               DEF<- subset(DEF,select=c(ENT_RESID,CAUSA_DEF,SEXO,edad_l,EDAD,ANIO_OCUR))                              
               DEF<-rename.vars(DEF,c("ENT_RESID","CAUSA_DEF","SEXO","edad_l","EDAD","ANIO_OCUR"),
                                c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}


  if (is.factor(DEF$ENT_RES)){
    DEF$ENT_RES<-as.integer(as.character(DEF$ENT_RES)) 
  }
  
  if (is.factor(DEF$EDAD_N)){
    DEF$EDAD_N<-as.integer(as.character(DEF$EDAD_N)) 
  }
  
  if (is.factor(DEF$SEXO)){
    DEF$SEXO<-as.integer(as.character(DEF$SEXO)) 
  }
  

  DEF<-subset(DEF,DEF$ANO_D>=initial_year)
  DEF<-subset(DEF,DEF$ANO_D<Nspecified_year)
  k <- dim(DEF)[1]
  #DEF<-subset(DEF,DEF$ENT_RES<33)    
  #DEF<-subset(DEF,DEF$SEXO<3)
  
  #DEF<-subset(DEF,DEF$SEXO>=1)
  
  if(i<2006){
    DEF$AGE<-DEF$EDAD_N
    DEF$EDAD_L=as.character(DEF$EDAD_L)
    DEF$AGE[DEF$EDAD_L=="D"] <- 0
    DEF$AGE[DEF$EDAD_L=="H"] <- 0
    DEF$AGE[DEF$EDAD_L=="M"] <- 0  
    DEF$AGE<-as.integer(DEF$AGE)
    #DEF<-subset(DEF,DEF$AGE<121)
     
  }
  
  if(i==2006){
    DEF$AGE<-DEF$EDAD_N-4000
    DEF$AGE[DEF$AGE<1] <- 0
    DEF$AGE<-as.integer(DEF$AGE)
    #DEF<-subset(DEF,DEF$AGE<121)
  }

  
  if(i>2006 & i<2009){
    DEF$AGE<-DEF$EDAD_N
    DEF$EDAD_L=as.character(DEF$EDAD_L)
    DEF$AGE[DEF$EDAD_L=="D"] <- 0
    DEF$AGE[DEF$EDAD_L=="H"] <- 0
    DEF$AGE[DEF$EDAD_L=="M"] <- 0  
    DEF$AGE<-as.integer(DEF$AGE) 
    #DEF<-subset(DEF,DEF$AGE<121)
    
  }
  
  if(i>=2009){
    DEF$AGE<-DEF$EDAD_N-4000
    DEF$AGE[DEF$AGE<1] <- 0
    DEF$AGE<-as.integer(DEF$AGE)
    #DEF<-subset(DEF,DEF$AGE<121)
  }
  
  DEF$CAUSA<-as.character(DEF$CAUSA)                  
  DEF$CV1<-substr(DEF$CAUSA,2,3)
  DEF$CV2<-substr(DEF$CAUSA,1,1)
  DEF$CV3<-substr(DEF$CAUSA,4,4)
  DEF$CV1<-as.integer(DEF$CV1)
  Deaths1<-rbind(Deaths1,DEF)  
  k1 <- k-dim(DEF)[1]
  k2 <- cbind(i,k1)
  Deaths.NoSpecified <- rbind(Deaths.NoSpecified,k2)
 print(i)
}

colSums(is.na(Deaths1))
keep(Deaths1,Deaths.NoSpecified,sure=T)

#load("Deaths1.RData")
### sigue clasificacion

####AM Classification for ICD 10:
#### 1. everything else
#### 2. Homicides
unique(Deaths1$CAUSA)
Deaths1$CAUSA<-as.character(Deaths1$CAUSA)                  
Deaths1$CV1<-substr(Deaths1$CAUSA,2,3)
Deaths1$CV2<-substr(Deaths1$CAUSA,1,1)
Deaths1$CV3<-substr(Deaths1$CAUSA,4,4)
Deaths1$CV1<-as.integer(Deaths1$CV1)  


colSums(is.na(Deaths1))

Deaths1$AM<-1
attach(Deaths1)
Deaths1$AM[Deaths1$CV2=="X" & Deaths1$CV1>=85 & Deaths1$CV1<=99] <- 2
Deaths1$AM[Deaths1$CV2=="Y" & Deaths1$CV1>=0 & Deaths1$CV1<=9] <- 2
detach(Deaths1)

table(Deaths1[Deaths1$ANO_D==2016, ]$SEXO,Deaths1[Deaths1$ANO_D==2016,]$AM)

Deaths <- subset(Deaths, select=c(ENT_RESID,CAUSA_DEF,SEXO,EDAD,ANIO_OCUR,AM))

Deaths1<- subset(Deaths1,select=c(ENT_RES,CAUSA,SEXO,AGE,ANO_D,AM))
Deaths1<-rename.vars(Deaths1,c("ENT_RES","CAUSA","SEXO","AGE","ANO_D","AM"),
                     c("ENT_RESID","CAUSA_DEF","SEXO","EDAD","ANIO_OCUR","AM"),info=TRUE)
save(Deaths1,file="Processed data/98-2016AM.RData")

AM_Data<-rbind(Deaths,Deaths1)

save(AM_Data,file="Processed data/Total_AMData.RData")

save(Deaths.NoSpecified ,file="Processed data/NoSpecified.RData")

