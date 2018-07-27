# Functions to replicate results of Pescador et al 

cause.name.vec <- c('Infectious','Cancers','Circulatory','Birth','Diabetes','OtherAM','IHD',
                    'HIV','Suicide','Lung','Cirrhosis','Homicide','RTA','Oheart','Ill','NonAM')

CoD.name.vec <- c('Infectious and respiratory', 'Cancers', 'Circulatory',
                  'Birth conditions', 'Diabetes', 'Other AMS', 'IHD', 'HIV', 
                  'Suicide', 'Lung Cancer', 'Cirrhosis', 'Homicide',
                  'RTA', 'Other HD', 'Rest','Total')

state.name.vec <- c("Aguascalientes","Baja California","Baja California Sur","Campeche",
                    "Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
                    "Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico State","Michoacan",
                    "Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
                    "Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas",
                    "Tlaxcala","Veracruz","Yucatan","Zacatecas")

state.code.vec <- 1:32

names(state.name.vec) <- state.code.vec

region.recvec            <- c(2,3,3,1,3,2,1,3,2,3,2,1,2,2,2,
                              2,1,2,3,1,1,2,1,3,3,3,1,3,2,1,1,3)

names(region.recvec)     <- 1:32

