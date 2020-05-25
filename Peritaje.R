# Fomento Urbano S.A.
#                                                                                            
#                                   Estudio Actuarial de Da?os y Perjuicios
#
# Autores: Laura Campos 
#          Isaac Z. Arias
#
# En este documento se realiza el c?lculo por da?os y perjuicios para la firma Fomento Urbano S.A. por la no-ejecuci?n 
# de un contrato de promesa compraventa.

# Paquetes utilizados:
library(dplyr)
library(ggplot2)
library(openxlsx)
library(lubridate)
Sys.setlocale("LC_TIME", "Spanish")


#------------------------------------------------------ Carga de Datos:

# Insumos para distribici?n de ventas:
VentasReales <- read.xlsx("Reservas_Quarzo_Novazul.xlsx")

#------------------------------------------------------ An?lisis Exploratorio:

# Cantidad por tipos de Casas:
Cantidad <- VentasReales %>% group_by(Proy, Obra) %>% summarise(Cantidad = n()) %>% ungroup()
Cantidad

# Se mimetizan los tipos de casas de acuerdo a sus caracter?sticas:
VentasReales <- VentasReales %>% 
  filter(!Obra%in% c("AMATISTA 2D", "AMATISTA 3D", "ALTADENA", "ALTADENA 2D", "ALTADENA 3D")) %>% 
  mutate(Obra = ifelse(Obra %in% c("AMBAR 2D", "OPALO"), "AMBAR",
                       ifelse(Obra %in% c("NOVAZUL 2D", "NOVAZUL 3D"), "NOVAZUL", 
                              ifelse(Obra %in% c("CITRINO 2D", "CITRINO 3D", "CITRINO 2D 2.0", "CITRINO 3D 2.0"), "CITRINO", 
                                     Obra)))) %>% 
  mutate(Fecha = paste("01", Fecha, Anos, sep = "-")) %>% 
  select(-Anos) %>% 
  mutate(Fecha = as.POSIXct(Fecha, format="%d-%b-%Y")) %>% 
  mutate(Num_Mes = ifelse(Proy == "Novazul", interval(as.POSIXct("01-jun-2018", format="%d-%b-%Y"), Fecha)/months(1),
                          interval(as.POSIXct("01-feb-2017", format="%d-%b-%Y"),Fecha)/months(1))) %>% 
  mutate(Num_Mes = as.numeric(Num_Mes)+1) %>% 
  select(-Proy, -Fecha)

# Caracteristicas de los Datos:
summary(VentasReales)

# Distribuci?n de ventas generales:
ggplot(VentasReales, aes(x = Num_Mes, color = Obra)) +
  geom_histogram(fill="white")

#------------------------------------------------------ Distribuci?n por Tipo de Casa:

Dist_Tipo <- VentasReales %>% group_by(Obra) %>% summarise(Prob = n()) %>% ungroup() %>% mutate(Prob = Prob/sum(Prob))
Dist_Tipo

#------------------------------------------------------ Distribuci?n de Frecuencia para cada Tipo de Casa:

#---------------------------- Tipo: ?mbar

# Filtramos:
Dist_Ambar <- VentasReales %>% filter(Obra == "AMBAR")

# Primera Visualizaci?n Distribuci?n
ggplot(Dist_Ambar, aes(x=Num_Mes, color=Obra)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="#FF6766") 

# Distribuci?n: lo ideal es un ancho de banda de 5 meses
ggplot(Dist_Ambar, aes(x=Num_Mes, color=Obra)) +
  geom_histogram() +
  stat_bin(binwidth = 5)

# Probabilidades para cada 5 meses:
Dist_Ambar <- Dist_Ambar %>% mutate(Num_Mes = ifelse(Num_Mes<6, 1,
                                                     ifelse(Num_Mes<11, 2,
                                                            ifelse(Num_Mes<16, 3,
                                                                   ifelse(Num_Mes<21, 4,
                                                                          ifelse(Num_Mes<26, 5,
                                                                                 ifelse(Num_Mes<31, 6, 7))))))) %>% 
  group_by(Num_Mes) %>% 
  summarise(Prob = n()) %>% 
  ungroup() %>% 
  mutate(Prob = Prob/sum(Prob))

# Visualizamos:
Dist_Ambar

#---------------------------- Tipo: Citrino

# Filtramos:
Dist_Citrino <- VentasReales %>% filter(Obra == "CITRINO")

# Primera Visualizaci?n Distribuci?n
ggplot(Dist_Citrino, aes(x=Num_Mes, color=Obra)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="#FF6766") 

# Distribuci?n: lo ideal es un ancho de banda de 3 meses
ggplot(Dist_Citrino, aes(x=Num_Mes, color=Obra)) +
  geom_histogram() +
  stat_bin(binwidth = 5)

# Probabilidades para cada 5 meses:
Dist_Citrino <- Dist_Citrino %>% mutate(Num_Mes = ifelse(Num_Mes<6, 1,
                                                     ifelse(Num_Mes<11, 2,
                                                            ifelse(Num_Mes<16, 3,
                                                                   ifelse(Num_Mes<21, 4,
                                                                          ifelse(Num_Mes<26, 5,
                                                                                 ifelse(Num_Mes<31, 6, 
                                                                                        ifelse(Num_Mes<36, 7,8)))))))) %>% 
  group_by(Num_Mes) %>% 
  summarise(Prob = n()) %>% 
  ungroup() %>% 
  mutate(Prob = Prob/sum(Prob))

# Visualizamos:
Dist_Citrino

#---------------------------- Tipo: Citrino

# Filtramos:
Dist_Citrino <- VentasReales %>% filter(Obra == "CITRINO")

# Primera Visualizaci?n Distribuci?n
ggplot(Dist_Citrino, aes(x=Num_Mes, color=Obra)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="#FF6766") 

# Distribuci?n: lo ideal es un ancho de banda de 3 meses
ggplot(Dist_Citrino, aes(x=Num_Mes, color=Obra)) +
  geom_histogram() +
  stat_bin(binwidth = 5)

# Probabilidades para cada 5 meses:
Dist_Citrino <- Dist_Citrino %>% mutate(Num_Mes = ifelse(Num_Mes<6, 1,
                                                         ifelse(Num_Mes<11, 2,
                                                                ifelse(Num_Mes<16, 3,
                                                                       ifelse(Num_Mes<21, 4,
                                                                              ifelse(Num_Mes<26, 5,
                                                                                     ifelse(Num_Mes<31, 6, 
                                                                                            ifelse(Num_Mes<36, 7,8)))))))) %>% 
  group_by(Num_Mes) %>% 
  summarise(Prob = n()) %>% 
  ungroup() %>% 
  mutate(Prob = Prob/sum(Prob))

# Visualizamos:
Dist_Citrino

#---------------------------- Tipo: Novaluz

# Filtramos:
Dist_Novazul <- VentasReales %>% filter(Obra == "NOVAZUL")

# Primera Visualizaci?n Distribuci?n
ggplot(Dist_Novazul, aes(x=Num_Mes, color=Obra)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="#FF6766") 

# Distribuci?n: lo ideal es un ancho de banda de 3 meses
ggplot(Dist_Novazul, aes(x=Num_Mes, color=Obra)) +
  geom_histogram() +
  stat_bin(binwidth = 5)

# Probabilidades para cada 5 meses:
Dist_Novazul <- Dist_Novazul %>% mutate(Num_Mes = ifelse(Num_Mes<6, 1,
                                                         ifelse(Num_Mes<11, 2,
                                                                ifelse(Num_Mes<16, 3,
                                                                       ifelse(Num_Mes<21, 4,
                                                                              ifelse(Num_Mes<26, 5,
                                                                                     ifelse(Num_Mes<31, 6, 
                                                                                            ifelse(Num_Mes<36, 7,8)))))))) %>% 
  group_by(Num_Mes) %>% 
  summarise(Prob = n()) %>% 
  ungroup() %>% 
  mutate(Prob = Prob/sum(Prob))

# Visualizamos:
Dist_Novazul

#------------------------------------------------------ Valor futuro del flujo:
Flujo_Gasto<-read.xlsx("Modelo Financiero Avante - Las Lajas.xlsx",sheet = "Gasto",colNames = T,rowNames = T,skipEmptyRows = T)

# Gastos Fijos

Flujo_Gasto<-as.data.frame(t(Flujo_Gasto[,-1]))
Flujo_Gasto<- Flujo_Gasto %>% mutate(Gasto_Fijo=apply(Flujo_Gasto,1,sum)) %>% mutate(Gasto_Fijo=Gasto_Fijo-`Conexiones Servicios Publicos`-`Bienes Inmuebles`,Gastos_Compartidos=G.Ingeniero+Seguridad+`Gastos Administrativos`+`Gastos de Ventas (comision y otros)`)

Flujo_Gasto$Gasto_Fijo[7:50]<-Flujo_Gasto$Gasto_Fijo[7:50]-Flujo_Gasto$Gastos_Compartidos[7:50]
Gasto_Fijo<-Flujo_Gasto$Gasto_Fijo[1:14]


# Flujo esperado de gastos aleatorios
Gastos_Aleatorios<-sum(Flujo_Gasto$Gastos_Compartidos[7:50],Flujo_Gasto$`Conexiones Servicios Publicos`,Flujo_Gasto$`Bienes Inmuebles`)
Dist_Ambar_2<-Dist_Ambar %>% mutate(Prob=Prob*Dist_Tipo$Prob[1])
Dist_Citrino_2<-Dist_Citrino %>% mutate(Prob=Prob*Dist_Tipo$Prob[2])
Dist_Novazul_2<-Dist_Novazul %>% mutate(Prob=Prob*Dist_Tipo$Prob[3])

Dist_Ventas<-merge(Dist_Ambar_2,Dist_Citrino_2,by="Num_Mes",all.x = T,all.y = T)
Dist_Ventas<-merge(Dist_Ventas,Dist_Novazul_2,by="Num_Mes",all.x = T,all.y = T)
Dist_Ventas<-Dist_Ventas %>% mutate(Prob=ifelse(is.na(Prob),0,Prob),Prob.x=ifelse(is.na(Prob.x),0,Prob.x),Prob.y=ifelse(is.na(Prob.y),0,Prob.y))
Dist_Ventas<-Dist_Ventas %>% mutate(Prob.total=Prob+Prob.x+Prob.y)
Dist_Ventas<-rbind(rep(0,5),Dist_Ventas) %>% mutate(Num_Mes=1:9)
Gasto_Aleatorios<-Gastos_Aleatorios*Dist_Ventas$Prob.total

 ## Gastos por construccion de casas
  Gastos_Construccion<-Dist_Ventas %>% mutate(Costos.a=123*Prob.x*42483,Costos.c=123*Prob.y*64640,Costos.n=123*Prob*53049.30) %>%
    mutate(Costo_Total=Costos.a+Costos.c+Costos.n)
  
  Gasto_Construccion<-NULL
  Gasto_Construccion[1]<-0
  Gasto_Construccion[2]<-(5/8)*(Gastos_Construccion$Costo_Total[1])
  for (j in 3:(length(Gastos_Construccion$Num_Mes)+1)) {
    Gasto_Construccion[j]<-(5/8)*(Gastos_Construccion$Costo_Total[j-1])+(3/8)*(Gastos_Construccion$Costo_Total[j-2])
    
  }
  Gasto_Construccion[j+1]<-(3/8)*(Gastos_Construccion$Costo_Total[j-1])
  
# Flujo esperado de ingresos
  
  Ingresos<-Dist_Ventas %>% mutate(Ingresos.a=123*Prob.x*106693,Ingresos.c=123*Prob.y*141326,Ingresos.n=123*Prob*122332) %>%
    mutate(Ingreso_Total=Ingresos.a+Ingresos.c+Ingresos.n)
  
  Ingreso_Ventas<-NULL
  Ingreso_Ventas[1]<-0
  Ingreso_Ventas[2]<-(5/7)*0.2*(Ingresos$Ingreso_Total[1])
  for (j in 3:(length(Ingresos$Num_Mes)+1)) {
    Ingreso_Ventas[j]<-(5/7)*0.2*(Ingresos$Ingreso_Total[j-1])+((2/7)*0.2+0.8)*(Ingresos$Ingreso_Total[j-2])
  }
  Ingreso_Ventas[j+1]<-((2/7)*0.2+0.8)*(Ingresos$Ingreso_Total[j-1])
  

# Flujo esperado de gastos por deuda
  
Flujo_Gasto<-Flujo_Gasto %>% mutate(Gastos_Deuda=`Sistema Pluvial`+`Sistema Sanitario`+`Sistema Potable`+Pavimentos+`Obras Complementarias`+`Mejoras a media calle`+`Laguna de Retardo`+Muros+`Juegos Infantiles`+Otros+`Imprevistos(5%)`+Desfogue )  
  

Deuda<-as.data.frame(1:11)
colnames(Deuda)<-"Num_Mes"
Deuda <- Deuda %>% mutate(Desembolso=c(sum(Flujo_Gasto$Gastos_Deuda[1:5],500000),sum(Flujo_Gasto$Gastos_Deuda[6:10]),sum(Flujo_Gasto$Gastos_Deuda[11:14]),rep(0,8))) %>%
  mutate(Desembolso=Desembolso+Gasto_Construccion, Ventas=c(0,0,Ingresos$Ingreso_Total)) 

Saldo<-NULL
Saldo[1]<-Deuda$Desembolso[1]
for(j in 2:length(Deuda$Num_Mes)){
  Saldo[j]<-ifelse(Saldo[j-1]+Deuda$Desembolso[j]-Deuda$Ventas[j]*0.65>0,Saldo[j-1]+Deuda$Desembolso[j]-Deuda$Ventas[j]*0.65,ifelse(Saldo[j-1]+Deuda$Desembolso[j]-round((Saldo[j-1]+Deuda$Desembolso[j])/Deuda$Ventas[j],1)*Deuda$Ventas[j]>0,Saldo[j-1]+Deuda$Desembolso[j]-round((Saldo[j-1]+Deuda$Desembolso[j])/Deuda$Ventas[j],1)*Deuda$Ventas[j],Saldo[j-1]+Deuda$Desembolso[j]-(round((Saldo[j-1]+Deuda$Desembolso[j])/Deuda$Ventas[j],1)-1)*Deuda$Ventas[j]))
}


Deuda$Saldo<-Saldo
Deuda<-rbind(Deuda,c(length(Deuda$Num_Mes)+1,0,0,0))
Deuda<-Deuda %>% mutate(Interes=Saldo*0.095*5/12)

  
# Tasas (Valor futuro)

TRI<-data.frame(PLazo=c(0.25,1,3,6,9,12,26,36,60),Tasa=c(0.0022,0.0074,0.0133,0.0257,0.0312,0.0379,0.0531,0.0530,0.0387))
Flujo_Final<-Deuda %>% select(Num_Mes,Interes)
Gasto_Fijo<-Gasto_Fijo<-c(sum(Gasto_Fijo[1:5]),sum(Gasto_Fijo[6:10]),sum(Gasto_Fijo[11:14]),rep(0,9))
Gasto_Aleatorio<-c(Gasto_Aleatorios,rep(0,3))
Gasto_Construccion<-c(Gasto_Construccion,0)
Ingreso_Ventas<-c(Ingreso_Ventas,0)
Flujo_Final<- cbind(Flujo_Final,Gasto_Fijo,Gasto_Aleatorio,Gasto_Construccion,Ingreso_Ventas)
Flujo_Final<-Flujo_Final %>% mutate(Flujo=Ingreso_Ventas-Gasto_Fijo-Gasto_Aleatorio-Gasto_Construccion-Interes)

Flujo_Final<-Flujo_Final %>% mutate(Tasa=approx(TRI$PLazo,TRI$Tasa,xout = abs(5*(1:12)-2*5))$y) %>%
  mutate(Factor_acumulacion=case_when(Num_Mes>2 ~ (1+Tasa)^-1,
                            Num_Mes<2 ~ 1+Tasa,
                          Num_Mes==2 ~ 1)) %>%
  mutate(Flujo_acumulado=Flujo*Factor_acumulacion)

Ganancia_0<-sum(Flujo_Final$Flujo_acumulado)
Ganancia_0

# Covid 

## Atraso de 5 meses

# Flujo esperado de gastos aleatorios

Dist_Ventas<-rbind(rep(0,5),Dist_Ventas) %>% mutate(Num_Mes=1:10)
Gasto_Aleatorios<-Gastos_Aleatorios*Dist_Ventas$Prob.total

## Gastos por construccion de casas
Gastos_Construccion<-Dist_Ventas %>% mutate(Costos.a=123*Prob.x*42483,Costos.c=123*Prob.y*64640,Costos.n=123*Prob*53049.30) %>%
  mutate(Costo_Total=Costos.a+Costos.c+Costos.n)

Gasto_Construccion<-NULL
Gasto_Construccion[1]<-0
Gasto_Construccion[2]<-(5/8)*(Gastos_Construccion$Costo_Total[1])
for (j in 3:(length(Gastos_Construccion$Num_Mes)+1)) {
  Gasto_Construccion[j]<-(5/8)*(Gastos_Construccion$Costo_Total[j-1])+(3/8)*(Gastos_Construccion$Costo_Total[j-2])
  
}
Gasto_Construccion[j+1]<-(3/8)*(Gastos_Construccion$Costo_Total[j-1])

# Flujo esperado de ingresos

Ingresos<-Dist_Ventas %>% mutate(Ingresos.a=123*Prob.x*106693,Ingresos.c=123*Prob.y*141326,Ingresos.n=123*Prob*122332) %>%
  mutate(Ingreso_Total=Ingresos.a+Ingresos.c+Ingresos.n)

Ingreso_Ventas<-NULL
Ingreso_Ventas[1]<-0
Ingreso_Ventas[2]<-(5/7)*0.2*(Ingresos$Ingreso_Total[1])
for (j in 3:(length(Ingresos$Num_Mes)+1)) {
  Ingreso_Ventas[j]<-(5/7)*0.2*(Ingresos$Ingreso_Total[j-1])+((2/7)*0.2+0.8)*(Ingresos$Ingreso_Total[j-2])
}
Ingreso_Ventas[j+1]<-((2/7)*0.2+0.8)*(Ingresos$Ingreso_Total[j-1])


# Flujo esperado de gastos por deuda

Flujo_Gasto<-Flujo_Gasto %>% mutate(Gastos_Deuda=`Sistema Pluvial`+`Sistema Sanitario`+`Sistema Potable`+Pavimentos+`Obras Complementarias`+`Mejoras a media calle`+`Laguna de Retardo`+Muros+`Juegos Infantiles`+Otros+`Imprevistos(5%)`+Desfogue )  


Deuda<-as.data.frame(1:12)
colnames(Deuda)<-"Num_Mes"
Deuda <- Deuda %>% mutate(Desembolso=c(sum(Flujo_Gasto$Gastos_Deuda[1:5],500000),sum(Flujo_Gasto$Gastos_Deuda[6:10]),sum(Flujo_Gasto$Gastos_Deuda[11:14]),rep(0,9))) %>%
  mutate(Desembolso=Desembolso+Gasto_Construccion, Ventas=c(0,0,Ingresos$Ingreso_Total)) 

Saldo<-NULL
Saldo[1]<-Deuda$Desembolso[1]
for(j in 2:length(Deuda$Num_Mes)){
  Saldo[j]<-ifelse(Saldo[j-1]+Deuda$Desembolso[j]-Deuda$Ventas[j]*0.65>0,Saldo[j-1]+Deuda$Desembolso[j]-Deuda$Ventas[j]*0.65,ifelse(Saldo[j-1]+Deuda$Desembolso[j]-round((Saldo[j-1]+Deuda$Desembolso[j])/Deuda$Ventas[j],1)*Deuda$Ventas[j]>0,Saldo[j-1]+Deuda$Desembolso[j]-round((Saldo[j-1]+Deuda$Desembolso[j])/Deuda$Ventas[j],1)*Deuda$Ventas[j],Saldo[j-1]+Deuda$Desembolso[j]-(round((Saldo[j-1]+Deuda$Desembolso[j])/Deuda$Ventas[j],1)-1)*Deuda$Ventas[j]))
}


Deuda$Saldo<-Saldo
Deuda<-rbind(Deuda,c(length(Deuda$Num_Mes)+1,0,0,0))
Deuda<-Deuda %>% mutate(Interes=Saldo*0.095*5/12)



# Tasas (Valor futuro)

Flujo_Final<-Deuda %>% select(Num_Mes,Interes)
Gasto_Fijo<-c(Gasto_Fijo,0)
Gasto_Aleatorio<-c(Gasto_Aleatorios,rep(0,3))
Gasto_Construccion<-c(Gasto_Construccion,0)
Ingreso_Ventas<-c(Ingreso_Ventas,0)
Flujo_Final<- cbind(Flujo_Final,Gasto_Fijo,Gasto_Aleatorio,Gasto_Construccion,Ingreso_Ventas)
Flujo_Final<-Flujo_Final %>% mutate(Flujo=Ingreso_Ventas-Gasto_Fijo-Gasto_Aleatorio-Gasto_Construccion-Interes)

Flujo_Final<-Flujo_Final %>% mutate(Tasa=approx(TRI$PLazo,TRI$Tasa,xout = abs(5*(1:13)-2*5))$y) %>%
  mutate(Factor_acumulacion=case_when(Num_Mes>2 ~ (1+Tasa)^-1,
                                      Num_Mes<2 ~ 1+Tasa,
                                      Num_Mes==2 ~ 1)) %>%
  mutate(Flujo_acumulado=Flujo*Factor_acumulacion)

Ganancia_5<-sum(Flujo_Final$Flujo_acumulado)
Ganancia_5


## Atraso de 10 meses

# Flujo esperado de gastos aleatorios

Dist_Ventas<-rbind(rep(0,5),Dist_Ventas) %>% mutate(Num_Mes=1:11)
Gasto_Aleatorios<-Gastos_Aleatorios*Dist_Ventas$Prob.total

## Gastos por construccion de casas
Gastos_Construccion<-Dist_Ventas %>% mutate(Costos.a=123*Prob.x*42483,Costos.c=123*Prob.y*64640,Costos.n=123*Prob*53049.30) %>%
  mutate(Costo_Total=Costos.a+Costos.c+Costos.n)

Gasto_Construccion<-NULL
Gasto_Construccion[1]<-0
Gasto_Construccion[2]<-(5/8)*(Gastos_Construccion$Costo_Total[1])
for (j in 3:(length(Gastos_Construccion$Num_Mes)+1)) {
  Gasto_Construccion[j]<-(5/8)*(Gastos_Construccion$Costo_Total[j-1])+(3/8)*(Gastos_Construccion$Costo_Total[j-2])
  
}
Gasto_Construccion[j+1]<-(3/8)*(Gastos_Construccion$Costo_Total[j-1])

# Flujo esperado de ingresos

Ingresos<-Dist_Ventas %>% mutate(Ingresos.a=123*Prob.x*106693,Ingresos.c=123*Prob.y*141326,Ingresos.n=123*Prob*122332) %>%
  mutate(Ingreso_Total=Ingresos.a+Ingresos.c+Ingresos.n)

Ingreso_Ventas<-NULL
Ingreso_Ventas[1]<-0
Ingreso_Ventas[2]<-(5/7)*0.2*(Ingresos$Ingreso_Total[1])
for (j in 3:(length(Ingresos$Num_Mes)+1)) {
  Ingreso_Ventas[j]<-(5/7)*0.2*(Ingresos$Ingreso_Total[j-1])+((2/7)*0.2+0.8)*(Ingresos$Ingreso_Total[j-2])
}
Ingreso_Ventas[j+1]<-((2/7)*0.2+0.8)*(Ingresos$Ingreso_Total[j-1])


# Flujo esperado de gastos por deuda

Flujo_Gasto<-Flujo_Gasto %>% mutate(Gastos_Deuda=`Sistema Pluvial`+`Sistema Sanitario`+`Sistema Potable`+Pavimentos+`Obras Complementarias`+`Mejoras a media calle`+`Laguna de Retardo`+Muros+`Juegos Infantiles`+Otros+`Imprevistos(5%)`+Desfogue )  


Deuda<-as.data.frame(1:13)
colnames(Deuda)<-"Num_Mes"
Deuda <- Deuda %>% mutate(Desembolso=c(sum(Flujo_Gasto$Gastos_Deuda[1:5],500000),sum(Flujo_Gasto$Gastos_Deuda[6:10]),sum(Flujo_Gasto$Gastos_Deuda[11:14]),rep(0,10))) %>%
  mutate(Desembolso=Desembolso+Gasto_Construccion, Ventas=c(0,0,Ingresos$Ingreso_Total)) 

Saldo<-NULL
Saldo[1]<-Deuda$Desembolso[1]
for(j in 2:length(Deuda$Num_Mes)){
  Saldo[j]<-ifelse(Saldo[j-1]+Deuda$Desembolso[j]-Deuda$Ventas[j]*0.65>0,Saldo[j-1]+Deuda$Desembolso[j]-Deuda$Ventas[j]*0.65,ifelse(Saldo[j-1]+Deuda$Desembolso[j]-round((Saldo[j-1]+Deuda$Desembolso[j])/Deuda$Ventas[j],1)*Deuda$Ventas[j]>0,Saldo[j-1]+Deuda$Desembolso[j]-round((Saldo[j-1]+Deuda$Desembolso[j])/Deuda$Ventas[j],1)*Deuda$Ventas[j],Saldo[j-1]+Deuda$Desembolso[j]-(round((Saldo[j-1]+Deuda$Desembolso[j])/Deuda$Ventas[j],1)-1)*Deuda$Ventas[j]))
}


Deuda$Saldo<-Saldo
Deuda<-rbind(Deuda,c(length(Deuda$Num_Mes)+1,0,0,0))
Deuda<-Deuda %>% mutate(Interes=Saldo*0.095*5/12)



# Tasas (Valor futuro)

Flujo_Final<-Deuda %>% select(Num_Mes,Interes)
Gasto_Fijo<-c(Gasto_Fijo,0)
Gasto_Aleatorio<-c(Gasto_Aleatorios,rep(0,3))
Gasto_Construccion<-c(Gasto_Construccion,0)
Ingreso_Ventas<-c(Ingreso_Ventas,0)
Flujo_Final<- cbind(Flujo_Final,Gasto_Fijo,Gasto_Aleatorio,Gasto_Construccion,Ingreso_Ventas)
Flujo_Final<-Flujo_Final %>% mutate(Flujo=Ingreso_Ventas-Gasto_Fijo-Gasto_Aleatorio-Gasto_Construccion-Interes)

Flujo_Final<-Flujo_Final %>% mutate(Tasa=approx(TRI$PLazo,TRI$Tasa,xout = abs(5*(1:14)-2*5))$y) %>%
  mutate(Factor_acumulacion=case_when(Num_Mes>2 ~ (1+Tasa)^-1,
                                      Num_Mes<2 ~ 1+Tasa,
                                      Num_Mes==2 ~ 1)) %>%
  mutate(Flujo_acumulado=Flujo*Factor_acumulacion)

Ganancia_10<-sum(Flujo_Final$Flujo_acumulado)
Ganancia_10

  

###### GANANCIA ESPERADA FINAL
GANANCIA_ESPERADA<-1/15*Ganancia_0+5/15*Ganancia_5+9/15*Ganancia_10
GANANCIA_ESPERADA
