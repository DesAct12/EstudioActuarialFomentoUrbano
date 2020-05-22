# Fomento Urbano S.A.
#                                                                                            
#                                   Estudio Actuarial de Daños y Perjuicios
#
# Autores: Laura Campos 
#          Isaac Z. Arias
#
# En este documento se realiza el cálculo por daños y perjuicios para la firma Fomento Urbano S.A. por la no-ejecución 
# de un contrato de promesa compraventa.

# Paquetes utilizados:
library(dplyr)
library(ggplot2)
library(openxlsx)
library(lubridate)
Sys.setlocale("LC_TIME", "Spanish")


#------------------------------------------------------ Carga de Datos:

# Insumos para distribición de ventas:
VentasReales <- read.xlsx("Reservas_Quarzo_Novazul.xlsx")

#------------------------------------------------------ Análisis Exploratorio:

# Cantidad por tipos de Casas:
Cantidad <- VentasReales %>% group_by(Proy, Obra) %>% summarise(Cantidad = n()) %>% ungroup()
Cantidad

# Se mimetizan los tipos de casas de acuerdo a sus características:
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

# Distribución de ventas generales:
ggplot(VentasReales, aes(x = Num_Mes, color = Obra)) +
  geom_histogram(fill="white")

#------------------------------------------------------ Distribución por Tipo de Casa:

Dist_Tipo <- VentasReales %>% group_by(Obra) %>% summarise(Prob = n()) %>% ungroup() %>% mutate(Prob = Prob/sum(Prob))
Dist_Tipo

#------------------------------------------------------ Distribución de Frecuencia para cada Tipo de Casa:

#---------------------------- Tipo: Ámbar

# Filtramos:
Dist_Ambar <- VentasReales %>% filter(Obra == "AMBAR")

# Primera Visualización Distribución
ggplot(Dist_Ambar, aes(x=Num_Mes, color=Obra)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="#FF6766") 

# Distribución: lo ideal es un ancho de banda de 5 meses
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

# Primera Visualización Distribución
ggplot(Dist_Citrino, aes(x=Num_Mes, color=Obra)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="#FF6766") 

# Distribución: lo ideal es un ancho de banda de 3 meses
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

# Primera Visualización Distribución
ggplot(Dist_Citrino, aes(x=Num_Mes, color=Obra)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="#FF6766") 

# Distribución: lo ideal es un ancho de banda de 3 meses
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

# Primera Visualización Distribución
ggplot(Dist_Novazul, aes(x=Num_Mes, color=Obra)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.2, fill="#FF6766") 

# Distribución: lo ideal es un ancho de banda de 3 meses
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
