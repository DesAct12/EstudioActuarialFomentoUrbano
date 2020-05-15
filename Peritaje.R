# Fomento Urbano S.A.
#                                                                                            
#                                   Estudio Actuarial de Daños y Perjuicios
#
# Autores: Laura Campos 
#          Isaac Z. Arias
#
# En este documento se realiza el cálculo por daños y perjuicios para la firma Fomento Urbano S.A. por la no no ejecución 
# de un contrato de promesa por compra y venta.

# Paquetes utilizados:
library(dplyr)
library(tidyr)
library(openxlsx)

# Carga de Datos:

# Insumos para distribición de ventas:
Insumo <- read.xlsx("Reservas_Quarzo_Novazul.xlsx")
