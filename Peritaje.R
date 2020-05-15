# Fomento Urbano S.A.
#                                                                                            
#                                   Estudio Actuarial de Da�os y Perjuicios
#
# Autores: Laura Campos 
#          Isaac Z. Arias
#
# En este documento se realiza el c�lculo por da�os y perjuicios para la firma Fomento Urbano S.A. por la no no ejecuci�n 
# de un contrato de promesa por compra y venta.

# Paquetes utilizados:
library(dplyr)
library(tidyr)


# Carga de Datos:

# Insumos para distribici�n de ventas:
Insumo <- read.xlsx("Reservas_Quarzo_Novazul.xlsx")