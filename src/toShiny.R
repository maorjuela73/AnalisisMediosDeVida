library(ggplot2)
library(dplyr)
library(DataExplorer)
library(plotly)
library(tidyverse)
library(FactoMineR)
library(readxl)
library(corrplot)
library(ggcorrplot)
library(reshape2)
# Text Mining
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
setwd("C:/Users/diant/Documents/deproyectos-Cauca/Bases/Análisis/Src")

# Graficos
source("graficos.R", encoding = "UTF-8")
# carga de los datos
source("preprocesamiento.R", encoding = "UTF-8")
# Calculos de actividades principales
source("actividadesPrincipales.R", encoding = "UTF-8")
# Calculos de capital humano
source("capitalhumano.R", encoding = "UTF-8")
# Capital Social
source("capitalsocial.R", encoding = "UTF-8")
# Capital físico
source("capitalfisico.R", encoding = "UTF-8")
# Capital financiero
source("capitalfinanciero.R", encoding = "UTF-8")

actividadesPrincipales <- actividadesPrincipalesFun(df_hogares)
capitalHumano(df_hogares, df_personas)
capitalSocial(df_hogares)
capitalFisico(df_hogares)
capitalFinanciero(df_hogares)

# source("preprocesamiento.R", encoding = "UTF-8")
# source("graficos.R", encoding = "UTF-8")


a <- data.frame(Capital = c(rep("general", length(actividadesPrincipales))),
                           nombre = names(actividadesPrincipales),
                           descripcion = c("Porcentaje de hogares que realizan alguna actividad econónica",
                                           "Numero de hogares por actividad económica / Tramo",
                                           "Relacion entre el % de hogares en las dos principales actividades",
                                           "Relacion entre el conteo de hogares en las dos principales actividades"))


actividadesPrincipales[[a$nombre[1]]]
