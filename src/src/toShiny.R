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
capitalHumanoSalida <- capitalHumano(df_hogares, df_personas)
capitalSoci <- capitalSocial(df_hogares)
capitalFisi <- capitalFisico(df_hogares)
df_financi <- capitalFinanciero(df_hogares)

# source("preprocesamiento.R", encoding = "UTF-8")
# source("graficos.R", encoding = "UTF-8")


df_princip <- data.frame(Capital = c(rep("general", length(actividadesPrincipales))),
                           nombre = names(actividadesPrincipales),
                           descripcion = c("Porcentaje de hogares que realizan alguna actividad econónica",
                                           "Numero de hogares por actividad económica / Tramo",
                                           "Relacion entre el % de hogares en las dos principales actividades",
                                           "Relacion entre el conteo de hogares en las dos principales actividades"))

df_humano <- data.frame(Capital = c(rep("humano", length(capitalHumanoSalida))),
                         nombre = names(capitalHumanoSalida),
                         descripcion = c("Personas - Nivel educativo",
                                         "Personas - Nivel educativo por tramo",
                                         "Personas - Tipo de contribución a la producción económica del hogar",
                                         "Porcentaje de tiempo dedicado a realizar las tareas del hogar",
                                         "Número de personas que compone el hogar",
                                         "Número de personas vs cuántos miembros trabajan (todos/algunos)",
                                         "Proporcion de personas que trabajan en el hogar",
                                         "Actividades que realizan las personas para aportar en el hogar"))

df_soci <- data.frame(Capital = c(rep("humano", length(capitalSoci))),
                      nombre = names(capitalSoci),
                      descripcion = c("Beneficios de pertenecer a una asociación de productores",
                                      "Beneficios de pertenecer a una asociación de productores en casos de inundación/lluvias",
                                      "Beneficios de pertenecer a una asociación comunitaria",
                                      "Percepcion de la importancia (1-10) de aprendizaje en obras para enfrentar situaciones de inundaciones/sequias",
                                      "Percepcion de la importancia (1-10)de aprendizaje de comportamiento social para enfrentar situaciones de inundaciones/sequias",
                                      "Percepcion de la importancia (1-10) de experiencia en inundaciones/ sequias para enfrentar situaciones de inundaciones/sequias",
                                      "Número de veces que se ha enfrentado a situaciones muy fuertes de inundacion / sequia",
                                      "Año desde el que los hogares encuestados recuerdan las situaciones de inundación o sequía",
                                      "Formas en que los hogares se enfrentan a eventos de lluvias, crecientes, inundaciones, veranos y sequias",
                                      "Tipo de ayuda que recibe de las entidades nacionales",
                                      "¿Quién toma la iniciativa de realizar uniones?"))


df_fisico <- data.frame(Capital = c(rep("capitalFisico", length(capitalFisi))),
                         nombre = names(capitalFisi),
                         descripcion = c("Puntuación respecto a diferentes servicios durante la etapa de construcción y contingencia",
                                         "Diferencia entre las puntuaciones construcción – contingencia reportadas por los hogares para las diferentes necesidades básicas por clase",
                                         "Lugares donde los hogares venden los productos",
                                         "Distancia desde el predio hasta donde saca los productos a la venta",
                                         "Resumen de infraestructura vial",
                                         "Costo de transportar los productos desde el predio a su punto de venta.",
                                         "Resumen costo de transportar por tramo",
                                         "¿Cómo saca los productos a la venta?",
                                         "Beneficios de pertenecer a una asociacion productiva",
                                         "Beneficios de pertenecer a una asociacion comunitaria",
                                         "Insumos para las actividades productivas adquirudos en el mercado",
                                         "Obtención de las semillas de forma que no requiera su compra",
                                         "Obtención de las semillas de forma que no requiera su compra Resistentes a las sequias/invierno",
                                         "Procesamiento de abono",
                                         "Tipo de abono, procesamiento",
                                         "Recursos del entorno"))


df_financi <- data.frame(Capital = c(rep("capitalFisico", length(df_financi))),
                                                nombre = names(df_financi),
                                                descripcion = c("Fuente de ingreso",
                                                                "Origen de las donaciones / en caso de recibir donaciones",
                                                                "Origen de las remesas / en caso de recibir remesas",
                                                                "Ingresos mensuales por fuente de ingreso y clase",
                                                                "Ingresos mensuales por fuente de ingreso y tramo",
                                                                "Porcentaje de autoconsumo por actividad",
                                                                "Porcentaje de los medios de vida del hogar por actividad",
                                                                "Porcentaje de autoconsumo por actividades",
                                                                "Distribución de los gastos reportados por los hogares encuestados",
                                                                "Descripción de los montos de los gastos mensuales del hogar por categorías de valores",
                                                                "Descripción de los gastos de los hogares por categorías de montos y por tramo.",
                                                                "Acreedor de los créditos adquiridos por los hogares encuestados.",
                                                                "Destino de los créditos de actividades productivas",
                                                                "Canitdad de veces en el año que solicita crédito",
                                                                "Costos estimados de las mejoras por conceptos de obras de riego e infraestructura relacionada con inundaciones/sequias",
                                                                "Origen del agua para consumo doméstico",
                                                                "Origen del agua para la producción agropecuaria",
                                                                "Monto del arriendo / en caso de que el hogar viva en arriendo",
                                                                "Acceso a la tierra"
                                                                ))
