capitalSocial <- function(df_hogares){
  
## Asociaciones

df_asociaciones <- df_hogares %>% select(starts_with("P38C"), "Nombre")
names(df_asociaciones)[11] <- "Tramo"
df_orig_p <- df_asociaciones[df_hogares$P38 == 1,]
nombres <- c("Acceso a recursos económicos", "Acceso a insumos", "Acceso a centros de acopio", "Acceso a ayuda técnica", "Acceso a transporte", "Facilidades de comercializacion")
df_grap <- df_orig_p[, 1:6]
beneficios_aso_prod <- graficos_binarios(df_grap, nombres, cat = "Beneficios", "porcentaje", "Acceso a beneficios", df_orig = df_orig_p)

## Ayudas

df_asociaciones_ay <- df_hogares %>% select(starts_with("P38D"))
df_asociaciones_ay[, "Tramo"] <- df_hogares[, "Nombre"]
df_orig_p <- df_asociaciones_ay[df_hogares$P38 == 1,]
df_grap <- df_orig_p[, 1:7]
nombres <- c("Permite continuar con actividades productivas",
             "Permite reanudar actividades productivas",
             "Da estabilidad temporal",
             "Apoyo en la cohesión de familias",
             "Apoyo en la cohesión comunitaria",
             "Acceso a insumos resistentes al cambio climático",
             "Acceso a infraestructura inundaciones/sequias")

ayuda_aso_prod <- graficos_binarios(df_grap, nombres, cat = "Beneficios", "porcentaje", "Acceso a beneficios", df_orig = df_orig_p)


## asociaciones comunitarias

df_orig_p <- df_asociaciones[df_hogares$P39 == 1,]
df_grap <- df_orig_p[, 1:6]
nombres <- c("Acceso a recursos económicos", "Acceso a insumos", "Acceso a centros de acopio", "Acceso a ayuda técnica", "Acceso a transporte", "Facilidades de comercializacion")
beneficios_aso_com <- graficos_binarios(df_grap, nombres, cat = "Beneficios", "porcentaje", "Acceso a beneficios", df_orig = df_orig_p)

wordcloud_graph(c(df_hogares$P39C1, df_hogares$P39C2, df_hogares$P39C3), 500)

# Asociaciones directamente relacionadas con las lluvias crecientes o inundaciones/veranos

# df_asociaciones <- df_hogares %>% select(starts_with("P40"), "Nombre")
# names(df_asociaciones)[12] <- "Tramo"
# df_hogares$P40 %>% table() %>% prop.table()
# 
# df_orig_p_ll <- df_asociaciones[df_hogares$P38 == 1,]
# df_grap <- df_orig_p_ll[, 1:6]
# nombres <- c("Acceso a recursos económicos", "Acceso a insumos", "Acceso a centros de acopio", "Acceso a ayuda técnica", "Acceso a transporte", "Facilidades de comercializacion")
# benefici_aso_lluvias <- graficos_binarios(df_grap, nombres, cat = "Beneficios", "porcentaje", "Acceso a beneficios", df_orig = df_orig_p_ll)

# beneficios_aso_prod, ayuda_aso_prod, beneficios_aso_com
# wordcloud_graph(c(df_hogares$P39C1, df_hogares$P39C2, df_hogares$P39C3), 500)

# Habilidades en el manejo de la inundación

df_hab <- df_hogares %>% select(starts_with("P36"))
df_hab[, "tramo"] <- df_hogares[, "Nombre"]

df_hab1_grap <- df_hab[, c(1, 4)]
df_hab2_grap <- df_hab[, c(2, 4)]
df_hab3_grap <- df_hab[, c(3, 4)]

df_hab1_grap$P36A <- as.factor(df_hab1_grap$P36A)
levels(df_hab1_grap$P36A) <- paste0("0" , levels(df_hab1_grap$P36A))
levels(df_hab1_grap$P36A)[3] <- "10"
df_hab1_grap$P36A <- as.character(df_hab1_grap$P36A)


df_hab2_grap$P36B <- as.factor(df_hab2_grap$P36B)
levels(df_hab2_grap$P36B) <- paste0("0" , levels(df_hab2_grap$P36B))
levels(df_hab2_grap$P36B)[3] <- "10"
df_hab2_grap$P36B <- as.character(df_hab2_grap$P36B)


df_hab3_grap$P36C <- as.factor(df_hab3_grap$P36C)
levels(df_hab3_grap$P36C) <- paste0("0" , levels(df_hab3_grap$P36C))
levels(df_hab3_grap$P36C)[3] <- "10"
df_hab3_grap$P36C <- as.character(df_hab3_grap$P36C)

names(df_hab1_grap) <- c("Aprendizaje relacionado con obras", "tramo")
names(df_hab2_grap) <- c("Aprendizaje de comportamiento soc", "tramo")
names(df_hab3_grap) <- c("Experiencia en inundaciones o sequias", "tramo")

aprendizaje_obras <- create_barplot_tramo_hor(df_hab1_grap, "Aprendizaje relacionado con obras", xlab = "Puntuación", ylab = "Número de hogares")
aprendizaje_comp_soc <- create_barplot_tramo_hor(df_hab2_grap, "Aprendizaje de comportamiento soc", xlab = "Puntuación", ylab = "Número de hogares")
expreiencia_inundaciones <- create_barplot_tramo_hor(df_hab3_grap, "Experiencia en inundaciones o sequias", xlab = "Puntuación", ylab = "Número de hogares")

names(df_hab) <- c("Aprendizaje relacionado con obras", "Aprendizaje de comportamiento soc", "Experiencia en inundaciones o sequias", "Tramo")
resumen_tramo(df_hab)$resumen


# Habilidades en el manejo de la inundación

# Experiencias con el manejo de inundaciones

df_exp <- df_hogares %>% select("P27")

df_exp$P27 <- df_hogares$P27 %>% as.numeric()

df_exp <- df_exp %>% 
  mutate(
    experiencias = case_when(
      is.na(P27) ~ NA_real_,
      P27 == 0 & P27 < 1 ~ 0,#"nunca",#"Entre 0 y 5mts",
      P27 >= 1 & P27 < 5 ~ 1,# "Entre 1 y 4 veces",#"Entre 5km y 10km",
      P27 >= 5 & P27 < 10 ~ 2,#"Entre 5 y 9 veces",#"Entre 10km y 50km",
      P27 >= 10 & P27 < 15 ~ 3,#"Entre 10  y 14 veces",#"Entre 10km y 50km",
      P27 >= 15 ~ 4,#"más de 15 veces",#"Entre 10km y 50km",
      TRUE ~ NA_real_
    )
  )


df_exp$experiencias <- as.factor(df_exp$experiencias)
levels(df_exp$experiencias) <- c("Nunca", "Entre 1 y 4 veces", "Entre 5 y 9 veces", "Entre 10 y 15 veces", "Más de 15 veces")
df_exp[, "tramo"] <- df_hogares[, "Nombre"]

df_exp <- df_exp[, -1]
veces_enfrentado_lluvias_sequias <- create_barplot_tramo(df_exp, "experiencias", xlab = "Veces que se ha enfrentado", ylab = "Número de hogares")
df_exp$experiencias %>% table() %>% prop.table()

# ¿Desde cuándo?

df_hogares$P28 %>% table() %>% prop.table()

fechas <- ifelse(df_hogares$P28 == "9999", NA, df_hogares$P28) %>% data_frame()
fechas[, "tramo"] <- df_hogares[, "Nombre"]
names(fechas) <- c("fechas", "tramo")
# create_barplot_tramo_hor(fechas, "fechas", xlab = "¿Desde cuándo?", ylab = "Número de hogares")
fechas_grap <- fechas$fechas %>% table() %>% data_frame()
fechas_grap[, "fecha"] <- names(table(fechas$fechas))
names(fechas_grap) <- c("hogares", "Año")
fechas_grap$Año <- as.numeric(fechas_grap$Año)
fechas_grap$hogares <- as.numeric(fechas_grap$hogares)

fecha_recuerda_lluv_inunda <- ggplot(fechas_grap, aes(Año, hogares)) + geom_line(col = "#005117")


df_hogares$P29 %>% table() %>% prop.table()


## ¿Cómo se enfrenta a las inundaciones?

df_enf <- df_hogares %>% select(starts_with("P31"))
df_enf[, "Tramo"] <- df_hogares[, "Nombre"]
beneficios <- graficos_binarios(df_grap, nombres, cat = "Beneficios", "porcentaje", "Acceso a beneficios", df_orig = df_orig_p)

nombres <- c("Deja de utilizar parcialmente la parcela", "Cambia el uso parcial de la parcela", "Cambia el uso total de la parcela", "Cambia de cultivo", "Cambia de actividad")
como_enfrenta_in_seq <- graficos_binarios(df_enf[,1:5], nombres, cat = "Procede", "porcentaje", "¿cómo enfrenta los eventos?", df_orig = df_enf)


wordcloud_graph(df_enf$P31OTHER)
wordcloud_graph(df_enf$P31E1)

df_hogares$P3101 %>% table() %>% prop.table()
df_hogares$P310102 %>% table() %>% prop.table()
df_hogares$P3102 %>% table() %>% prop.table()
df_hogares$P310201 %>% table() %>% prop.table()
wordcloud_graph(df_hogares$P310201)

df_ayudas <- df_enf[, 12:14]

nombres_ayuda <- c("Créditos blandos", "Herramientas", "Alimento", "Otros")
ayudas_recibe <- graficos_binarios(df_ayudas[df_enf$P3101 == 1,], nombres_ayuda, cat = "Ayuda", "porcentaje", "¿Qué tipo de ayuda recibe?", df_orig = df_enf[df_enf$P3101 == 1,])

df_hogares$P33 %>% table() %>% prop.table()
df_hogares$P34 %>% table() %>% prop.table()

df_union <- df_hogares %>% select(starts_with(c("P3401")))
df_union[, "Tramo"] <- df_hogares$Nombre
nombres_union <- c("Iniciativa propia", "iniciativa del lider comunal", "Los vecinos lo convocan", "El representante de la autoridad")
iniciativas_reunir <- graficos_binarios(df_union[df_hogares$P34 == 1, -c(4,5)], nombres_union, cat = "Reune", "porcentaje", "¿Quién toma la iniciativa?", df_orig = df_union[df_hogares$P34 == 1,])

df_union$P3401OTHER %>% table() %>% prop.table() %>% sort()

return(list(beneficios_aso_prod = beneficios_aso_prod$graph,
            ayuda_aso_prod = ayuda_aso_prod$graph,
            beneficios_aso_com = beneficios_aso_com$graph,
            aprendizaje_obras = aprendizaje_obras,
            aprendizaje_comp_soc = aprendizaje_comp_soc,
            expreiencia_inundaciones = expreiencia_inundaciones,
            veces_enfrentado_lluvias_sequias = veces_enfrentado_lluvias_sequias,
            fecha_recuerda_lluv_inunda = fecha_recuerda_lluv_inunda,
            como_enfrenta_in_seq = como_enfrenta_in_seq$graph,
            ayudas_recibe = ayudas_recibe$graph,
            iniciativas_reunir = iniciativas_reunir$graph))

}
# beneficios_aso_prod, ayuda_aso_prod, beneficios_aso_com, aprendizaje_obras, aprendizaje_comp_soc, expreiencia_inundaciones, veces_enfrentado_lluvias_sequias, fecha_recuerda_lluv_inunda, como_enfrenta_in_seq, ayudas_recibe, iniciativas_reunir
# wordcloud_graph(c(df_hogares$P39C1, df_hogares$P39C2, df_hogares$P39C3), 500)
# que_hace_durante <- wordcloud_graph(df_enf$P31OTHER)
# que_activiades_hace <- wordcloud_graph(df_enf$P31E1)
# organizadores: wordcloud_graph(df_hogares$P310201)
# 


