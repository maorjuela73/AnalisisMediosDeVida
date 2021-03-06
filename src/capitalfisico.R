# Analisis del capital físico
capitalFisico <- function(df_hogares){
# Infraestructura - Necesidades básicas

df_infraes_constr <- df_hogares %>% select(starts_with("P6901"))
df_infraes_contin <- df_hogares %>% select(starts_with("P6902"))

for(i in 1:ncol(df_infraes_constr)){
  df_infraes_constr[, i] <- transformacion_faltantes(df_infraes_constr[, i] %>% unlist() %>% as.numeric())
  df_infraes_contin[, i] <- transformacion_faltantes(df_infraes_contin[, i] %>% unlist() %>% as.numeric())
}

# Etapa de construcción vs estapa de contingencia

names(df_infraes_constr) <- c("Transporte",
                       "Salud",
                       "Educación",
                       "Comercialización",
                       "Seguridad",
                       "Relaciones_comunitarias")

names(df_infraes_contin) <- c("Transporte",
                       "Salud",
                       "Educación",
                       "Comercialización",
                       "Seguridad",
                       "Relaciones_comunitarias")


df_infraes_constr[, "Etapa"] <- "Construcción"

df_infraes_contin[, "Etapa"] <- "Contingencia"

df_infraes <- rbind(df_infraes_constr, df_infraes_contin)

df_infraes_graph <- melt(df_infraes) %>% data_frame()


infraes_grap <- ggplot(df_infraes_graph, aes(value, fill = Etapa))
infraest_puntuacion <- infraes_grap + geom_bar(position = "dodge")+ facet_wrap(. ~ variable, ncol = 3) + xlab("Infraestructura") + ylab("Número de hogares") +
  scale_fill_manual("Etapa", values = c("Construcción" = "#8CBD0E", "Contingencia" = "#005117"))
# autoc_df_graf_temp

# Diferencia entre las puntuaciones

infra_diferencia <- df_infraes_constr[, -7]

for(i in 1:ncol(infra_diferencia)){
  infra_diferencia[, i] <-  df_infraes_constr[, i] - df_infraes_contin[, i]
}

infra_diferencia[, "Clase"] <- df_hogares$CLASE
infra_diferencia_grap <- melt(infra_diferencia, id.vars = "Clase") %>% data_frame()
infra_diferencia_grap$Clase <- as.factor(infra_diferencia_grap$Clase)
levels(infra_diferencia_grap$Clase) <- c("Cabecera", "Corregimiento", "Rural")



dif_grap <- ggplot(infra_diferencia_grap, aes(x = value, fill = Clase))
infraest_diferencia_puntuacion <- dif_grap + geom_bar(position = "dodge")+ facet_wrap(. ~ variable, ncol = 3) +
  xlab("Diferencia entre la etapa de construcción y contingencia") + ylab("Número de hogares")

t.test(infra_diferencia_grap$value)



# Infraestructura vial

df_inf_vial <- df_hogares %>% select("P68A", "P68B","P68C", "P68D", "P68E", "P6801", "P6802")


# Distancia a donde se sacan los productos

for(i in 1:ncol(df_inf_vial)){
  df_inf_vial[, i] <- transformacion_faltantes(df_inf_vial[, i] %>% unlist() %>% as.numeric())
}

names(df_inf_vial) <- c("Predio", "Mercados_locales", "Mercados_regionales", "Mercados_nacionales", "Intermediarios", "Distancia", "Costo")

df_inf_donde_venta <- df_inf_vial[, 1:5]
df_inf_donde_venta[, "Tramo"] <- df_hogares$Nombre

df_inf_donde_venta <- melt(df_inf_donde_venta, id.vars = "Tramo") %>% data_frame()
df_inf_donde_venta$value <- as.factor(df_inf_donde_venta$value)
levels(df_inf_donde_venta$value) <- c("No", "Si")


resumen_ventas <- df_inf_donde_venta %>% group_by(variable, value) %>% 
  summarise(conteo = n()) %>% 
  mutate(porcentaje = conteo/sum(conteo)) %>% 
  arrange(porcentaje, variable)

names(resumen_ventas)[2] <- "Ventas"

resumen_ventas %>% ggplot(aes(x=reorder(variable, desc(porcentaje)), y = porcentaje, fill = Ventas))+geom_col()


resumen_ventas <- resumen_ventas %>% arrange(porcentaje, Ventas)


resumen_ventas["pos"] <- NA
for(i in 1:nrow(resumen_ventas)){
  x = resumen_ventas[i, "porcentaje"]
  act = resumen_ventas[i, "Ventas"]
  # resumen_ventas[i, "pos"] = ifelse(act == "Si", x*100/2, (1-x+x/2)*100)
}


act_g1 <- ggplot(resumen_ventas, aes(x = variable, y = porcentaje*100, fill = Ventas))
donde_vende_productos <- act_g1 + geom_bar(stat = "identity") +
  xlab("¿Dónde vende los productos?") +
  ylab("Porcentaje") +
  scale_fill_manual("Ventas", values = c("No" = "#8CBD0E", "Si" = "#005117")) + coord_flip()

# Distancia y precio

df_inf_vial <- df_inf_vial %>% 
  mutate(
    DistanciaIntervalos = case_when(
      is.na(Distancia) ~ NA_real_,
      Distancia >= 0 & Distancia < 1000 ~ 1,#"Entre 0 y 5mts",
      Distancia >= 1000 & Distancia < 10000 ~ 2,#"Entre 5mts y 1km",
      Distancia >= 10000 & Distancia < 100000 ~ 4,#"Entre 5km y 10km",
      Distancia >= 100000 & Distancia < 500000 ~ 5,#"Entre 10km y 50km",
      Distancia >= 500000 ~ 6,#"más de 50km",
      TRUE ~ NA_real_
    )
  )

df_inf_vial <- df_inf_vial %>% 
  mutate(
    CostoIntervalos = case_when(
      is.na(Costo) ~ NA_real_,
      Costo >= 0 & Costo < 10000 ~ 1,#"Entre 0 y 5mts",
      Costo >= 10000 & Costo < 100000 ~ 2,#"Entre 5km y 10km",
      Costo >= 100000 & Costo < 500000 ~ 3,#"Entre 10km y 50km",
      Costo >= 500000 & Costo < 1000000 ~ 4,#"Entre 10km y 50km",
      Costo >= 1000000 & Costo < 5000000 ~ 5,#"Entre 10km y 50km",
      Costo >= 5000000 ~ 6,#"más de 50km",
      TRUE ~ NA_real_
    )
  )

# Distancia

df_inf_vial$DistanciaIntervalos <- as.factor(df_inf_vial$DistanciaIntervalos)
levels(df_inf_vial$DistanciaIntervalos) <- c("Entre 0 y 1km",
                                         "Entre 1km y 10km",
                                         "Entre 10km y 100km",
                                         "Entre 100km y 500km",
                                         "Mas de 500km")

df_inf_vial[, "Tramo"] <- df_hogares[, "Nombre"]

bar_dista <- ggplot(df_inf_vial[!is.na(df_inf_vial$DistanciaIntervalos), ], aes(x = DistanciaIntervalos, fill = Tramo))
distancia_sacar_productos <- bar_dista + geom_bar(position = "dodge") + ylab("Número de hogares") + xlab("Distancia")

resumen_inf_vial <- df_inf_vial %>% group_by(Tramo, DistanciaIntervalos) %>% count() %>%  mutate(Porcentaje = n/nrow(df_inf_vial))

# Costo

df_inf_vial$CostoIntervalos <- as.factor(df_inf_vial$CostoIntervalos)
levels(df_inf_vial$CostoIntervalos) <- c("Entre $0 y $10.000",
                                         "Entre $10.000 y $100.000",
                                         "Entre $100.000 y $500.000",
                                         "Entre $500.000 y $1'000.000",
                                         "Entre $1'000.000 y $5'000.000",
                                         "Más de $5'000.000")

bar_costo <- ggplot(df_inf_vial[!is.na(df_inf_vial$CostoIntervalos), ], aes(x = CostoIntervalos, fill = Tramo))
costo_sacar_productos <- bar_costo + geom_bar(position = "dodge") + ylab("Número de hogares") + xlab("Costo de transporte") + coord_flip()

resumen_inf_vial_cost <- df_inf_vial %>% group_by(Tramo, CostoIntervalos) %>% count() %>%  mutate(Porcentaje = n/nrow(df_inf_vial))


## ¿Cómo saca los productos a la venta?

medio_trans <- df_hogares %>% select(starts_with("P68030"))
names(medio_trans) <- c("Carro Propio", "Carro alquilado", "Moto propia", "Mototaxi", "A pie", "Los compradores llegan", "Transporte público", "Otro", "De que forma", "Cuales")

medio_trans1 <- medio_trans[, 1:7]

for(i in 1:ncol(medio_trans1)){
  medio_trans1[, i] <- transformacion_faltantes(medio_trans1[, i] %>% unlist() %>% as.numeric())
}
medio_trans1[, "Tramo"] <- df_hogares[, "Nombre"]



medio_trans1 <- medio_trans1 %>% melt("Tramo") %>% data_frame()

medio_trans1$value <- as.factor(medio_trans1$value)
levels(medio_trans1$value) <- c("No", "Si")


resumen_mtrans <- medio_trans1 %>% group_by(variable, value) %>% 
  summarise(conteo = n()) %>% 
  mutate(porcentaje = conteo/sum(conteo)) %>% 
  arrange(porcentaje, variable)

names(resumen_mtrans)[2] <- "MedioTrans"

resumen_mtrans %>% ggplot(aes(x=reorder(variable, desc(porcentaje)), y = porcentaje, fill = MedioTrans))+geom_col()


resumen_mtrans <- resumen_mtrans %>% arrange(porcentaje, MedioTrans)


resumen_mtrans["pos"] <- NA
for(i in 1:nrow(resumen_mtrans)){
  x = resumen_mtrans[i, "porcentaje"]
  act = resumen_mtrans[i, "MedioTrans"]
  # resumen_ventas[i, "pos"] = ifelse(act == "Si", x*100/2, (1-x+x/2)*100)
}


act_g1 <- ggplot(resumen_mtrans, aes(x = variable, y = porcentaje*100, fill = MedioTrans))
como_transporta <- act_g1 + geom_bar(stat = "identity") +
  xlab("¿Cómo saca los productos a la venta?") +
  ylab("Porcentaje") +
  scale_fill_manual("Medio de transporte", values = c("No" = "#8CBD0E", "Si" = "#005117")) + coord_flip()


# Otros

# ¿ que actividad proviene el sustento del hogar?

wordcloud_graph(c(medio_trans$Otro, medio_trans$`De que forma`, medio_trans$Cuales))
# Infraestructura productiva

# Ganadería

# % de hogares con establos o instalaciones especiales, herramientas agricolas suficientes,

df_inf_prod <- df_hogares %>% select(c("P67", "P64", "P6401", "P6402", "P6405", "P6404", "P640401"))
names(df_inf_prod) <- c("Establos", "Herramientas Necesarias", "Animales", "Tractores", "Otras herramientas", "Herramientas_dificiles", "Porque")

df_inf_prod[, "Tramo"] <- df_hogares[, "Nombre"]
df_inf_prod_tabl <- df_inf_prod[, -c(6,7)] %>% melt(id.vars = "Tramo") %>% data_frame()
df_inf_prod_resumen <- df_inf_prod_tabl %>% group_by(Tramo, variable, value) %>% summarise(conteo = n()) %>% mutate(porcentaje = conteo/sum(conteo))

df_inf_prod[, "Tramo"] <- df_hogares[, "Nombre"]
df_inf_prod_tabl <- df_inf_prod[, -c(6,7)] %>% melt(id.vars = "Tramo") %>% data_frame()
df_inf_prod_resumen <- df_inf_prod_tabl %>% group_by(variable, value) %>% summarise(conteo = n()) %>% mutate(porcentaje = conteo/sum(conteo))

df_hogares %>% select(c("P67")) %>% table()
df_hogares %>% select(c("P64")) %>% table()




####

wordcloud_graph(df_inf_prod$Herramientas_dificiles)

wordcloud_graph <- function(variable, maxWords = 200){
  
  docs <- Corpus(VectorSource(c(variable) %>% na.omit() ))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("spanish"))
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)

  d$word <- gsub(",", "", d$word)
  # d$word <- gsub(".", "", d$word)
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=maxWords, random.order=TRUE, rot.per=0.8, 
            colors=brewer.pal(8, "Dark2"))
  
}


wordcloud_graph(df_inf_prod$Herramientas_dificiles)
wordcloud_graph(df_inf_prod$Porque, 200)
wordcloud_graph(df_hogares$P6405A, 150)

## Tipificación de la vivienda

df_vivienda <- df_hogares %>% select(starts_with("P60"))
names(df_vivienda) <- c("Vive en la misma area", "Donde vive", "Propia", "Termino de pagar", "Cuánto debe", "valor estimado",  "arrendada","desdeAño", "MontoArriendo", "OtraFormaTenencia", "cual", "rep")

resumen_cat <- function(x){
  a <- table(x)
  b <- round(a/sum(a)*100)
  return(data_frame(a, b))
}

df_vivienda$`Vive en la misma area` %>% resumen_cat()
df_vivienda$`Donde vive` %>% resumen_cat()
df_vivienda$Propia %>% resumen_cat()
df_vivienda$`Termino de pagar` %>% resumen_cat()
df_vivienda$arrendada %>% resumen_cat()
df_vivienda$OtraFormaTenencia %>% resumen_cat()
df_vivienda$cual %>% resumen_cat()

# Manejo de inundaciones

df_hogares %>% select(starts_with("P62"))

# Infraestructura productiva

df_asociaciones <- df_hogares %>% select(starts_with("P38C"))
df_hogares$P38 %>% resumen_cat()

df_asociaciones[df_hogares$P38 == 1,]
df_asociaciones["Tramo"] <- df_hogares$Nombre


medio_trans <- df_hogares %>% select(starts_with("P68030"))

graficos_binarios <- function(df_graph, nombres_gra, tramo = T, cat, xlab_grap, ylab_grap, df_orig){
  names(df_graph) <- nombres_gra
  
  for(i in 1:ncol(df_graph)){
    df_graph[, i] <- transformacion_faltantes(df_graph[, i] %>% unlist() %>% as.numeric())
  }
  
    df_graph[, "Tramo"] <- df_orig[, "Tramo"]
    df_graph <- df_graph %>% melt("Tramo") %>% data_frame()
    df_graph$value <- as.factor(df_graph$value)
    levels(df_graph$value) <- c("No", "Si")

    resumen_df_graph <- df_graph %>% group_by(variable, value) %>% 
      summarise(conteo = n()) %>% 
      mutate(porcentaje = conteo/sum(conteo)) %>% 
      arrange(porcentaje, variable)
    
    resumen_df_graph <- na.omit(resumen_df_graph)
  
  names(resumen_df_graph)[2] <- cat
  
  
  act_g1 <- ggplot(resumen_df_graph, aes(x = reorder(variable, -porcentaje), y = porcentaje*100, fill =!!sym(cat)))+
   geom_bar(stat = "identity") +
    xlab(xlab_grap) +
    ylab("Porcentaje") +
    scale_fill_manual(cat, values = c("No" = "#8CBD0E", "Si" = "#005117")) + coord_flip()
  
  return( list(resumen = resumen_df_graph, graph = act_g1))
}

df_orig_p <- df_asociaciones[df_hogares$P38 == 1,]
df_grap <- df_orig_p[, 1:6]
nombres <- c("Acceso a recursos económicos", "Acceso a insumos", "Acceso a centros de acopio", "Acceso a ayuda técnica", "Acceso a transporte", "Facilidades de comercializacion")
beneficios_aso_comun <- graficos_binarios(df_grap, nombres, cat = "Beneficios", "porcentaje", "Acceso a beneficios", df_orig = df_orig_p)


## asociaciones comunitarias

df_orig_p <- df_asociaciones[df_hogares$P39 == 1,]
df_grap <- df_orig_p[, 1:6]
nombres <- c("Acceso a recursos económicos", "Acceso a insumos", "Acceso a centros de acopio", "Acceso a ayuda técnica", "Acceso a transporte", "Facilidades de comercializacion")
beneficios_aso_com1 <- graficos_binarios(df_grap, nombres, cat = "Beneficios", "porcentaje", "Acceso a beneficios", df_orig = df_orig_p)

wordcloud_graph(c(df_hogares$P39C1, df_hogares$P39C2, df_hogares$P39C3), 300)


# Insumos de actividades productivas

df_hogares$P45 %>% table() %>% prop.table()

df_insumos <- df_hogares %>% select(starts_with("P45"))
df_insumos[, "Tramo"] <- df_hogares[, "Nombre"]

df_insumos_grap <- df_insumos[df_hogares$P45 == 1,]
df_grap <- df_insumos_grap[, -c(1, 11)]
nombres <- c("Semillas resistentes a sequias/inundaciones", "Semillas no resistentes", "Abono orgánico", "Abono químico", "Plaguicidas orgánicos", "Plaguicidas químicos", "Fungicidas orgánicos", "Fungicidas químicos", "Insumos para actividades no agrícolas")

insumosG <- graficos_binarios(df_grap, nombres, cat = "Insumo", "porcentaje", "Tipo de insumo", df_orig = df_insumos_grap)


# Acceso a las semillas

df_hogares$P46 %>% table() %>% prop.table()

df_semillas <- df_hogares %>% select(starts_with("P46"))
df_semillas[, "Tramo"] <- df_hogares[, "Nombre"]

df_semillas_grap <- df_semillas[df_hogares$P46 == 1,]
df_grap <- df_semillas_grap[, -c(1, 7, 8)]
nombres <- c("Trueque", "Intercambio por semillas", "Las produce usted mismo", "Las obtiene de vecinos o familiares", "intercambio por producto")

semillasG <- graficos_binarios(df_grap, nombres, cat = "Insumo", "porcentaje", "Tipo de insumo", df_orig = df_semillas_grap)
semillasG$resumen
semillasG$graph


# Acceso a las semillas resistentes

df_hogares$P47 %>% table() %>% prop.table()

df_semillas <- df_hogares %>% select(starts_with("P47"))
df_semillas[, "Tramo"] <- df_hogares[, "Nombre"]

df_semillas_grap <- df_semillas[df_hogares$P47 == 1,]
df_grap <- df_semillas_grap[, -c(1, 7, 8)]
nombres <- c("Trueque", "Intercambio por semillas", "Las produce usted mismo", "Las obtiene de vecinos o familiares", "intercambio por producto")

semillasRes <- graficos_binarios(df_grap, nombres, cat = "Insumo", "porcentaje", "Tipo de insumo", df_orig = df_semillas_grap)
semillasRes$resumen
semillasRes$graph

df_semillas$P471OTHER %>% table()


# Abonos vegetales

df_hogares$P48 %>% table() %>% prop.table()

df_abonos <- df_hogares %>% select(starts_with("P48"))
df_abonos[, "Tramo"] <- df_hogares[, "Nombre"]

df_abonos_grap <- df_abonos[df_hogares$P48 == 1,]
df_grap <- df_abonos_grap[, -c(1, 6)]
nombres <- c("Realiza su compostaje", "Otra manera", "Otro proceso")

abonosG <- graficos_binarios(df_grap, nombres, cat = "Abono", "porcentaje", "Tipo de abono", df_orig = df_abonos_grap)
semillasG$resumen
semillasG$graph

df_semillas$P471OTHER %>% table()

# Abonos animales

df_hogares$P49 %>% table() %>% prop.table()

df_abonos <- df_hogares %>% select(starts_with("P49"))
df_abonos[, "Tramo"] <- df_hogares[, "Nombre"]

df_abonos_grap <- df_abonos[df_hogares$P49 == 1,]

df_abonos_grap$P4901 %>% table() %>% prop.table()

df_grap <- df_abonos_grap[, -c(2, 3, 4)]
nombres <- c("Utiliza los residuos", "Realiza procesos de compostaje", "Otro proceso")

abonosAnim <- graficos_binarios(df_grap, nombres, cat = "Abono", "porcentaje", "Tipo de abono", df_orig = df_abonos_grap)

df_semillas$P471OTHER %>% table()

# Recursos del entorno

df_hogares$P501 %>% transformacion_faltantes() %>%  table() %>% prop.table()

df_entorno <- df_hogares %>% select(starts_with("P50"))
df_entornoG <- df_entorno[, c(1:4)]
nombres <- c("Lodo o sedimento", "Suelo de rio", "Otros que no compra", "Ninguno")

entornoG <- graficos_binarios(df_entornoG, nombres, cat = "Recursos del entorno", "porcentaje", "Tipo de recurso", df_orig = df_hogares)
entornoG$resumen
entornoG$graph


# Insumos malesas

df_hogares$P51 %>% table() %>% prop.table()
df_hogares$P5103 %>% table() %>% prop.table()
df_hogares$P5102 %>% table() %>% prop.table()
df_hogares$P5103 %>% table() %>% prop.table()

wordcloud_graph(c(df_hogares$P5101A, df_hogares$P5102A, df_hogares$P5103A))


# insumos plagas y enfermedades 
df_hogares$P52 %>% table() %>% prop.table()
df_plagas <- df_hogares[df_hogares$P52 == 1,] %>% select(starts_with("P52"))

df_plagas$P5201 %>% table()

return(list(infraest_puntuacion = infraest_puntuacion,
            infraest_diferencia_puntuacion = infraest_diferencia_puntuacion,
            donde_vende_productos = donde_vende_productos,
            distancia_sacar_productos = distancia_sacar_productos,
            resumen_inf_vial = resumen_inf_vial,
            costo_sacar_productos = costo_sacar_productos,
            resumen_inf_vial_cost = resumen_inf_vial_cost,
            como_transporta = como_transporta,
            beneficios_aso_comun = beneficios_aso_comun$graph,
            beneficios_aso_com1 = beneficios_aso_com1$graph,
            insumosG = insumosG$graph,
            semillasG = semillasG$graph,
            semillasRes = semillasRes$graph,
            abonosG = abonosG$graph,
            abonosAnim = abonosAnim$graph,
            entornoG = entornoG$graph))
}
# infraest_puntuacion, infraest_diferencia_puntuacion, donde_vende_productos, distancia_sacar_productos, resumen_inf_vial, costo_sacar_productos, resumen_inf_vial_cost, como_transporta, resumen_mediostrans, beneficios_aso_comun, beneficios_aso_com1, insumosG, semillasG, semillasRes, abonosG, abonosAnim, entornoG
# wordcloud_graph(df_inf_prod$Herramientas_dificiles)
# wordcloud_graph(c(medio_trans$Otro, medio_trans$`De que forma`, medio_trans$Cuales))
# wordcloud_graph(df_inf_prod$Herramientas_dificiles)
# wordcloud_graph(df_inf_prod$Porque, 200)
# wordcloud_graph(df_hogares$P6405A, 150)
# wordcloud_graph(c(df_hogares$P39C1, df_hogares$P39C2, df_hogares$P39C3), 300)
# wordcloud_graph(c(df_hogares$P5101A, df_hogares$P5102A, df_hogares$P5103A)) # Insumos malesas
