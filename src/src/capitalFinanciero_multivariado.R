
# Analisis mulivariado ----------------------------------------------------

df_multi_capital_financiero %>% data_frame() %>% View()
df_multi_capital_financiero[, "credito"] <- ifelse(df_multi_capital_financiero$acceso_credito == "No", 0, 1)
head(df_multi_capital_financiero)

# Analisis de las variables continuas

df_financiero_acp <- df_multi_capital_financiero[, c(1:6, 9, 10, 13, 15:24, 26:35)] %>% data_frame()
df_financiero_acp <- df_financiero_acp[, -15] # Se saca el distrito de riego porque solo tiene ceros
df_financiero_acp$Remesas[is.na(df_financiero_acp$Remesas)] <- 0
df_financiero_acp$Donaciones[is.infinite(df_financiero_acp$Donaciones)] <- 0
agrup_financiero <- hclust(dist(df_financiero_acp))
agrup_financiero %>% plot()


corrplot(cor(df_financiero_acp))

acp_financiero <- PCA(df_financiero_acp[, c(1:7)])
plot(acp_financiero)
acp_financier

# Relacion entre las variables de acceso al agua y tenencia de la tierra
## Acceso al agua

df_agua_multi <- df_multi_capital_financiero[, c(15:23)]
acp_agua <- PCA(df_agua_multi)
plot(acp_agua)
acp_agua$var
## Por contribución a los cuatro primeros ejes
df_agua_agrup <- data.frame( acueducto = df_agua_multi[, 7],
                             rios = apply(df_agua_multi[, c(1, 2)], 1, sum),
                             agua_lluvia = apply(df_agua_multi[, c(5, 8)], 1, sum),
                             ausente = apply(df_agua_multi[, c(3,4,9)], 1, sum))

## tenencia de la tierra
df_tierra_multi <- df_multi_capital_financiero[, c(26:34)]

acp_tierra <- PCA(df_tierra_multi)
plot(acp_tierra)
acp_tierra$eig
acp_tierra$var
df_tierra_agrup <- data.frame( propia = df_tierra_multi$Propia,
                               arriendo = df_tierra_multi$Arriendo,
                               otros = apply(df_tierra_multi[, c(3:9)], 1, sum))

# Análisis variables categóricas
# Total
df_multi_capital_financiero <- df_multi_capital_financiero %>%  mutate(
  total_cat = case_when(
    is.na(Total) ~ NA_real_,
    Total >= 0 & Total < 10000 ~ 1,#"Entre 0 y 5mts",
    Total >= 10000 & Total < 50000 ~ 3,#"Entre 5mts y 1km",
    Total >= 50000 & Total < 100000 ~ 4,#"Entre 5km y 10km",
    Total >= 100000 & Total < 200000 ~ 5,#"Entre 10km y 50km",
    Total >= 200000 & Total < 500000 ~ 6,#"Entre 10km y 50km",
    Total >= 500000 & Total < 1000000 ~ 7,#"Entre 10km y 50km",
    Total >= 1000000 & Total < 2000000 ~ 8,#"Entre 10km y 50km",
    Total >= 2000000 & Total < 5000000 ~ 9,#"Entre 10km y 50km",
    Total >= 500000 ~ 10,#"Entre 10km y 50km",
    TRUE ~ NA_real_
  )
)

df_multi_capital_financiero$total_cat <- as.factor(df_multi_capital_financiero$total_cat)
df_multi_capital_financiero$Clase <- as.factor(df_multi_capital_financiero$Clase)
levels(df_multi_capital_financiero$Clase) <- c("Cabecera", "Corregimiento", "Rural")

# 
df_multi_capital_financiero <- df_multi_capital_financiero %>%  mutate(
  autoconsumo_cat = case_when(
    is.na(autoconsumo) ~ NA_real_,
    autoconsumo >= 0 & autoconsumo < 10000 ~ 1,#"Entre 0 y 5mts",
    autoconsumo >= 10000 & autoconsumo < 50000 ~ 3,#"Entre 5mts y 1km",
    autoconsumo >= 50000 & autoconsumo < 100000 ~ 4,#"Entre 5km y 10km",
    autoconsumo >= 100000 & autoconsumo < 200000 ~ 5,#"Entre 10km y 50km",
    autoconsumo >= 200000 & autoconsumo < 500000 ~ 6,#"Entre 10km y 50km",
    autoconsumo >= 500000 & autoconsumo < 1000000 ~ 7,#"Entre 10km y 50km",
    autoconsumo >= 1000000 & autoconsumo < 2000000 ~ 8,#"Entre 10km y 50km",
    autoconsumo >= 2000000 & autoconsumo < 5000000 ~ 9,#"Entre 10km y 50km",
    autoconsumo >= 500000 ~ 10,#"Entre 10km y 50km",
    TRUE ~ NA_real_
  )
)

df_multi_capital_financiero$autoconsumo_cat <- as.factor(df_multi_capital_financiero$autoconsumo_cat)

df_acm <- cbind(df_multi_capital_financiero[, c(36,7,8, 37, 11, 12, 13)],
                df_agua_agrup,
                df_tierra_agrup)

for(i in 1:ncol(df_acm)){
  df_acm[, i] <- as.factor(df_acm[, i])
}

df_acm %>% data_frame()


acm<-dudi.acm(df_acm,nf=10,scannf = FALSE)
fviz_mca_var(acm,repel=T)+labs(title ="Nube de puntos de las variables del capital financiero")

fviz_contrib(acm,axes=c(1,2),choice = "var" )+labs(title = "Contribución de variables en el primer plano factorial")

acm$rank
acm$co

fviz_cos2(acm,axes=c(1,2),choice = "var" )+labs(title = "Cos2 de las variables  en el primer plano factorial")


df_capital_financiero["Tramo"] = as.factor(df_hogares$Nombre)

acmsup <- MCA(df_capital_financiero,quali.sup = 17,ncp=2,graph = TRUE)

coor_cat<-acmsup$quali.sup$coord
coor_cat
