# df_hogares %>% View()

source("ingresos.R", encoding = "UTF-8")

capitalFinanciero <- function(df_hogares){

# Ingresos ----------------------------------------------------------------


ingresos[, "autoconsumo"] <- autoconsumo

df_multi_capital_financiero <- ingresos

# Costos totales --------------------------------------------------------


df_multi_capital_financiero[, "gastos"] <- df_hogares$P59 %>% as.numeric()
df_multi_capital_financiero$gastos <- ifelse((is.na(df_multi_capital_financiero$gastos) |
                                               df_multi_capital_financiero$gastos == 99 |
                                               df_multi_capital_financiero$gastos == 9999), 561248, df_multi_capital_financiero$gastos)


# estimación del presupuesto mensual de gastos del hogar ----------------------------------------------------------

df_gastos <- df_hogares %>% select(
  c(
    CONSECUTIVO:NOMBRES, 
    starts_with("P59")
    )
  )
# 
# df_dicc %>% View()
# df_gastos %>% head() %>% View()

df_gastos$P59 <- df_multi_capital_financiero$gastos

ggplot(df_gastos, aes(as.numeric(P59)))+
  geom_histogram(col = "#005117", fill = "#8CBD0E")+
  labs(title = "Distribución de gastos mensuales del hogar",
       x="Cantidad (millones de pesos)",
       y="Frecuencia")

ggplot(df_gastos, aes(x="P59", y=P59))+geom_boxplot()

plt1 <- df_gastos %>% select(P59) %>%
  ggplot(aes(x="", y = P59)) +
  geom_boxplot(col = "#005117", fill = "#8CBD0E") + 
  coord_flip() +
  theme_classic() +
  xlab("") +
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt2 <- ggplot(df_gastos, aes(as.numeric(P59)))+
  geom_histogram(col = "#005117", fill = "#8CBD0E")+
  labs(x="Cantidad (millones de pesos)", y="Frecuencia")+
  scale_x_continuous(labels = scales::comma)+
  theme_classic()

gastos_hogares <- egg::ggarrange(plt2, plt1, heights = 2:1)



df_gastos <- df_gastos %>%  mutate(
  gastos_mes_hogar = case_when(
    is.na(P59) ~ NA_real_,
    P59 >= 0 & P59 < 100000 ~ 1,#"Entre 0 y 5mts",
    P59 >= 0 & P59 < 200000 ~ 2,#"Entre 0 y 5mts",
    P59 >= 200000 & P59 < 500000 ~ 3,#"Entre 5mts y 1km",
    P59 >= 500000 & P59 < 1000000 ~ 4,#"Entre 5km y 10km",
    P59 >= 1000000 & P59 < 2000000 ~ 5,#"Entre 10km y 50km",
    P59 >= 2000000 ~ 6,#"más de 50km",
    TRUE ~ NA_real_
  )
)

df_gastos$gastos_mes_hogar <- as.factor(df_gastos$gastos_mes_hogar)
levels(df_gastos$gastos_mes_hogar) <- c("Entre $0 y $100.000",
                                             "Entre $100.000\ny $200.000",
                                             "Entre $200.000\ny $500.000",
                                             "Entre $500.000\ny $1'000.000",
                                             "Entre $1'000.000\ny $2'000.000",
                                             "Mas de $2'000.000")

df_multi_capital_financiero[, "gastos_cat"] <- df_gastos$gastos_mes_hogar

gastos_mensuales <- df_gastos %>% 
  dplyr::filter(!is.na(gastos_mes_hogar)) %>% 
  ggplot(aes(gastos_mes_hogar))+
  geom_bar(col = "#005117", fill = "#8CBD0E")+
  geom_text(aes(label = paste0(round(..count..*100/length(df_gastos$gastos_mes_hogar)),"%"),digits=3), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black")+  
  labs(x="Gastos mensuales del hogar", y="Número de hogares")+
  scale_y_continuous(limits = c(0,3000))+
  coord_flip()

df_gastos$tramo <- df_hogares$Nombre

gastos_mensuales_tramo <- df_gastos %>% 
  dplyr::filter(!is.na(get("gastos_mes_hogar"))) %>% 
  ggplot(aes(gastos_mes_hogar, fill=tramo))+
  geom_bar(col = "#005117", position = "dodge")+
  geom_text( aes(label = paste0(round(..count..*100/length(df_gastos$gastos_mes_hogar),digits=1),"%")), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black",position =  position_dodge(.9), size=2.7)+  
  labs(x="Gastos mensuales del hogar", y="Número de hogares")+
  scale_y_continuous(limits = c(0,2100))+
  coord_flip()+
  scale_fill_manual(values=c("#005117", "#8CBD0E", "#5ABCB9"), 
                                     name="Tramo")+
  theme(panel.background = element_rect(fill = "gray97",
                                        colour = "gray97",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))


createBarPlot <- function( df_data, var_name)
{
  library(dplyr)
  library(ggplot2)
  plot <- df_data %>% 
    as_tibble() %>% 
    filter_(!is.na(df_data[var_name])) %>% 
    ggplot(aes_(as.name(var_name), fill=as.name("tramo")))+
    geom_bar(col = "#005117", position = "dodge")+
    geom_text( aes(label = paste0(round(..count..*100/length(as.name(var_name)),digits=1),"%")), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black",position =  position_dodge(.9), size=2.7)+  
    labs(x="Gastos mensuales del hogar", y="Número de hogares")+
    scale_y_continuous(limits = c(0,2100))+
    coord_flip()+
    scale_fill_manual(values=c("#005117", "#8CBD0E", "#5ABCB9"), 
                      name="Tramo")+
    theme(panel.background = element_rect(fill = "gray97",
                                          colour = "gray97",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"))
  return(plot)
}

createBarPlot(df_gastos, "gastos_mes_hogar")
  
Cstack_info()

create_barplot_categories <- function(df_datos, var_plot, var_cat, var_cat_name, xlab, ylab) {
  df_datos %>% 
    as_tibble() %>% 
    filter(!is.na(!!sym(var_plot))) %>% 
    ggplot(aes(x=!!sym(var_plot), fill=!!sym(var_cat)))+
      geom_bar(col = "#005117", position = "dodge")+
      geom_text(aes(label = paste0(round(..count..*100/length(df_datos[[var_plot]]),digits=1),"%")), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black",position =  position_dodge(.9), size=2.7)+  
      labs(x=xlab, y=ylab)+
      scale_y_continuous(limits = c(0,2100))+
      coord_flip()+
      scale_fill_manual(values=c("#005117", "#8CBD0E", "#5ABCB9"), 
                      name=var_cat_name)+
      theme(panel.background = element_rect(fill = "gray97",
                                          colour = "gray97",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"))
}

create_barplot_categories(df_gastos, "gastos_mes_hogar", "tramo", "Tramo", xlab = "Gastos mensuales del hogar", ylab = "Número de hogares")



# Perdidas ----------------------------------------------------------------

  
df_perdidas <- df_hogares %>% select(
  c(
    CONSECUTIVO:NOMBRES, 
    starts_with(c("P06","P07"))
  )
)

df_perdidas %>% head()# %>% View()


# Cuotas creditos de actividades productivas ------------------------------


df_cuotas <- df_hogares %>% select(
  c(
    CONSECUTIVO:NOMBRES, 
    starts_with(c("P55"))
  )
)

df_cuotas %>% head() #%>% View()

df_cuotas %>% 
  mutate(P55 = factor(P55, labels=c("Si",'No'))) %>% 
  ggplot(aes(P55))+
    geom_bar(col = "#005117")

table(df_cuotas$P55) %>% prop.table()

df_multi_capital_financiero[, "acceso_credito"] <- df_hogares$P55 %>% as.factor()
levels(df_multi_capital_financiero$acceso_credito) <- c("Si", "No")

## Interpretación: Aproximadamente el 82% de los hogares encuestados no tienen acceso a créditos.

df_cuotas %>% 
  filter(!is.na(P5501)) %>% 
  mutate(P5501 = factor(P5501, labels=c("Si",'No'))) %>% 
  ggplot(aes(P5501))+
  geom_bar(col = "#005117")

table(df_cuotas$P5501) %>% prop.table()

## Del 18% que sí tiene acceso a créditos, actualmente el 70% hace uso de algún crédito para desempeñar su actividad productiva


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
  
  
  act_g1 <- ggplot(resumen_df_graph, aes(x = variable, y = porcentaje*100, fill =!!sym(cat)))
  act_g1 + geom_bar(stat = "identity") +
    xlab(xlab_grap) +
    ylab("Porcentaje") +
    scale_fill_manual(cat, values = c("No" = "#8CBD0E", "Si" = "#005117")) + coord_flip()
}


table(df_cuotas$P5501)[[1]]

df_cuotas_bancos <- df_cuotas %>% 
  select(c("P550101A","P550101B","P550101C","P550101D")) %>% 
  `colnames<-`(c("Banco privado", "Estado", "Persona natural", "Almacen")) %>% 
  mutate_all(as.integer) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  pivot_longer(
    cols = everything(),
    names_to = 'acreedor',
    names_prefix = "acre",
    values_to = "num_deudores",
    values_drop_na = TRUE
  ) %>% 
  mutate(
    porcentaje=num_deudores/table(df_cuotas$P5501)[[1]]
  ) %>% 
  ggplot(aes(x=reorder(acreedor, -porcentaje), y=porcentaje))+
    geom_bar( stat = "identity", position = "fill", fill = "#8CBD0E")+
    geom_bar( stat = "identity", fill='#005117')+
    geom_text(aes(label = round(porcentaje,2)), stat = "identity", vjust = 0.5,hjust = 2, colour = "white",position =  position_dodge(.9), size=2.7)+  
    labs(x='Acreedor', y='Porcentaje')+
    scale_y_continuous(labels = scales::percent_format())+
    coord_flip()




create_barplot_binaries <- function(df_datos, var_plot, var_cat, var_cat_name, xlab, ylab) {
    
}


# Otros tipos de credito --------------------------------------------------

df_otros_creditos <- df_hogares %>% 
  select(starts_with("P550101E") | starts_with("P550101F")) 

wordcloud_graph(df_otros_creditos$P550101E...749)

## Los otros tipos de crédito usados por 25 hogares se refieren al Banco Agrario, 
## Cooperativas, fundaciones, cajas de compensación, microempresas, empresas 
## familiares, tarjetas y Bancamía 


# Actividades para las que se usa dando el crédito ------------------------

df_actividades_credito <- df_hogares %>% 
  select(starts_with("P5502")) 

para_que_credito <- df_actividades_credito %>% 
  select(P5502A:P5502F) %>% 
  `colnames<-`(c("Preparar terreno\nde siembra", "Cosechas", "Beneficio\n(alistamiento de productos)", "Transporte", "Almacenamiento (acopio)", "Inversiones en infraestructura")) %>% 
  mutate_all(as.integer) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  pivot_longer(
    cols = everything(),
    names_to = 'proceso',
    names_prefix = "proc",
    values_to = "num_creditos",
    values_drop_na = TRUE
  ) %>% 
  mutate(
    porcentaje=num_creditos/table(df_cuotas$P5501)[[1]]
  ) %>% 
  ggplot(aes(x=reorder(proceso, -porcentaje), y=porcentaje))+
  geom_bar( stat = "identity", position = "fill", fill = "#8CBD0E")+
  geom_bar( stat = "identity", fill='#005117')+
  geom_text(aes(label = round(porcentaje*100,1)), stat = "identity", vjust = 0.5,hjust = 1.3, colour = "white",position =  position_dodge(.9), size=2.7)+  
  labs(x='Proceso', y='Porcentaje')+
  scale_y_continuous(labels = scales::percent_format())+
  coord_flip()



## Aproximadamente el 40% de los créditos son usados para preparar el terreno de siembra,
## 36% se destina a actividades y procesos relacionados con cosecha

wordcloud_graph(df_actividades_credito$P5502OTHER)
df_actividades_credito$P5502OTHER %>% table() %>% sort()


## Otros usos que se dan a los creditos son hacer inversiones, compra de animales, herramientas y materias primas.


# Veces al año que se solicita crédito ------------------------------------

df_frec_credito <- df_hogares %>% select(P5503)

df_frec_credito$P5503 <- transformacion_faltantes(df_frec_credito$P5503)


plt1 <- df_frec_credito %>% select(P5503) %>%
  ggplot(aes(x="", y = P5503)) +
  geom_boxplot(col = "#005117", fill = "#8CBD0E") + 
  coord_flip() +
  theme_classic() +
  xlab("") +
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt2 <- ggplot(df_frec_credito, aes(as.numeric(P5503)))+
  geom_histogram(col = "#005117", fill = "#8CBD0E")+
  labs(x="Cantidad de veces en el año", y="Frecuencia")+
  scale_x_continuous(labels = scales::comma)+
  theme_classic()

veces_año_credito <- egg::ggarrange(plt2, plt1, heights = 2:1)


df_frec_credito %>%
  ggplot(aes(P5503))+
    geom_bar()

table(df_frec_credito$P5503) %>% prop.table()

df_frec_credito %>% filter(P5503 != 0) %>%  table() %>% prop.table()

## Aproximadamente el 88% de los hogares encuestados no solicita créditos en el año.
## Del 12% restante, el 78% pide crédito una vez al año, entre dos y tres veces al 
## año el 19%, y el 3% restante solicita créditos más de tres veces en el año 


# Logra cubrir los gastos del crédito -------------------------------------

table(df_hogares$P550301) %>% prop.table()

## Del total de personas que solicita un crédito, aproximadamente el 77% reporta lograr
## cubrir los gastos del crédito con su producción, mientras que el 23% restante no
## lo logra.


# Cómo se las arreglan para cubrir los créditos ---------------------------

wordcloud_graph(df_hogares$P550301A)
df_hogares$P550301A %>% table() %>% sort()


## Entre las formas empleadas por los hogares para cubrir los créditos, destacan
## los pagos atrasados, refinanciaientos, otrs préstamos, ventas familiares y datacrédito

# Hogares que estiman los costos > 0
df_costos_inf <- df_hogares[, c("P63", "Nombre")]
df_costos_inf_grap <- df_costos_inf[(df_costos_inf$P63 != 0 & !is.na(df_costos_inf$P63) & df_costos_inf$P63 != "99" & df_costos_inf$P63 != "9999"), c("P63", "Nombre")]

(df_hogares$P63==0) %>% table() %>% prop.table()

df_costos_inf_grap$P63 %>% as.numeric() %>% summary()
df_costos_inf_grap$P63 <- as.numeric(df_costos_inf_grap$P63)

df_costos_inf_grap <- df_costos_inf_grap %>%  mutate(
  costos_estimados = case_when(
    is.na(P63) ~ NA_real_,
    P63 >= 0 & P63 < 100000 ~ 1,#"Entre 0 y 5mts",
    P63 >= 100000 & P63 < 500000 ~ 3,#"Entre 5mts y 1km",
    P63 >= 500000 & P63 < 1000000 ~ 4,#"Entre 5km y 10km",
    P63 >= 1000000 & P63 < 5000000 ~ 5,#"Entre 10km y 50km",
    P63 >= 5000000 & P63 < 10000000 ~ 6,#"Entre 10km y 50km",
    P63 >= 10000000 ~ 7,#"Entre 10km y 50km",
    TRUE ~ NA_real_
  )
)

df_multi_capital_financiero[, "estimacion_mejoras"] <- ifelse(is.na(df_hogares$P63)|
                                                                df_hogares$P63 == 99 |
                                                                df_hogares$P63 == 999, 7080503, df_hogares$P63) %>% as.numeric()

df_multi_capital_financiero <- df_multi_capital_financiero %>%  mutate(
  estimacion_mejoras_cat = case_when(
    is.na(estimacion_mejoras) ~ NA_real_,
    estimacion_mejoras >= 0 & estimacion_mejoras < 100000 ~ 1,#"Entre 0 y 5mts",
    estimacion_mejoras >= 100000 & estimacion_mejoras < 500000 ~ 3,#"Entre 5mts y 1km",
    estimacion_mejoras >= 500000 & estimacion_mejoras < 1000000 ~ 4,#"Entre 5km y 10km",
    estimacion_mejoras >= 1000000 & estimacion_mejoras < 5000000 ~ 5,#"Entre 10km y 50km",
    estimacion_mejoras >= 5000000 & estimacion_mejoras < 10000000 ~ 6,#"Entre 10km y 50km",
    estimacion_mejoras >= 10000000 ~ 7,#"Entre 10km y 50km",
    TRUE ~ NA_real_
  )
)



df_multi_capital_financiero$estimacion_mejoras_cat <- as.factor(df_multi_capital_financiero$estimacion_mejoras_cat)
levels(df_multi_capital_financiero$estimacion_mejoras_cat) <- c("Entre 0 y $100.000",
                                                 "Entre $100.000 y $500.000",
                                                 "Entre $500.000 y $1'000.000",
                                                 "Entre $1'000.000 y $5'000.000",
                                                 "Entre $5'000.000 y $10'000.000",
                                                 "Mas de $10'000.000")


df_costos_inf_grap$costos_estimados <- as.factor(df_costos_inf_grap$costos_estimados)
levels(df_costos_inf_grap$costos_estimados) <- c("Entre 0 y $100.000",
                                                 "Entre $100.000 y $500.000",
                                                 "Entre $500.000 y $1'000.000",
                                                 "Entre $1'000.000 y $5'000.000",
                                                 "Entre $5'000.000 y $10'000.000",
                                                 "Mas de $10'000.000")
names(df_costos_inf_grap)[2] <- "tramo"

costos_estimados <- create_barplot_tramo(df_costos_inf_grap, "costos_estimados", xlab = "Costos estimados", ylab = "Número de hogares")



## Acceso al agua

df_agua <- df_hogares %>% select(starts_with("P44B"))
df_agua[,"Tramo"] <- df_hogares[, "Nombre"]
nombres_ag <- c("Rio, quebrada, arroyo",
                "Pozos, aljibes o jagüey",
                "Lago o laguna",
                "Ciénaga o humedal",
                "Embalse o represa",
                "Distrito de riego",
                "Acueducto",
                "Agua lluvia",
                "No tiene acceso")

df_agua_multi <- df_agua[, -10]
names(df_agua_multi) <- nombres_ag
df_multi_capital_financiero <- cbind(df_multi_capital_financiero, transformar_si_no_base(df_agua_multi))

agua_resumen <- graficos_binarios(df_agua[,-10], nombres_ag, T, cat = "Proviene", "Origen del agua para consumo doméstico", "Número de hogares", df_agua)

df_agua <- df_hogares %>% select(starts_with("P44C"))
df_agua[,"Tramo"] <- df_hogares[, "Nombre"]
nombres_ag <- c("Una zanja",
                "Manguera",
                "Tubería individual",
                "Manguera o tubería comunitaria")

agua_resumen <- graficos_binarios(df_agua[,-c(5,6)], nombres_ag, T, cat = "Proviene", "Cómo conduce el agua hacia sus parcelas", "Número de hogares", df_agua)

df_agua$P44COTHER %>% table() %>% sort()

df_agua$P44COTHER %>% wordcloud_graph()


# Acceso al agua produccion

df_agua <- df_hogares %>% select(starts_with("P44A"))
df_agua[,"Tramo"] <- df_hogares[, "Nombre"]
nombres_ag <- c("Rio, quebrada, arroyo",
                "Pozos, aljibes o jagüey",
                "Lago o laguna",
                "Ciénaga o humedal",
                "Embalse o represa",
                "Distrito de riego",
                "Acueducto",
                "Agua lluvia",
                "No tiene acceso")
agua_produccion_resumen <- graficos_binarios(df_agua[,-10], nombres_ag, T, cat = "Proviene", "Origen del agua para la producción agropecuaria", "Número de hogares", df_agua)


# Pagos pendientes

df_hogares$P6004 %>% table() %>% prop.table()
df_hogares$P6004B[df_hogares$P6004==1 & df_hogares$P6004B!= "99" & df_hogares$P6004B != "9999"] %>% na.omit() %>% as.numeric() %>% summary()

# Arriendos

df_costos_arr <- df_hogares[, c("P6004B", "Nombre")]
# Si no tiene arriendo, no paga nada, así tenemos las info de las dos variables en una sola
df_multi_capital_financiero[, "costo_arriendo"] <- ifelse((is.na(df_costos_arr$P6004B) | df_costos_arr$P6004B == "99" | df_costos_arr$P6004B == "9999"), 0, df_costos_arr$P6004B) %>% as.numeric()

df_costos_arr_gr <- df_costos_arr[(df_costos_arr$P6004B != 0 & !is.na(df_costos_arr$P6004B) & df_costos_arr$P6004B != "99" & df_costos_arr$P6004B != "9999"), c("P6004B", "Nombre")]

(df_hogares$P6004B==0) %>% table() %>% prop.table()

df_costos_arr_gr$P6004B %>% as.numeric() %>% summary()
df_costos_arr_gr$P6004B <- as.numeric(df_costos_arr_gr$P6004B)

df_costos_arr_gr <- df_costos_arr_gr %>%  mutate(
  costos_estimados = case_when(
    is.na(P6004B) ~ NA_real_,
    P6004B >= 0 & P6004B < 10000 ~ 1,#"Entre 0 y 5mts",
    P6004B >= 10000 & P6004B < 50000 ~ 3,#"Entre 5mts y 1km",
    P6004B >= 50000 & P6004B < 100000 ~ 4,#"Entre 5km y 10km",
    P6004B >= 100000 & P6004B < 200000 ~ 5,#"Entre 10km y 50km",
    P6004B >= 200000 & P6004B < 500000 ~ 6,#"Entre 10km y 50km",
    P6004B >= 500000 ~ 7,#"Entre 10km y 50km",
    TRUE ~ NA_real_
  )
)

df_costos_arr_gr$costos_estimados <- as.factor(df_costos_arr_gr$costos_estimados)
levels(df_costos_arr_gr$costos_estimados) <- c("Entre 0\n y $10.000",
                                                 "Entre $10.000\n y $50.000",
                                                 "Entre $50.000\n y $100.000",
                                                 "Entre $100.000\n y $200.000",
                                                 "Entre $300.000\n y $500.000",
                                                 "Mas de $500.000")

temp <- df_multi_capital_financiero
df_multi_capital_financiero <- df_multi_capital_financiero %>%  mutate(
  costo_arriendo_cat = case_when(
    is.na(costo_arriendo) ~ NA_real_,
    costo_arriendo >= 0 & costo_arriendo < 10000 ~ 1,#"Entre 0 y 5mts",
    costo_arriendo >= 10000 & costo_arriendo < 50000 ~ 3,#"Entre 5mts y 1km",
    costo_arriendo >= 50000 & costo_arriendo < 100000 ~ 4,#"Entre 5km y 10km",
    costo_arriendo >= 100000 & costo_arriendo < 200000 ~ 5,#"Entre 10km y 50km",
    costo_arriendo >= 200000 & costo_arriendo < 500000 ~ 6,#"Entre 10km y 50km",
    costo_arriendo >= 500000 ~ 7,#"Entre 10km y 50km",
    TRUE ~ NA_real_
  )
)

df_multi_capital_financiero$costo_arriendo_cat <- as.factor(df_multi_capital_financiero$costo_arriendo_cat)
levels(df_multi_capital_financiero$costo_arriendo_cat) <- c("Entre 0\n y $10.000",
                                               "Entre $10.000\n y $50.000",
                                               "Entre $50.000\n y $100.000",
                                               "Entre $100.000\n y $200.000",
                                               "Entre $300.000\n y $500.000",
                                               "Mas de $500.000")


names(df_costos_arr_gr)[2] <- "tramo"

arriendo <- create_barplot_tramo(df_costos_arr_gr, "costos_estimados", xlab = "Monto del arriendo", ylab = "Número de hogares")

# Mano de obra

df_mano_obra <- df_hogares %>% select("P20A", "P02B")

# actividades para las que usa la mano de obra

mo <- df_hogares %>% select(starts_with("P21"), "Nombre")

mo$P21A1 %>% table() %>% sort()

wordcloud_graph(c(mo$P21A1, mo$P21A2, mo$P21A3))
wordcloud_graph(c(mo$P21B1, mo$P21B2, mo$P21B3))


c(mo$P21A1, mo$P21A2, mo$P21A3) %>% table() %>% sort()
c(mo$P21B1, mo$P21B2, mo$P21B3) %>% table() %>% sort()


df_hogares$P22A %>% na.omit() %>% as.numeric() %>% summary()
df_hogares$P23A %>% na.omit() %>% as.numeric() %>% summary()

# Acceso a la tierra

df_acceso <- df_hogares %>% select(starts_with("P42"), "Tramo")
nombres_acceso <- c("Propia",
                    "Arriendo",
                    "Aparcería",
                    "Usufructo",
                    "Comodato",
                    "Ocupación",
                    "Propiedad",
                    "Adjudicata",
                    "Otra forma")
df_grap <- df_acceso[, c(1:9)]

df_acc_multi <- transformar_si_no_base(df_grap)
names(df_acc_multi) <- nombres_acceso

## Mostrar por tramo***
acceso_resumen <- graficos_binarios(df_grap, nombres_acceso, T, cat = "Proviene", "Acceso a la tierra", "Número de hogares", df_acceso)

df_hogares$P4201 %>% table() %>% sort()

df_multi_capital_financiero <- cbind(df_multi_capital_financiero, df_acc_multi)

return(list(origen_igresos = origen_igresos,
            origen_donaciones = origen_donaciones,
            origen_remesas = origen_remesas,
            ingresos_hogar = ingresos_hogar,
            Ingresos_tramo = Ingresos_tramo,
            autoconsumo_actividades = autoconsumo_actividades,
            actividades_porcentaje_medios_vida = actividades_porcentaje_medios_vida, 
            porcentaje_autoc_actividada = porcentaje_autoc_actividada,
            gastos_hogares = gastos_hogares,
            gastos_mensuales = gastos_mensuales,
            gastos_mensuales_tramo = gastos_mensuales_tramo,
            df_cuotas_bancos = df_cuotas_bancos,
            para_que_credito = para_que_credito,
            veces_año_credito = veces_año_credito,
            costos_estimados = costos_estimados,
            agua_resumen = agua_resumen,
            agua_produccion_resumen = agua_produccion_resumen,
            arriendo = arriendo,
            acceso_resumen = acceso_resumen))

# Ingresos
# wordcloud_graph(df_hogares$P5801, 100)
# Capital financiero
# gastos_hogares, gastos_mensuales, gastos_mensuales_tramo, df_cuotas_bancos, para_que_credito, veces_año_credito, costos_estimados, agua_resumen, agua_produccion_resumen, arriendo, acceso_resumen
#wordcloud_graph(df_otros_creditos$P550101E...749) # Otros creditos
# wordcloud_graph(df_actividades_credito$P5502OTHER)
# wordcloud_graph(df_hogares$P550301A)
# df_agua$P44COTHER %>% wordcloud_graph()
# wordcloud_graph(c(mo$P21A1, mo$P21A2, mo$P21A3)) # Mano de obra
# wordcloud_graph(c(mo$P21B1, mo$P21B2, mo$P21B3)) # Actividades para la mano de obra
}

