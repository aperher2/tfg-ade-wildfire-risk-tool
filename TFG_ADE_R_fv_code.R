
#CÓDIGO R TFG ADE: Desarrollo de una herramienta estadística para la gestión 
# y prevención del riesgo de incendios forestales en la Comunidad Valenciana

#Paquetes a instalar
install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

install.packages("glmnet")
library(glmnet)

install.packages("corrplot")
library(corrplot)

install.packages("pROC")
library(pROC)

install.packages("mgcv")
library(mgcv)

install.packages("ResourceSelection")
library(ResourceSelection)

install.packages("car")
library(car)

install.packages("spdep")
library(spdep)

install.packages("sf")
library(sf)

install.packages("DHARMa")
library(DHARMa)

install.packages("mgcViz")
library("mgcViz")

install.packages("glmmTMB")
library(glmmTMB)

install.packages("performance")
library(performance)

install.packages("terra")
library(terra)

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#ANÁLISIS EXPLORATORIO DE DATOS (EDA)

file.choose()
df_descriptive_analysis <-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\dataset_final\\Resultado_final\\forest_fire.csv" , header= TRUE , sep=",")
View(df_descriptive_analysis)

#Histograma superficie en escala generaL (hectáreas) 
hist(df_descriptive_analysis$Area_ha_EGIF,
     breaks = 200,
     col ="firebrick2" ,
     border = "white",
     main="Histograma de Superficie Incendiada",
     xlab = "Superficie", 
     ylab="Frecuencia")

#Histograma superficie en escala logarítmica
hist(log(df_descriptive_analysis$Area_ha_EGIF),
     breaks = 200,
     col = "firebrick2",
     border = "white",
     main = "Histograma de log(Superficie Incendiada)",
     xlab = "log(Superficie)",
     ylab= "Frecuencia")



#OBTENCIÓN DEL DIAGRAMA DE PARETO
#Ordeno los incendios por superficie afectada (descendente)
df_Pareto <- df_descriptive_analysis[order(df_descriptive_analysis$Area_ha_EGIF, decreasing = TRUE), ]

#Calculo el porcentaje y el porcentaje acumulado 
df_Pareto$perc <- df_Pareto$Area_ha_EGIF / sum(df_Pareto$Area_ha_EGIF)
df_Pareto$cum_perc <- cumsum(df_Pareto$perc)
df_Pareto$cum_perc

#Crear índice de incendios ordenados
df_Pareto$incendio_id <- seq_len(nrow(df_Pareto))

# Creo el Diagrama de Pareto
ggplot(df_Pareto, aes(x = incendio_id)) + geom_col(aes(y = Area_ha_EGIF)) +
  geom_line(aes(y = cum_perc * max(Area_ha_EGIF)),color = "red", size = 1) +
  geom_hline(yintercept = 0.8 * max(df_Pareto$Area_ha_EGIF), linetype = "dashed", color = "blue") +
  scale_y_continuous(name = "Superficie afectada (ha)",sec.axis = sec_axis(~ ./max(df_Pareto$Area_ha_EGIF),
                                                                           name = "Porcentaje acumulado")) +
  labs(title = "Diagrama de Pareto de Siniestros (Área quemada en ha)",
       x = "Siniestros (ordenados por área afectada)") +
  theme_minimal()

#20 incendios son responsables del 80% de la superficie incendiada
length(which(df_Pareto$cum_perc <=0.8057250))


#INTRODUCCIÓN
#Muestra total
summary(df_descriptive_analysis$Area_ha_EGIF)

#Stage 1: incendios vs conatos 
table(df_descriptive_analysis$es_incendio)

#Stage 2: siniestros que son incendios (superficie >= 1 ha)
summary(df_descriptive_analysis$Area_ha_EGIF[df_descriptive_analysisArea_ha_EGIF >= 1])


#ANÁLISIS DE CAUSALIDAD DE LAS IGNICIONES
table(df_descriptive_analysis$Causa,df_descriptive_analysis$es_incendio)

tipo_causa<-c("Desconocida","Rayo","Humanos")
frec_abs_causa<-c(278,1617,5160)
frec_rel_causa<-frec_abs_causa/7055
sum(round(frec_rel_causa*100,2))
df_causalidad<-data.frame(tipo_causa,frec_abs_causa, frec_rel_causa)
df_causalidad

#Diagrama circular
pie(df_causalidad$frec_abs_causa, labels= c("Desconocida (3.94%)","Rayo (22.92%)","Humanos (73.14%)"),
    main = c("Causalidad de incendios"))



#ANÁLISIS DE DISTRIBUCIÓN TEMPORAL 
#ANUAL
#Distribución anual del tipo de siniestros
table(df_descriptive_analysis$es_incendio,df_descriptive_analysis$Campania)


conatos_campania <- table(df_descriptive_analysis$Campania[df_descriptive_analysis$es_incendio == 0])
incendios_campania <- table(df_descriptive_analysis$Campania[df_descriptive_analysis$es_incendio == 1])
totales_campania <- conatos_campania + incendios_campania

matriz_siniestros_campania <- rbind(totales_campania, conatos_campania, incendios_campania)

barplot(matriz_siniestros_campania, beside = TRUE, col = c("blue", "gold", "firebrick2"),
        xlab = "Campaña",ylab = "Número de siniestros")

legend("topright",legend = c("Total","Conatos","Incendios"),fill = c("blue","gold","firebrick2"),
       horiz = TRUE, cex=0.7)



#Distribución anual del tipo de siniestros
agregado_superficie_campania<-aggregate(Area_ha_EGIF ~ Campania, data = df_descriptive_analysis, sum)
agregado_superficie_campania
barplot(agregado_superficie_campania$Area_ha_EGIF, names.arg=agregado_superficie_campania$Campania,
        xlab= "Campaña", ylab="Superficie(ha)", col="firebrick2", space=0.5)


#MENSUAL
#Distribución mensual del tipo de siniestros
table(df_descriptive_analysis$es_incendio,df_descriptive_analysis$Mes)

conatos_mes <- table(df_descriptive_analysis$Mes[df_descriptive_analysis$es_incendio == 0])
incendios_mes <- table(df_descriptive_analysis$Mes[df_descriptive_analysis$es_incendio == 1])
totales_mes <- conatos_mes + incendios_mes

matriz_siniestros_mes <- rbind(totales_mes, conatos_mes, incendios_mes)

barplot(matriz_siniestros_mes, beside = TRUE, col = c("blue", "gold", "firebrick2"),
        xlab = "Mes", ylab = "Número de siniestros")

legend("topright", legend = c("Total","Conatos","Incendios"),
       fill = c("blue","gold","firebrick2"), cex =1)


#Distribución mensual de la superficie afectada
agregado_superficie_mes <- aggregate(Area_ha_EGIF ~ Mes,data = df_descriptive_analysis,
                                     sum)

agregado_superficie_mes

barplot(agregado_superficie_mes$Area_ha_EGIF,names.arg = agregado_superficie_mes$Mes,
        xlab = "Mes", ylab = "Superficie (ha)", col = "firebrick2", width= 0.8, space=0.5)


#ANÁLISIS ESPACIAL: Se mapea en QGIS
#Comarcas
#Para el número de siniestros 
table(df_descriptive_analysis$ComarcaIsl,df_descriptive_analysis$es_incendio)

#Para la superficie 
agregado_superficie_comarcas<- aggregate(Area_ha_EGIF ~ ComarcaIsl, data = df_descriptive_analysis, sum)
agregado_superficie_comarcas 


#ANÁLISIS DEL TIEMPO DE RESPUESTA INICIAL (TRI)  
#TRI está en minutos. Lo he calculado como la diferencia entre la fecha de llegada del 
#primer medio terrestre - la fecha de detección del incendio en Excel que permite operaciones entre fechas.

summary(df_descriptive_analysis$TRI)
df_descriptive_analysis[!is.na(df_descriptive_analysis$TRI) & df_descriptive_analysis$TRI>500000,]
#La fecha de llegada del primer medio terrestre para el Numero de Parte 2005120078 está mal registrada

df_descriptive_analysis_para_TRI_v1<-df_descriptive_analysis[!is.na(df_descriptive_analysis$TRI) & df_descriptive_analysis$TRI<500000,]
summary(df_descriptive_analysis_para_TRI_v1$TRI)


df_descriptive_analysis_para_TRI_v1[df_descriptive_analysis_para_TRI_v1$TRI>2890,]
#La fecha de llegada del primer medio terrestre para el Numero de Parte 2015030055 está mal registrada

df_descriptive_analysis_para_TRI_v2<-df_descriptive_analysis_para_TRI_v1[df_descriptive_analysis_para_TRI_v1$TRI<2890,]
summary(df_descriptive_analysis_para_TRI_v2$TRI)

df_descriptive_analysis_para_TRI_v2[df_descriptive_analysis_para_TRI_v2$TRI>1900,]

#En muchos casos existen muchos datos de la fecha de llegada del primer medio terrestre
#que han sido mal registrados, ya que no tienen sentido si los comparas con el área quemada.
#Esto imposibilita realizar un análisis de TRI vs Area_ha_EGIF.



#ANÁLISIS DESCRIPTIVO 
#STAGE 1
forest_fire<-read.csv( "C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\dataset_final\\Resultado_final\\forest_fire.csv", header= TRUE , sep=",")


#Resumen estadístico covariables principales Stage 1
vars_s1 <- forest_fire[ , c("Area_ha_EGIF", "Altitud","Pendiente","FCC","prox_caminos",
                            "prox_carreteras","prox_cortafuegos","prox_GR","prox_SH","proxx_NB",
                            "prox_puntosagua","dens_poblacional","prox_observatorios","prox_depositos","OM_temp_media_2m","OM_humedad_rel_media_2m",
                            "OM_viento_vel_media_10m","OM_lluvia_total","OM_presion_media_mls","OM_cobertura_nubosa_media","OM_humedad_suelo_media_28_100cm",
                            "delta_viento_ladera")]

resumen_estadisticos_s1 <- data.frame(Min = sapply(vars_s1, min),
                                      Q1 = sapply(vars_s1, quantile, probs = 0.25),
                                      Mean = sapply(vars_s1, mean),
                                      Median = sapply(vars_s1, median),
                                      Q3 = sapply(vars_s1, quantile, probs = 0.75),
                                      Max = sapply(vars_s1, max),
                                      SD = sapply(vars_s1, sd))

round(resumen_estadisticos_s1,2)

#Relación entre las covariables principales y la variable dependiente del Stage 1.
forest_fire$es_incendio


#Para las variables continuas, se analizan los boxplots
colores <- c("gold", "firebrick2")

par(mfrow = c(2, 3),
    mar = c(3,3,2,1),
    cex.main = 0.9,
    cex.lab = 0.8,
    cex.axis = 0.8)

boxplot(Altitud ~ es_incendio,data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Altitud (m)",main = "Altitud (m)")

boxplot(Pendiente ~ es_incendio,data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Pendiente (%)", main = "Pendiente (%)")

boxplot(FCC ~ es_incendio, data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"), xlab = "Tipo de evento",
        ylab = "FCC (%)", main = "FCC (%)")

boxplot(prox_caminos ~ es_incendio, data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"), xlab = "Tipo de evento",
        ylab = "Proximidad a caminos (km)",
        main = "Proximidad a caminos (km)")

boxplot(prox_carreteras ~ es_incendio, data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Proximidad a carreteras (km)",
        main = "Proximidad a carreteras (km)")

boxplot(prox_observatorios ~ es_incendio, data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Proximidad a observatorios (km)",
        main = "Proximidad a observatorios (km)")

boxplot(prox_cortafuegos ~ es_incendio, data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Proximidad a cortafuegos (km)",
        main = "Proximidad a cortafuegos (km)")

boxplot(dens_poblacional ~ es_incendio,data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Densidad poblacional (hab/km²)",
        main = "Densidad poblacional (hab/km²)")

boxplot(OM_temp_media_2m ~ es_incendio, data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Temperatura media (ºC)",
        main = "Temperatura media (ºC)")

boxplot(OM_humedad_rel_media_2m ~ es_incendio,data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Humedad relativa media(%)",
        main = "Humedad relativa media (%)")

boxplot(OM_viento_vel_media_10m ~ es_incendio,data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Velocidad del viento (km/h)",
        main = "Velocidad del viento (km/h)")

boxplot(OM_lluvia_total ~ es_incendio,data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"),xlab = "Tipo de evento",
        ylab = "Lluvia total (mm)",
        main = "Lluvia total (mm)")

boxplot(OM_cobertura_nubosa_media ~ es_incendio,data = forest_fire, 
        col = colores,
        names = c("Conato", "Incendio ≥1ha"),
        xlab = "Tipo de evento", ylab = "Cobertura nubosa media (%)",
        main = "Cobertura nubosa media (%)")

boxplot(OM_humedad_suelo_media_28_100cm ~ es_incendio, data = forest_fire, 
        col = colores, names = c("Conato", "Incendio ≥1ha"),
        xlab = "Tipo de evento",ylab = "Humedad del suelo (28–100 cm) (%)",
        main = "Humedad del suelo (28–100 cm)(%)")

boxplot(delta_viento_ladera ~ es_incendio, data = forest_fire, col = colores,
        names = c("Conato", "Incendio ≥1ha"), xlab = "Tipo de evento",
        ylab = "Delta viento ladera (º)",
        main = "Diferencia angular mínima entre la dirección del viento y la orientación de la ladera")

dev.off()

#Para las variables categóricas.
#Nota: Para los modelos de Scott & Burgan se hace una reclasificación
table(forest_fire$es_incendio)
table(forest_fire$es_incendio,forest_fire$Provincia)
table(forest_fire$es_incendio, forest_fire$burned_before)
table(forest_fire$es_incendio, forest_fire$MC_SB_grupo)


#STAGE 2 
#Creamos dataset solo con incendio (>=1 ha)
dataset_superficie<-subset(forest_fire, forest_fire$Area_ha_EGIF>= 1)


# Covariables principales Stage 2
dataset_superficie_num <- dataset_superficie[, c(
  "Area_ha_EGIF","Altitud", "Pendiente", "FCC",
  "prox_caminos", "prox_carreteras", "prox_cortafuegos", "prox_GR", "prox_SH",
  "proxx_NB", "prox_puntosagua", "dens_poblacional", "prox_observatorios",
  "prox_depositos", "OM_temp_media_2m", "OM_humedad_rel_media_2m",
  "OM_viento_vel_media_10m", "OM_lluvia_total", "OM_presion_media_mls",
  "OM_cobertura_nubosa_media", "OM_humedad_suelo_media_28_100cm",
  "delta_viento_ladera")]


# Correlación con log(Area_ha_EGIF)
cor_logarea <- cor( dataset_superficie_num,
                    log(dataset_superficie_num$Area_ha_EGIF),
                    use = "pairwise.complete.obs")

# Data frame de correlaciones
cor_logarea_df <- data.frame(variable = rownames(cor_logarea),
                             cor_logarea = as.vector(cor_logarea))

# Quitar autocorrelación
cor_logarea_df <- cor_logarea_df[cor_logarea_df$variable != "Area_ha_EGIF",]

# Ordenar
cor_logarea_df <- cor_logarea_df[order(cor_logarea_df$cor_logarea),]

# Gráfico
par(mar = c(8,4,4,2))

barplot(cor_logarea_df$cor_logarea,
        names.arg = cor_logarea_df$variable,
        las = 2,              
        cex.names = 0.45,      
        mgp = c(3, 0.3, 0),   
        col = "lightblue",
        main = "Correlación de Pearson con log(Area_ha_EGIF)",
        ylab = "Correlación",
        ylim = c(min(cor_logarea_df$cor_logarea, na.rm = TRUE) - 0.05,
                 max(cor_logarea_df$cor_logarea, na.rm = TRUE) + 0.05))




#Para variables categóricas creo una tabla por cuantiles (4 grupos)
dataset_superficie$Area_cat <- cut(dataset_superficie$Area_ha_EGIF,
                                   breaks = quantile(dataset_superficie$Area_ha_EGIF,probs = seq(0, 1, 0.25)),
                                   include.lowest = TRUE)


levels(dataset_superficie$Area_cat)
table(dataset_superficie$Area_cat, dataset_superficie$burned_before)
table(dataset_superficie$Area_cat, dataset_superficie$MC_SB_grupo)



#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#MODELIZACIÓN: STAGE 1
#Recomiendo borrar todos los objetos para evitar reasignaciones

file.choose()
forest_fire<-read.csv( "C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\dataset_final\\Resultado_final\\forest_fire.csv", header= TRUE , sep=",")
View(forest_fire)

forest_fire$day_of_week<-as.factor(forest_fire$day_of_week)
forest_fire$Provincia<-as.factor(forest_fire$Provincia)
forest_fire$burned_before<- as.factor(forest_fire$burned_before)
forest_fire$MC_SB_grupo <- as.factor(forest_fire$MC_SB_grupo)


#1.MODELO LOGIT-BASE
#Creamos el modelo de regresión logística mínima de referencia. Este modelo 
#incluye solo el intercepto. Este modelo servirá como punto de comparación para 
# saber cuanto ganan los modelos posteriores, y detectar si los modelos
#posteriores realmente aportan señal.

modelo_logit_base<-glm(es_incendio ~ 0+1,data = forest_fire, family = binomial)
summary(modelo_logit_base)

#_______________________________________________________________________________
#2.SELECCIÓN DE VARIABLES POR LASSO

#2.1.Preparación para de los datos
#Eliminamos todo los objetos del enviorement
#Cargamos el dataset
forest_fire <-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\dataset_final\\Resultado_final\\forest_fire.csv", header= TRUE , sep=",")
View(forest_fire)

forest_fire$day_of_week<-as.factor(forest_fire$day_of_week)
forest_fire$Provincia<-as.factor(forest_fire$Provincia)
forest_fire$burned_before<- as.factor(forest_fire$burned_before)
forest_fire$MC_SB_grupo <- as.factor(forest_fire$MC_SB_grupo)

#Eliminamos las columnas del dataset que no formarán parte del análisis 
forest_fire<-subset(forest_fire, select = -c(fid, NumeroPart,OM_NumParte, Area_ha_EGIF, 
                                             f_detec,season, Municipio,ComarcaIsl,Causa, Motivacion,
                                             detecpor, detecp_txt, clasedia,cdia_txt, f_llegadapm,
                                             f_llegadapmae,f_llegadapbh, f_llegadapac, f_control, f_extinción,
                                             TRI, TRA , TRH, TRAC, TDC, TDE,MC_Scott_Burgan, Sup_Arbola,
                                             Sup_No_Arb, Sup_total_, Sup_agríc, Otras_Sup_, observ, area_m2, 
                                             length_m2, Huso, Coordenada_X, Coordenada_Y, NIVELPREE, muertos,
                                             heridos, valoración_pérdidas, OM_time, hora_, diff_viento_ladera))



#Variable respuesta (0/1)
y_s1 <- forest_fire$es_incendio
length(y_s1)

#Matriz de predictores con model.matrix ,se quita la columna del intercepto
x_s1 <- model.matrix(es_incendio ~ ., data = forest_fire)[, -1] 
nrow(x_s1)
View(x_s1)


#2.2.Ajustar Lasso logístico con validación cruzada
set.seed(123)  

modelo_logit_lasso <- cv.glmnet(x = x_s1, y = y_s1,family = "binomial",alpha  = 1, 
                                nfolds = 10, type.measure = "deviance")  


#Para visualizar la curva de CV 
plot(modelo_logit_lasso)


#2.3.Elección de lambda óptimo 
lambda_min_s1  <- modelo_logit_lasso$lambda.min
lambda_1se_s1 <- modelo_logit_lasso$lambda.1se

lambda_min_s1
lambda_1se_s1

#Usaremos lambda_min

#2.4.Obtención del modelo penalizado y de las variables seleccionadas

# Coeficientes del modelo en lambda.min
coef_lasso_s1 <- coef(modelo_logit_lasso, s = "lambda.min")
coef_lasso_s1
exp(coef_lasso_s1)


#Para obtener los nombres de las variables seleccionadas
vars_seleccionadas_s1 <- rownames(coef_lasso_s1)[coef_lasso_s1[,1] != 0]
vars_seleccionadas_s1


#_______________________________________________________________________________


#3.ANÁLISIS DE LA MULTICOLINEALIDAD 
#Lasso tolera la multicolinealidad

correlation_s1 <- cor(forest_fire[, c("Campania","Mes","Altitud_hm","Pendiente",
                                      "OM_DV_X","OM_DV_Y","FCC","prox_caminos","prox_carreteras",
                                      "X_UTM30N","Y_UTM30N","prox_cortafuegos","prox_GR","prox_SH","prox_TU",
                                      "OM_temp_media_2m","OM_temp_max_2m","OM_viento_rafagas_max_10m","OM_et0","OM_duracion_dia",
                                      "OM_lluvia_total","OM_horas_precipitacion","OM_cobertura_nubosa_media","OM_rocio_min_2m",
                                      "OM_humedad_rel_media_2m","OM_humedad_rel_min_2m","OM_presion_media_mls",
                                      "OM_viento_rafagas_media_10m","OM_viento_vel_media_10m","OM_viento_rafagas_min_10m",
                                      "OM_viento_vel_min_10m","OM_bulbo_humedo_min_2m","OM_deficit_presion_vapor_max",
                                      "OM_humedad_suelo_media_0_7cm","OM_humedad_suelo_media_28_100cm",
                                      "prox_observatorios","prox_depositos","proxx_NB","Orientación", "Orientación_X", 
                                      "Orientación_Y", "delta_viento_ladera")])


cor_high_s1 <- which(abs(correlation_s1) > 0.7 & abs(correlation_s1) < 1, arr.ind = TRUE)

high_pairs_s1 <- data.frame(Var1 = rownames(correlation_s1)[cor_high_s1[,1]],
                            Var2 = colnames(correlation_s1)[cor_high_s1[,2]],
                            r = correlation_s1[cor_high_s1])

high_pairs_s1

#Los resultados del análisis de Multicolinelalidad son:

#BLOQUE TEMPERATURA 
#OM_temp_media_2m ↔ OM_temp_max_2m (0.965)
#OM_temp_media_2m ↔ OM_et0 (0.763)
#OM_temp_media_2m ↔ OM_duracion_dia (0.707)
#OM_et0 ↔ OM_temp_max_2m (0.798)
#OM_et0 ↔ OM_duracion_dia (0.770)
#OM_bulbo_humedo_min_2m ↔ OM_temp_media_2m (0.915)
#Nos quedamos solo con una: OM_temp_media_2m

#BLOQUE HUMEDAD
#OM_bulbo_humedo_min_2m ↔ OM_temp_media_2m (0.915)
#OM_rocio_min_2m ↔ OM_bulbo_humedo_min_2m (0.911)
#OM_deficit_presion_vapor_max ↔ OM_temp_media_2m (0.752)
#OM_deficit_presion_vapor_max ↔ OM_et0 (0.817)
#OM_humedad_rel_media_2m ↔ OM_humedad_rel_min_2m (0.870)
#Nos quedamos solo co: OM_humedad_rel_media_2m

#BLOQUE VIENTO 
#Son prácticamente todas iguales
#Nos quedamos solo con OM_viento_vel_media_10m
#Orientación y Orientación_Y están correlacionadas. 

#BLOQUE LLUVIA 
#OM_horas_precipitacion ↔ OM_lluvia_total (0.793)
#Nos quedamos con OM_lluvia_total 

#BLOQUE OBSERVACIÓN 
#prox_observatorios ↔ prox_depositos (0.817)
#Nos quedamos con prox_observatorios, por revisión bibliográfica. 

#_______________________________________________________________________________

#4. AJUSTE DE MODELO LOGIT POST LASSO
#Ajustamos un Modelo Logit con las variables seleccionadas por Lasso, teniendo
#en cuenta el análisis de Multicolinealidad

modelo_logit_postlasso<- glm(es_incendio ~ Campania + Mes + day_of_week + Provincia + 
                               Pendiente  + FCC  + 
                               prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                               MC_SB_grupo + prox_SH + prox_TU + OM_temp_media_2m + 
                               OM_lluvia_total + OM_cobertura_nubosa_media + 
                               OM_humedad_rel_media_2m + 
                               OM_presion_media_mls + OM_viento_vel_media_10m + 
                               OM_humedad_suelo_media_0_7cm + 
                               OM_humedad_suelo_media_28_100cm + prox_observatorios + 
                               Orientación_X + Orientación_Y + delta_viento_ladera 
                             ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso)


#5.ELIMINACIÓN HACIA ATRÁS (BACKWARD)
#Se eliminan una a una las variables que no son estadísticamente significativas
#Se utilizan AIC/BIC como cfriterios de comparación y selección de modelos.

#Modelo_logit_postlasso_back_v1: modelo sin day_of_week

modelo_logit_postlasso_back_v1<- glm(es_incendio ~ Campania + Mes + Provincia + 
                                       Pendiente  + FCC  + 
                                       prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                                       MC_SB_grupo + prox_SH + prox_TU + OM_temp_media_2m + 
                                       OM_lluvia_total + OM_cobertura_nubosa_media + 
                                       OM_humedad_rel_media_2m + 
                                       OM_presion_media_mls + OM_viento_vel_media_10m + 
                                       OM_humedad_suelo_media_0_7cm + 
                                       OM_humedad_suelo_media_28_100cm + prox_observatorios + 
                                       Orientación_X + Orientación_Y + delta_viento_ladera
                                     ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v1)


#Modelo_logit_postlasso_back_v2: modelo sin Provincia
modelo_logit_postlasso_back_v2<- glm(es_incendio ~ Campania + Mes +
                                       Pendiente  + FCC  + 
                                       prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                                       MC_SB_grupo + prox_SH + prox_TU + OM_temp_media_2m + 
                                       OM_lluvia_total + OM_cobertura_nubosa_media + 
                                       OM_humedad_rel_media_2m + 
                                       OM_presion_media_mls + OM_viento_vel_media_10m + 
                                       OM_humedad_suelo_media_0_7cm + 
                                       OM_humedad_suelo_media_28_100cm + prox_observatorios + 
                                       Orientación_X + Orientación_Y + delta_viento_ladera
                                     ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v2)

#Aunque la eliminación de Provincia incrementó ligeramente el AIC, se optó por 
#excluir esta variable atendiendo al principio de parsimonia y a consideraciones
#de redundancia espacial. Dado que el modelo incorpora las coordenadas 
#UTM (X_UTM30N, Y_UTM30N), que proporcionan una caracterización espacial 
#continua y de mayor resolución, la inclusión adicional de 
#Provincia (unidad administrativa de naturaleza discreta y más agregada) aporta 
#información espacial parcialmente solapada. 


#Modelo_logit_postlasso_back_v3: modelo sin Orientación
modelo_logit_postlasso_back_v3<- glm(es_incendio ~ Campania + Mes +
                                       Pendiente  + FCC  + 
                                       prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                                       MC_SB_grupo + prox_SH + prox_TU + OM_temp_media_2m + 
                                       OM_lluvia_total + OM_cobertura_nubosa_media + 
                                       OM_humedad_rel_media_2m + 
                                       OM_presion_media_mls + OM_viento_vel_media_10m + 
                                       OM_humedad_suelo_media_0_7cm + 
                                       OM_humedad_suelo_media_28_100cm + prox_observatorios + 
                                       delta_viento_ladera
                                     ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v3)


#Modelo_logit_postlasso_back_v4: modelo sin delta_viento_ladera
modelo_logit_postlasso_back_v4<- glm(es_incendio ~ Campania + Mes +
                                       Pendiente  + FCC  + 
                                       prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                                       MC_SB_grupo + prox_SH + prox_TU + OM_temp_media_2m + 
                                       OM_lluvia_total + OM_cobertura_nubosa_media + 
                                       OM_humedad_rel_media_2m + 
                                       OM_presion_media_mls + OM_viento_vel_media_10m + 
                                       OM_humedad_suelo_media_0_7cm + 
                                       OM_humedad_suelo_media_28_100cm + prox_observatorios
                                     ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v4)


#Modelo_logit_postlasso_back_v5: modelo sin OM_humedad_suelo_media_0_7cm
modelo_logit_postlasso_back_v5<- glm(es_incendio ~ Campania + Mes +
                                       Pendiente  + FCC  + 
                                       prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                                       MC_SB_grupo + prox_SH + prox_TU + OM_temp_media_2m + 
                                       OM_lluvia_total + OM_cobertura_nubosa_media + 
                                       OM_humedad_rel_media_2m + 
                                       OM_presion_media_mls + OM_viento_vel_media_10m + 
                                       OM_humedad_suelo_media_28_100cm + prox_observatorios
                                     ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v5)


#Modelo_logit_postlasso_back_v6: modelo sin prox_SH
modelo_logit_postlasso_back_v6<- glm(es_incendio ~ Campania + Mes +
                                       Pendiente  + FCC  + 
                                       prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                                       MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                       OM_lluvia_total + OM_cobertura_nubosa_media + 
                                       OM_humedad_rel_media_2m + 
                                       OM_presion_media_mls + OM_viento_vel_media_10m + 
                                       OM_humedad_suelo_media_28_100cm + prox_observatorios
                                     ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v6)


#Modelo_logit_postlasso_back_v7: modelo sin FCC
modelo_logit_postlasso_back_v7<- glm(es_incendio ~ Campania + Mes +
                                       Pendiente  + 
                                       prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                                       MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                       OM_lluvia_total + OM_cobertura_nubosa_media + 
                                       OM_humedad_rel_media_2m + 
                                       OM_presion_media_mls + OM_viento_vel_media_10m + 
                                       OM_humedad_suelo_media_28_100cm + prox_observatorios
                                     ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v7)


#Modelo_logit_postlasso_back_v8: modelo sin prox_observatorios
modelo_logit_postlasso_back_v8<- glm(es_incendio ~ Campania + Mes +
                                       Pendiente  + 
                                       prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                                       MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                       OM_lluvia_total + OM_cobertura_nubosa_media + 
                                       OM_humedad_rel_media_2m + 
                                       OM_presion_media_mls + OM_viento_vel_media_10m + 
                                       OM_humedad_suelo_media_28_100cm 
                                     ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v8)
#AIC aumenta en 0,1. Prácticamente, el modelo no cambia. Se elimina atendiendo
#al principio de parsimonia.




#Modelo_logit_postlasso_back_v9: modelo sin MC_SB_grupo
modelo_logit_postlasso_back_v9<- glm(es_incendio ~ Campania + Mes +
                                       Pendiente  + 
                                       prox_caminos + prox_carreteras + X_UTM30N + Y_UTM30N  + 
                                       prox_TU + OM_temp_media_2m + 
                                       OM_lluvia_total + OM_cobertura_nubosa_media + 
                                       OM_humedad_rel_media_2m + 
                                       OM_presion_media_mls + OM_viento_vel_media_10m + 
                                       OM_humedad_suelo_media_28_100cm 
                                     ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v9)
#Al quitar esta variable el modelo empeora en más de 300 puntos. 
#MC_SB_grupo permancerá en el modelo


#Modelo_logit_postlasso_back_v10: modelo sin Y_UTM30N
modelo_logit_postlasso_back_v10<- glm(es_incendio ~ Campania + Mes +
                                        Pendiente  + 
                                        prox_caminos + prox_carreteras + X_UTM30N +  
                                        MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                        OM_lluvia_total + OM_cobertura_nubosa_media + 
                                        OM_humedad_rel_media_2m + 
                                        OM_presion_media_mls + OM_viento_vel_media_10m + 
                                        OM_humedad_suelo_media_28_100cm 
                                      ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v10)
#AIC aumenta en 0,5. Prácticamente, el modelo no cambia. Se elimina atendiendo
#al principio de parsimonia.


#Modelo_logit_postlasso_back_v11: modelo sin Campania
modelo_logit_postlasso_back_v11<- glm(es_incendio ~ Mes +
                                        Pendiente  + 
                                        prox_caminos + prox_carreteras + X_UTM30N +  
                                        MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                        OM_lluvia_total + OM_cobertura_nubosa_media + 
                                        OM_humedad_rel_media_2m + 
                                        OM_presion_media_mls + OM_viento_vel_media_10m + 
                                        OM_humedad_suelo_media_28_100cm 
                                      ,data = forest_fire, family = binomial)

summary(modelo_logit_postlasso_back_v11)
#AIC aumenta 1,6 puntos. 
#Se opta por eliminar la variable Campania por las siguientes razones:

#- Aunque la exclusión de la variable Campania produce un aumento moderado del 
#AIC (1,6 unidades), este cambio no sugiere una pérdida relevante de capacidad 
#explicativa. Además, dicha variable no mostró significación estadística 
#consistente a lo largo de los distintos modelos considerados.

#-Campania actúa como un proxy temporal específico del periodo 
#2000–2016, su inclusión podría limitar la transferibilidad del modelo a 
#periodos posteriores. En consecuencia, se optó por su exclusión, priorizando 
#un modelo más parsimonioso y robusto, basado en variables físicas, espaciales 
#y meteorológicas con mayor estabilidad temporal.


#_______________________________________________________________________________

#6.EXTENSIÓN DEL MODELO LINEAL MEDIANTE LA INCLUSIÓN DE INTERACCIONES CON 
#METODOLOGÍA FORWARD 

#Se va a evaluar mediante una metodología forward un conjunto reducido y 
#predefinido de interacciones metereológicas, topográficas y humanas.
#Estas son: 

#OM_temp_media_2m:OM_humedad_rel_media_2m --> SÍ
#Pendiente:OM_viento_vel_media_10m --> NO 
#OM_temp_media_2m: OM_viento_vel_media_10m -->SI
#OM_viento_vel_media_10m:OM_humedad_rel_media_2m -->NO
#prox_caminos: Pendiente-->SÍ


#Modelo_logit_interacciones_v1: se incluye la interacción OM_temp_media_2m:OM_humedad_rel_media_2m
modelo_logit_interacciones_v1<- glm(es_incendio ~ Mes +
                                      Pendiente  + 
                                      prox_caminos + prox_carreteras + X_UTM30N +  
                                      MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                      OM_lluvia_total + OM_cobertura_nubosa_media + 
                                      OM_humedad_rel_media_2m + 
                                      OM_presion_media_mls + OM_viento_vel_media_10m + 
                                      OM_humedad_suelo_media_28_100cm +
                                      OM_temp_media_2m:OM_humedad_rel_media_2m
                                    ,data = forest_fire, family = binomial)

summary(modelo_logit_interacciones_v1)
#AIC disminuye 3,5 puntos. Es significativa y tiene un signo coherente con la 
#revisión bibliográficaLa interacción y sus efectos simples permanecerán 
#en el modelo

#Modelo_logit_interacciones_v2: se incluye la interacción Pendiente:OM_viento_vel_media_10m
modelo_logit_interacciones_v2<- glm(es_incendio ~ Mes +
                                      Pendiente  + 
                                      prox_caminos + prox_carreteras + X_UTM30N +  
                                      MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                      OM_lluvia_total + OM_cobertura_nubosa_media + 
                                      OM_humedad_rel_media_2m + 
                                      OM_presion_media_mls + OM_viento_vel_media_10m + 
                                      OM_humedad_suelo_media_28_100cm +
                                      OM_temp_media_2m:OM_humedad_rel_media_2m +
                                      Pendiente:OM_viento_vel_media_10m +
                                      ,data = forest_fire, family = binomial)

summary(modelo_logit_interacciones_v2)
#No se mantiene la interacción Pendiente:OM_viento_vel_media_10m


#Modelo_logit_interacciones_v3: se incluye la interacción OM_temp_media_2m: OM_viento_vel_media_10m
modelo_logit_interacciones_v3<- glm(es_incendio ~ Mes +
                                      Pendiente  + 
                                      prox_caminos + prox_carreteras + X_UTM30N +  
                                      MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                      OM_lluvia_total + OM_cobertura_nubosa_media + 
                                      OM_humedad_rel_media_2m + 
                                      OM_presion_media_mls + OM_viento_vel_media_10m + 
                                      OM_humedad_suelo_media_28_100cm +
                                      OM_temp_media_2m:OM_humedad_rel_media_2m +
                                      OM_temp_media_2m: OM_viento_vel_media_10m 
                                    ,data = forest_fire, family = binomial)

summary(modelo_logit_interacciones_v3)

#AIC disminuye casi 2 puntos. Es significativa y tiene un signo coherente con la 
#revisión bibliográficaLa interacción y sus efectos simples permanecerán 
#en el modelo

#Modelo_logit_interacciones_v3: se incluye la interacción OM_temp_media_2m: OM_viento_vel_media_10m
modelo_logit_interacciones_v3<- glm(es_incendio ~ Mes +
                                      Pendiente  + 
                                      prox_caminos + prox_carreteras + X_UTM30N +  
                                      MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                      OM_lluvia_total + OM_cobertura_nubosa_media + 
                                      OM_humedad_rel_media_2m + 
                                      OM_presion_media_mls + OM_viento_vel_media_10m + 
                                      OM_humedad_suelo_media_28_100cm +
                                      OM_temp_media_2m:OM_humedad_rel_media_2m +
                                      OM_temp_media_2m: OM_viento_vel_media_10m 
                                    ,data = forest_fire, family = binomial)

summary(modelo_logit_interacciones_v3)


#Modelo_logit_interacciones_v4: se incluye la interacción OM_viento_vel_media_10m:OM_humedad_rel_media_2m
modelo_logit_interacciones_v4<- glm(es_incendio ~ Mes +
                                      Pendiente  + 
                                      prox_caminos + prox_carreteras + X_UTM30N +  
                                      MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                      OM_lluvia_total + OM_cobertura_nubosa_media + 
                                      OM_humedad_rel_media_2m + 
                                      OM_presion_media_mls + OM_viento_vel_media_10m + 
                                      OM_humedad_suelo_media_28_100cm +
                                      OM_temp_media_2m:OM_humedad_rel_media_2m +
                                      OM_temp_media_2m: OM_viento_vel_media_10m +
                                      OM_viento_vel_media_10m:OM_humedad_rel_media_2m
                                    ,data = forest_fire, family = binomial)

summary(modelo_logit_interacciones_v4)
#No se mantiene la interacción OM_viento_vel_media_10m:OM_humedad_rel_media_2m



#Modelo_logit_interacciones_v5: se incluye la interacción OM_temp_media_2m: OM_viento_vel_media_10m
modelo_logit_interacciones_v5<- glm(es_incendio ~ Mes +
                                      Pendiente  + 
                                      prox_caminos + prox_carreteras + X_UTM30N +  
                                      MC_SB_grupo + prox_TU + OM_temp_media_2m + 
                                      OM_lluvia_total + OM_cobertura_nubosa_media + 
                                      OM_humedad_rel_media_2m + 
                                      OM_presion_media_mls + OM_viento_vel_media_10m + 
                                      OM_humedad_suelo_media_28_100cm +
                                      OM_temp_media_2m:OM_humedad_rel_media_2m +
                                      OM_temp_media_2m: OM_viento_vel_media_10m +
                                      prox_caminos: Pendiente
                                    ,data = forest_fire, family = binomial)

summary(modelo_logit_interacciones_v5)
#AIC disminuye 27 puntos. Es significativa y tiene un signo coherente con la 
#revisión bibliográfica. La interacción y sus efectos simples permanecerán 
#en el modelo.

#_______________________________________________________________________________


#7.AJUSTE DE MODELOS GAM
#Para el ajuste de los Modelos Aditivos Generalizados (GAM) se considerarán las 
#variables seleccionadas mediante LASSO, teniendo en cuenta el análsisi de  
#de multicolinealidad. Además, se incorporarán variables no seleccionadas por 
#LASSO pero relevantes según la bibliografía, ya que su efecto podría ser no 
#lineal y, por tanto, no haber sido detectado adecuadamente en el modelo lineal.

#Además se utilizará select = TRUE. Esto permite que el GAM haga selección 
#automática de predictores, penalizando suavizados irrelevantes hasta hacerlos 
#prácticamente cero.


#Para captar la estructura espacial se utilizará: s(X_UTM30N, Y_UTM30N)
#La orientación y la dirección del viento son variables circulares. 
#Por tanto,se incluirán de la siguientes manera:
summary(forest_fire$Orientación)
summary(forest_fire$OM_viento_direccion_10m)
forest_fire$Orientación[forest_fire$Orientación == 360] <- 0
forest_fire$OM_viento_direccion_10m[forest_fire$OM_viento_direccion_10m == 360] <- 0

#s(Orientación, bs="cc")
#s(OM_viento_direccion_10m, bs="cc")

#La variable Mes, también se incluirá de la siguiente manera: s(Mes, bs="cc", k=12)

#Modelo_gam_logit_v1 
modelo_gam_logit_v1 <- gam(es_incendio ~ s(Mes, bs="cc", k=12) + 
                             s(Pendiente) + 
                             s(prox_caminos) + 
                             s(prox_carreteras) + 
                             s(X_UTM30N,Y_UTM30N, bs="tp", k=100) + 
                             MC_SB_grupo + 
                             s(prox_TU) +
                             s(OM_temp_media_2m) + 
                             s(OM_lluvia_total) + 
                             s(OM_cobertura_nubosa_media) + 
                             s(OM_humedad_rel_media_2m) +
                             s(OM_presion_media_mls) + 
                             s(OM_viento_vel_media_10m) + 
                             s(OM_humedad_suelo_media_28_100cm)+
                             s(Altitud_hm) +
                             burned_before +
                             s(FCC) +
                             s(dens_poblacional) +
                             s(prox_GR) +
                             s(prox_SH) +
                             s(proxx_NB)+
                             s(Orientación, bs="cc")+
                             s(OM_viento_direccion_10m, bs="cc")+
                             s(prox_observatorios)+
                             s(delta_viento_ladera)
                           ,data= forest_fire,
                           family = binomial(link = "logit"),method = "REML", 
                           select = TRUE,
                           knots = list(Mes = c(1,12), Orientación= c(0,360), 
                                        OM_viento_direccion_10m=c(0,360)))


summary(modelo_gam_logit_v1)
AIC(modelo_gam_logit_v1)
gam.check(modelo_gam_logit_v1) 


#Modelo_gam_logit_v2. Quitamos las variables que edf ≈ 0 y no son significativas.
#Estas son:
#s(Altitud_hm)
#s(FCC)
#s(dens_poblacional)
#s(prox_GR)
#s(prox_SH)
#s(Orientación, bs="cc")
#s(OM_viento_direccion_10m, bs="cc")
#s(prox_observatorios)
#s(delta_viento_ladera)



modelo_gam_logit_v2 <- gam(es_incendio ~ s(Mes, bs="cc", k=12) + 
                             s(Pendiente) + 
                             s(prox_caminos) + 
                             s(prox_carreteras) + 
                             s(X_UTM30N,Y_UTM30N, bs="tp", k=100) + 
                             MC_SB_grupo + 
                             s(prox_TU) +
                             s(OM_temp_media_2m) + 
                             s(OM_lluvia_total) + 
                             s(OM_cobertura_nubosa_media) + 
                             s(OM_humedad_rel_media_2m) +
                             s(OM_presion_media_mls) + 
                             s(OM_viento_vel_media_10m) + 
                             s(OM_humedad_suelo_media_28_100cm)+
                             burned_before +
                             s(proxx_NB)
                           ,data= forest_fire,
                           family = binomial(link = "logit"),method = "REML", 
                           select = TRUE,
                           knots = list(Mes = c(1,12)))


summary(modelo_gam_logit_v2)
AIC(modelo_gam_logit_v2)
gam.check(modelo_gam_logit_v2) 
concurvity(modelo_gam_logit_v2, full=TRUE)


#Modelo_gam_logit_v3. Subimos K para s(X_UTM30N, Y_UTM30N)
modelo_gam_logit_v3 <- gam(es_incendio ~ s(Mes, bs="cc", k=12) + 
                             s(Pendiente) + 
                             s(prox_caminos) + 
                             s(prox_carreteras) + 
                             s(X_UTM30N,Y_UTM30N, bs="tp", k=200) + 
                             MC_SB_grupo + 
                             s(prox_TU) +
                             s(OM_temp_media_2m) + 
                             s(OM_lluvia_total) + 
                             s(OM_cobertura_nubosa_media) + 
                             s(OM_humedad_rel_media_2m) +
                             s(OM_presion_media_mls) + 
                             s(OM_viento_vel_media_10m) + 
                             s(OM_humedad_suelo_media_28_100cm)+
                             burned_before +
                             s(proxx_NB)
                           ,data= forest_fire,
                           family = binomial(link = "logit"),method = "REML", 
                           select = TRUE,
                           knots = list(Mes = c(1,12)))


summary(modelo_gam_logit_v3)
AIC(modelo_gam_logit_v3)
gam.check(modelo_gam_logit_v3) 
#Aún sale aviso de K bajo para s(X_UTM30N,Y_UTM30N, bs="tp"). No subirlo más.
concurvity(modelo_gam_logit_v3, full=TRUE)


#Modelo_gam_logit_v4: modelo sin s(OM_presion_media_mls)
modelo_gam_logit_v4 <- gam(es_incendio ~ s(Mes, bs="cc", k=12) + 
                             s(Pendiente) + 
                             s(prox_caminos) + 
                             s(prox_carreteras) + 
                             s(X_UTM30N,Y_UTM30N, bs="tp", k=200) + 
                             MC_SB_grupo + 
                             s(prox_TU) +
                             s(OM_temp_media_2m) + 
                             s(OM_lluvia_total) + 
                             s(OM_cobertura_nubosa_media) + 
                             s(OM_humedad_rel_media_2m) +
                             s(OM_viento_vel_media_10m) + 
                             s(OM_humedad_suelo_media_28_100cm)+
                             burned_before +
                             s(proxx_NB)
                           ,data= forest_fire,
                           family = binomial(link = "logit"),method = "REML", 
                           select = TRUE,
                           knots = list(Mes = c(1,12)))


summary(modelo_gam_logit_v4)
AIC(modelo_gam_logit_v4)
gam.check(modelo_gam_logit_v4) 
concurvity(modelo_gam_logit_v4, full=TRUE)



#Modelo_gam_logit_v5: modelo sin burned_before
modelo_gam_logit_v5 <- gam(es_incendio ~ s(Mes, bs="cc", k=12) + 
                             s(Pendiente) + 
                             s(prox_caminos) + 
                             s(prox_carreteras) + 
                             s(X_UTM30N,Y_UTM30N, bs="tp", k=200) + 
                             MC_SB_grupo + 
                             s(prox_TU) +
                             s(OM_temp_media_2m) + 
                             s(OM_lluvia_total) + 
                             s(OM_cobertura_nubosa_media) + 
                             s(OM_humedad_rel_media_2m) +
                             s(OM_viento_vel_media_10m) + 
                             s(OM_humedad_suelo_media_28_100cm)+
                             s(proxx_NB)
                           ,data= forest_fire,
                           family = binomial(link = "logit"),method = "REML", 
                           select = TRUE,
                           knots = list(Mes = c(1,12)))


summary(modelo_gam_logit_v5)
AIC(modelo_gam_logit_v5)
gam.check(modelo_gam_logit_v5) 
concurvity(modelo_gam_logit_v5, full=TRUE)


#AUC-in sample
predicciones_gam_logit_v5<- predict(modelo_gam_logit_v5, type = "response")
roc(forest_fire$es_incendio, predicciones_gam_logit_v5) 
#AUC es 0,7953

#_______________________________________________________________________________

#8.SELECCIÓN DE MODELOS CANDIDATOS Y COMPARACIÓN DE MODELOS
#Seleccionamos como modelos candidatos los siguientes:
# M1: modelo_logit_base
# M2: modelo_logit_postlasso_back_v11
# M3: modelo_logit_interacciones_v5
# M4: modelo_gam_logit_v5

#Para la selcción del modelo final utilizaremos los siguientes criterios: AIC,
#BIC, AUC, Tjur's R² , Hosmer-Lemeshow

#Reasignación de los modelos 
M1<-modelo_logit_base
M2<-modelo_logit_postlasso_back_v11
M3<-modelo_logit_interacciones_v5
M4<-modelo_gam_logit_v5

#Respuesta
y_s1<-forest_fire$es_incendio 

#Probabilidades predichas
p_M1<- predict(M1, type = "response")
p_M2<- predict(M2, type = "response")
p_M3<- predict(M3, type = "response")
p_M4<- predict(M4, type = "response")

#AIC
AIC(M1,M2,M3,M4)

#BIC    
BIC(M1,M2,M3,M4)

#AUC
AUC_M1<-roc(y_s1, p_M1) 
AUC_M2<-roc(y_s1, p_M2) 
AUC_M3<-roc(y_s1, p_M3) 
AUC_M4<-roc(y_s1, p_M4) 

#Log-Loss
eps <- 1e-15
logloss_M1 <- -mean(y_s1*log(p_M1 + eps) + (1-y_s1)*log(1 - p_M1 + eps))
logloss_M2 <- -mean(y_s1*log(p_M2 + eps) + (1-y_s1)*log(1 - p_M2 + eps))
logloss_M3 <- -mean(y_s1*log(p_M3 + eps) + (1-y_s1)*log(1 - p_M3 + eps))
logloss_M4 <- -mean(y_s1*log(p_M4 + eps) + (1-y_s1)*log(1 - p_M4 + eps))

#Brier-Score
Brier_M1<-mean((y_s1 - p_M1)^2)
Brier_M2<-mean((y_s1 - p_M2)^2)
Brier_M3<-mean((y_s1 - p_M3)^2)
Brier_M4<-mean((y_s1 - p_M4)^2)

#Tjur's R²
tjur_M1 <- mean(p_M1[y_s1 == 1]) - mean(p_M1[y_s1 == 0])
tjur_M2 <- mean(p_M2[y_s1 == 1]) - mean(p_M2[y_s1 == 0])
tjur_M3 <- mean(p_M3[y_s1 == 1]) - mean(p_M3[y_s1 == 0])
tjur_M4 <- mean(p_M4[y_s1 == 1]) - mean(p_M4[y_s1 == 0])

#Test de Hosmer-Lemeshow (g=10)
hl_M1 <- hoslem.test(y_s1, p_M1, g = 10)
hl_M2 <- hoslem.test(y_s1, p_M2, g = 10)
hl_M3 <- hoslem.test(y_s1, p_M3, g = 10)
hl_M4 <- hoslem.test(y_s1, p_M4, g = 10)

#Resumen final
M_s1<-c("M1", "M2", "M3","M4")
AIC_s1<-c(AIC(M1), AIC(M2), AIC(M3), AIC(M4))
BIC_s1<-c(BIC(M1), BIC(M2), BIC(M3), BIC(M4))
auc_s1<-c(AUC_M1$auc, AUC_M2$auc, AUC_M3$auc, AUC_M4$auc)
logloss_s1<-c(logloss_M1, logloss_M2, logloss_M3, logloss_M4)
Brier_Score_s1<-c(Brier_M1, Brier_M2, Brier_M3, Brier_M4)
tjur_s1<- c(tjur_M1, tjur_M2, tjur_M3, tjur_M4)
hl_s1<-c(NA, hl_M2$p.value, hl_M3$p.value, hl_M4$p.value)

resultados_M_Stage_1<-data.frame(M_s1, AIC_s1, BIC_s1, auc_s1, logloss_s1, Brier_Score_s1, tjur_s1, hl_s1)
View(resultados_M_Stage_1)


#Se eligen los modelos M3 (GLM con interacciones) y M4 (modelo GAM)

#M3
M3<- glm(es_incendio ~ Mes +Pendiente  +   prox_caminos + prox_carreteras + 
           X_UTM30N + MC_SB_grupo + prox_TU + OM_temp_media_2m + 
           OM_lluvia_total + OM_cobertura_nubosa_media + 
           OM_humedad_rel_media_2m + OM_presion_media_mls + 
           OM_viento_vel_media_10m + OM_humedad_suelo_media_28_100cm +
           OM_temp_media_2m:OM_humedad_rel_media_2m +
           OM_temp_media_2m: OM_viento_vel_media_10m +
           prox_caminos: Pendiente ,data = forest_fire, family = binomial)


#M4
M4 <- gam(es_incendio ~ s(Mes, bs="cc", k=12) + 
            s(Pendiente) + 
            s(prox_caminos) + 
            s(prox_carreteras) + 
            s(X_UTM30N,Y_UTM30N, bs="tp", k=200) + 
            MC_SB_grupo + 
            s(prox_TU) +
            s(OM_temp_media_2m) + 
            s(OM_lluvia_total) + 
            s(OM_cobertura_nubosa_media) + 
            s(OM_humedad_rel_media_2m) +
            s(OM_viento_vel_media_10m) + 
            s(OM_humedad_suelo_media_28_100cm)+
            s(proxx_NB)
          ,data= forest_fire,
          family = binomial(link = "logit"),method = "REML", 
          select = TRUE,
          knots = list(Mes = c(1,12)))


#_______________________________________________________________________________
#9.VALIDACIÓN Y COMPARACIÓN DE MODELOS DEL STAGE 1

#9.1.Diagnóstico del ajuste
#9.A.Diagnóstico del ajuste para M3 (GLM Logístico)

#Multicolinealidad (VIF)
vif(M3)

#Diagnóstico DHARMa M3
sim_M3<-simulateResiduals(fittedModel = M3, n=1000)

testUniformity(M3)
testDispersion(M3)
testOutliers(M3)

#9.B.Diagnóstico del ajuste para M4 (GAM-Logit)

#Resumen del GAM:deviance explained, edf y significancia
summary(M4)

#Diagnóstico GAM: k-index y residuos
gam.check(M4)

#Concurvidad 
concurvity(M4, full = TRUE)

#Diagnóstico DHARMa M4
sim_M4<-simulateResiduals(fittedModel = M4, n=1000)

testUniformity(M4)
testDispersion(M4) #A pesar de ser significativo, D = 0.97, no hay problemas graves de dispersión
summary(M4)$dispersion
testOutliers(M4)


#9.2.Evaluación predictiva (in-sample y validación espacial out of sample)

#9.2.1 In sample
y_s1 <- forest_fire$es_incendio

# Probabilidades predichas
p_M3 <- predict(M3, type = "response")
p_M4 <- predict(M4, type = "response")


#Brier
Brier_M3
Brier_M4

#Logloss
logloss_M3
logloss_M4

# AUC y ROC 
roc_M3 <- roc(y_s1,p_M3)
roc_M4 <- roc(y_s1,p_M4)

auc(roc_M3)
auc(roc_M4)


# Curva ROC 
plot(roc_M3, main = "Curva ROC (in-sample): M3 vs M4", col = "blue", lwd = 2)
plot(roc_M4, add = TRUE, col = "green", lwd = 2)

legend("bottomright",legend = c(paste0("M3  AUC = ", round(auc(roc_M3), 4)),
                                paste0("M4  AUC = ", round(auc(roc_M4), 4))),
       col = c("blue", "green"),lwd = 2,bty = "n")



#9.2.2. Validación out of sample (K-Fold espacial por bloques)

#Test de I-Moran por umbral de distancia

#1)Obtenemos la distancia que asegura la conectividad de los grafos
#Si los grafos no están conectados, los resultados del Test no son interpretables
#La regla es:
#Si nc = 1 → grafo totalmente conectado 
#Si nc > 1 → hay subgrafos (fragmentación)

#Coordenadas UTM (metros)
coords_s1 <- cbind(forest_fire$X_UTM30N, forest_fire$Y_UTM30N)

#3km
nb_3km_s1 <- dnearneigh(coords_s1, 0, 3000)
n.comp.nb(nb_3km_s1)$nc        
# número de componentes (subgrafos): 167

#5km
nb_5km_s1 <- dnearneigh(coords_s1, 0, 5000)
n.comp.nb(nb_5km_s1)$nc        
# número de componentes (subgrafos): 12

#6km
nb_6km_s1 <- dnearneigh(coords_s1, 0, 6000)
n.comp.nb(nb_6km_s1)$nc        
# número de componentes (subgrafos): 12

#7km
nb_7km_s1 <- dnearneigh(coords_s1, 0, 7000)
n.comp.nb(nb)$nc        
# número de componentes (subgrafos): 1 


#3)Distancia máxima (en metros): la mínima que asegura conectividad de los grafos
dmax_s1 <- 7000

#4) Vecindad y pesos
nb_s1 <- dnearneigh(coords_s1, d1 = 0, d2 = dmax_s1, longlat = FALSE)
lw_s1 <- nb2listw(nb_s1, style = "W", zero.policy = TRUE)


#5) Test de I Moran

#M3 
res_M3 <- residuals(M3, type = "pearson")
set.seed(123)
moran.mc(res_M3, lw_s1, nsim = 999, zero.policy = TRUE)
#Existe autocorrelación espacial de los resiudos, existe estructura espacial
#no captada por el modelo

#M4 
res_M4 <- residuals(M4, type = "pearson")
set.seed(123)
moran.mc(res_M4, lw_s1, nsim = 999, zero.policy = TRUE)
#No existe autocorrelación espacial de los residuos 





#Validación por bloques espaciales 
set.seed(123)

df_s1 <- forest_fire
coords_s1 <- cbind(df_s1$X_UTM30N, df_s1$Y_UTM30N)
yname_s1 <- "es_incendio"

#1) Grid 20x20 km
pts_s1 <- st_as_sf(df_s1, coords = c("X_UTM30N","Y_UTM30N"), crs = 25830)
grid_s1 <- st_make_grid(pts_s1, cellsize = 20000, square = TRUE)
grid_sf_s1 <- st_sf(cell_id_s1 = seq_along(grid_s1), geometry = grid_s1)

pts_join_s1 <- st_join(pts_s1, grid_sf_s1, join = st_within)
df_s1$cell_id_s1 <- pts_join_s1$cell_id_s1
df_s1 <- df_s1[!is.na(df_s1$cell_id_s1), ]

#2) Folds por bloque
cells_s1 <- sort(unique(df_s1$cell_id_s1))
fold_id_s1 <- sample(rep(1:5, length.out = length(cells_s1)))
cell2fold_s1 <- data.frame(cell_id_s1 = cells_s1, fold_s1 = fold_id_s1)

df_s1 <- df_s1 %>% left_join(cell2fold_s1, by = "cell_id_s1")
print(table(df_s1$fold_s1))


#3) CV
form_M3_s1 <- formula(M3)
form_M4_s1 <- formula(M4)

eps_s1 <- 1e-15
cv_res_s1 <- data.frame()

for (k_s1 in 1:5) {
  
  train_s1 <- df_s1[df_s1$fold_s1 != k_s1, ]
  test_s1  <- df_s1[df_s1$fold_s1 == k_s1, ]
  
  # Ajuste modelos
  m3_k_s1 <- glm(form_M3_s1, data = train_s1, family = binomial())
  
  m4_k_s1 <- bam(form_M4_s1,
                 data = train_s1,
                 family = binomial(link = "logit"),
                 method = "fREML",
                 discrete = TRUE,
                 knots = list(Mes = c(1,12)))  
  
  # Respuesta y predicciones
  y_test_s1 <- test_s1[[yname_s1]]
  p3_s1 <- predict(m3_k_s1, newdata = test_s1, type = "response")
  p4_s1 <- predict(m4_k_s1, newdata = test_s1, type = "response")
  
  # DEBUG
  cat("\nFold", k_s1,
      "| NA y:", sum(is.na(y_test_s1)),
      "| NA p3:", sum(is.na(p3_s1)),
      "| NA p4:", sum(is.na(p4_s1)), "\n")
  
  # Filtrar filas válidas
  ok3_s1 <- complete.cases(y_test_s1, p3_s1)
  ok4_s1 <- complete.cases(y_test_s1, p4_s1)
  
  y3_s1 <- y_test_s1[ok3_s1]; p3_ok_s1 <- p3_s1[ok3_s1]
  y4_s1 <- y_test_s1[ok4_s1]; p4_ok_s1 <- p4_s1[ok4_s1]
  
  # Prevalencia
  prev_k_s1 <- mean(y_test_s1, na.rm = TRUE)
  
  # AUC
  roc3_s1 <- roc(y3_s1, p3_ok_s1, levels = c(0,1), direction = "<", quiet = TRUE)
  roc4_s1 <- roc(y4_s1, p4_ok_s1, levels = c(0,1), direction = "<", quiet = TRUE)
  
  AUC3_s1 <- as.numeric(auc(roc3_s1))
  AUC4_s1 <- as.numeric(auc(roc4_s1))
  
  # Brier
  Brier3_s1 <- mean((y3_s1 - p3_ok_s1)^2)
  Brier4_s1 <- mean((y4_s1 - p4_ok_s1)^2)
  
  # LogLoss
  LogLoss3_s1 <- -mean(y3_s1*log(p3_ok_s1 + eps_s1) + (1-y3_s1)*log(1 - p3_ok_s1 + eps_s1))
  LogLoss4_s1 <- -mean(y4_s1*log(p4_ok_s1 + eps_s1) + (1-y4_s1)*log(1 - p4_ok_s1 + eps_s1))
  
  cv_res_s1 <- rbind(cv_res_s1,
                     data.frame(
                       fold = k_s1,
                       n_test = nrow(test_s1),
                       prev = prev_k_s1,
                       n_ok_M3 = length(y3_s1),
                       n_ok_M4 = length(y4_s1),
                       AUC_M3 = AUC3_s1,
                       Brier_M3 = Brier3_s1,
                       LogLoss_M3 = LogLoss3_s1,
                       AUC_M4 = AUC4_s1,
                       Brier_M4 = Brier4_s1,
                       LogLoss_M4 = LogLoss4_s1))}


#Resultados por fold
cv_res_s1


#Resumen (media ± sd)
cv_summary_s1 <- data.frame(Modelo = c("M3", "M4"),
                            AUC_mean = c(mean(cv_res_s1$AUC_M3), mean(cv_res_s1$AUC_M4)),
                            AUC_sd   = c(sd(cv_res_s1$AUC_M3),   sd(cv_res_s1$AUC_M4)),
                            Brier_mean = c(mean(cv_res_s1$Brier_M3), mean(cv_res_s1$Brier_M4)),
                            Brier_sd   = c(sd(cv_res_s1$Brier_M3),   sd(cv_res_s1$Brier_M4)),
                            LogLoss_mean = c(mean(cv_res_s1$LogLoss_M3), mean(cv_res_s1$LogLoss_M4)),
                            LogLoss_sd   = c(sd(cv_res_s1$LogLoss_M3),   sd(cv_res_s1$LogLoss_M4)))

cv_summary_s1



#_______________________________________________________________________________
#10.CALIBRACIÓN Y UMBRAL DE CLASIFICACIÓN

#M3 se mantiene como modelo alternativo parsimonioso; no obstante, el análisis 
#de calibración y clasificación se realiza sobre M4 al ser el modelo 
#seleccionado para aplicación predictiva.

# Datos
y_s1 <- forest_fire$es_incendio
p_M4 <- predict(M4, type="response")


#Calibración M4
cal_M4 <- data.frame(y=y_s1, p=p_M4) %>%
  mutate(bin = ntile(p, 10)) %>%
  group_by(bin) %>%
  summarise(p_mean = mean(p),
            y_mean = mean(y),
            n = n())

cal_M4

ggplot(cal_M4, aes(x=p_mean, y=y_mean))+
  geom_point(size=2)+
  geom_line()+
  geom_abline(slope=1, intercept=0, linetype=2)+
  labs(title="Curva de calibración (M4)",
       x="Probabilidad predicha (media por decil)",
       y="Proporción observada (media por decil)")+
  ylim(0,1)+xlim(0,1)


# Intercepto y slope: y ~ logit(p)
logit <- function(p) log(p/(1-p))

eps <- 1e-15
lp_M4 <- logit(pmin(pmax(p_M4, eps), 1-eps))

cal_slope_M4 <- glm(y_s1 ~ lp_M4, family=binomial())
summary(cal_slope_M4)

# ROC
roc_M4 <- roc(y_s1, p_M4)
roc_M4

# Umbral óptimo Youden
thr_M4 <- coords(roc_M4, x="best", best.method="youden", ret="threshold")
thr_M4
thr_M4 <- as.numeric(thr_M4) 

# Predicciones clase
pred_M4 <- ifelse(p_M4 >= thr_M4, 1, 0)

# Matriz confusión (M4)
tab_M4 <- table(Pred=pred_M4, Obs=y_s1)
tab_M4

# Métricas M4
TP <- tab_M4["1","1"]
TN <- tab_M4["0","0"]
FP <- tab_M4["1","0"]
FN <- tab_M4["0","1"]

acc_M4 <- (TP+TN)/sum(tab_M4)
sens_M4 <- TP/(TP+FN)
spec_M4 <- TN/(TN+FP)
prec_M4 <- TP/(TP+FP)
f1_M4 <- 2*(prec_M4*sens_M4)/(prec_M4+sens_M4)

c(accuracy=acc_M4, sensibilidad=sens_M4, especificidad=spec_M4,
  precision=prec_M4, F1=f1_M4)


#_______________________________________________________________________________


#11. SELECCIÓN DEL MODELO FINAL E INTERPRETACIÓN DE LAS CURVAS DE SUAVIZADO
summary(M4)

plot(M4, select=1 ,ylim=c(-0.5,0.8))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M4, select=2 ,ylim=c(-1.5,1), xlim=c(0,60))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v =14 , col = "blue", lwd = 2, lty = 2)
abline(v =43 , col = "blue", lwd = 2, lty = 2)


plot(M4, select=3 ,ylim=c(-4.5,0.8))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v =0.55 , col = "blue", lwd = 2, lty = 2)
abline(v = 1.96, col = "blue", lwd = 2, lty = 2)

plot(M4, select=4 ,ylim=c(-0.5,0.5), xlim=c(0,5))
abline(h = 0, col = "red", lty = 2, lwd = 2)

#Para s(X_UTM30N, Y_UTM30N) siendo theta el giro horizontal y phi la inclinación del gráfico
#No se puede interpretar así que se abre en QGIS, código de la función utilizada al final
#del script de código del stage_2.R
#theta=0
vis.gam(M4,view = c("X_UTM30N","Y_UTM30N"),plot.type = "persp",theta = 0,phi = 0)    

#theta=90
vis.gam(M4,view = c("X_UTM30N","Y_UTM30N"),plot.type = "persp",theta = 90,phi = 0)     

#theta=180
vis.gam(M4,view = c("X_UTM30N","Y_UTM30N"),plot.type = "persp",theta = 180,phi = 0)     

#theta=270
vis.gam(M4, view = c("X_UTM30N","Y_UTM30N"),plot.type = "persp",theta = 270,phi = 0)     


plot(M4, select = 6, ylim = c(-1.5, 1.2))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M4, select=7 ,ylim=c(-0.8,1.5))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v = 13, col = "blue", lty = 2, lwd = 2)

plot(M4, select=8 ,ylim=c(-2,0.5), xlim=c(0,30))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v = 5, col = "blue", lty = 2, lwd = 2)

plot(M4, select=9 ,ylim=c(-0.2,0.2))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M4, select=10 ,ylim=c(-1,1))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M4, select=11 ,ylim=c(-0.5,2))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M4, select=12 ,ylim=c(-1.5,0.7))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M4, select=13 ,ylim=c(-4,1), xlim=c(0,2.5))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v = 0.15, col = "blue", lwd = 2, lty = 2)
abline(v = 1.67, col = "blue", lwd = 2, lty = 2)


par(mfrow = c(2, 3))
dev.off()


#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#Recomiendo borrar todos los objetos que se hayan generado en el Stage 1
#Puede que se produzcan reasignaciones

#STAGE 2 

#1.JUSTIFICACIÓN DE LA FAMILIA GAMMA
file.choose()
forest_fire<-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\dataset_final\\Resultado_final\\forest_fire.csv" , header= TRUE , sep=",")
View(forest_fire)

dataset_superficie<-subset(forest_fire, forest_fire$Area_ha_EGIF>= 1)

#·Soporte positivo: el valor mínimo de Area_ha_EGIF es 1 hectárea.
summary(dataset_superficie$Area_ha_EGIF)

#·Distribución fuertemente asimétrica
#Histograma escala original
hist(dataset_superficie$Area_ha_EGIF,breaks = 200,col = "firebrick2",
     main = "Histograma de Area_ha_EGIF Stage 2",xlab = "Superficie")

hist(dataset_superficie$Area_ha_EGIF,breaks = 20000,col = "firebrick2",
     main = "Histograma de Area_ha_EGIF Stage 2",xlab = "Superficie", xlim=c(0,100))

#Histograma escala log
hist(log(dataset_superficie$Area_ha_EGIF),
     breaks = 200,
     col = "firebrick2",
     border = "white",
     main = "Histograma de log(Area_ha_EGIF) Stage 2",
     xlab = "log(Superficie)")


#·Relación media–varianza ~ cuadrática
y_s2 <- dataset_superficie$Area_ha_EGIF

# Corto por cuantiles (15 grupos)
cuts_s2 <- quantile(y_s2, probs = seq(0, 1, length.out = 16))
cuts_s2 <- unique(cuts_s2)  

# Se asigna bin a cada observación
bin_s2 <- cut(y_s2, breaks = cuts_s2, include.lowest = TRUE, labels = FALSE)


#Media y varianza por bin
mean_bin_s2 <- tapply(y_s2, bin_s2, mean)
variance_bin_s2 <- tapply(y_s2, bin_s2, var)


#Gráfico media-varianza
plot(mean_bin_s2, variance_bin_s2,
     xlab = "Media por bin",
     ylab = "Varianza por bin",
     main = "Relación media-varianza")

data.frame(mean_bin_s2,variance_bin_s2)
#Ajuste pendiente: log ~ log 
coef(lm(log(variance_bin_s2) ~ log(mean_bin_s2)))

#Por tanto, para este caso la mejor opción es utilizar Regresión Gamma con link log,
#ya que Gamma con link log: 
#-Admite asimetría de los residuos
#-Maneja mejor la modelización de varibales que tienen cola larga 
#-No exige normalidad en log.
#-Es consistente con procesos multiplicativos 

#1.2.Ajustamos un modelo_gamma_base para comparar.
#Uso glmmMTB porque, en los modelos de más adelante, glm() no converge.
modelo_gamma_base <- glmmTMB(Area_ha_EGIF ~ 1,
                             data = dataset_superficie,
                             family = Gamma(link = "log"))

summary(modelo_gamma_base)
AIC(modelo_gamma_base)

#_______________________________________________________________________________

#2.SELECCIÓN DE VARIABLES POR LASSO
#La selección de variables se hará sobre logS ya que glmnet no sporta la familia gamma

#2.1.Preparación de los datos
#Eliminamos todo los objetos del enviorement
#Cargamos el dataset
forest_fire <-read.csv("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\dataset_final\\Resultado_final\\forest_fire.csv", header= TRUE , sep=",")
dataset_superficie<-subset(forest_fire, forest_fire$Area_ha_EGIF>= 1)

dataset_superficie$day_of_week<-as.factor(dataset_superficie$day_of_week)
dataset_superficie$Provincia<-as.factor(dataset_superficie$Provincia)
dataset_superficie$burned_before<- as.factor(dataset_superficie$burned_before)
dataset_superficie$MC_SB_grupo <- as.factor(dataset_superficie$MC_SB_grupo)


#Eliminamos las columnas del dataset que no formarán parte del análisis 
dataset_superficie<-subset(dataset_superficie, select = -c(fid, NumeroPart,OM_NumParte, es_incendio, 
                                                           f_detec,season, Municipio,ComarcaIsl,Causa, Motivacion,
                                                           detecpor, detecp_txt, clasedia,cdia_txt, f_llegadapm,
                                                           f_llegadapmae,f_llegadapbh, f_llegadapac, f_control, f_extinción,
                                                           TRI, TRA , TRH, TRAC, TDC, TDE,MC_Scott_Burgan, Sup_Arbola,
                                                           Sup_No_Arb, Sup_total_, Sup_agríc, Otras_Sup_, observ, area_m2, 
                                                           length_m2, Huso, Coordenada_X, Coordenada_Y, NIVELPREE, muertos,
                                                           heridos, valoración_pérdidas, OM_time, hora_, diff_viento_ladera))


# Variable dependiente de superficie
y_s2 <- log(dataset_superficie$Area_ha_EGIF)
length(y_s2)

#Matriz de predictores.
#    Excluye la variable respuesta y cualquier identificador.
#    Usa model.matrix para crear dummies de las categóricas.
x_s2 <- model.matrix(Area_ha_EGIF ~ .,
                     data = dataset_superficie)[, -1]  # quitamos la columna de intercepto

nrow(x_s2)
View(x_s2)



#2.2.Ajustar Lasso lineal con validación cruzada
set.seed(123)
modelo_logS_lasso <- cv.glmnet(x_s2, y_s2,alpha = 1,family = "gaussian",standardize = TRUE,
                               nfolds = 10)

#Para visualizar la curva de CV 
plot(modelo_logS_lasso)


#2.3.Elección de lambda óptimo 
lambda_min_s2  <- modelo_logS_lasso$lambda.min
lambda_1se_s2 <- modelo_logS_lasso$lambda.1se

lambda_min_s2
lambda_1se_s2


#2.4.Obtención del modelo penalizado y de las variables seleccionadas
# Coeficientes del modelo en lambda.min
coef_lasso_s2 <- coef(modelo_logS_lasso, s = "lambda.min")
coef_lasso_s2

#Para obtener los nombres de las variables seleccionadas
vars_seleccionadas_s2 <- rownames(coef_lasso_s2)[coef_lasso_s2[,1] != 0]
vars_seleccionadas_s2

#_______________________________________________________________________________

#3.ANÁLISIS DE LA MULTICOLINEALIDAD 
#Lasso tolera la multicolinealidad
correlation_s2 <- cor(dataset_superficie[ , c("Campania","X_UTM30N","Pendiente",
                                              "FCC",
                                              "prox_caminos", "prox_carreteras",
                                              "OM_temp_min_2m",
                                              "OM_viento_vel_max_10m", "OM_viento_rafagas_max_10m",
                                              "OM_et0",
                                              "OM_lluvia_total",
                                              "OM_cobertura_nubosa_media",
                                              "OM_humedad_rel_media_2m", "OM_humedad_rel_max_2m", "OM_humedad_rel_min_2m",
                                              "OM_viento_vel_media_10m",
                                              "OM_deficit_presion_vapor_max",
                                              "OM_humedad_suelo_media_28_100cm",
                                              "prox_GR", "prox_SH", "prox_TU",
                                              "prox_depositos",
                                              "Orientación_X","Orientación_Y",
                                              "delta_viento_ladera")])


cor_high_s2 <- which(abs(correlation_s2) > 0.7 & abs(correlation_s2) < 1, arr.ind = TRUE)

high_pairs_s2 <- data.frame(Var1 = rownames(correlation_s2)[cor_high_s2[,1]],
                            Var2 = colnames(correlation_s2)[cor_high_s2[,2]],r = correlation_s2[cor_high_s2])
high_pairs_s2

cor.test(dataset_superficie$OM_temp_media_2m,
         dataset_superficie$OM_humedad_rel_media_2m)

#Se observa de nuevo una alta correlación entre variables climáticas. Se escogen
#las más representativas. Ver Stage 1 apartado de multicolinealidad tras Lasso.

#BLOQUE TEMPERATURA 
#Nos quedamos solo con una: OM_temp_media_2m

#BLOQUE HUMEDAD
#Nos quedamos solo co: OM_humedad_rel_media_2m

#BLOQUE VIENTO 
#Son prácticamente todas iguales
#Nos quedamos solo con OM_viento_vel_media_10m

#BLOQUE LLUVIA 
#OM_horas_precipitacion ↔ OM_lluvia_total (0.793)
#Nos quedamos con OM_lluvia_total 

#_______________________________________________________________________________

#4. AJUSTE DE MODELO GAMMA POST LASSO
#Ajustamos un Modelo Logit con las variables seleccionadas por Lasso, teniendo
#en cuenta el análisis de Multicolinealidad
#Se incluye Mes por revsión bibliográfica y análisis descriptivo
#Altitud se incluye por la revisión bibliográfica
#Al incluir Orientación_X, se incluye también Orientación_Y 
#burned_before se incluye por la revisión bibliográfica

modelo_gamma_postlasso <- glm(Area_ha_EGIF ~ Campania + Mes +
                                day_of_week + Provincia + MC_SB_grupo +
                                X_UTM30N + Y_UTM30N + Altitud + Pendiente + FCC +
                                burned_before +
                                prox_caminos + prox_carreteras + dens_poblacional+
                                OM_temp_media_2m +
                                OM_lluvia_total + OM_cobertura_nubosa_media +
                                OM_humedad_rel_media_2m + 
                                OM_viento_vel_media_10m +
                                OM_humedad_suelo_media_28_100cm +
                                prox_GR + prox_SH + prox_TU + proxx_NB
                              prox_depositos +
                                Orientación_X + Orientación_Y +
                                delta_viento_ladera,
                              data = dataset_superficie,
                              family = Gamma(link = "log"))

#No converge.
#Como alternativa se usa glmmTMB
#Al quitar las coordenadas X_UTM30N y Y_UTM30N covariables el GLM Gamma(log) converge. 
#Las coordenadas espaciales no se incluyeron como efectos lineales debido a problemas 
#de convergencia y a la incapacidad de capturar adecuadamente la estructura espacial. 
#En su lugar, se modelaron mediante un término suave bidimensional en un GAM.
#Con prox_observatorios no converge tampoco, se escoge prox_depositos (ro=0.817)

modelo_gamma_postlasso <- glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                    day_of_week + Provincia + MC_SB_grupo +
                                    Altitud + Pendiente + FCC +
                                    burned_before +
                                    prox_caminos + prox_carreteras + dens_poblacional+
                                    OM_temp_media_2m +
                                    OM_lluvia_total + OM_cobertura_nubosa_media +
                                    OM_humedad_rel_media_2m + 
                                    OM_presion_media_superficie +
                                    OM_viento_vel_media_10m +
                                    OM_humedad_suelo_media_28_100cm +
                                    prox_GR + prox_SH + prox_TU + proxx_NB+
                                    prox_depositos +
                                    Orientación_X + Orientación_Y +
                                    delta_viento_ladera,
                                  data = dataset_superficie,
                                  family = Gamma(link = "log"))


summary(modelo_gamma_postlasso)
AIC(modelo_gamma_postlasso)

summary(forest_fire$es_incendio)
#_______________________________________________________________________________

#5.ELIMINACIÓN HACIA ATRÁS (BACKWARD)
#Se eliminan una a una las variables que no son estadísticamente significativas
#Se utilizan AIC/BIC como cfriterios de comparación y selección de modelos.

#Modelo_gamma_postlasso_back_v1: modelo sin day_of_week 
modelo_gamma_postlasso_back_v1 <- glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                            Provincia + MC_SB_grupo +
                                            Altitud + Pendiente + FCC +
                                            burned_before +
                                            prox_caminos + prox_carreteras + dens_poblacional+
                                            OM_temp_media_2m +
                                            OM_lluvia_total + OM_cobertura_nubosa_media +
                                            OM_humedad_rel_media_2m + 
                                            OM_presion_media_superficie +
                                            OM_viento_vel_media_10m +
                                            OM_humedad_suelo_media_28_100cm +
                                            prox_GR + prox_SH + prox_TU + proxx_NB+
                                            prox_depositos +
                                            Orientación_X + Orientación_Y +
                                            delta_viento_ladera,
                                          data = dataset_superficie,
                                          family = Gamma(link = "log"))

summary(modelo_gamma_postlasso_back_v1)
AIC(modelo_gamma_postlasso_back_v1)

anova(modelo_gamma_postlasso, modelo_gamma_postlasso_back_v1, test="Chisq")
#Aunque, AIC aumente, la variable day_of_week se elimina del modelo final 
#por carecer de basefísico-ambiental por parsimonia y por transferibilidad.
#Ya que esta actúa como proxy operativo, lo cual limita transferibiliad del 
#período de entrenamiento a períodos de 2026 en adelante.



#Modelo_gamma_postlasso_back_v2: modelo sin MC_SB_grupo
modelo_gamma_postlasso_back_v2 <- glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                            Provincia + 
                                            Altitud + Pendiente + FCC +
                                            burned_before +
                                            prox_caminos + prox_carreteras + dens_poblacional+
                                            OM_temp_media_2m +
                                            OM_lluvia_total + OM_cobertura_nubosa_media +
                                            OM_humedad_rel_media_2m + 
                                            OM_presion_media_superficie +
                                            OM_viento_vel_media_10m +
                                            OM_humedad_suelo_media_28_100cm +
                                            prox_GR + prox_SH + prox_TU + proxx_NB+
                                            prox_depositos +
                                            Orientación_X + Orientación_Y +
                                            delta_viento_ladera,
                                          data = dataset_superficie,
                                          family = Gamma(link = "log"))


summary(modelo_gamma_postlasso_back_v2)
AIC(modelo_gamma_postlasso_back_v2)


#Modelo_gamma_postlasso_back_v3: modelo sin prox_carreteras
modelo_gamma_postlasso_back_v3 <- glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                            Provincia + 
                                            Altitud + Pendiente + FCC +
                                            burned_before +
                                            prox_caminos + dens_poblacional +
                                            OM_temp_media_2m +
                                            OM_lluvia_total + OM_cobertura_nubosa_media +
                                            OM_humedad_rel_media_2m + 
                                            OM_presion_media_superficie +
                                            OM_viento_vel_media_10m +
                                            OM_humedad_suelo_media_28_100cm +
                                            prox_GR + prox_SH + prox_TU + proxx_NB+
                                            prox_depositos +
                                            Orientación_X + Orientación_Y +
                                            delta_viento_ladera,
                                          data = dataset_superficie,
                                          family = Gamma(link = "log"))


summary(modelo_gamma_postlasso_back_v3)
AIC(modelo_gamma_postlasso_back_v3)



#Modelo_gamma_postlasso_back_v4: modelo sin Pendiente
modelo_gamma_postlasso_back_v4 <- glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                            Provincia + 
                                            Altitud + FCC +
                                            burned_before +
                                            prox_caminos + dens_poblacional +
                                            OM_temp_media_2m +
                                            OM_lluvia_total + OM_cobertura_nubosa_media +
                                            OM_humedad_rel_media_2m + 
                                            OM_presion_media_superficie +
                                            OM_viento_vel_media_10m +
                                            OM_humedad_suelo_media_28_100cm +
                                            prox_GR + prox_SH + prox_TU + proxx_NB+
                                            prox_depositos +
                                            Orientación_X + Orientación_Y +
                                            delta_viento_ladera,
                                          data = dataset_superficie,
                                          family = Gamma(link = "log"))

summary(modelo_gamma_postlasso_back_v4)
AIC(modelo_gamma_postlasso_back_v4)


#Modelo_gamma_postlasso_back_v5: modelo sin OM_cobertura_nubosa_media
modelo_gamma_postlasso_back_v5 <- glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                            Provincia + 
                                            Altitud + FCC +
                                            burned_before +
                                            prox_caminos + dens_poblacional +
                                            OM_temp_media_2m +
                                            OM_lluvia_total + 
                                            OM_humedad_rel_media_2m + 
                                            OM_presion_media_superficie +
                                            OM_viento_vel_media_10m +
                                            OM_humedad_suelo_media_28_100cm +
                                            prox_GR + prox_SH + prox_TU + proxx_NB+
                                            prox_depositos +
                                            Orientación_X + Orientación_Y +
                                            delta_viento_ladera,
                                          data = dataset_superficie,
                                          family = Gamma(link = "log"))



summary(modelo_gamma_postlasso_back_v5)
AIC(modelo_gamma_postlasso_back_v5)

#Modelo_gamma_postlasso_back_v6: modelo sin Altitud
modelo_gamma_postlasso_back_v6 <-  glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                             Provincia + 
                                             FCC +
                                             burned_before +
                                             prox_caminos + dens_poblacional +
                                             OM_temp_media_2m +
                                             OM_lluvia_total + 
                                             OM_humedad_rel_media_2m + 
                                             OM_presion_media_superficie +
                                             OM_viento_vel_media_10m +
                                             OM_humedad_suelo_media_28_100cm +
                                             prox_GR + prox_SH + prox_TU + proxx_NB+
                                             prox_depositos +
                                             Orientación_X + Orientación_Y +
                                             delta_viento_ladera,
                                           data = dataset_superficie,
                                           family = Gamma(link = "log"))

summary(modelo_gamma_postlasso_back_v6)
AIC(modelo_gamma_postlasso_back_v6)




#Modelo_gamma_postlasso_back_v7: modelo sin prox_SH
modelo_gamma_postlasso_back_v7 <- glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                            Provincia + 
                                            FCC +
                                            burned_before +
                                            prox_caminos + dens_poblacional +
                                            OM_temp_media_2m +
                                            OM_lluvia_total + 
                                            OM_humedad_rel_media_2m + 
                                            OM_presion_media_superficie +
                                            OM_viento_vel_media_10m +
                                            OM_humedad_suelo_media_28_100cm +
                                            prox_GR + prox_TU + proxx_NB+
                                            prox_depositos +
                                            Orientación_X + Orientación_Y +
                                            delta_viento_ladera,
                                          data = dataset_superficie,
                                          family = Gamma(link = "log"))


summary(modelo_gamma_postlasso_back_v7)
AIC(modelo_gamma_postlasso_back_v7)




#Modelo_gamma_postlasso_back_v8: modelo sin dens_poblacional
modelo_gamma_postlasso_back_v8 <-  glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                             Provincia + 
                                             FCC +
                                             burned_before +
                                             prox_caminos + 
                                             OM_temp_media_2m +
                                             OM_lluvia_total + 
                                             OM_humedad_rel_media_2m + 
                                             OM_presion_media_superficie +
                                             OM_viento_vel_media_10m +
                                             OM_humedad_suelo_media_28_100cm +
                                             prox_GR + prox_TU + proxx_NB+
                                             prox_depositos +
                                             Orientación_X + Orientación_Y +
                                             delta_viento_ladera,
                                           data = dataset_superficie,
                                           family = Gamma(link = "log"))


summary(modelo_gamma_postlasso_back_v8)
AIC(modelo_gamma_postlasso_back_v8)



#Modelo_gamma_postlasso_back_v9: modelo sin prox_caminos
modelo_gamma_postlasso_back_v9 <- glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                            Provincia + 
                                            FCC +
                                            burned_before +
                                            OM_temp_media_2m +
                                            OM_lluvia_total + 
                                            OM_humedad_rel_media_2m + 
                                            OM_presion_media_superficie +
                                            OM_viento_vel_media_10m +
                                            OM_humedad_suelo_media_28_100cm +
                                            prox_GR + prox_TU + proxx_NB+
                                            prox_depositos +
                                            Orientación_X + Orientación_Y +
                                            delta_viento_ladera,
                                          data = dataset_superficie,
                                          family = Gamma(link = "log"))


summary(modelo_gamma_postlasso_back_v9)
AIC(modelo_gamma_postlasso_back_v9)


#Modelo_gamma_postlasso_back_v10: modelo sin OM_lluvia_total
modelo_gamma_postlasso_back_v10 <- glmmTMB(Area_ha_EGIF ~ Campania + Mes +
                                             Provincia + 
                                             FCC +
                                             burned_before +
                                             OM_temp_media_2m +
                                             OM_humedad_rel_media_2m + 
                                             OM_presion_media_superficie +
                                             OM_viento_vel_media_10m +
                                             OM_humedad_suelo_media_28_100cm +
                                             prox_GR + prox_TU + proxx_NB+
                                             prox_depositos +
                                             Orientación_X + Orientación_Y +
                                             delta_viento_ladera,
                                           data = dataset_superficie,
                                           family = Gamma(link = "log"))

summary(modelo_gamma_postlasso_back_v10)
AIC(modelo_gamma_postlasso_back_v10)


#Modelo_gamma_postlasso_back_v11: modelo sin Campania
modelo_gamma_postlasso_back_v11 <- glmmTMB(Area_ha_EGIF ~ Mes +
                                             Provincia + 
                                             FCC +
                                             burned_before +
                                             OM_temp_media_2m +
                                             OM_humedad_rel_media_2m + 
                                             OM_presion_media_superficie +
                                             OM_viento_vel_media_10m +
                                             OM_humedad_suelo_media_28_100cm +
                                             prox_GR + prox_TU + proxx_NB+
                                             prox_depositos +
                                             Orientación_X + Orientación_Y +
                                             delta_viento_ladera,
                                           data = dataset_superficie,
                                           family = Gamma(link = "log"))

summary(modelo_gamma_postlasso_back_v11)
AIC(modelo_gamma_postlasso_back_v11)

#La variable campaña anual se excluyó del modelo final aunque su inclusión 
#reducía sustancialmente el AIC, debido a que representa un efecto específico 
#del periodo histórico de entrenamiento (2000–2016). Su mantenimiento habría 
#limitado la transferibilidad temporal del modelo a campañas posteriores. 
#Se priorizó capacidad de generalización frente a ajuste dentro de muestra.


#_______________________________________________________________________________

#6.EXTENSIÓN DEL MODELO LINEAL MEDIANTE LA INCLUSIÓN DE INTERACCIONES CON 
#METODOLOGÍA FORWARD 

#Se va a evaluar mediante una metodología forward un conjunto reducido y 
#predefinido de interacciones metereológicas, topográficas y humanas.
#Estas son: 

#OM_temp_media_2m:OM_humedad_rel_media_2m --> SÍ
#Pendiente:OM_viento_vel_media_10m --> NO 
#Pendiente:delta_viento_ladera-->NO
#FCC : OM_temp_media_2m --> SÍ
#OM_temp_media_2m: OM_viento_vel_media_10m -->NO
#OM_viento_vel_media_10m : OM_humedad_rel_media_2m-->NO


#Modelo_gamma_interacciones_v1: se incluye la interacción OM_temp_media_2m:OM_humedad_rel_media_2m
modelo_gamma_interacciones_v1 <- glmmTMB(Area_ha_EGIF ~ Mes +
                                           Provincia + 
                                           FCC +
                                           burned_before +
                                           OM_temp_media_2m +
                                           OM_humedad_rel_media_2m + 
                                           OM_presion_media_superficie +
                                           OM_viento_vel_media_10m +
                                           OM_humedad_suelo_media_28_100cm +
                                           prox_GR + prox_TU + proxx_NB+
                                           prox_depositos +
                                           Orientación_X + Orientación_Y +
                                           delta_viento_ladera +
                                           OM_temp_media_2m:OM_humedad_rel_media_2m ,
                                         data = dataset_superficie,
                                         family = Gamma(link = "log"))


summary(modelo_gamma_interacciones_v1)
AIC(modelo_gamma_interacciones_v1)


#Modelo_gamma_interacciones_v2: se incluye la interacción Pendiente:OM_viento_vel_media_10m
modelo_gamma_interacciones_v2 <-glmmTMB(Area_ha_EGIF ~ Mes +
                                          Provincia + 
                                          FCC +
                                          burned_before +
                                          OM_temp_media_2m +
                                          OM_humedad_rel_media_2m + 
                                          OM_presion_media_superficie +
                                          OM_viento_vel_media_10m +
                                          OM_humedad_suelo_media_28_100cm +
                                          prox_GR + prox_TU + proxx_NB+
                                          prox_depositos +
                                          Orientación_X + Orientación_Y +
                                          delta_viento_ladera +
                                          OM_temp_media_2m:OM_humedad_rel_media_2m +
                                          Pendiente:OM_viento_vel_media_10m ,
                                        data = dataset_superficie,
                                        family = Gamma(link = "log"))


summary(modelo_gamma_interacciones_v2)
AIC(modelo_gamma_interacciones_v2)


#Modelo_gamma_interacciones_v3: se incluye la interacción Pendiente:delta_viento_ladera
modelo_gamma_interacciones_v3 <- glmmTMB(Area_ha_EGIF ~ Mes +
                                           Provincia + 
                                           FCC +
                                           burned_before +
                                           OM_temp_media_2m +
                                           OM_humedad_rel_media_2m + 
                                           OM_presion_media_superficie +
                                           OM_viento_vel_media_10m +
                                           OM_humedad_suelo_media_28_100cm +
                                           prox_GR + prox_TU + proxx_NB+
                                           prox_depositos +
                                           Orientación_X + Orientación_Y +
                                           delta_viento_ladera +
                                           OM_temp_media_2m:OM_humedad_rel_media_2m +
                                           Pendiente:delta_viento_ladera,
                                         data = dataset_superficie,
                                         family = Gamma(link = "log"))



summary(modelo_gamma_interacciones_v3)
AIC(modelo_gamma_interacciones_v3)

#Modelo_gamma_interacciones_v4: se incluye la interacción FCC : OM_temp_media_2m
modelo_gamma_interacciones_v4 <- glmmTMB(Area_ha_EGIF ~ Mes +
                                           Provincia + 
                                           FCC +
                                           burned_before +
                                           OM_temp_media_2m +
                                           OM_humedad_rel_media_2m + 
                                           OM_presion_media_superficie +
                                           OM_viento_vel_media_10m +
                                           OM_humedad_suelo_media_28_100cm +
                                           prox_GR + prox_TU + proxx_NB+
                                           prox_depositos +
                                           Orientación_X + Orientación_Y +
                                           delta_viento_ladera +
                                           OM_temp_media_2m:OM_humedad_rel_media_2m +
                                           FCC : OM_temp_media_2m ,
                                         data = dataset_superficie,
                                         family = Gamma(link = "log"))


summary(modelo_gamma_interacciones_v4)
AIC(modelo_gamma_interacciones_v4)

#Modelo_gamma_interacciones_v5: se incluye la interacción OM_temp_media_2m: OM_viento_vel_media_10m 
modelo_gamma_interacciones_v5 <- glmmTMB(Area_ha_EGIF ~ Mes +
                                           Provincia + 
                                           FCC +
                                           burned_before +
                                           OM_temp_media_2m +
                                           OM_humedad_rel_media_2m + 
                                           OM_presion_media_superficie +
                                           OM_viento_vel_media_10m +
                                           OM_humedad_suelo_media_28_100cm +
                                           prox_GR + prox_TU + proxx_NB+
                                           prox_depositos +
                                           Orientación_X + Orientación_Y +
                                           delta_viento_ladera +
                                           OM_temp_media_2m:OM_humedad_rel_media_2m +
                                           FCC : OM_temp_media_2m +
                                           OM_temp_media_2m: OM_viento_vel_media_10m,
                                         data = dataset_superficie,
                                         family = Gamma(link = "log"))



summary(modelo_gamma_interacciones_v5)
AIC(modelo_gamma_interacciones_v5)

#Modelo_gamma_interacciones_v6: se incluye la interacción OM_viento_vel_media_10m : OM_humedad_rel_media_2m
modelo_gamma_interacciones_v6 <- glmmTMB(Area_ha_EGIF ~ Mes +
                                           Provincia + 
                                           FCC +
                                           burned_before +
                                           OM_temp_media_2m +
                                           OM_humedad_rel_media_2m + 
                                           OM_presion_media_superficie +
                                           OM_viento_vel_media_10m +
                                           OM_humedad_suelo_media_28_100cm +
                                           prox_GR + prox_TU + proxx_NB+
                                           prox_depositos +
                                           Orientación_X + Orientación_Y +
                                           delta_viento_ladera +
                                           OM_temp_media_2m:OM_humedad_rel_media_2m +
                                           FCC : OM_temp_media_2m + 
                                           OM_viento_vel_media_10m : OM_humedad_rel_media_2m ,
                                         data = dataset_superficie,
                                         family = Gamma(link = "log"))

summary(modelo_gamma_interacciones_v6)
AIC(modelo_gamma_interacciones_v6)


#_______________________________________________________________________________

#7.AJUSTE DE MODELOS GAM
#Para el ajuste de los Modelos Aditivos Generalizados (GAM) se considerarán las 
#variables seleccionadas mediante LASSO, teniendo en cuenta el análisis de  
#de multicolinealidad. Además, se incorporarán variables no seleccionadas por 
#LASSO pero relevantes según la bibliografía, ya que su efecto podría ser no 
#lineal y, por tanto, no haber sido detectado adecuadamente en el modelo lineal.

#Además se utilizará select = TRUE. Esto permite que el GAM haga selección 
#automática de predictores, penalizando suavizados irrelevantes hasta hacerlos 
#prácticamente cero.


#Para captar la estructura espacial se utilizará: s(X_UTM30N, Y_UTM30N)
#La orientación es una variable circular. 

#Modelo_gam_gamma_v1
modelo_gam_gamma_v1 <- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
                             Provincia + MC_SB_grupo + 
                             s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
                             s(Altitud) + s(Pendiente) + s(FCC) +
                             burned_before +
                             s(prox_caminos) + s(prox_carreteras) + 
                             s(dens_poblacional) +
                             s(OM_temp_media_2m) +
                             s(OM_lluvia_total) + s(OM_cobertura_nubosa_media) +
                             s(OM_humedad_rel_media_2m) + 
                             s(OM_presion_media_superficie) +
                             s(OM_viento_vel_media_10m) +
                             s(OM_humedad_suelo_media_28_100cm) +
                             s(prox_GR) + s(prox_SH) + s(prox_TU) +
                             s(proxx_NB) +
                             s(prox_depositos) +
                             s(Orientación, bs="cc") +
                             s(OM_viento_direccion_10m, bs="cc") +
                             s(delta_viento_ladera) ,
                           data = dataset_superficie,
                           family = Gamma(link = "log"),method = "REML",
                           select=TRUE, 
                           knots = list(Mes = c(1,12), 
                                        Orientación= c(0,360), 
                                        OM_viento_direccion_10m=c(0,360)))

summary(modelo_gam_gamma_v1)
gam.check(modelo_gam_gamma_v1)
AIC(modelo_gam_gamma_v1)





#Modelo_gam_gamma_v2. #Quitamos variables que no aportan 
#Estas son:
#s(dens_poblacional)      edf≈0.002   p=0.92
#s(OM_lluvia_total)       edf≈0.26    p=0.52
#s(OM_presion_media_superficie) edf≈0.003 p=0.83
#s(prox_depositos)        edf≈0.003   p=0.72


modelo_gam_gamma_v2 <- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
                             Provincia + MC_SB_grupo + 
                             s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
                             s(Altitud) + s(Pendiente) + s(FCC) +
                             burned_before +
                             s(prox_caminos) + s(prox_carreteras) + 
                             s(OM_temp_media_2m) +
                             s(OM_cobertura_nubosa_media) +
                             s(OM_humedad_rel_media_2m) + 
                             s(OM_viento_vel_media_10m) +
                             s(OM_humedad_suelo_media_28_100cm) +
                             s(prox_GR) + s(prox_SH) + s(prox_TU) +
                             s(proxx_NB) +
                             s(Orientación, bs="cc") +
                             s(OM_viento_direccion_10m, bs="cc") +
                             s(delta_viento_ladera) ,
                           data = dataset_superficie,
                           family = Gamma(link = "log"),method = "REML",
                           select=TRUE, 
                           knots = list(Mes = c(1,12), 
                                        Orientación= c(0,360), 
                                        OM_viento_direccion_10m=c(0,360)))


summary(modelo_gam_gamma_v2)
gam.check(modelo_gam_gamma_v2)
AIC(modelo_gam_gamma_v2)


#Modelo_gam_gamma_v3
#Seguimos quitando variables que no aportan:
#MC_SB_grupo
#s(prox_GR)
#s(proxx_NB)
#s(prox_caminos)

modelo_gam_gamma_v3 <- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
                             Provincia +  
                             s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
                             s(Altitud) + s(Pendiente) + s(FCC) +
                             burned_before +
                             s(prox_carreteras) + 
                             s(OM_temp_media_2m) +
                             s(OM_cobertura_nubosa_media) +
                             s(OM_humedad_rel_media_2m) + 
                             s(OM_viento_vel_media_10m) +
                             s(OM_humedad_suelo_media_28_100cm) +
                             s(prox_SH) + s(prox_TU) +
                             s(Orientación, bs="cc") +
                             s(OM_viento_direccion_10m, bs="cc") +
                             s(delta_viento_ladera) ,
                           data = dataset_superficie,
                           family = Gamma(link = "log"),method = "REML",
                           select=TRUE, 
                           knots = list(Mes = c(1,12), 
                                        Orientación= c(0,360), 
                                        OM_viento_direccion_10m=c(0,360)))


summary(modelo_gam_gamma_v3)
gam.check(modelo_gam_gamma_v3)
AIC(modelo_gam_gamma_v3)


#Modelo_gam_gamma_v4
#Quitamos s(prox_SH)
modelo_gam_gamma_v4 <- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
                             Provincia +  
                             s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
                             s(Altitud) + s(Pendiente) + s(FCC) +
                             burned_before +
                             s(prox_carreteras) + 
                             s(OM_temp_media_2m) +
                             s(OM_cobertura_nubosa_media) +
                             s(OM_humedad_rel_media_2m) + 
                             s(OM_viento_vel_media_10m) +
                             s(OM_humedad_suelo_media_28_100cm) +
                             s(prox_TU) +
                             s(Orientación, bs="cc") +
                             s(OM_viento_direccion_10m, bs="cc") +
                             s(delta_viento_ladera) ,
                           data = dataset_superficie,
                           family = Gamma(link = "log"),method = "REML",
                           select=TRUE, 
                           knots = list(Mes = c(1,12), 
                                        Orientación= c(0,360), 
                                        OM_viento_direccion_10m=c(0,360)))


summary(modelo_gam_gamma_v4)
gam.check(modelo_gam_gamma_v4)
AIC(modelo_gam_gamma_v4)


#Modelo_gam_gamma_v5
#Quitamos Provincia
modelo_gam_gamma_v5 <- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
                             s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
                             s(Altitud) + s(Pendiente) + s(FCC) +
                             burned_before +
                             s(prox_carreteras) + 
                             s(OM_temp_media_2m) +
                             s(OM_cobertura_nubosa_media) +
                             s(OM_humedad_rel_media_2m) + 
                             s(OM_viento_vel_media_10m) +
                             s(OM_humedad_suelo_media_28_100cm) +
                             s(prox_TU) +
                             s(Orientación, bs="cc") +
                             s(OM_viento_direccion_10m, bs="cc") +
                             s(delta_viento_ladera) ,
                           data = dataset_superficie,
                           family = Gamma(link = "log"),method = "REML",
                           select=TRUE, 
                           knots = list(Mes = c(1,12), 
                                        Orientación= c(0,360), 
                                        OM_viento_direccion_10m=c(0,360)))


summary(modelo_gam_gamma_v5)
gam.check(modelo_gam_gamma_v5)
AIC(modelo_gam_gamma_v5)


#Modelo_gam_gamma_v6
#Quitamos Pendiente
modelo_gam_gamma_v6 <- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
                             s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
                             s(Altitud) + s(FCC) +
                             burned_before +
                             s(prox_carreteras) + 
                             s(OM_temp_media_2m) +
                             s(OM_cobertura_nubosa_media) +
                             s(OM_humedad_rel_media_2m) + 
                             s(OM_viento_vel_media_10m) +
                             s(OM_humedad_suelo_media_28_100cm) +
                             s(prox_TU) +
                             s(Orientación, bs="cc") +
                             s(OM_viento_direccion_10m, bs="cc") +
                             s(delta_viento_ladera) ,
                           data = dataset_superficie,
                           family = Gamma(link = "log"),method = "REML",
                           select=TRUE, 
                           knots = list(Mes = c(1,12), 
                                        Orientación= c(0,360), 
                                        OM_viento_direccion_10m=c(0,360)))


summary(modelo_gam_gamma_v6)
gam.check(modelo_gam_gamma_v6)
AIC(modelo_gam_gamma_v6)
#Mantenemos Pendiente, v5 es el modelo final 



#_______________________________________________________________________________
#8.SELECCIÓN DE MODELOS CANDIDATOS Y COMPARACIÓN DE MODELOS
#Seleccionamos como modelos candidatos los siguientes:
M5 <-modelo_gamma_base
M6 <-modelo_gamma_postlasso_back_v11
M7 <-modelo_gamma_interacciones_v1
M8 <- modelo_gam_gamma_v5

#Al principio se selccionó como M7 modelo_gamma_interacciones_v4, pero explotaba 
#la multicolinealidad por las interacciones. Por tanto, se continúo como M7 con
#el modelo_gamma_interacciones_v1

#Para la selcción del modelo final utilizaremos los siguientes criterios: AIC,
#BIC, % Deviance explained, RMSE, MAE, RMSE_log, MAE_log

#Respuesta
y_s2<-dataset_superficie$Area_ha_EGIF

#Superficie predicha (ha)
p_M5<- predict(M5, type="response") 
p_M6<- predict(M6, type="response")
p_M7<- predict(M7, type="response")
p_M8<- predict(M8, type="response")


#AIC
AIC(M5, M6, M7, M8)

#BIC
BIC(M5, M6, M7, M8)

#Deviance explained(%)
DEV_M5 <- 0 #por definición
DEV_M6 <- (1 - deviance(M6) / deviance(M5)) * 100
DEV_M7 <- (1 - deviance(M7) / deviance(M5)) * 100
DEV_M8 <- (1 - deviance(M8) / deviance(M5)) * 100
DEV_M8
summary(M8)$dev.expl * 100

#RMSE #sqrt(mean((y − ŷ)^2))
RMSE_M5<-sqrt(mean((y_s2 - p_M5)^2))
RMSE_M6<-sqrt(mean((y_s2 - p_M6)^2))
RMSE_M7<-sqrt(mean((y_s2 - p_M7)^2))
RMSE_M8<-sqrt(mean((y_s2 - p_M8)^2))

#RMSE_log  sqrt(mean((log_y − log_ŷ)^2))
RMSE_log_M5<-sqrt(mean((log10(y_s2) - log10(p_M5))^2))
RMSE_log_M6<-sqrt(mean((log10(y_s2) - log10(p_M6))^2))
RMSE_log_M7<-sqrt(mean((log10(y_s2) - log10(p_M7))^2))
RMSE_log_M8<-sqrt(mean((log10(y_s2) - log10(p_M8))^2))

#MAE mean(abs(y − ŷ))
MAE_M5<-mean(abs(y_s2-p_M5))
MAE_M6<-mean(abs(y_s2-p_M6))
MAE_M7<-mean(abs(y_s2-p_M7))
MAE_M8<-mean(abs(y_s2-p_M8))

#MAE_log mean(abs(logy − logŷ))
MAE_log_M5<-mean(abs(log10(y_s2)-log10(p_M5)))
MAE_log_M6<-mean(abs(log10(y_s2)-log10(p_M6)))
MAE_log_M7<-mean(abs(log10(y_s2)-log10(p_M7)))
MAE_log_M8<-mean(abs(log10(y_s2)-log10(p_M8)))


#Resultados en forma de tabla 
M_s2<-c("M5", "M6", "M7","M8")
AIC_s2<-c(AIC(M5), AIC(M6), AIC(M7), AIC(M8))
BIC_s2<-c(BIC(M5), BIC(M6), BIC(M7), BIC(M8))           
DEV_s2<-c(DEV_M5,DEV_M6,DEV_M7, DEV_M8)      
RMSE_s2<-c(RMSE_M5, RMSE_M6, RMSE_M7, RMSE_M8)
RMSE_log_s2<-c(RMSE_log_M5, RMSE_log_M6, RMSE_log_M7, RMSE_log_M8)
MAE_s2<-c(MAE_M5,MAE_M6,MAE_M7,MAE_M8)
MAE_log_s2<-c(MAE_log_M5, MAE_log_M6, MAE_log_M7, MAE_log_M8)

resultados_M_Stage_2<-data.frame(M_s2, AIC_s2, BIC_s2, DEV_s2, RMSE_s2, RMSE_log_s2, MAE_s2, MAE_log_s2)
View(resultados_M_Stage_2)

#Se seleccionaron como modelos candidatos finales el modelo GAM (M8) y el modelo
#Gamma con interacciones (M7) 

#M7
M7 <- glmmTMB(Area_ha_EGIF ~ Mes +
                Provincia + 
                FCC +
                burned_before +
                OM_temp_media_2m +
                OM_humedad_rel_media_2m + 
                OM_presion_media_superficie +
                OM_viento_vel_media_10m +
                OM_humedad_suelo_media_28_100cm +
                prox_GR + prox_TU + proxx_NB+
                prox_depositos +
                Orientación_X + Orientación_Y +
                delta_viento_ladera +
                OM_temp_media_2m:OM_humedad_rel_media_2m +
                FCC : OM_temp_media_2m ,
              data = dataset_superficie,
              family = Gamma(link = "log"))


#M8

M8<- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
           s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
           s(Altitud) + s(Pendiente) + s(FCC) +
           burned_before +
           s(prox_carreteras) + 
           s(OM_temp_media_2m) +
           s(OM_cobertura_nubosa_media) +
           s(OM_humedad_rel_media_2m) + 
           s(OM_viento_vel_media_10m) +
           s(OM_humedad_suelo_media_28_100cm) +
           s(prox_TU) +
           s(Orientación, bs="cc") +
           s(OM_viento_direccion_10m, bs="cc") +
           s(delta_viento_ladera) ,
         data = dataset_superficie,
         family = Gamma(link = "log"),method = "REML",
         select=TRUE, 
         knots = list(Mes = c(1,12), 
                      Orientación= c(0,360), 
                      OM_viento_direccion_10m=c(0,360)))




#_______________________________________________________________________________
#9.VALIDACIÓN Y COMPARACIÓN DE MODELOS DEL STAGE 2


#9.A.Diagnóstico del ajuste para M7 (glmmTMB familia gamma)

#Multicolinealidad #vif no funciona para glmmTMB
#Calculemos GVIF y GVIF(1/(2*df)) se calculan de
# matriz de diseño
mm_M7 <- model.matrix(M7)

# quito intercepto
X_M7 <- mm_M7[, -1, drop = FALSE]

# asigno las columnas a términos
assign_M7 <- attr(mm_M7, "assign")[-1]

# nombres de términos
terms_M7 <- attr(terms(M7), "term.labels")

# matriz de correlación e inversa
R_M7 <- cor(X_M7)
Rinv_M7 <- solve(R_M7)

# objetos de salida
gvif_M7 <- numeric(length(terms_M7))
df_M7 <- numeric(length(terms_M7))

for (i in seq_along(terms_M7)) {cols_M7 <- which(assign_M7 == i)df_M7[i] <- length(cols_M7)
submat_M7 <- Rinv_M7[cols_M7, cols_M7, drop = FALSE]gvif_M7[i] <- det(submat_M7)^(1 / df_M7[i])}

gvif_adj_M7 <- gvif_M7^(1 / (2 * df_M7))
gvif_table_M7 <- data.frame(Term = terms_M7,Df = df_M7,GVIF = gvif_M7,GVIF_1_2Df = gvif_adj_M7)
gvif_table_M7



#Residuos (response)
res_M7_resp <- y_s2 - p_M7
res_M7_resp
summary(res_M7_resp)

#Residuos
res_M7_dev <- residuals(M7, type = "deviance")
res_M7_dev
res_M7_pear <- residuals(M7, type = "pearson")
res_M7_pear

#Residuos pearson vs ajustados
plot(p_M7, res_M7_pear,xlab = "Ajustados (ha) - M7",ylab = "Residuos Pearson - M7",main = "M7: residuos Pearson vs ajustados")
abline(h = 0, lty = 2)


#Histograma + QQ-plot (deviance)
hist(res_M7_dev,breaks = 30,main = "M7: histograma residuos deviance",xlab = "Residuo deviance")
qqnorm(res_M7_dev,main = "M7: QQ plot residuos deviance")
qqline(res_M7_dev, lty = 2)


# Residuos Pearson vs predictores clave 
dat<-dataset_superficie

plot(dat$Mes, res_M7_pear, xlab = "Mes", ylab = "Residuo Pearson", main = "M7: Mes"); abline(h = 0, lty = 2)
plot(dat$FCC, res_M7_pear, xlab = "FCC", ylab = "Residuo Pearson", main = "M7: FCC"); abline(h = 0, lty = 2)
plot(dat$OM_temp_media_2m, res_M7_pear, xlab = "Temp (2m)", ylab = "Residuo Pearson", main = "M7: Temp"); abline(h = 0, lty = 2)
plot(dat$OM_humedad_rel_media_2m, res_M7_pear, xlab = "HR (2m)", ylab = "Residuo Pearson", main = "M7: HR"); abline(h = 0, lty = 2)
plot(dat$OM_presion_media_superficie, res_M7_pear, xlab = "Presión", ylab = "Residuo Pearson", main = "M7: Presión superficie"); abline(h = 0, lty = 2)
plot(dat$OM_viento_vel_media_10m, res_M7_pear, xlab = "Viento vel (10m)", ylab = "Residuo Pearson", main = "M7: Viento"); abline(h = 0, lty = 2)
plot(dat$OM_humedad_suelo_media_28_100cm, res_M7_pear, xlab = "Hum. suelo", ylab = "Residuo Pearson", main = "M7: Hum suelo"); abline(h = 0, lty = 2)
plot(dat$prox_GR, res_M7_pear, xlab = "Prox GR", ylab = "Residuo Pearson", main = "M7: prox_GR"); abline(h = 0, lty = 2)
plot(dat$prox_TU, res_M7_pear, xlab = "Prox TU", ylab = "Residuo Pearson", main = "M7: prox_TU"); abline(h = 0, lty = 2)
plot(dat$proxx_NB, res_M7_pear, xlab = "Prox NB", ylab = "Residuo Pearson", main = "M7: prox_NB"); abline(h = 0, lty = 2)
plot(dat$prox_depositos, res_M7_pear, xlab = "Prox depósitos", ylab = "Residuo Pearson", main = "M7: prox_depositos"); abline(h = 0, lty = 2)
plot(dat$Orientación_X, res_M7_pear, xlab = "Orientación X", ylab = "Residuo Pearson", main = "M7: Orientación X"); abline(h = 0, lty = 2)
plot(dat$Orientación_Y, res_M7_pear, xlab = "Orientación Y", ylab = "Residuo Pearson", main = "M7: Orientación Y"); abline(h = 0, lty = 2)
plot(dat$delta_viento_ladera, res_M7_pear, xlab = "Delta viento ladera", ylab = "Residuo Pearson", main = "M7: Delta viento"); abline(h = 0, lty = 2)

boxplot(res_M7_pear ~ dat$Provincia,xlab = "Provincia", ylab = "Residuo Pearson", main = "M7: Provincia")
abline(h = 0, lty = 2)

boxplot(res_M7_pear ~ dat$burned_before,xlab = "Burned before", ylab = "Residuo Pearson", main = "M7: Burned before")
abline(h = 0, lty = 2)


#Diagnóstico DHARMa M7 
sim_M7 <- simulateResiduals(fittedModel = M7, n = 1000)

plot(sim_M7)
testUniformity(sim_M7)
testDispersion(sim_M7)
testOutliers(sim_M7)

#Observaciones influyentes: 73 incendios
length(which(abs(res_M7_pear) > 3))



#9.B.Diagnóstico del ajuste para M8 (gam familia gamma)

#Diágnostico específico del GAM
#Resumen del GAM: deviance explained, edf y significancia
summary(M8)
#Diagnóstico GAM: k-index y residuos
gam.check(M8)
#Concurvidad
concurvity(M8, full = TRUE)

#Altitud y Temperatura tienen alta concurvidad. Hacemos un análisis de sensibilidad
M8_noAlt<- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
                 s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
                 s(Pendiente) + s(FCC) +
                 burned_before +
                 s(prox_carreteras) + 
                 s(OM_temp_media_2m) +
                 s(OM_cobertura_nubosa_media) +
                 s(OM_humedad_rel_media_2m) + 
                 s(OM_viento_vel_media_10m) +
                 s(OM_humedad_suelo_media_28_100cm) +
                 s(prox_TU) +
                 s(Orientación, bs="cc") +
                 s(OM_viento_direccion_10m, bs="cc") +
                 s(delta_viento_ladera) ,
               data = dataset_superficie,
               family = Gamma(link = "log"),method = "REML",
               select=TRUE, 
               knots = list(Mes = c(1,12), 
                            Orientación= c(0,360), 
                            OM_viento_direccion_10m=c(0,360)))


M8_noTemp<- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
                  s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
                  s(Altitud) + s(Pendiente) + s(FCC) +
                  burned_before +
                  s(prox_carreteras) + 
                  s(OM_cobertura_nubosa_media) +
                  s(OM_humedad_rel_media_2m) + 
                  s(OM_viento_vel_media_10m) +
                  s(OM_humedad_suelo_media_28_100cm) +
                  s(prox_TU) +
                  s(Orientación, bs="cc") +
                  s(OM_viento_direccion_10m, bs="cc") +
                  s(delta_viento_ladera) ,
                data = dataset_superficie,
                family = Gamma(link = "log"),method = "REML",
                select=TRUE, 
                knots = list(Mes = c(1,12), 
                             Orientación= c(0,360), 
                             OM_viento_direccion_10m=c(0,360)))

#Comparamos estabilidad de edf en cada término, Deviance expained y AIC.
summary(M8_noAlt) #respecto a M8 cambian edf de Pendiente los demás no cambian y sus curvas de suavizado tampoco
summary(M8_noTemp) #respecto a M8 no cambian edf 
AIC(M8, M8_noAlt, M8_noTemp)
concurvity(M8_noAlt) #La temperatura sigue teniendo un estimate > 0.7
concurvity(M8_noTemp)#La altitud sigue teniendo un estimate > 0.7
#Conclusión: se mantienen tanto altitud como temperatura, M8 sigue siendo el modelo candidato



#Residuos (mgcv)
res_M8_dev  <- residuals(M8, type = "deviance")
res_M8_pear <- residuals(M8, type = "pearson")

#Residuos response (para comparar con M7)
res_M8_resp <- y_s2 - p_M8
summary(res_M8_resp)

#Residuos pearson vs ajustados
plot(p_M8, res_M8_pear,xlab = "Ajustados (ha) - M8",ylab = "Residuos Pearson - M8",main = "M8: residuos Pearson vs ajustados")
abline(h = 0, lty = 2)


#Histograma + QQ-plot (deviance)
hist(res_M8_dev,breaks = 30, main = "M8: histograma residuos deviance", xlab = "Residuo deviance")
qqnorm(res_M8_dev,main = "M8: QQ plot residuos deviance")
qqline(res_M8_dev, lty = 2)

# Residuos Pearson vs predictores clave 
plot(dat$Mes, res_M8_pear, xlab = "Mes", ylab = "Residuo Pearson", main = "M8: Mes"); abline(h=0,lty=2)
plot(dat$X_UTM30N, res_M8_pear, xlab = "X UTM30N", ylab = "Residuo Pearson", main = "M8: X_UTM30N"); abline(h=0,lty=2)
plot(dat$Y_UTM30N, res_M8_pear, xlab = "Y UTM30N", ylab = "Residuo Pearson", main = "M8: Y_UTM30N"); abline(h=0,lty=2)
plot(dat$Altitud, res_M8_pear, xlab = "Altitud", ylab = "Residuo Pearson", main = "M8: Altitud"); abline(h=0,lty=2)
plot(dat$Pendiente, res_M8_pear, xlab = "Pendiente", ylab = "Residuo Pearson", main = "M8: Pendiente"); abline(h=0,lty=2)
plot(dat$FCC, res_M8_pear, xlab = "FCC", ylab = "Residuo Pearson", main = "M8: FCC"); abline(h=0,lty=2)
plot(dat$prox_carreteras, res_M8_pear, xlab = "Prox carreteras", ylab = "Residuo Pearson", main = "M8: prox_carreteras"); abline(h=0,lty=2)
plot(dat$OM_temp_media_2m, res_M8_pear, xlab = "Temp (2m)", ylab = "Residuo Pearson", main = "M8: Temp"); abline(h=0,lty=2)
plot(dat$OM_cobertura_nubosa_media, res_M8_pear, xlab = "Cobertura nubosa", ylab = "Residuo Pearson", main = "M8: Cobertura nubosa"); abline(h=0,lty=2)
plot(dat$OM_humedad_rel_media_2m, res_M8_pear, xlab = "HR (2m)", ylab = "Residuo Pearson", main = "M8: HR"); abline(h=0,lty=2)
plot(dat$OM_viento_vel_media_10m, res_M8_pear, xlab = "Viento vel (10m)", ylab = "Residuo Pearson", main = "M8: Viento"); abline(h=0,lty=2)
plot(dat$OM_humedad_suelo_media_28_100cm, res_M8_pear, xlab = "Hum. suelo", ylab = "Residuo Pearson", main = "M8: Hum suelo"); abline(h=0,lty=2)
plot(dat$prox_TU, res_M8_pear, xlab = "Prox TU", ylab = "Residuo Pearson", main = "M8: prox_TU"); abline(h=0,lty=2)
plot(dat$Orientación, res_M8_pear, xlab = "Orientación", ylab = "Residuo Pearson", main = "M8: Orientación"); abline(h=0,lty=2)
plot(dat$OM_viento_direccion_10m, res_M8_pear, xlab = "Dirección viento (10m)", ylab = "Residuo Pearson", main = "M8: Dirección viento"); abline(h=0,lty=2)
plot(dat$delta_viento_ladera, res_M8_pear, xlab = "Delta viento ladera", ylab = "Residuo Pearson", main = "M8: Delta viento"); abline(h=0,lty=2)
boxplot(res_M8_pear ~ dat$burned_before,
        xlab="Burned before", ylab="Residuo Pearson", main="M8: Burned before")
abline(h=0,lty=2)


#Diagnóstico DHARMa M8
sim_M8 <- simulateResiduals(fittedModel = M8, n = 1000)

plot(sim_M8)
testUniformity(sim_M8)
testDispersion(sim_M8)
testOutliers(sim_M8)

#Observaciones influyentes
which(abs(res_M8_pear) > 3)




#9.2.2. Validación out of sample (K-Fold espacial por bloques)

#9.2.2.A Test de I-Moran por umbral de distancia

#Obtenemos la distancia que asegura la conectividad de los grafos
#Si los grafos no están conectados, los resultados del Test no son interpretables
#Regla:
#Si nc = 1 → grafo totalmente conectado 
#Si nc > 1 → hay subgrafos (fragmentación)

#Coordenadas UTM (metros)
coords_s2 <- cbind(dataset_superficie$X_UTM30N, dataset_superficie$Y_UTM30N)

#3km
nb_3km_s2 <- dnearneigh(coords_s2, 0, 3000)
n.comp.nb(nb_3km_s2)$nc        
# número de componentes (subgrafos): 323


#6km
nb_6km_s2 <- dnearneigh(coords_s2, 0, 6000)
n.comp.nb(nb_6km_s2)$nc        
# número de componentes (subgrafos): 66


#10km
nb_10km_s2 <- dnearneigh(coords_s2, 0, 10000)
n.comp.nb(nb_10km_s2)$nc        
# número de componentes (subgrafos): 11


#17,5km
nb_17_5_km_s2 <- dnearneigh(coords_s2, 0, 17500)
n.comp.nb(nb_17_5_km_s2)$nc        
# número de componentes (subgrafos): 2


#18 km
nb_18km_s2 <- dnearneigh(coords, 0, 18000)
n.comp.nb(nb_18km_s2)$nc        
# número de componentes (subgrafos): 1




#3)Distancia máxima (en metros): la mínima que asegura conectividad de los grafos
dmax_s2 <- 18000

# 4) Vecindad y pesos
nb_s2 <- dnearneigh(coords_s2, d1 = 0, d2 = dmax_s2, longlat = FALSE)
lw_s2 <- nb2listw(nb_s2, style = "W", zero.policy = TRUE)


# 5) Test de Moran, versión por permutaciones (más robusta)
#Uso los residuos de response para poder comparar 

#M7
res_M7_resp <- y_s2 - p_M7
set.seed(123)
moran.mc(res_M7_resp, lw_s2, nsim = 999, zero.policy = TRUE)

# M8
res_M8_resp <- y_s2 - p_M8
set.seed(123)
moran.mc(res_M8_resp, lw_s2, nsim = 999, zero.policy = TRUE)



#9.2.2.B. Validación por bloques espaciales 
set.seed(123)
df_s2 <- dataset_superficie
yname_s2 <- "Area_ha_EGIF"
coords_s2 <- cbind(df_s2$X_UTM30N, df_s2$Y_UTM30N)

# Creamos los bloques espaciales (40 km x 40 km)
pts_s2 <- st_as_sf(df_s2, coords = c("X_UTM30N","Y_UTM30N"), crs = 25830)
grid_s2 <- st_make_grid(st_bbox(pts_s2), cellsize = 40000, square = TRUE)
grid_sf_s2 <- st_sf(cell_id_s2 = seq_along(grid_s2), geometry = grid_s2)

pts_join_s2 <- st_join(pts_s2, grid_sf_s2, join = st_within)

df_s2$cell_id_s2 <- pts_join_s2$cell_id_s2
df_s2 <- df_s2[!is.na(df_s2$cell_id_s2), ]
y_s2  <- df_s2$Area_ha_EGIF

# Asignamos las celdas del grid a cada fold
cells_s2 <- sort(unique(df_s2$cell_id_s2))
fold_id_s2 <- sample(rep(1:5, length.out = length(cells_s2)))

cell2fold_s2 <- data.frame(cell_id_s2 = cells_s2, fold_s2 = fold_id_s2)
df_s2 <- df_s2 %>% left_join(cell2fold_s2, by = "cell_id_s2")

# puntos por fold
print(table(df_s2$fold_s2))

# Preparo las fórmulas y los knots
form_M7_s2 <- formula(M7)
form_M8_s2 <- formula(M8)
knots_M8_s2 <- list(Mes = c(1, 12),Orientación = c(0, 360),OM_viento_direccion_10m = c(0, 360))

# CV 5-fold
cv_rows_s2 <- data.frame()

for(k_s2 in 1:5){
  
  train_s2 <- df_s2[df_s2$fold_s2 != k_s2, ]
  test_s2  <- df_s2[df_s2$fold_s2 == k_s2, ]
  
  # Ajuste M7 (glmmTMB Gamma)
  fit_M7_s2 <- glmmTMB(
    form_M7_s2,
    data   = train_s2,
    family = Gamma(link = "log")
  )
  
  # Ajuste M8 (GAM Gamma)
  fit_M8_s2 <- gam(
    form_M8_s2,
    data   = train_s2,
    family = Gamma(link = "log"),
    method = "REML",
    select = TRUE,
    knots  = knots_M8_s2
  )
  
  # Predicciones en test (escala original, ha)
  p7_s2 <- predict(fit_M7_s2, newdata = test_s2, type = "response")
  p8_s2 <- predict(fit_M8_s2, newdata = test_s2, type = "response")
  
  y_test_s2 <- test_s2$Area_ha_EGIF
  
  # Protección numérica para log
  eps_s2 <- .Machine$double.eps
  p7c_s2 <- pmax(p7_s2, eps_s2)
  p8c_s2 <- pmax(p8_s2, eps_s2)
  
  # Métricas (por fold)
  RMSE7_s2 <- sqrt(mean((y_test_s2 - p7_s2)^2))
  RMSE8_s2 <- sqrt(mean((y_test_s2 - p8_s2)^2))
  
  MAE7_s2  <- mean(abs(y_test_s2 - p7_s2))
  MAE8_s2  <- mean(abs(y_test_s2 - p8_s2))
  
  RMSElog7_s2 <- sqrt(mean((log10(y_test_s2) - log10(p7c_s2))^2))
  RMSElog8_s2 <- sqrt(mean((log10(y_test_s2) - log10(p8c_s2))^2))
  
  MAElog7_s2  <- mean(abs(log10(y_test_s2) - log10(p7c_s2)))
  MAElog8_s2  <- mean(abs(log10(y_test_s2) - log10(p8c_s2)))
  
  # Guardar resultados
  cv_rows_s2 <- rbind(
    cv_rows_s2,
    data.frame(
      fold_s2 = k_s2,
      model_s2 = "M7",
      RMSE_s2 = RMSE7_s2,
      MAE_s2  = MAE7_s2,
      RMSE_log_s2 = RMSElog7_s2,
      MAE_log_s2  = MAElog7_s2
    ),
    data.frame(
      fold_s2 = k_s2,
      model_s2 = "M8",
      RMSE_s2 = RMSE8_s2,
      MAE_s2  = MAE8_s2,
      RMSE_log_s2 = RMSElog8_s2,
      MAE_log_s2  = MAElog8_s2))}

# Resultados por fold y resumen final
print(cv_rows_s2)
print(cv_rows_s2)

cv_summary_s2 <- aggregate(cbind(RMSE_s2, MAE_s2, RMSE_log_s2, MAE_log_s2) ~ model_s2,
                           data = cv_rows_s2, FUN = function(x) c(mean = mean(x), sd = sd(x)))

cv_summary_s2


#_______________________________________________________________________________

#10. CALIBRACIÓN DEL MODELO

#Sesgo medio de predicción
summary(res_M8_resp)

#Calibración por percentiles
y_s2   <- dataset_superficie$Area_ha_EGIF
eps <- 1e-6

cal_M8 <- data.frame(y = y_s2, p = p_M8) %>%
  mutate(bin = ntile(p, 100)) %>%
  group_by(bin) %>%
  summarise(
    p_mean = mean(p, na.rm = TRUE),
    y_mean = mean(y, na.rm = TRUE),
    n = n(),
    .groups = "drop")

print(cal_M8, n=100)


#Gráfico
ggplot(cal_M8, aes(x = p_mean, y = y_mean)) +
  geom_point(size = 1.5) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(
    title = "Curva de calibración (M8, 100 bins)",
    x = "Superficie predicha por percentil",
    y = "Superficie observada por percentil")


#Intercepto y pendiente de la recta de calibración
cal_slope_M8 <- lm(y_s2 ~ p_M8)
summary(cal_slope_M8)
coef(cal_slope_M8)
confint(cal_slope_M8)


#_______________________________________________________________________________ 

#11. SELECCIÓN DEL MODELO FINAL E INTERPRETACIÓN DE LAS CURVAS DE SUAVIZADO
summary(M8)

plot(M8, select=1 ,ylim=c(-1,1.2))
abline(h = 0, col = "red", lty = 2, lwd = 2)


#Para s(X_UTM30N, Y_UTM30N) siendo theta el giro horizontal y phi la inclinación del gráfico
#theta=0
vis.gam(M8,
        view = c("X_UTM30N","Y_UTM30N"),
        plot.type = "persp",
        theta = 0,   
        phi = 0)     

#theta=90
vis.gam(M8,
        view = c("X_UTM30N","Y_UTM30N"),
        plot.type = "persp",
        theta = 90,   
        phi = 0)     

#theta=180
vis.gam(M8,
        view = c("X_UTM30N","Y_UTM30N"),
        plot.type = "persp",
        theta = 180,  
        phi = 0)    

#theta=270
vis.gam(M8,
        view = c("X_UTM30N","Y_UTM30N"),
        plot.type = "persp",
        theta = 270,   
        phi = 0)     

plot(M8, select=3 ,ylim=c(-2,2.5))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v = 300, col = "blue", lty = 2, lwd = 2)
abline(v = 1080, col = "blue", lty = 2, lwd = 2)

plot(M8, select=4 ,ylim=c(-2.5,0.8))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M8, select=5 ,ylim=c(-2.5,0.8))
abline(h = 0, col = "red", lty = 2, lwd = 2)


plot(M8, select=6 ,ylim=c(-2.5,0.8))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v = 2.3, col = "blue", lty = 2, lwd = 2)

plot(M8, select=7 ,ylim=c(-2,3))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v = 17.5, col = "blue", lty = 2, lwd = 2)

plot(M8, select=8 ,ylim=c(-2,0.5))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v = 17.5, col = "blue", lty = 2, lwd = 2)

plot(M8, select=9 ,ylim=c(-1,1.7))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M8, select=10 ,ylim=c(-1,3))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M8, select=11 ,ylim=c(-2.5,1))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M8, select=12 ,ylim=c(-3.5,1.5))
abline(h = 0, col = "red", lty = 2, lwd = 2)
abline(v = 0.35, col = "blue", lty = 2, lwd = 2)
abline(v = 1.65, col = "blue", lty = 2, lwd = 2)


plot(M8, select=13 ,ylim=c(-0.8,0.8))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M8, select=14 ,ylim=c(-0.8,0.8))
abline(h = 0, col = "red", lty = 2, lwd = 2)

plot(M8, select=15 ,ylim=c(-0.6,0.6))
abline(h = 0, col = "red", lty = 2, lwd = 2)


summary(M4)
par(mfrow = c(2, 3))
dev.off()

#_______________________________________________________________________________
#12.ANÁLSIS DE SENSIBILIDAD SIN EL OUTLIER DE CORTES DE PALLÀS DE 2012
dataset_superficie[dataset_superficie$Area_ha_EGIF > 30000, ]
30691.39 /sum(forest_fire$Area_ha_EGIF)

#Este incendio supone el 27,33% de la superficie incendiada entre 2000 y 2016 en 
#la Comunidad Valenciana para los incendios estudiados.



dataset_superficie_sin_outlier <- dataset_superficie[dataset_superficie$Area_ha_EGIF <= 30000, ]
summary(dataset_superficie_sin_outlier$Area_ha_EGIF)

M8_sin_outlier <- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
                        s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
                        s(Altitud) + s(Pendiente) + s(FCC) +
                        burned_before +
                        s(prox_carreteras) + 
                        s(OM_temp_media_2m) +
                        s(OM_cobertura_nubosa_media) +
                        s(OM_humedad_rel_media_2m) + 
                        s(OM_viento_vel_media_10m) +
                        s(OM_humedad_suelo_media_28_100cm) +
                        s(prox_TU) +
                        s(Orientación, bs="cc") +
                        s(OM_viento_direccion_10m, bs="cc") +
                        s(delta_viento_ladera) ,
                      data = dataset_superficie_sin_outlier,
                      family = Gamma(link = "log"),method = "REML",
                      select=TRUE, 
                      knots = list(Mes = c(1,12), 
                                   Orientación= c(0,360), 
                                   OM_viento_direccion_10m=c(0,360)))

summary(M8_sin_outlier)


#Métricas in-sample
dataset_superficie_sin_outlier$Area_ha_EGIF
p_M8_sin_outlier <- predict(M8_sin_outlier, type = "response")


RMSE_M8_sin_outlier <- sqrt(mean((dataset_superficie_sin_outlier$Area_ha_EGIF - p_M8_sin_outlier)^2))
RMSE_log_M8_sin_outlier<-log(RMSE_M8_sin_outlier)

MAE_M8_sin_outlier<-mean((abs(dataset_superficie_sin_outlier$Area_ha_EGIF - p_M8_sin_outlier)))
MAE_log_M8_sin_outlier<-log(MAE_M8_sin_outlier)

resultados_M_Stage_2_sin_outlier <- matrix(
  c(RMSE_M8_sin_outlier, 
    MAE_M8_sin_outlier, RMSE_log_M8_sin_outlier, MAE_log_M8_sin_outlier),
  nrow = 1,
  dimnames = list("M8_sin_outlier", c("RMSE", "MAE", "RMSE_log", "MAE_log")))

resultados_M_Stage_2_sin_outlier                                            


#Residuos en escala response
res_M8_sin_outlier<-dataset_superficie_sin_outlier$Area_ha_EGIF - p_M8_sin_outlier
summary(res_M8_sin_outlier)
summary(dataset_superficie_sin_outliers$Area_ha_EGIF)


#Análisis convencional de los residuos 
res_dev_M8_sin_outlier <- residuals(M8_sin_outlier, type = "deviance")
res_pearson_M8_sin_outlier <- residuals(M8_sin_outlier, type = "pearson")

hist(res_dev_M8_sin_outlier,breaks = 30, main = "Histograma de residuos deviance M8_sin_outlier",
     xlab = "Residuos deviance")

qqnorm(res_dev_M8_sin_outlier,main = "QQ plot de residuos deviance M8_sin_outlier")
qqline(res_dev_M8_sin_outlier, lwd = 2, col = "red")


plot(p_M8_sin_outlier, res_pearson_M8_sin_outlier ,xlab = "Ajustados (ha) - M8",
     ylab = "Residuos Pearson - M8",main = "M8_sin_outlier: residuos Pearson vs ajustados")
abline(h = 0, lty = 2)


#DHARMa
# Simulación de residuos
res_M8_sin_outlier_DHARMa <- simulateResiduals(fittedModel = M8_sin_outlier,n = 1000)

# Diagnóstico gráfico general
plot(res_M8_sin_outlier_DHARMa)

# Tests estadísticos
testUniformity(res_M8_sin_outlier_DHARMa)
testDispersion(res_M8_sin_outlier_DHARMa)
testOutliers(res_M8_sin_outlier_DHARMa)

#Comparación de los histogramas de las predicciones con los modelos 
#entrenados con y sin el outlier
hist(p_M8,breaks=250, main="Predicciones modelo M8 (ha)")
summary(p_M8)

hist(pred_M8_sin_outlier, breaks= 250, main="Predicciones M8 modelo sin outlier(ha)")
summary(pred_M8_sin_outlier)


#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#.OBTENCIÓN DE LOS RASTERS DE LOS EFECTOS ESPACIALES S(x,y) EN FORMATO .tif
#El objetivo de este apartado es obtener las curvas de suavizado de los efectos
#espaciales en formato .tif para su interpretación en QGIS.


# FUNCIÓN para la creación de un raster del efecto espacial

crear_raster_efecto_espacial <- function(modelo, datos,
                                         x = "X_UTM30N",
                                         y = "Y_UTM30N",
                                         resolucion = 1000,
                                         archivo_salida = "efecto_espacial.tif") {
  
  # Extensión de los datos
  xmin <- min(datos[[x]], na.rm = TRUE)
  xmax <- max(datos[[x]], na.rm = TRUE)
  ymin <- min(datos[[y]], na.rm = TRUE)
  ymax <- max(datos[[y]], na.rm = TRUE)
  
  # Malla regular
  xs <- seq(xmin, xmax, by = resolucion)
  ys <- seq(ymin, ymax, by = resolucion)
  grid <- expand.grid(X_UTM30N = xs, Y_UTM30N = ys)
  
  names(grid)[1] <- x
  names(grid)[2] <- y
  
  # Variables que usa el modelo
  vars_modelo <- all.vars(formula(modelo))[-1]
  
  # Crear newdata con valores fijos para el resto de variables
  newdata <- data.frame(matrix(nrow = nrow(grid), ncol = 0))
  
  for (v in vars_modelo) {
    if (v %in% names(grid)) {
      newdata[[v]] <- grid[[v]]
    } else {
      if (is.factor(datos[[v]])) {
        newdata[[v]] <- factor(levels(datos[[v]])[1], levels = levels(datos[[v]]))
      } else {
        newdata[[v]] <- median(datos[[v]], na.rm = TRUE)
      }
    }
  }
  
  # Predicción de términos del modelo
  pred_terms <- predict(modelo, newdata = newdata, type = "terms")
  
  # Nombres de términos 
  print(colnames(pred_terms))
  
  # Extraer el término espacial
  nombre_termino <- paste0("s(", x, ",", y, ")")
  efecto <- pred_terms[, nombre_termino]
  
  # Añadir efecto al grid
  grid$efecto <- efecto
  
  # Convertir a raster
  r <- rast(grid, type = "xyz", crs = "EPSG:25830")
  
  # Guardar
  writeRaster(r, archivo_salida, overwrite = TRUE)
  
  return(r)}


# Para M4
r_M4 <- crear_raster_efecto_espacial(modelo = M4, datos = forest_fire,
                                     x = "X_UTM30N",y = "Y_UTM30N",resolucion = 1000,
                                     archivo_salida = "M4_efecto_espacial.tif")

plot(r_M4)


# Para M8
r_M8 <- crear_raster_efecto_espacial(modelo = M8,datos = dataset_superficie,
                                     x = "X_UTM30N",y = "Y_UTM30N",resolucion = 1000,
                                     archivo_salida = "M8_efecto_espacial.tif")

plot(r_M8)


#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#Generación de mapas predictivos para 31/07/2026. Estamos a 15/05/2026. 

#Primero cargamos los modelos finales
#M4: GAM-Logit 
forest_fire$MC_SB_grupo <- as.factor(forest_fire$MC_SB_grupo)

M4 <- gam(es_incendio ~ s(Mes, bs="cc", k=12) + 
            s(Pendiente) + 
            s(prox_caminos) + 
            s(prox_carreteras) + 
            s(X_UTM30N,Y_UTM30N, bs="tp", k=200) + 
            MC_SB_grupo + 
            s(prox_TU) +
            s(OM_temp_media_2m) + 
            s(OM_lluvia_total) + 
            s(OM_cobertura_nubosa_media) + 
            s(OM_humedad_rel_media_2m) +
            s(OM_viento_vel_media_10m) + 
            s(OM_humedad_suelo_media_28_100cm)+
            s(proxx_NB)
          ,data= forest_fire,
          family = binomial(link = "logit"),method = "REML", 
          select = TRUE,
          knots = list(Mes = c(1,12)))

summary(M4)

#Las covariables del modelo M4 son:
# Mes
# Pendiente
# prox_caminos
# prox_carreteras
# X_UTM30N
# Y_UTM30N
# MC_SB_grupo  (es una variable categórica)
# prox_TU
# OM_temp_media_2m
# OM_lluvia_total
# OM_cobertura_nubosa_media
# OM_humedad_rel_media_2m
# OM_viento_vel_media_10m
# OM_humedad_suelo_media_28_100cm
# proxx_NB


#M8: GAM-gamma
dataset_superficie$burned_before<- as.factor(dataset_superficie$burned_before)

M8<- gam(Area_ha_EGIF ~ s(Mes, bs="cc", k=12) +
           s(X_UTM30N, Y_UTM30N, bs="tp",k=80) +
           s(Altitud) + s(Pendiente) + s(FCC) +
           burned_before +
           s(prox_carreteras) + 
           s(OM_temp_media_2m) +
           s(OM_cobertura_nubosa_media) +
           s(OM_humedad_rel_media_2m) + 
           s(OM_viento_vel_media_10m) +
           s(OM_humedad_suelo_media_28_100cm) +
           s(prox_TU) +
           s(Orientación, bs="cc") +
           s(OM_viento_direccion_10m, bs="cc") +
           s(delta_viento_ladera) ,
         data = dataset_superficie,
         family = Gamma(link = "log"),method = "REML",
         select=TRUE, 
         knots = list(Mes = c(1,12), 
                      Orientación= c(0,360), 
                      OM_viento_direccion_10m=c(0,360)))




summary(M8)


#Las covariables del modelo M8 son:
# Mes
# X_UTM30N
# Y_UTM30N
# Altitud
# Pendiente
# FCC
# burned_before
# prox_carreteras
# OM_temp_media_2m
# OM_cobertura_nubosa_media
# OM_humedad_rel_media_2m
# OM_viento_vel_media_10m
# OM_humedad_suelo_media_28_100cm
# prox_TU
# Orientación
# OM_viento_direccion_10m
# delta_viento_ladera


#_______________________________________________________________________________

#Cargamos rasters procesados en QGIS
file.choose()
# RASTER BASE: referencia para extensión del mapa predictivo

RASTER_BASE <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\RASTER_BASE.tif")
MC_SB_grupo <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\MC_SB_grupo.tif")
Mes <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\Mes.tif")
X_UTM30N <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\X_UTM30N.tif")
Y_UTM30N <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\Y_UTM30N.tif")
Altitud <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\Altitud.tif")
Pendiente <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\Pendiente.tif")
Orientación <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\Orientación.tif")
FCC <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\FCC.tif")
prox_carreteras <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\prox_carreteras.tif")
prox_caminos <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\prox_caminos.tif")
prox_TU <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\prox_TU.tif")
proxx_NB <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\prox_NB.tif")
burned_before <-rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\burned_before.tif")
OM_temp_media_2m <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\OM_temp_media_2m.tif")
OM_lluvia_total <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\OM_lluvia_total.tif")
OM_cobertura_nubosa_media <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\OM_cobertura_nubosa_media.tif")
OM_humedad_rel_media_2m <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\OM_humedad_rel_media_2m.tif")
OM_viento_vel_media_10m <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\OM_viento_vel_media_10m.tif")
OM_humedad_suelo_media_28_100cm <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\OM_humedad_suelo_media_28_100cm.tif")
u <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\u.tif")
v <- rast("C:\\Users\\aguia\\OneDrive\\Escritorio\\Obtención_de_datos\\prediccion\\Capas_a_500m_alienadas\\v.tif")



#_______________________________________________________________________________
#Recortar y enmascarar todas las capas con raster base 

# Función auxiliar para adaptar cualquier raster al RASTER_BASE y gestionar NoData
ajustar_raster <- function(r) {
  # Convertir NoData
  r[r == -9999] <- NA
  
  # Ajustar al raster base
  r <- crop(r, RASTER_BASE)
  r <- mask(r, RASTER_BASE)
  
  return(r)}



#_______________________________________________________________________________
#Aplicamos la función a todas las capas 

MC_SB_grupo <- ajustar_raster(MC_SB_grupo)
Mes <- ajustar_raster(Mes)
X_UTM30N <- ajustar_raster(X_UTM30N)
Y_UTM30N <- ajustar_raster(Y_UTM30N)
Altitud <- ajustar_raster(Altitud)
Pendiente <- ajustar_raster(Pendiente)
Orientación <- ajustar_raster(Orientación)
FCC <- ajustar_raster(FCC)
prox_carreteras <- ajustar_raster(prox_carreteras)
prox_caminos <- ajustar_raster(prox_caminos)
prox_TU <- ajustar_raster(prox_TU)
proxx_NB <- ajustar_raster(proxx_NB)
burned_before <- ajustar_raster(burned_before)
OM_temp_media_2m <- ajustar_raster(OM_temp_media_2m)
OM_lluvia_total <- ajustar_raster(OM_lluvia_total)
OM_cobertura_nubosa_media <- ajustar_raster(OM_cobertura_nubosa_media)
OM_humedad_rel_media_2m <- ajustar_raster(OM_humedad_rel_media_2m)
OM_viento_vel_media_10m <- ajustar_raster(OM_viento_vel_media_10m)
OM_humedad_suelo_media_28_100cm <- ajustar_raster(OM_humedad_suelo_media_28_100cm)
u <- ajustar_raster(u)
v <- ajustar_raster(v)


#Comparamos las geometrías para asegurar que ha salido todo correcto 
compareGeom(
  RASTER_BASE,
  MC_SB_grupo, Mes, X_UTM30N, Y_UTM30N, Altitud, Pendiente,
  Orientación, FCC, prox_carreteras, prox_caminos, prox_TU,
  proxx_NB, burned_before, OM_temp_media_2m, OM_lluvia_total,
  OM_cobertura_nubosa_media, OM_humedad_rel_media_2m,
  OM_viento_vel_media_10m, OM_humedad_suelo_media_28_100cm,
  u, v,
  stopOnError = FALSE)

unique(values(burned_before))
unique(values(MC_SB_grupo))


#Calculamos los rasters de las covariables 
#OM_viento_direccion_10mdelta_viento_ladera
OM_viento_direccion_10m <- (atan2(-u, -v) * 180 / pi + 360) %% 360

names(OM_viento_direccion_10m) <- "OM_viento_direccion_10m"

#delta_viento_ladera
delta_viento_ladera <- ifel(abs(Orientación - OM_viento_direccion_10m) > 180,
                            360 - abs(Orientación - OM_viento_direccion_10m),
                            abs(Orientación - OM_viento_direccion_10m))

names(delta_viento_ladera) <- "delta_viento_ladera"


#Comprobamos rangos y comparamos geometría
global(OM_viento_direccion_10m, c("min", "max"), na.rm = TRUE)
global(delta_viento_ladera, c("min", "max"), na.rm = TRUE)


compareGeom(RASTER_BASE,OM_viento_direccion_10m,delta_viento_ladera,stopOnError = FALSE)


plot(OM_viento_direccion_10m)
plot(delta_viento_ladera)



#_______________________________________________________________________________

#Pasamos los rasters de burned_before y MC_SB_grupo a factor raster
levels(forest_fire$MC_SB_grupo)
levels(dataset_superficie$burned_before)

MC_SB_grupo <- as.factor(MC_SB_grupo)
burned_before <- as.factor(burned_before)
levels(MC_SB_grupo)
levels(burned_before)

#_______________________________________________________________________________

#Preparamos stacks de nombres para la predicción.
names(delta_viento_ladera) <- "delta_viento_ladera"
names(OM_viento_direccion_10m) <- "OM_viento_direccion_10m"
names(proxx_NB) <- "proxx_NB"

covs_M4 <- c(Mes,Pendiente,prox_caminos,prox_carreteras,X_UTM30N,Y_UTM30N,
             MC_SB_grupo,prox_TU,OM_temp_media_2m,OM_lluvia_total,
             OM_cobertura_nubosa_media,OM_humedad_rel_media_2m,
             OM_viento_vel_media_10m,OM_humedad_suelo_media_28_100cm,proxx_NB)



covs_M8 <- c(Mes,X_UTM30N,Y_UTM30N,Altitud,Pendiente,FCC,burned_before,
             prox_carreteras,OM_temp_media_2m,OM_cobertura_nubosa_media,
             OM_humedad_rel_media_2m,OM_viento_vel_media_10m,
             OM_humedad_suelo_media_28_100cm,prox_TU,Orientación,
             OM_viento_direccion_10m,delta_viento_ladera)

names(covs_M4)
names(covs_M8)

levels(covs_M4$MC_SB_grupo)
levels(covs_M8$burned_before)

#_______________________________________________________________________________
#Stage 1 
pred_M4 <- predict(covs_M4, M4, type = "response")
plot(pred_M4)

#Stage 2 
pred_M8 <- predict(covs_M8, M8, type = "response")
plot(pred_M8)

#Riesgo final
riesgo_final <- pred_M4 * pred_M8
plot(riesgo_final)






#Me guardo los rasters
writeRaster(delta_viento_ladera, "C:/Users/aguia/OneDrive/Escritorio/delta_viento_ladera.tif",overwrite = TRUE)
writeRaster(pred_M4, "C:/Users/aguia/OneDrive/Escritorio/pred_M4.tif",overwrite = TRUE)
writeRaster(pred_M8,"C:/Users/aguia/OneDrive/Escritorio/pred_M8.tif",overwrite = TRUE)
writeRaster(riesgo_final,"C:/Users/aguia/OneDrive/Escritorio/riesgo_final.tif",overwrite = TRUE)



#FIN :)



#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________


#ANEXO DEL CÓDIGO
#Prueba con otras distribuciones para el Stage 2 para abordar el problema de los
#eventos extremos


#DISTRIBUCIÓN TWEEDIE
#glmMTB no convergía así que lo he hecho directamente con mgcv

M8_twd <- gam(Area_ha_EGIF ~ s(Mes, bs = "cc", k = 12) +
                s(X_UTM30N, Y_UTM30N, bs = "tp", k = 80) + s(Altitud) + s(Pendiente) + 
                s(FCC) +burned_before + s(prox_carreteras) + s(OM_temp_media_2m) +
                s(OM_cobertura_nubosa_media) +s(OM_humedad_rel_media_2m) + 
                s(OM_viento_vel_media_10m) +s(OM_humedad_suelo_media_28_100cm) +
                s(prox_TU) +s(Orientación, bs = "cc") +s(OM_viento_direccion_10m, bs = "cc") +
                s(delta_viento_ladera), data = dataset_superficie,family = tw(link = "log"),
              method = "REML",select = TRUE,knots = list(Mes = c(1, 12),Orientación = c(0, 360),
                                                         OM_viento_direccion_10m = c(0, 360)))

summary(M8_twd)

summary(res_M8_resp)
summary(y_s2-p_twd)

#Similares resultados a gamma
y_s2 <- dataset_superficie$Area_ha_EGIF
p_twd <- predict(M8_twd, type = "response")
RMSE_twd <- sqrt(mean((y_s2 - p_twd)^2))
RMSE_twd

summary(res_M8_resp)
summary(y_s2-p_twd)

#DHARMa 
sim_M8_twd <- simulateResiduals(fittedModel = M8_twd, n = 1000)
plot(sim_M8_twd)
testUniformity(sim_M8_twd)
testDispersion(sim_M8_twd)
testOutliers(sim_M8_twd)

#Resultados similares que con distribución gamma, pero M8_twd no predice outliers
#a diferencia de M8.


#LOG (SUPERFICIE CON FAMILIA GAUSSIANA)
M8_log <- gam(log(Area_ha_EGIF) ~ s(Mes, bs = "cc", k = 12) +
                s(X_UTM30N, Y_UTM30N, bs = "tp", k = 80) +s(Altitud) +s(Pendiente) +
                s(FCC) +burned_before +s(prox_carreteras) +s(OM_temp_media_2m) +
                s(OM_cobertura_nubosa_media) +s(OM_humedad_rel_media_2m) +
                s(OM_viento_vel_media_10m) +s(OM_humedad_suelo_media_28_100cm) +
                s(prox_TU) +s(Orientación, bs = "cc") +s(OM_viento_direccion_10m, bs = "cc") +
                s(delta_viento_ladera),data = dataset_superficie, family = gaussian(),
              method = "REML",select = TRUE,knots = list(Mes = c(1, 12),Orientación = c(0, 360),
                                                         OM_viento_direccion_10m = c(0, 360)))


summary(M8_log)

p_log <- predict(M8_log, type = "response")
p_area <- exp(p_log)
y_s2<- dataset_superficie$Area_ha_EGIF
RMSE_log <- sqrt(mean((y_s2 - p_area)^2))
RMSE_log

sim_M8_log <- simulateResiduals(fittedModel = M8_log, n = 1000)
plot(sim_M8_log)
testUniformity(sim_M8_log)
testDispersion(sim_M8_log)
testOutliers(sim_M8_log)


#LOG(LOG(S+1))
M8_log_mas_1 <- gam(log(log(Area_ha_EGIF)+1) ~ s(Mes, bs = "cc", k = 12) +
                      s(X_UTM30N, Y_UTM30N, bs = "tp", k = 80) +s(Altitud) +s(Pendiente) +
                      s(FCC) +burned_before +s(prox_carreteras) +s(OM_temp_media_2m) +
                      s(OM_cobertura_nubosa_media) +s(OM_humedad_rel_media_2m) +
                      s(OM_viento_vel_media_10m) +s(OM_humedad_suelo_media_28_100cm) +
                      s(prox_TU) +s(Orientación, bs = "cc") +s(OM_viento_direccion_10m, bs = "cc") +
                      s(delta_viento_ladera),
                    data = dataset_superficie,family = gaussian(),method = "REML",select = TRUE,
                    knots = list(Mes = c(1, 12),Orientación = c(0, 360),
                                 OM_viento_direccion_10m = c(0, 360)))


summary(M8_log_mas_1)

p_loglog <- predict(M8_log_mas_1, type = "response")
p_area <- exp(exp(p_loglog) - 1)
res_M8_log_mas_1 <- y_s2 - p_area
rmse_M8_log_mas_1 <- sqrt(mean(res_M8_log_mas_1^2))
RMSE_M8_log_mas_1


#DHARMa
res_M8_log_mas_1_DHARMa <- simulateResiduals(fittedModel = M8_log_mas_1,n = 1000)
plot(res_M8_log_mas_1_DHARMa)
testUniformity(res_M8_log_mas_1_DHARMa)
testDispersion(res_M8_log_mas_1_DHARMa)
testOutliers(res_M8_log_mas_1_DHARMa)





#WEIBULL
library(survival)

M8_weibull <- survreg(Surv(Area_ha_EGIF) ~ Mes + X_UTM30N + Y_UTM30N +
                        Altitud +Pendiente +FCC +burned_before +prox_carreteras +
                        OM_temp_media_2m +OM_cobertura_nubosa_media +OM_humedad_rel_media_2m +
                        OM_viento_vel_media_10m +OM_humedad_suelo_media_28_100cm +
                        prox_TU +Orientación +OM_viento_direccion_10m + delta_viento_ladera,
                      data = dataset_superficie,
                      dist = "weibull")

summary(M8_weibull)

p_M8_weibull <- predict(M8_weibull, type = "response")
res_M8_weibull <- y_s2 - pred_M8_weibull

RMSE_M8_weibull <- sqrt(mean(res_M8_weibull^2))
RMSE_M8_weibull


#SCAT
M8_scat <- gam(Area_ha_EGIF ~ s(Mes, bs = "cc", k = 12) +
                 s(X_UTM30N, Y_UTM30N, bs = "tp", k = 80) +
                 s(Altitud) +
                 s(Pendiente) +
                 s(FCC) +
                 burned_before +
                 s(prox_carreteras) +
                 s(OM_temp_media_2m) +
                 s(OM_cobertura_nubosa_media) +
                 s(OM_humedad_rel_media_2m) +
                 s(OM_viento_vel_media_10m) +
                 s(OM_humedad_suelo_media_28_100cm) +
                 s(prox_TU) +
                 s(Orientación, bs = "cc") +
                 s(OM_viento_direccion_10m, bs = "cc") +
                 s(delta_viento_ladera),
               data = dataset_superficie,
               family = scat(link = "log"),
               method = "REML",
               select = TRUE,
               knots = list(
                 Mes = c(1, 12),
                 Orientación = c(0, 360),
                 OM_viento_direccion_10m = c(0, 360)))


RMSE_scat<-sqrt(mean((y_s2-predict(M8_scat,type="response"))^2))
RMSE_scat

summary(M8_scat)

#DHARMa
sim_M8_scat <- simulateResiduals(M8_scat)
plot(sim_M8_scat)
testUniformity(sim_M8_scat)
testDispersion(sim_M8_scat)
testOutliers(sim_M8_scat)


