setwd("D:/respaldo-ADATA/volatilidad_secciones-main")
rm(list=ls(all=TRUE))
options(scipen = 999)
###----------------
library(pacman)
p_load(purrr, dplyr,tidyverse, haven, tibble, sjlabelled,janitor, 
       readr, ggplot2, readxl, openxlsx, foreign, sf)
#-------
gob_2024 <- read.xlsx("GTO_GUB_2024.xlsx")
pres_2024 <- read.xlsx("presidente-2024.xlsx")
alcaldes_2024 <- read.xlsx("alcaldes-gto-2024.xlsx")
#-----------------------
gob_2024 <- gob_2024|>group_by(SECCION)|>
  summarize(PAN=sum(PAN,na.rm = T),
            PRI=sum(PRI,na.rm = T),
            PRD=sum(PRD,na.rm = T),
            PVEM=sum(PVEM,na.rm = T),
            PT=sum(PT,na.rm = T),
            MC=sum(MC,na.rm = T),
            MORENA=sum(MORENA,na.rm = T),
            PAN_PRI_PRD=sum(PAN_PRI_PRD,na.rm = T),
            PAN_PRI=sum(PAN_PRI,na.rm = T),
            PAN_PRD=sum(PAN_PRD,na.rm = T),
            PRI_PRD=sum(PRI_PRD,na.rm = T),
            PVEM_PT_MORENA=sum(PVEM_PT_MORENA,na.rm = T),
            PT_MORENA=sum(PT_MORENA,na.rm = T),
            PVEM_MORENA=sum(PVEM_MORENA,na.rm = T),
            PVEM_PT=sum(PVEM_PT,na.rm = T),
            CAND_IND_1=sum(CAND_IND_1,na.rm = T),
            NoReg=sum(NoReg,na.rm = T),
            Nulos=sum(Nulos,na.rm = T),
            TOTAL_VOTOS=sum(TOTAL_VOTOS,na.rm = T),.groups = "drop")

#-----
pres_gto_2024 <- pres_2024|>filter(ENTIDAD=="GUANAJUATO")
pres_gto_2024$SECCION <- as.integer(pres_gto_2024$SECCION)
rm(pres_2024)


colnames(pres_gto_2024)
pres_gto_2024 <- pres_gto_2024|>mutate(across(6:24,as.numeric))


pres_gto_2024 <- pres_gto_2024|>group_by(ENTIDAD,SECCION)|>
  summarize(PAN=sum(PAN,na.rm = T),
            PRI=sum(PRI,na.rm = T),
            PRD=sum(PRD,na.rm = T),
            PVEM=sum(PVEM,na.rm = T),
            PT=sum(PT,na.rm = T),
            MC=sum(MC,na.rm = T),
            MORENA=sum(MORENA,na.rm = T),
            PAN_PRI_PRD=sum(PAN_PRI_PRD,na.rm = T),
            PAN_PRI=sum(PAN_PRI,na.rm = T),
            PAN_PRD=sum(PAN_PRD,na.rm = T),
            PRI_PRD=sum(PRI_PRD,na.rm = T),
            PVEM_PT_MORENA=sum(PVEM_PT_MORENA,na.rm = T),
            PVEM_PT=sum(PVEM_PT,na.rm = T),
            PVEM_MORENA=sum(PVEM_MORENA,na.rm = T),
            PT_MORENA=sum(PT_MORENA,na.rm = T),
            CNR=sum(CNR,na.rm = T),
            VN=sum(VN,na.rm = T),
            TOTAL_VOTOS_CALCULADO=sum(TOTAL_VOTOS_CALCULADOS,na.rm = T),
            LISTA_NOMINAL=sum(LISTA_NOMINAL,na.rm = T),.groups = "drop")


#-------------------------
alcaldes_2024 <- alcaldes_2024|>dplyr::select(!starts_with("P_"))
colnames(alcaldes_2024)


alcaldes_2024 <- alcaldes_2024|>group_by(SECCION)|>
  summarize(PAN=sum(PAN,na.rm = T),
            PRI=sum(PRI,na.rm = T),
            PRD=sum(PRD,na.rm = T),
            PVEM=sum(PVEM,na.rm = T),
            PT=sum(PT,na.rm = T),
            MC=sum(MC,na.rm = T),
            MORENA=sum(MORENA,na.rm = T),
            PAN_PRI_PRD=sum(PAN_PRI_PRD,na.rm = T),
            PAN_PRI=sum(PAN_PRI,na.rm = T),
            PAN_PRD=sum(PAN_PRD,na.rm = T),
            PRI_PRD=sum(PRI_PRD,na.rm = T),
            PVEM_PT_MORENA=sum(PVEM_PT_MORENA,na.rm = T),
            PT_MORENA=sum(PT_MORENA,na.rm = T),
            PVEM_MORENA=sum(PVEM_MORENA,na.rm = T),
            PVEM_PT=sum(PVEM_PT,na.rm = T),
            CAND_IND_1=sum(CAND_IND_1,na.rm = T),
            NoReg=sum(NoReg,na.rm = T),
            Nulos=sum(Nulos,na.rm = T),
            TOTAL_VOTOS=sum(TOTAL_VOTOS,na.rm = T),
            LISTA_NOMINAL=sum(LISTA_NOMINAL,na.rm = T),.groups = "drop")

#---------------------------------
secciones_gto <- read_sf("D:/respaldo-ADATA/volatilidad_secciones-main/cartografias/SECCION-2024-ED.geojson")
secciones_gto <- secciones_gto|>filter(ENTIDAD==11)

table(secciones_gto$TIPO)
#----------------------------------------------------------------
alcaldes_2024 <- inner_join(alcaldes_2024,secciones_gto,
                            by=c("SECCION"))
pres_gto_2024 <- inner_join(pres_gto_2024,secciones_gto,
                            by=c("SECCION"))
gob_2024 <- inner_join(gob_2024,secciones_gto,
                       by=c("SECCION"))
#--------------------------------------------
#alcaldes_2024 <- alcaldes_2024|>filter(MUNICIPIO==15)
#pres_gto_2024 <- pres_gto_2024|>filter(MUNICIPIO==15)
#gob_2024 <- gob_2024|>filter(MUNICIPIO==15)
#----------------------------------
#Sumando voto x alianza y partido
#---Presidencial
pres_gto_2024 <- pres_gto_2024|>mutate(VOTO_CLAUDIA=MORENA+PT+PVEM+PVEM_PT_MORENA+PVEM_PT+PVEM_MORENA+PT_MORENA,
                                       VOTO_XOCHITL=PAN+PRI+PRD+PAN_PRI_PRD+PAN_PRI+PAN_PRD+PRI_PRD,
                                       VOTO_JAM=MC)
#---Gobernador
gob_2024 <- gob_2024|>mutate(VOTO_LIBIA=PAN+PRI+PRD+PAN_PRI_PRD+PAN_PRI+PAN_PRD+PRI_PRD,
                             VOTO_ALMA=MORENA+PVEM+PT+PVEM_PT_MORENA+PVEM_MORENA+PVEM_PT+PT_MORENA,
                             VOTO_YULMA=MC)
#---Alcalde-Guanajuato
alcaldes_2024 <- alcaldes_2024|>mutate(VOTO_SAMATHA=PAN+PRI+PRD+PAN_PRI_PRD+PAN_PRI+PAN_PRD,
                                       VOTO_JORGE=MORENA,
                                       VOTO_LILIANA=MC,
                                       VOTO_PALOMA=PT,
                                       VOTO_ROBERTO=PVEM)
#----Calculando porcentaje
pres_gto_2024 <- pres_gto_2024|>mutate(PORC_CLAUDIA=VOTO_CLAUDIA*100/TOTAL_VOTOS_CALCULADO,
                                       PORC_XOCHITL=VOTO_XOCHITL*100/TOTAL_VOTOS_CALCULADO,
                                       PORC_JAM=VOTO_JAM*100/TOTAL_VOTOS_CALCULADO,
                                       PORC_NULOS_PRES=VN*100/TOTAL_VOTOS_CALCULADO,
                                       PORC_NOREG_PRES=CNR*100/TOTAL_VOTOS_CALCULADO)
#---Gobernador
gob_2024 <- gob_2024|>mutate(PORC_LIBIA=VOTO_LIBIA*100/TOTAL_VOTOS,
                             PORC_ALMA=VOTO_ALMA*100/TOTAL_VOTOS,
                             PORC_YULMA=VOTO_YULMA*100/TOTAL_VOTOS,
                             PORC_NULOS_GOB=Nulos*100/TOTAL_VOTOS,
                             PORC_NOREG_GOB=NoReg*100/TOTAL_VOTOS,
                             PORC_IND_GOB=CAND_IND_1*100/TOTAL_VOTOS)
#---Alcalde-Guanajuato
alcaldes_2024 <- alcaldes_2024|>mutate(PORC_SAMATHA=VOTO_SAMATHA*100/TOTAL_VOTOS,
                                       PORC_JORGE=VOTO_JORGE*100/TOTAL_VOTOS,
                                       PORC_LILIANA=VOTO_LILIANA*100/TOTAL_VOTOS,
                                       PORC_PALOMA=VOTO_PALOMA*100/TOTAL_VOTOS,
                                       PORC_ROBERTO=VOTO_ROBERTO*100/TOTAL_VOTOS,
                                       PORC_NULOS_GTO=Nulos*100/TOTAL_VOTOS,
                                       PORC_NOREG_GTO=NoReg*100/TOTAL_VOTOS,
                                       PORC_IND_GTO=CAND_IND_1*100/TOTAL_VOTOS)


#-----------------------------
pres_gto_2024 <- pres_gto_2024|>dplyr::select(ENTIDAD.x,ENTIDAD.y,SECCION,starts_with("PORC_"),starts_with("VOTO"))
gob_2024 <- gob_2024|>dplyr::select(ENTIDAD,SECCION,starts_with("PORC_"),starts_with("VOTO"))
alcaldes_2024 <- alcaldes_2024|>dplyr::select(ENTIDAD,SECCION,starts_with("PORC_"),starts_with("VOTO"))
#-------------------------
pres_gto_2024 <- pres_gto_2024|>rename(ENTIDAD="ENTIDAD.x",
                                       ID_ENTIDAD="ENTIDAD.y")

gob_2024 <- gob_2024|>rename(ID_ENTIDAD="ENTIDAD")
alcaldes_2024 <- alcaldes_2024|>rename(ID_ENTIDAD="ENTIDAD")
#--Votacion alcaldía Guanajuato
votacion_guanajuato <- inner_join(pres_gto_2024,
                                  gob_2024,
                                  by=c("ID_ENTIDAD","SECCION"))

votacion_guanajuato <- inner_join(votacion_guanajuato,
                                  alcaldes_2024,
                                  by=c("ID_ENTIDAD","SECCION"))

rm(alcaldes_2024,gob_2024,pres_gto_2024)
#-----------
colnames(votacion_guanajuato)
#------IDENTIFICANDO A LOS GANADORES X ELECCION EN CADA SECCION 
votacion_guanajuato <- votacion_guanajuato|>mutate(PORC_GANADOR_PRES = apply(votacion_guanajuato[4:8], 1, max, na.rm=TRUE),
                                                   PORC_GANADOR_GOB = apply(votacion_guanajuato[12:17], 1, max, na.rm=TRUE),
                                                   PORC_GANADOR_MUN= apply(votacion_guanajuato[21:28], 1, max, na.rm=TRUE))


votacion_guanajuato

#Cálculo de estratos socioeconómicos
#----------------------------------
datos_eceg <- read.xlsx("D:/mapas-coyoacan/eceg_2020_csv/conjunto_de_datos/INE_SECCION_2020.xlsx")

datos_eceg <- datos_eceg|>rename(
  ID_ENTIDAD="ENTIDAD",
  ID_DTO="DISTRITO")

names(datos_eceg)
#-----------------
datos_eceg_guanajuato <- datos_eceg|>filter(ID_ENTIDAD==11&MUNICIPIO==15)
datos_eceg_guanajuato <- datos_eceg_guanajuato|>dplyr::select(5,7:226)

colnames(datos_eceg_guanajuato)
glimpse(datos_eceg_guanajuato)
#-------------------------
#SECCIONES <- read_sf("H:/Mi unidad/mapas-coyoacan/SECCIONES.geojson")
#MUNICIPIO <- read_sf("H:/Mi unidad/mapas-coyoacan/MUNICIPIO/MUNICIPIO.geojson")
#--------
intersect(names(datos_eceg_guanajuato),names(secciones_gto))
glimpse(secciones_gto)
rm(datos_eceg)
#-----------
datos_eceg_guanajuato <- inner_join(datos_eceg_guanajuato, secciones_gto, 
                                 by=c("SECCION"))


datos_eceg_guanajuato <- datos_eceg_guanajuato|>dplyr::select(ENTIDAD,
                                 DISTRITO,
                                 MUNICIPIO,
                                 SECCION,
                                 POBTOT,
                                 GRAPROES,
                                 PAFIL_IPRI,
                                 P_0A2,
                                 P_3A5,
                                 P_6A11,
                                 P_8A14,
                                 P_12A14,
                                 P_15A17,
                                 P_18A24,
                                 P_60YMAS,
                                 P_60YMAS_F,
                                 PSIND_LIM,
                                 P12A14NOA,
                                 P18YM_PB,
                                 PCON_DISC,
                                 PRO_OCUP_C,
                                 PEA,
                                 PE_INAC,
                                 POCUPADA,
                                 PDESOCUP,
                                 PSINDER,
                                 VIVTOT,
                                 TVIVPARHAB,
                                 VPH_NDACMM,
                                 VPH_SNBIEN,
                                 VPH_LAVAD,
                                 VPH_REFRI,
                                 VPH_PC,
                                 VPH_INTER,
                                 VPH_SINCIN,
                                 VPH_AUTOM,
                                 VPH_SINLTC,
                                 VPH_SINCIN,VIVPAR_HAB,230:233)
#-------------------------
datos_eceg_guanajuato|>ggplot(aes(geometry=geometry,fill=POBTOT))+
  geom_sf()

#--------------
intersect(names(votacion_guanajuato),names(datos_eceg_guanajuato))

datos_eceg_guanajuato <- datos_eceg_guanajuato|>rename(ID_ENTIDAD="ENTIDAD")

votacion_guanajuato <- votacion_guanajuato|>mutate(ID_ENTIDAD=as.integer(ID_ENTIDAD))
votacion_guanajuato <- left_join(votacion_guanajuato,
                                 datos_eceg_guanajuato,by=c("ID_ENTIDAD","SECCION"))




votacion_guanajuato_2 <-  votacion_guanajuato|>filter(!is.na(DISTRITO))

cor(votacion_guanajuato_2$PORC_LIBIA,votacion_guanajuato_2$POBTOT)
#----------------------------------------
names(datos_eceg_guanajuato)

datos_eceg_guanajuato <- datos_eceg_guanajuato|>mutate(
  PSINDER_porcen= PSINDER/POBTOT*100,
  PAFIL_IPRIV_porcen= PAFIL_IPRI/POBTOT*100,  
  VPH_INTER_porcen= VPH_INTER/TVIVPARHAB*100)


pca_matrix <- datos_eceg_guanajuato %>% 
  dplyr::select(GRAPROES,PSINDER_porcen,PAFIL_IPRIV_porcen,
                PRO_OCUP_C,VPH_INTER_porcen) 



st_geometry(pca_matrix) <- NULL
glimpse(pca_matrix)

res.pca <- pca_matrix %>% 
  drop_na() %>% 
  prcomp( scale = TRUE)

library(factoextra)
fviz_eig(res.pca)  

factor_guanajuato <- predict(res.pca, newdata = pca_matrix)
summary(factor_guanajuato)
dim(factor_guanajuato)
dim(datos_eceg_guanajuato)

secciones_factor_previo <- cbind(datos_eceg_guanajuato,factor_guanajuato)
st_geometry(secciones_factor_previo) <- NULL
glimpse(secciones_factor_previo)

secciones_factor <- datos_eceg_guanajuato %>% 
  left_join(secciones_factor_previo)

glimpse(secciones_factor)
#Clases del  índice de estratifiación social ----


#Clase que qiuero
no_classes <- 5
# Extraer cuantiles
q1 <- secciones_factor %>%
  pull(PC1) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric

# Así se crean las etiquetas
labels <- imap_chr(q1, function(., idx){
  return(paste0(round(q1[idx] , digits=1),
                "",
                " – ",
                round(q1[idx + 1] , digits=1),
                ""))
})


# Se elimina la última etiqueta
# En caso contrario sería hasta NA
labels <- labels[1:length(labels) - 1]
labels

# Crear la variable
secciones_factor  <- secciones_factor|>
  mutate(PC1_q = cut(PC1,
                     breaks = q1,
                     labels = labels,
                     include.lowest = T))


hist(secciones_factor$PC1)
glimpse(secciones_factor)
table(secciones_factor$PC1_q)

secciones_factor$estrato=factor(secciones_factor$PC1_q,
                               levels = c("-4 – -1.6",
                                          "-1.6 – -0.9",
                                          "-0.9 – 0.2",
                                          "0.2 – 2",
                                          "2 – 3.6"),
                               labels = c("Medio Alto/Alto", "Medio", "Medio Bajo", "Bajo", "Muy Bajo"))
#Nota: Al nivel más alto le llamo "medio alto/alto", porque sería dificil argumentar
#que el 20% más alto es estrato alto, en términos de estratificación social.
table(secciones_factor$estrato)
#-------------------------------
colnames(secciones_factor)
secciones_factor_2 <- secciones_factor|>dplyr::select(1:4,39:52)

t <-  intersect(names(votacion_guanajuato_2),names(secciones_factor_2))

votacion_guanajuato_2 <- inner_join(votacion_guanajuato_2,secciones_factor_2,
                                    by=t)
#-----------------
cor(votacion_guanajuato_2$PORC_LIBIA,votacion_guanajuato_2$PC1*-1)

#------------------------------------------
votacion_guanajuato_2|>ggplot(aes(PORC_CLAUDIA,PC1))+
  geom_point()


secciones_factor_2|>ggplot(aes(geometry=geometry,fill=estrato))+
  geom_sf()

#-------------------------
st_write(secciones_factor_2, "estrato-socioeconomico-secciones-guanajuato.geojson", driver = "GeoJSON")
write.csv(secciones_factor_2,"estrato-socioeconomico-secciones-guanajuato.csv")
write.xlsx(secciones_factor_2,"estrato-socioeconomico-secciones-guanajuato.xlsx")

#----Analisis voto cruzado: primer set de modelos 
#-Base edgar
voto_cruzado <- read.xlsx("base_mapa_voto_cruzado_Gto.xlsx")
voto_cruzado <- voto_cruzado|>rename(SECCION="SECC")
#--Votacion Guanajuato: calculo de la diferencias
colnames(voto_cruzado)

votacion_guanajuato <- votacion_guanajuato|>mutate(DIF_CS_ALMA=VOTO_CLAUDIA-VOTO_ALMA,
                                                   DIF_XG_LIBIA=VOTO_XOCHITL-VOTO_LIBIA,
                                                   DIF_JAM_YULMA=VOTO_JAM-VOTO_YULMA,
                                                   DIF_CS_ALMA_PORC=PORC_CLAUDIA-PORC_ALMA,
                                                   DIF_XG_LIBIA_PORC=PORC_XOCHITL-PORC_LIBIA,
                                                   DIF_JAM_YULMA_PORC=PORC_JAM-PORC_YULMA)
#---------------------------
hist(votacion_guanajuato$DIF_CS_ALMA_PORC)
hist(votacion_guanajuato$DIF_XG_LIBIA_PORC)
hist(votacion_guanajuato$DIF_JAM_YULMA_PORC)

#-----------
p_load(report)##Para reportar resultados
#--Modelos voto cruzado:diferencias voto pres-voto gob
voto_cruzado_1 <-  lm(DIF_CS_ALMA_PORC~DIF_XG_LIBIA_PORC+DIF_JAM_YULMA_PORC,data = votacion_guanajuato,na.action = na.omit)
voto_cruzado_2 <-  lm(DIF_XG_LIBIA_PORC~DIF_CS_ALMA_PORC+DIF_JAM_YULMA_PORC,data = votacion_guanajuato,na.action = na.omit)
voto_cruzado_3 <-  lm(DIF_JAM_YULMA_PORC~DIF_XG_LIBIA_PORC+DIF_CS_ALMA_PORC,data = votacion_guanajuato,na.action = na.omit)

summary(voto_cruzado_1)
plot(ggeffects::ggpredict(voto_cruzado_1))
report(voto_cruzado_2)
#-------------
voto_cruzado <- inner_join(voto_cruzado,votacion_guanajuato,
                           by=c("SECCION"))


voto_cruzado <- voto_cruzado|>mutate(PORC_PART_PRES=TOTAL_VOTOS_CALCULADOS_PRES*100/LISTA_NOMINAL_PRES)
#--Modelos voto cruzado: diferencias pres-gob + margen victoria pres/gob
voto_cruzado_4 <-  lm(DIF_CS_ALMA~DIF_XG_LIBIA+DIF_JAM_YULMA+dif_PORC_PRES+dif__PORC_GOB+PART_GOB,data = voto_cruzado,na.action = na.omit)
voto_cruzado_5 <-  lm(DIF_XG_LIBIA~DIF_CS_ALMA+DIF_JAM_YULMA+dif_PORC_PRES+dif__PORC_GOB+PART_GOB,data = voto_cruzado,na.action = na.omit)
voto_cruzado_6 <-  lm(DIF_JAM_YULMA~DIF_XG_LIBIA+DIF_CS_ALMA+dif_PORC_PRES+dif__PORC_GOB+PART_GOB,data = voto_cruzado,na.action = na.omit)

summary(voto_cruzado_5)
plot(ggeffects::ggpredict(voto_cruzado_5))

#---------------------------
table(voto_cruzado[11])
voto_cruzado$PRIMERO_PRES

voto_cruzado <- voto_cruzado|>mutate(GANA_CS=ifelse(PRIMERO_PRES==" CSH_MORE-PT-PVEM",1,0),
                                     GANA_XG=ifelse(PRIMERO_PRES==" XG_PAN-PRI-PRD",1,0),
                                     GANA_ALMA=ifelse(PRIMERO_GOB==" ALMA_MORE-PT-PVEM",1,0),
                                     GANA_LIBIA=ifelse(PRIMERO_GOB==" LIBIA_PAN-PRI-PRD",1,0))
#---------------------
gana_libia <-  glm(GANA_LIBIA~DIF_XG_LIBIA+DIF_CS_ALMA+dif_PORC_PRES+dif__PORC_GOB+PART_GOB,family = "binomial",data = voto_cruzado,na.action = na.omit)
gana_alma <- glm(GANA_ALMA~DIF_XG_LIBIA+DIF_CS_ALMA+dif_PORC_PRES+dif__PORC_GOB+PART_GOB,family = "binomial",data = voto_cruzado,na.action = na.omit)

summary(gana_libia)
plot(ggeffects::ggpredict(gana_libia))
#-----------
invlogit <- function(x) {
  return(plogis(x))
}

invlogit(coef(gana_alma))*100
invlogit(coef(gana_libia))*100


#----------
voto_cruzado_na <- na.omit(voto_cruzado)
cor(voto_cruzado_na$DIF_CS_ALMA_PORC,voto_cruzado_na$DIF_XG_LIBIA)

table(voto_cruzado$crit_gober)

#------------------------------
mean(voto_cruzado_na$DIF_XG_LIBIA_PORC)
mean(voto_cruzado_na$DIF_CS_ALMA_PORC)

#-----------
volatilidad_guanajuato <- read.xlsx("volatilidad-guanajuato-computos-VF.xlsx")
names(volatilidad_guanajuato)

volatilidad_guanajuato <- inner_join(voto_cruzado,volatilidad_guanajuato,
                                     by=c("SECCION"))


#----------------------------
names(volatilidad_guanajuato)

summary(glm(GANA_LIBIA~VOLATILIDAD_TOTAL_GOB+VOLATILIDAD_TOTAL_PRES+
             dif_PORC_PRES+dif__PORC_GOB+PART_GOB,
            data = volatilidad_guanajuato,na.action = na.omit,
            family="binomial"))


volatilidad_guanajuato|>filter(PART_GOB<101)|>
  ggplot(aes(VOLATILIDAD_TOTAL_GOB,DIF_CS_ALMA_PORC,color=PRIMERO_GOB))+
  geom_point()+facet_wrap(~PRIMERO_PRES)

#-----------
invlogit(coef(glm(GANA_LIBIA~VOLATILIDAD_TOTAL_GOB+VOLATILIDAD_TOTAL_PRES+
                    dif_PORC_PRES+dif__PORC_GOB+PART_GOB,
                  data = volatilidad_guanajuato,na.action = na.omit,
                  family="binomial")))



invlogit(coef(glm(GANA_ALMA~VOLATILIDAD_TOTAL_GOB+VOLATILIDAD_TOTAL_PRES+
                    dif_PORC_PRES+dif__PORC_GOB+PART_GOB,
                  data = volatilidad_guanajuato,na.action = na.omit,
                  family="binomial")))

#----------------
voto_cruzado_7 <-  lm(DIF_CS_ALMA_PORC~PORC_XOCHITL+PORC_JAM+PORC_LIBIA+PORC_YULMA+PART_GOB,data = voto_cruzado,na.action = na.omit)
voto_cruzado_8 <-  lm(DIF_XG_LIBIA_PORC~PORC_CLAUDIA+PORC_JAM+PORC_ALMA+PORC_YULMA+PART_GOB,data = voto_cruzado,na.action = na.omit)
voto_cruzado_9 <-  lm(DIF_JAM_YULMA_PORC~PORC_CLAUDIA+PORC_XOCHITL+PORC_ALMA+PORC_LIBIA+PART_GOB,data = voto_cruzado,na.action = na.omit)

summary(voto_cruzado_7)
plot(ggeffects::ggpredict(voto_cruzado_7))

#---------Graficas voto cruzado 
extrafont::loadfonts()
#-------------
split_vote_theme <-  theme(legend.box ="vertical",
                           #legend.position="none",
                           legend.position = c(0.12, 0.9),
                           legend.key = element_rect(fill = "white", colour = "white"),
                           legend.title = element_text(hjust =0.1,  
                                                       size = 16, face = "bold", color="black",family="JetBrains Mono"),
                           legend.text= element_text(size = 20, face = "bold", color = "black",family="JetBrains Mono"),
                           axis.title.y = element_text(size = 14, face="bold", colour = "black",family="JetBrains Mono"),
                           axis.title.x= element_text(size = 14, face="bold", colour = "black",family="JetBrains Mono"),
                           axis.ticks  =  element_blank(),
                           #axis.text.y = element_blank(),
                           axis.text.y = element_text(size = 13.5, angle=0,color = "black",face="bold",family="JetBrains Mono"),
                           axis.text.x = element_text(size = 13.5, angle=0,color = "black",face="bold",family="JetBrains Mono"),
                           #axis.text.y = element_text(size = 17, color = "black",face="bold",family="JetBrains Mono"),
                           #panel.grid.major.x  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
                           #panel.grid.minor.x  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
                           panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
                           legend.box.background = element_rect(fill = "white", colour = "black"),
                           plot.title = element_text(face = "bold", size = 18,margin = margin(10,0,20,0), hjust =0, family="JetBrains Mono",colour = "black"),
                           plot.subtitle = element_text(hjust = 0, face = "bold", size = 19, colour = "black", family = "JetBrains Mono"),
                           plot.caption = element_text(hjust = 0,face = "bold", colour = "black", family="JetBrains Mono",size = 14),
                           axis.line.y  = element_blank(), 
                           legend.background = element_rect(color="black",fill = "white"),
                           strip.background = element_rect(color="black",fill
                                                           = "white"),
                           strip.text = element_text(hjust = 0.5, face = "bold",
                                                     family = "JetBrains Mono", size = 17, color = "black"))

#--------------
#gana_pres <- c("Gana Claudia Sheinbaum","Gana Xóchitl Gálvez")
voto_cruzado$PRIMERO_PRES[voto_cruzado$PRIMERO_PRES==" XG_PAN-PRI-PRD"]<-"Gana Xóchitl Gálvez"
voto_cruzado$PRIMERO_PRES[voto_cruzado$PRIMERO_PRES==" CSH_MORE-PT-PVEM"]<-"Gana Claudia Sheinbaum"



voto_cruzado|>filter(PART_GOB<101)|>
  ggplot(aes(DIF_CS_ALMA_PORC,DIF_XG_LIBIA_PORC,color=PRIMERO_GOB))+
  geom_point(size=3)+facet_wrap(~PRIMERO_PRES,
                                labeller = labeller(voto_cruzado$PRIMERO_PRES))+
  scale_color_manual(values=c("darkred","navy"),
                     labels=c("Alma Alcaraz","Libia Dennise García"),
                     name="Ganadora por sección")+
  labs(title = stringr::str_wrap("Guanajuato: diferencia en porcentaje de votos obtenidos entre candidatas a gobernadora y presidenta", width = 200),
       subtitle = stringr::str_wrap("Porcentajes a nivel sección electoral", width = 200),
       x ="Diferencia Claudia Sheinbaum-Alma Alcaraz", 
       y ="Diferencia Xóchitl Gálvez-Libia Dennise",
       caption = "Fuente: elaboración propia con datos del Cómputo Distrital 2024 (INE, IEEG)")+
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, 
            ymax = Inf, color = "black", fill = NA)+
  split_vote_theme-> diferencias_guanajuato_secciones

diferencias_guanajuato_secciones

dev.off()
ggsave(diferencias_guanajuato_secciones, filename = "diferencias-guanajuato-secciones.png",width = 20, height = 10, dpi = 100)
#-------
colnames(voto_cruzado)


voto_cruzado|>filter(PART_GOB<101)|>
  ggplot(aes(dif__PORC_GOB,dif_PORC_PRES,color=PRIMERO_GOB))+
  geom_point(size=3)+facet_wrap(~PRIMERO_PRES,
                                labeller = labeller(voto_cruzado$PRIMERO_PRES))+
  scale_color_manual(values=c("darkred","navy"),
                     labels=c("Alma Alcaraz","Libia Dennise García"),
                     name="Ganadora por sección")+
  labs(title = stringr::str_wrap("Guanajuato: diferencia en porcentaje de votos obtenidos entre primer y segundo lugar, gobernador y presidente", width = 200),
       subtitle = stringr::str_wrap("Porcentajes a nivel sección electoral", width = 200),
       x ="Diferencia 1o-2o Gobernador", 
       y ="Diferencia 1o-2o Presidente",
       caption = "Fuente: elaboración propia con datos del Cómputo Distrital 2024 (INE, IEEG)")+
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, 
            ymax = Inf, color = "black", fill = NA)+
  split_vote_theme -> diferencias_margenes_guanajuato

diferencias_margenes_guanajuato
dev.off()
ggsave(diferencias_margenes_guanajuato, filename = "diferencias-margenes-guanajuato.png",width = 20, height = 10, dpi = 100)

#-----------------------------------------
cor(volatilidad_guanajuato$VOLATILIDAD_TOTAL_GOB,volatilidad_guanajuato$VOLATILIDAD_TOTAL_PRES)
mean(volatilidad_guanajuato$VOLATILIDAD_TOTAL_GOB)
mean(volatilidad_guanajuato$VOLATILIDAD_TOTAL_PRES)


vol_gana_cs <- volatilidad_guanajuato|>filter(GANA_CS==1)
vol_gana_xg <- volatilidad_guanajuato|>filter(GANA_XG==1)

mean(vol_gana_cs$VOLATILIDAD_TOTAL_GOB)
mean(vol_gana_cs$VOLATILIDAD_TOTAL_PRES)

mean(vol_gana_xg$VOLATILIDAD_TOTAL_GOB)
mean(vol_gana_xg$VOLATILIDAD_TOTAL_PRES)

#---------------------------
volatilidad_guanajuato|>filter(PART_GOB<101)|>
  ggplot(aes(VOLATILIDAD_TOTAL_PRES,VOLATILIDAD_TOTAL_GOB,color=PRIMERO_GOB))+
  geom_point(size=3)+facet_wrap(~PRIMERO_PRES,
                                labeller = labeller(voto_cruzado$PRIMERO_PRES))+
  scale_color_manual(values=c("darkred","navy"),
                     labels=c("Alma Alcaraz","Libia Dennise García"),
                     name="Ganadora por sección")+
  labs(title = stringr::str_wrap("Guanajuato: volatilidad electoral por sección, presidente y gobernador", width = 200),
       subtitle = stringr::str_wrap("Volatilidad electoral es el % neto de votantes que cambiaron su voto entre elecciones", width = 200),
       x ="Volatilidad presidente (2018-2024)", 
       y ="Volatilidad gobernador (2018-2024)",
       caption = "Fuente: elaboración propia con datos del INE e IEEG")+
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, 
            ymax = Inf, color = "black", fill = NA)+
  split_vote_theme


# --------------------------------------------------------------
partido <- c("MORENA-PVEM-PT","PAN-PRI-PRD","MC")
presidencial <- c("47.22","39.99","10.56")
gobernador <- c("41.05","51.21","5.65")
diferencias <- c("180088","-293887","137700")

comparativo <- cbind(partido,presidencial,gobernador,diferencias)
comparativo <- as.data.frame(comparativo)
#--------
comparativo <- comparativo|>gather(eleccion,voto,2:3)
comparativo$voto <- as.numeric(comparativo$voto)
comparativo$partido <- factor(comparativo$partido,levels = c("MORENA-PVEM-PT","PAN-PRI-PRD","MC"))

pd <- position_dodge(0.0)
#------------------
p_4 <- theme(legend.box  ="horizontal",
             legend.position ="bottom",
             #legend.position = c(0.9, 0.85),
             legend.title = element_text(hjust =0,  size = 21, face = "bold", color="black", family="JetBrains Mono"),
             legend.text= element_text(size = 17, face = "bold", color = "black", family="JetBrains Mono"),
             #axis.title.y = element_text(size = 18, face="bold", colour = "black"),
             axis.title.y = element_blank(),
             #axis.title.x = element_text(size = 18, face="bold", colour = "black"),
             axis.ticks.x    =  element_line(colour = "black", size = 0.8),
             axis.text.x = element_text(size = 20,face="bold", colour = "black", family = "JetBrains Mono"),
             #axis.text.y = element_text(size = 15 ,face="bold", colour = "black", family = "Times New Roman"),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             #axis.text.y = element_text(size = 18, face="bold", colour = "black", family = "Times New Roman"),
             #panel.grid.major  = element_blank(),
             #panel.grid.minor  = element_blank(),
             panel.grid.major  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
             panel.grid.minor  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
             panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
             plot.background = element_rect(fill = "white", colour = "black", size = 0.5),
             legend.box.background = element_rect(fill = "white", colour = "black"),
             plot.title = element_text(face = "bold", size = 17.8,margin = margin(10,0,20,0), hjust =0, colour = "black",family="JetBrains Mono"),
             plot.subtitle = element_text(hjust = 0, face = "bold", size = 20, colour = "black",family="JetBrains Mono"),
             plot.caption = element_text(hjust = 0,face = "bold", colour = "black", size = 17, family="JetBrains Mono"),
             axis.line.y  = element_blank(), 
             legend.background = element_rect(color="white",fill = "white"),
             strip.background = element_rect(color="black",fill = "white"),
             strip.text = element_text(hjust = 0.5, face = "bold", size = 17, color = "black", family = "JetBrains Mono"))


#-----------
comparativo|>ggplot(aes(partido,voto,fill=eleccion,group=eleccion))+
  geom_bar(stat = "identity",
           position = position_dodge(0.7),width = 0.5,color="black")+
  scale_y_continuous(limits = c(0,80),breaks = c(0,50,80))+
  geom_text(aes(label=paste0(voto,"%")),position=position_dodge(width=0.7), vjust=-0.5,
            fontface="bold" ,family="JetBrains Mono",size=7)+
  scale_fill_manual(values=c("#A0522D","#F4A460"),
                     labels=c("Gobernador","Presidente"),
                     name="Elección")+
  labs(title = stringr::str_wrap("", width = 200),
       subtitle = stringr::str_wrap("Guanajuato: votación obtenida por cada coalición en la elección a presidente y gobernador", width = 200),
       x ="", 
       y ="",
       caption = "Fuente: elaboración propia con datos del Cómputo Distrital 2024 (INE, IEEG)")+p_4 -> votacion_comparativo
  
votacion_comparativo
dev.off()
ggsave(votacion_comparativo, filename = "diferencias-comparativo-guanajuato.png",width = 20, height = 10, dpi = 100)
#-------------
encuesta_gto <- read_dta("Guanajuato - UNIDA Mayo de 2024_n2000_v060524_pond.dta")
#---------------
etiquetado <- function(x){
  
  x|>mutate_if(haven::is.labelled, haven::as_factor) |>
    sjlabelled::label_to_colnames() -> x
  return(x)
  
}
###--------------------------------------------------
u <- names(encuesta_gto)
#-----Asigno etiquetas y sustituyo los nombres de variables
encuesta_gto <- etiquetado(encuesta_gto)
names(encuesta_gto) <- u
head(encuesta_gto)
#----------
encuesta_gto$q0021[encuesta_gto$q0021=="No sabe"]<-"Ninguno"
#--------------------------------------------------
p_load(pollster)

partidismo <- encuesta_gto|>topline(q0021,FACPOND3,remove = c("(Missing)"))
partidismo <- partidismo|>select(1,4)
partidismo <- partidismo|>rename(Partido="Response",Porcentaje=`Valid Percent`)


partidismo$Partido <- factor(partidismo$Partido,
                              labels = c("Otro","PAN","PRI",
                                         "PRD","PVEM","PT","MC",
                                         "MORENA","Ninguno"))

partidismo$Partido <- factor(partidismo$Partido,
                             levels = c("PAN","MORENA","PRI",
                                        "MC","PVEM","PRD","PT","Otro",
                                        "Ninguno"))

partidismo$Porcentaje <- round(partidismo$Porcentaje,1)
#---------
p_5 <- theme(#legend.box = "none",
             legend.position ="none",
             #legend.position = c(0.9, 0.85),
             legend.title = element_text(hjust =0,  size = 21, face = "bold", color="black", family="JetBrains Mono"),
             legend.text= element_text(size = 17, face = "bold", color = "black", family="JetBrains Mono"),
             #axis.title.y = element_text(size = 18, face="bold", colour = "black"),
             axis.title.y = element_blank(),
             #axis.title.x = element_text(size = 18, face="bold", colour = "black"),
             axis.ticks.x    =  element_line(colour = "black", size = 0.8),
             axis.text.x = element_text(size = 15,face="bold", colour = "black", family = "JetBrains Mono"),
             #axis.text.y = element_text(size = 15 ,face="bold", colour = "black", family = "Times New Roman"),
             axis.text.y = element_blank(),
             axis.title.x = element_blank(),
             #axis.text.y = element_text(size = 18, face="bold", colour = "black", family = "Times New Roman"),
             #panel.grid.major  = element_blank(),
             #panel.grid.minor  = element_blank(),
             panel.grid.major  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
             panel.grid.minor  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
             panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
             plot.background = element_rect(fill = "white", colour = "black", size = 0.5),
             legend.box.background = element_rect(fill = "white", colour = "black"),
             plot.title = element_text(face = "bold", size = 20,margin = margin(10,0,20,0), hjust =0, colour = "black",family="JetBrains Mono"),
             plot.subtitle = element_text(hjust = 0, face = "bold", size = 17.8, colour = "black",family="JetBrains Mono"),
             plot.caption = element_text(hjust = 0,face = "bold", colour = "black", size = 15, family="JetBrains Mono"),
             axis.line.y  = element_blank(), 
             legend.background = element_rect(color="white",fill = "white"),
             strip.background = element_rect(color="black",fill = "white"),
             strip.text = element_text(hjust = 0.5, face = "bold", size = 17, color = "black", family = "JetBrains Mono"))

#------------
partidismo|>ggplot(aes(Partido,Porcentaje,fill=Partido))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values=c("navy","darkred","darkgreen","darkorange",
                             "green","yellow","red",
                             "black","gray"))+
  geom_text(aes(label=paste0(Porcentaje,"%")),position=position_dodge(width=0.7), vjust=-0.5,
            fontface="bold" ,family="JetBrains Mono",size=7)+
  labs(title = stringr::str_wrap("Identidad partidista: Sin importar por quién haya votado en el pasado, ¿con cuál partido político se identifica usted?", width = 200),
       subtitle = stringr::str_wrap("Encuesta estatal Guanajuato", width = 200),
       x ="", 
       y ="",
       caption = "Fuente: encuesta Áltica Guanajuato Mayo 2024")+p_5 -> partidismo_guanajuato


partidismo_guanajuato
dev.off()
ggsave(partidismo_guanajuato, filename = "partidismo-guanajuato.png",width = 20, height = 10, dpi = 100)

#--Análisis final 
#--Modelos voto cruzado:diferencias voto pres-voto gob
cor(voto_cruzado_na$DIF_XG_LIBIA,voto_cruzado_na$DIF_CS_ALMA)

t.test(voto_cruzado$DIF_CS_ALMA,
       voto_cruzado$DIF_XG_LIBIA)


voto_cruzado <- voto_cruzado|>mutate(LOG_CLAUDIA=log(VOTO_CLAUDIA),
                                     LOG_XOCHITL=log(VOTO_XOCHITL),
                                     LOG_JAM=log(VOTO_JAM),
                                     LOG_ALMA=log(VOTO_ALMA),
                                     LOG_LIBIA=log(VOTO_LIBIA),
                                     LOG_YULMA=log(VOTO_YULMA))
#----------------------
tipo_secciones <-  secciones_gto|>select(SECCION,TIPO)
tipo_secciones$geometry <- NULL

voto_cruzado <- inner_join(voto_cruzado,tipo_secciones,by=c("SECCION"))
voto_cruzado <- voto_cruzado|>mutate(SECCION_CAT=case_when(
  TIPO==2~"URBANA",
  TIPO==3~"MIXTA",
  TIPO==4~"RURAL",
  TRUE~NA))

table(voto_cruzado$SECCION_CAT)

voto_cruzado$SECCION_CAT <- factor(voto_cruzado$SECCION_CAT,
                                   levels = c("MIXTA","RURAL","URBANA"))

voto_cruzado <- within(voto_cruzado, SECCION_CAT <- relevel(SECCION_CAT, ref = "MIXTA"))
#---------------
voto_cruzado_1 <-  lm(DIF_CS_ALMA~VOTO_XOCHITL + 
                        VOTO_LIBIA + 
                        VOTO_JAM + 
                        VOTO_YULMA+TOTAL_VOTOS_GOB+SECCION_CAT,
                      data = voto_cruzado,na.action = na.omit)


voto_cruzado_2 <-  lm(DIF_XG_LIBIA_PORC~VOTO_CLAUDIA+
                        VOTO_ALMA+
                        VOTO_JAM+
                        VOTO_YULMA+TOTAL_VOTOS_GOB+SECCION_CAT,
                      data = voto_cruzado,na.action = na.omit)
#----------------------------
p_load(ggeffects)
#---------
p_6 <- theme(#legend.box = "none",
  legend.position ="none",
  #legend.position = c(0.9, 0.85),
  legend.title = element_text(hjust =0,  size = 21, face = "bold", color="black", family="JetBrains Mono"),
  legend.text= element_text(size = 17, face = "bold", color = "black", family="JetBrains Mono"),
  axis.title.y = element_text(size = 18, face="bold", colour = "black",family = "JetBrains Mono"),
  #axis.title.y = ,
  axis.title.x = element_text(size = 18, face="bold", colour = "black",family = "JetBrains Mono"),
  axis.ticks.x    =  element_line(colour = "black", size = 0.8),
  axis.text.x = element_text(size = 17,face="bold", colour = "black", family = "JetBrains Mono"),
  axis.text.y = element_text(size = 15 ,face="bold", colour = "black", family = "JetBrains Mono"),
  #axis.text.y = element_blank(),
  #axis.title.x = element_blank(),
  #axis.text.y = element_text(size = 18, face="bold", colour = "black", family = "Times New Roman"),
  #panel.grid.major  = element_blank(),
  #panel.grid.minor  = element_blank(),
  panel.grid.major  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
  panel.grid.minor  = element_line(color="#D3D3D3", size=0.5,linetype="solid"),
  panel.background = element_rect(fill = "white", colour = "black", size = 0.5),
  plot.background = element_rect(fill = "white", colour = "black", size = 0.5),
  legend.box.background = element_rect(fill = "white", colour = "black"),
  plot.title = element_text(face = "bold", size = 17,margin = margin(10,0,20,0), hjust =0, colour = "black",family="JetBrains Mono"),
  plot.subtitle = element_text(hjust = 0, face = "bold", size = 17, colour = "black",family="JetBrains Mono"),
  plot.caption = element_text(hjust = 0,face = "bold", colour = "black", size = 17, family="JetBrains Mono"),
  axis.line.y  = element_blank(), 
  legend.background = element_rect(color="white",fill = "white"),
  strip.background = element_rect(color="black",fill = "white"),
  strip.text = element_text(hjust = 0.5, face = "bold", size = 13, color = "black", family = "JetBrains Mono"))

#----------------------------
voto_cruzado_1_pred <-  ggpredict(voto_cruzado_1)

voto_cruzado_1_pred_1 <- as.data.frame(voto_cruzado_1_pred[[1]])
voto_cruzado_1_pred_2 <- as.data.frame(voto_cruzado_1_pred[[2]])
voto_cruzado_1_pred_3 <- as.data.frame(voto_cruzado_1_pred[[3]])
voto_cruzado_1_pred_4 <- as.data.frame(voto_cruzado_1_pred[[4]])
voto_cruzado_1_pred_5 <- as.data.frame(voto_cruzado_1_pred[[5]])
voto_cruzado_1_pred_6 <- as.data.frame(voto_cruzado_1_pred[[6]])

voto_cruzado_1_pred <- rbind(voto_cruzado_1_pred_1,
                             voto_cruzado_1_pred_2,
                             voto_cruzado_1_pred_3,
                             voto_cruzado_1_pred_4,
                             voto_cruzado_1_pred_5,
                             voto_cruzado_1_pred_6)

voto_cruzado_1_pred
#--------------------
voto_cruzado_1_pred|>filter(!group=="VOTO_JAM")|>
filter(!group=="VOTO_YULMA")|>filter(!group=="TOTAL_VOTOS_GOB")|>
filter(!group=="SECCION_CAT")|>  
  ggplot(aes(as.numeric(x),predicted,group=group,color=group))+
  scale_x_continuous(limits = c(0,7500),
                     breaks = c(0,2500,5000,7500))+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high))+
  geom_line(size=1)+geom_point(size=7)+
  facet_wrap(~group,ncol=2,scales = "free_y",
                               labeller = as_labeller(c(
                                 VOTO_LIBIA = "Libia Dennise",
                                 VOTO_XOCHITL = "Xóchitl Gálvez")))+
  scale_color_manual(values = c("steelblue","navy"))+
  labs(title = stringr::str_wrap("Efecto del voto en cada sección electoral por Libia Dennise y Xóchitl Gálvez en la diferencia de votos Claudia Sheinbaum-Alma Alcaraz", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión lineal: lm(DIF_CS_ALMA ~ VOTO_XOCHITL + VOTO_LIBIA + VOTO_JAM + 
    VOTO_YULMA + PART_GOB + TIPO_SECC, data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Diferencia votación Claudia-Alma", 
       x ="Votos obtenidos",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 ->diferencia_claudia_alma

#---------------
voto_cruzado_1_pred|>
  filter(group=="SECCION_CAT")|>  
  ggplot(aes(x,predicted,group=group,color=x))+
  geom_point(size=7)+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high),
                width = 0.2,size=1)+
  #geom_line(size=1)+
  scale_color_manual(values = c("darkred","darkred","darkred"))+
  labs(title = stringr::str_wrap("Efecto del tipo de sección electoral en la diferencia de votos Claudia Sheinbaum-Alma Alcaraz", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión lineal: lm(DIF_CS_ALMA ~ VOTO_XOCHITL + VOTO_LIBIA + VOTO_JAM + 
    VOTO_YULMA + PART_GOB + TIPO_SECC, data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Diferencia votación Claudia-Alma", 
       x ="Sección electoral",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 -> diferencia_claudia_alma_tipo_sec
  
#-----------------------
dev.off()
ggsave(diferencia_claudia_alma, filename = "diferencia-claudia-alma.png",width = 20, height = 10, dpi = 100)
ggsave(diferencia_claudia_alma_tipo_sec, filename = "diferencia-claudia-alma-secc.png",width = 20, height = 10, dpi = 100)
#-----------------------
voto_cruzado_2_pred <-  ggpredict(voto_cruzado_2)
voto_cruzado_2_pred_1 <- as.data.frame(voto_cruzado_2_pred[[1]])
voto_cruzado_2_pred_2 <- as.data.frame(voto_cruzado_2_pred[[2]])
voto_cruzado_2_pred_3 <- as.data.frame(voto_cruzado_2_pred[[3]])
voto_cruzado_2_pred_4 <- as.data.frame(voto_cruzado_2_pred[[4]])
voto_cruzado_2_pred_5 <- as.data.frame(voto_cruzado_2_pred[[5]])
voto_cruzado_2_pred_6 <- as.data.frame(voto_cruzado_2_pred[[6]])

voto_cruzado_2_pred <- rbind(voto_cruzado_2_pred_1,
                             voto_cruzado_2_pred_2,
                             voto_cruzado_2_pred_3,
                             voto_cruzado_2_pred_4,
                             voto_cruzado_2_pred_5,
                             voto_cruzado_2_pred_6)

voto_cruzado_2_pred
#--------------------
voto_cruzado_2_pred|>filter(!group=="VOTO_JAM")|>
  filter(!group=="VOTO_YULMA")|>filter(!group=="TOTAL_VOTOS_GOB")|>
  filter(!group=="SECCION_CAT")|>
  ggplot(aes(as.numeric(x),predicted,group=group,color=group))+
  scale_x_continuous(limits = c(0,6000),
                     breaks = c(0,2500,5500))+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high))+
  geom_line(size=1)+geom_point(size=7)+
  facet_wrap(~group,ncol=2,scales = "free_y",
                               labeller = as_labeller(c(
                                 VOTO_ALMA = "Alma Alcaraz",
                                 VOTO_CLAUDIA = "Claudia Sheinbaum")))+
  scale_color_manual(values = c("#B22222","#8B0000"))+
  labs(title = stringr::str_wrap("Efecto del voto en cada sección electoral por Alma Alcaraz y Claudia Sheinbaum en la diferencia de votos Xóchitl Gálvez-Libia Dennise", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión lineal: lm(DIF_XG_LIBIA ~ VOTO_CLAUDIA + VOTO_ALMA + VOTO_JAM + 
    VOTO_YULMA + PART_GOB+TIPO_SECC, data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Diferencia votos Xóchitl-Libia", 
       x ="Votos obtenidos",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 -> diferencia_xochitl_libia
#----------------------------
voto_cruzado_2_pred|>
  filter(group=="SECCION_CAT")|>  
  ggplot(aes(x,predicted,group=group,color=x))+
  geom_point(size=7)+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high),
                width = 0.2,size=1)+
  scale_y_continuous(limits = c(-13,-9),
                     breaks = seq(-13,-9,1))+
  scale_color_manual(values = c("navy","navy","navy"))+
  labs(title = stringr::str_wrap("Efecto del tipo de sección electoral en la diferencia de votos Xóchitl Gálvez-Libia Dennise", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión lineal: lm(DIF_XG_LIBIA ~ VOTO_CLAUDIA + VOTO_ALMA + VOTO_JAM + 
    VOTO_YULMA + PART_GOB+TIPO_SECC, data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Diferencia votación Xóchitl-Libia", 
       x ="Sección electoral",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 -> diferencia_xochitl_libia_tipo_sec  
#----------------
dev.off()
ggsave(diferencia_xochitl_libia, filename = "diferencia-xochitl-libia.png",width = 20, height = 10, dpi = 100)
ggsave(diferencia_xochitl_libia_tipo_sec, filename = "diferencia-xochitl-libia-secc.png",width = 20, height = 10, dpi = 100)#-----------------------
colnames(voto_cruzado)
#rm(voto_cruzado_3,voto_cruzado_4,voto_cruzado_5,voto_cruzado_6,voto_cruzado_7,voto_cruzado_8,voto_cruzado_9)
#----------------------------
voto_cruzado_3 <-  lm(DIF_CS_ALMA~Dif_GOB+Dif_PRES+
                        #TOTAL_VOTOS_CALCULADOS_PRES+
                        TOTAL_VOTOS_GOB+SECCION_CAT,
                      data = voto_cruzado,na.action = na.omit)

voto_cruzado_4 <-  lm(DIF_XG_LIBIA_PORC~Dif_GOB+Dif_PRES+
                        #TOTAL_VOTOS_CALCULADOS_PRES+
                        TOTAL_VOTOS_GOB+SECCION_CAT,
                      data = voto_cruzado,na.action = na.omit)
#--------------------
voto_cruzado_3_pred <-  ggpredict(voto_cruzado_3)
voto_cruzado_3_pred_1 <- as.data.frame(voto_cruzado_3_pred[[1]])
voto_cruzado_3_pred_2 <- as.data.frame(voto_cruzado_3_pred[[2]])
voto_cruzado_3_pred_3 <- as.data.frame(voto_cruzado_3_pred[[3]])
voto_cruzado_3_pred_4 <- as.data.frame(voto_cruzado_3_pred[[4]])

voto_cruzado_3_pred <- rbind(voto_cruzado_3_pred_1,
                             voto_cruzado_3_pred_2,
                             voto_cruzado_3_pred_3,
                             voto_cruzado_3_pred_4)
voto_cruzado_3_pred
#--------------------
voto_cruzado_3_pred|>filter(!group=="TOTAL_VOTOS_CALCULADOS_PRES")|>
  filter(!group=="TOTAL_VOTOS_GOB")|>
  filter(!group=="SECCION_CAT")|>
  ggplot(aes(as.numeric(x),predicted,group=group,color=group))+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high))+
  geom_line(size=1)+geom_point(size=7)+
  facet_wrap(~group,ncol=2,scales = "free_y",
              labeller = as_labeller(c(
                Dif_GOB = "Margen de victoria Gobernatura",
                Dif_PRES = "Margen de victoria Presidente")))+
  scale_color_manual(values = c("#8B008B","#8A2BE2"))+
  labs(title = stringr::str_wrap("Efecto del margen de victoria en cada sección electoral en elección presidencial y de gobernador sobre la diferencia de votos Claudia Sheinbaum-Alma Alcaraz", width = 100),
       subtitle = stringr::str_wrap("Modelo de regresión lineal: lm(DIF_CS_ALMA ~ Dif_GOB + Dif_PRES + PART_GOB + TIPO_SECC, 
    data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Diferencia votos Claudia-Alma", 
       x ="Diferencia de votos 1o-2o lugar",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6->margen_claudia_alma
#-----------------
voto_cruzado_3_pred|>
  filter(group=="SECCION_CAT")|>  
  ggplot(aes(x,predicted,group=group,color=x))+
  geom_point(size=7)+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high),
                width = 0.2,size=1)+
  scale_y_continuous(limits = c(10,70),breaks = seq(10,70,10))+
  scale_color_manual(values = c("black","black","black"))+
  labs(title = stringr::str_wrap("Efecto del tipo de sección electoral en la diferencia de votos Claudia Sheinbuam-Alma Alcaraz", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión lineal: lm(DIF_CS_ALMA ~ Dif_GOB + Dif_PRES + PART_GOB + TIPO_SECC, data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Diferencia votación Claudia-Alma", 
       x ="Sección electoral",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 -> margen_claudia_alma_sec
#----------------------
dev.off()
ggsave(margen_claudia_alma, filename = "margen-claudia-alma.png",width = 20, height = 10, dpi = 100)
ggsave(margen_claudia_alma_sec, filename = "margen-claudia-alma-secc.png",width = 20, height = 10, dpi = 100)
#---------------------------------
voto_cruzado_4_pred <-  ggpredict(voto_cruzado_4)
voto_cruzado_4_pred_1 <- as.data.frame(voto_cruzado_4_pred[[1]])
voto_cruzado_4_pred_2 <- as.data.frame(voto_cruzado_4_pred[[2]])
voto_cruzado_4_pred_3 <- as.data.frame(voto_cruzado_4_pred[[3]])
voto_cruzado_4_pred_4 <- as.data.frame(voto_cruzado_4_pred[[4]])

voto_cruzado_4_pred <- rbind(voto_cruzado_4_pred_1,
                             voto_cruzado_4_pred_2,
                             voto_cruzado_4_pred_3,
                             voto_cruzado_4_pred_4)
voto_cruzado_4_pred
#--------------------
voto_cruzado_4_pred|>filter(!group=="TOTAL_VOTOS_CALCULADOS_PRES")|>
  filter(!group=="TOTAL_VOTOS_GOB")|>
  filter(!group=="SECCION_CAT")|>
  ggplot(aes(as.numeric(x),predicted,group=group,color=group))+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high))+
  geom_line(size=1)+geom_point(size=7)+
  facet_wrap(~group,ncol=2,scales = "free_y",
             labeller = as_labeller(c(
               Dif_GOB = "Margen de victoria Gobernatura",
               Dif_PRES = "Margen de victoria Presidente")))+
  scale_color_manual(values = c("#8B008B","#8A2BE2"))+
  labs(title = stringr::str_wrap("Efecto del margen de victoria en cada sección electoral en elección presidencial y de gobernador sobre la diferencia de votos Xóchitl Gálvez-Libia Dennise", width = 100),
       subtitle = stringr::str_wrap("Modelo de regresión lineal: lm(DIF_XG_LIBIA ~ Dif_GOB + Dif_PRES + PART_GOB + TIPO_SECC, 
    data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Diferencia votos Xóchitl-Libia", 
       x ="Diferencia de votos 1o-2o lugar",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 -> margen_xochitl_libia
#--------------------------
voto_cruzado_4_pred|>
  filter(group=="SECCION_CAT")|>  
  ggplot(aes(x,predicted,group=group,color=x))+
  geom_point(size=7)+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high),
                width = 0.2,size=1)+
  scale_y_continuous(limits = c(-13,-9),breaks = seq(-13,-9,1))+
  scale_color_manual(values = c("black","black","black"))+
  labs(title = stringr::str_wrap("Efecto del tipo de sección electoral en la diferencia de votos Xóchitl Gálvez-Libia Dennise", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión lineal: lm(DIF_XG_LIBIA ~ Dif_GOB + Dif_PRES + PART_GOB + TIPO_SECC, data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Diferencia votación Xóchitl-Libia", 
       x ="Sección electoral",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 -> margen_xochitl_libia_sec
##------------------------------
dev.off()
ggsave(margen_xochitl_libia, filename = "margen-xochitl-libia.png",width = 20, height = 10, dpi = 100)
ggsave(margen_xochitl_libia_sec, filename = "margen-xochitl-libia-secc.png",width = 20, height = 10, dpi = 100)
#----------------------------------
gana_libia_1 <-  glm(GANA_LIBIA~DIF_XG_LIBIA+DIF_CS_ALMA+DIF_JAM_YULMA+SECCION_CAT,family = "binomial",data = voto_cruzado,na.action = na.omit)
gana_alma_1 <- glm(GANA_ALMA~DIF_XG_LIBIA+DIF_CS_ALMA+DIF_JAM_YULMA+SECCION_CAT,family = "binomial",data = voto_cruzado,na.action = na.omit)

gana_libia_2 <-  glm(GANA_LIBIA~Dif_PRES+Dif_GOB+SECCION_CAT,data = voto_cruzado,family = "binomial",na.action = na.omit)
gana_alma_2 <- glm(GANA_ALMA~Dif_PRES+Dif_GOB+SECCION_CAT,data = voto_cruzado,family = "binomial",na.action = na.omit)

summary(gana_alma_2)
#---------------------------
gana_libia
gana_libia_1_pred <-  ggpredict(gana_libia_1)
gana_libia_1_pred_1 <- as.data.frame(gana_libia_1_pred[[1]])
gana_libia_1_pred_2 <- as.data.frame(gana_libia_1_pred[[2]])
gana_libia_1_pred_3 <- as.data.frame(gana_libia_1_pred[[3]])
gana_libia_1_pred_4 <- as.data.frame(gana_libia_1_pred[[4]])

gana_libia_1_pred <- rbind(gana_libia_1_pred_1,
                           gana_libia_1_pred_2,
                           gana_libia_1_pred_3)


gana_libia_1_pred
gana_libia_1_pred[2:5] <- gana_libia_1_pred[2:5]*100
gana_libia_1_pred$group <- factor(gana_libia_1_pred$group, 
                                  levels =  c("DIF_XG_LIBIA","DIF_CS_ALMA","DIF_JAM_YULMA"))

#------------------------------------------------------------------
gana_libia_1_pred|>
  ggplot(aes(x,predicted,group=group,color=group))+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high),width = 0.2,size=1)+
  geom_line(size=1)+geom_point(size=7)+
  facet_wrap(~group,ncol=3,
             labeller = as_labeller(c(
               DIF_CS_ALMA = "Diferencia votos Claudia Sheinbaum-Alma Alcaraz",
               DIF_JAM_YULMA = "Diferencia votos Jorge Álvarez Máynez-Yulma Rocha",
               DIF_XG_LIBIA="Diferencia votos Xóchitl Gálvez-Libia Dennise García")))+
  scale_color_manual(values = c("steelblue","darkred","darkorange"))+
  labs(title = stringr::str_wrap("Efecto de las diferencias voto presidente-voto gobernador en la probabilidad de triunfo de Libia Dennise en cada sección electoral", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión logística: glm(GANA_LIBIA ~ DIF_XG_LIBIA + DIF_CS_ALMA + DIF_JAM_YULMA+TIPO_SECC,family = binomial,data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Probabilidad de triunfo Libia Dennise (%)", 
       x ="Diferencia de votos",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 -> prob_libia_1

#--------------------------

as.data.frame(ggeffect(gana_libia_1)[[4]])|>
  ggplot(aes(x,predicted*100,group=group,color=x))+
  geom_point(size=7)+
  geom_errorbar(aes(ymin =conf.low*100, ymax =conf.high*100),
                width = 0.2,size=1)+
  scale_color_manual(values = c("black","black","black"))+
  labs(title = stringr::str_wrap("Probabilidad de triunfo de Libia Dennise en cada sección electoral por tipo de sección", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión logística: glm(GANA_LIBIA ~ DIF_XG_LIBIA + DIF_CS_ALMA + DIF_JAM_YULMA+TIPO_SECC,family = binomial,data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Probabilidad de triunfo Libia Dennise (%)", 
       x ="Sección electoral",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 -> prob_libia_1_sec
#-------------------------
dev.off()
ggsave(prob_libia_1, filename = "prob_libia_1.png",width = 20, height = 10, dpi = 100)
ggsave(prob_libia_1_sec, filename = "prob_libia_sec.png",width = 20, height = 10, dpi = 100)

#--------------------------------
gana_alma_1
gana_alma_1_pred <-  ggpredict(gana_alma_1)
gana_alma_1_pred_1 <- as.data.frame(gana_alma_1_pred[[1]])
gana_alma_1_pred_2 <- as.data.frame(gana_alma_1_pred[[2]])
gana_alma_1_pred_3 <- as.data.frame(gana_alma_1_pred[[3]])
#voto_cruzado_4_pred_4 <- as.data.frame(voto_cruzado_4_pred[[4]])

gana_alma_1_pred <- rbind(gana_alma_1_pred_1,
                           gana_alma_1_pred_2,
                           gana_alma_1_pred_3)

gana_alma_1_pred[2:5] <- gana_alma_1_pred[2:5]*100
gana_alma_1_pred$group <- factor(gana_alma_1_pred$group, 
                                  levels =  c("DIF_XG_LIBIA","DIF_CS_ALMA","DIF_JAM_YULMA"))

gana_alma_1_pred
#------------------------------------------------------------------
gana_alma_1_pred|>
  ggplot(aes(x,predicted,group=group,color=group))+
  geom_errorbar(aes(ymin =conf.low , ymax = conf.high))+
  geom_line(size=1)+geom_point(size=7)+
  facet_wrap(~group,ncol=3,
             labeller = as_labeller(c(
               DIF_CS_ALMA = "Diferencia votos Claudia Sheinbaum-Alma Alcaraz",
               DIF_XG_LIBIA="Diferencia Xóchitl Gálvez-Libia Dennise García",
               DIF_JAM_YULMA = "Diferencia Jorge Álvarez Máynez-Yulma Rocha")))+
  scale_color_manual(values = c("steelblue","darkred","darkorange"))+
  labs(title = stringr::str_wrap("Efecto de las diferencias voto presidente-voto gobernador en la probabilidad de triunfo de Alma Alcaraz en cada sección electoral", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión logística: glm(GANA_ALMA ~ DIF_XG_LIBIA + DIF_CS_ALMA + DIF_JAM_YULMA+TIPO_SECC,family = binomial,data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Probabilidad de triunfo Alma Alcaraz (%)", 
       x ="Diferencia de votos",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 ->prob_alma_1

#----------------------------

as.data.frame(ggeffect(gana_alma_1)[[4]])|>
  ggplot(aes(x,predicted*100,group=group,color=x))+
  geom_point(size=7)+
  geom_errorbar(aes(ymin =conf.low*100, ymax =conf.high*100),
                width = 0.2,size=1)+
  scale_color_manual(values = c("black","black","black"))+
  labs(title = stringr::str_wrap("Probabilidad de triunfo de Alma Alcaraz en cada sección electoral por tipo de sección", width = 200),
       subtitle = stringr::str_wrap("Modelo de regresión logística: glm(GANA_ALMA ~ DIF_XG_LIBIA + DIF_CS_ALMA + DIF_JAM_YULMA+TIPO_SECC,family = binomial,data = voto_cruzado, na.action = na.omit)", width = 100),
       y ="Probabilidad de triunfo Alma Alcaraz (%)", 
       x ="Sección electoral",
       caption = "Fuente: Cómputos Distritales 2024 (INE,IEEG)")+p_6 -> prob_alma_1_sec
#-------------------------
dev.off()
ggsave(prob_alma_1, filename = "prob_alma_1.png",width = 20, height = 10, dpi = 100)
ggsave(prob_alma_1_sec, filename = "prob_alma_sec.png",width = 20, height = 10, dpi = 100)
#------------------------
voto_cruzado <- voto_cruzado|>mutate(VOTO_CRUZADO=case_when(
  GANA_CS==1&GANA_ALMA==1~"CLaudia-Alma",
  GANA_CS==1&GANA_LIBIA==1~"Claudia-Libia",
  GANA_XG==1&GANA_LIBIA==1~"Xóchitl-Libia",
  GANA_XG==1&GANA_ALMA==1~"Xóchitl-Alma",
  TRUE~NA))

secciones_voto_cruzado <-  as.data.frame(table(voto_cruzado$VOTO_CRUZADO))
names(secciones_voto_cruzado) <- c("Voto","Secciones")
secciones_voto_cruzado <- secciones_voto_cruzado|>mutate(porcentaje=Secciones*100/sum(Secciones))
secciones_voto_cruzado$porcentaje <- round(secciones_voto_cruzado$porcentaje,2)
#------------------
secciones_voto_cruzado|>ggplot(aes(Voto,porcentaje,fill=Voto))+
  geom_bar(stat = "identity")+
  scale_y_continuous(limits = c(0,70),breaks = c(0,25,50))+
  scale_fill_manual(values=c("darkred","#B22222","steelblue","navy"))+
  geom_text(aes(label=paste0(porcentaje,"%")),position=position_dodge(width=0.7), vjust=-3.5,
            fontface="bold" ,family="JetBrains Mono",size=7)+
  geom_text(aes(label=paste0("(",Secciones,")")),position=position_dodge(width=0.7), vjust=-1.5,
            fontface="bold" ,family="JetBrains Mono",size=7)+
  labs(title = stringr::str_wrap("", width = 200),
       subtitle = stringr::str_wrap("Guanajuato: Porcentaje y número de secciones electorales por ganador presidente-gobernador", width = 200),
       x ="", 
       y ="",
       caption = "Fuente: Cómputos Distritales 2024 (INE, IEEG)")+p_5 -> voto_diferenciado

voto_cruzado <- voto_cruzado|>rename(VOTO_CRUZADO_CAT="VOTO_CRUZADO")
#------------------
dev.off()
ggsave(voto_diferenciado, filename = "voto_diferenciado.png",width = 20, height = 10, dpi = 100)
#-----------------
prob_cruce <-  encuesta_gto|>crosstab(q0048,q0050,FACPOND3,
                       pct_type = "row",format = "long")

prob_cruce <- prob_cruce|>select(1:3)
prob_cruce
#------
prob_cruce <-  prob_cruce|>filter(!q0048=="Ninguna")
prob_cruce <-  prob_cruce|>filter(!q0048=="No sabe")
#--------
prob_cruce$q0050 <- as.character(prob_cruce$q0050)
prob_cruce$q0050[prob_cruce$q0050=="Libia Denisse García Muñoz Ledo (PAN-PRI-PRD)"]<-"Libia\nDennise"
prob_cruce$q0050[prob_cruce$q0050=="Alma Alcaraz (MORENA-PT-PVEM)"]<-"Alma\nAlcaraz"
prob_cruce$q0050[prob_cruce$q0050=="Yulma Rocha (Movimiento Ciudadano)"]<-"Yulma\nRocha"

prob_cruce$q0050 <- factor(prob_cruce$q0050,levels = c("Libia\nDennise","Alma\nAlcaraz","Yulma\nRocha","Ninguna","No sabe"))




prob_cruce$q0048 <- as.character(prob_cruce$q0048)
prob_cruce$q0048[prob_cruce$q0048=="Libia Denisse García Muñoz Ledo (PAN-PRI-PRD)"]<-"Libia"
prob_cruce$q0048[prob_cruce$q0048=="Alma Alcaraz (MORENA-PT-PVEM)"]<-"Alma"
prob_cruce$q0048[prob_cruce$q0048=="Yulma Rocha (Movimiento Ciudadano)"]<-"Yulma"


prob_cruce$q0048 <- factor(prob_cruce$q0048,levels = c("Libia","Alma","Yulma"))
prob_cruce$pct <- round(prob_cruce$pct,0)


prob_cruce|>
  ggplot(aes(q0050,pct,group=q0050,fill=q0050))+
  geom_bar(stat = "identity")+
  facet_wrap(~q0048,labeller = as_labeller(c(
    Libia= "Segunda opción Libia Dennise García",
               Alma="Segunda opción  Alma Alcaraz",
              Yulma= "Segunda opción Yulma Rocha")))+
  scale_fill_manual(values = c("navy","darkred","darkorange","black","gray"))+
  geom_text(aes(label=paste0(pct,"%")),position=position_dodge(width=0.7), vjust=-0.5,
            fontface="bold" ,family="JetBrains Mono",size=7)+
  labs(title = stringr::str_wrap("Independientemente de quién piense votar, ¿Quién cree que tiene más posibilidades de ganar la elección?, por segunda opción a gobernadora", width = 100),
       subtitle = stringr::str_wrap("Encuesta estatal Guanajuato", width = 200),
       x ="", 
       y ="",
       caption = "Fuente: encuesta Áltica Guanajuato Mayo 2024, 2000 casos")+p_5 -> cruce_altica
#-----------------
ggsave(cruce_altica, filename = "cruce_altica.png",width = 20, height = 10, dpi = 100)


