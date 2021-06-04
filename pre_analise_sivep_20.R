library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)

inter_sivep_20 <- fread("bases/INFLUD-24-05-2021.csv")

head(inter_sivep_20)

################# Base de Internação ############### 

inter_sivep_20$DT_INTERNA <- as.Date (inter_sivep_20$DT_INTERNA, format = '%d/%m/%Y' )
inter_sivep_20$DT_NASC <- as.Date (inter_sivep_20$DT_NASC, format = '%d/%m/%Y' )
inter_sivep_20$IDADE = as.numeric ((inter_sivep_20$DT_INTERNA - inter_sivep_20$DT_NASC)/365.25)


################### Base com dados por idade e somente residentes Brasil ############################ 

inter_sivep_20 <- filter(inter_sivep_20, IDADE < 19)
inter_sivep_20 <- subset(inter_sivep_20,inter_sivep_20$ID_PAIS == "BRASIL")

##############  Exclusao de colunas não necessárias para análise ########## 

inter_sivep_20$SEM_NOT <- NULL 
inter_sivep_20$SEM_PRI <- NULL 
inter_sivep_20$CO_REGIONA <- NULL 
inter_sivep_20$CO_UNI_NOT <- NULL
inter_sivep_20$COD_IDADE <- NULL
inter_sivep_20$CO_PAIS <- NULL
inter_sivep_20$CO_RG_RESI <- NULL
inter_sivep_20$CO_RG_INTE <- NULL
inter_sivep_20$CO_MU_INTE <- NULL
inter_sivep_20$CO_PS_VGM <- NULL
inter_sivep_20$LO_PS_VGM <- NULL
inter_sivep_20$DT_VGM <- NULL
inter_sivep_20$DT_UT_DOSE <- NULL
inter_sivep_20$DT_VAC_MAE <- NULL
inter_sivep_20$DT_DOSEUNI <- NULL
inter_sivep_20$DT_1_DOSE <- NULL
inter_sivep_20$DT_2_DOSE <- NULL
inter_sivep_20$DT_DIGITA <- NULL
inter_sivep_20$ID_REGIONA <- NULL
inter_sivep_20$CO_MUN_NOT <- NULL
inter_sivep_20$ID_RG_RESI <- NULL
inter_sivep_20$SURTO_SG <- NULL
inter_sivep_20$NOSOCOMIAL <- NULL
inter_sivep_20$AVE_SUINO <- NULL 
inter_sivep_20$ANTIVIRAL <- NULL
inter_sivep_20$TP_ANTIVIR <- NULL
inter_sivep_20$OUT_ANTIV <- NULL
inter_sivep_20$DT_ANTIVIR <- NULL
inter_sivep_20$ID_RG_INTE <- NULL
inter_sivep_20$DT_ENTUTI <- NULL
inter_sivep_20$DT_SAIDUTI <- NULL
inter_sivep_20$RAIOX_OUT <- NULL

inter_sivep_20$DT_COLETA <- NULL
inter_sivep_20$TP_AMOSTRA <- NULL
inter_sivep_20$OUT_AMOST <- NULL
inter_sivep_20$DT_PCR <- NULL
inter_sivep_20$POS_PCRFLU <-NULL
inter_sivep_20$POS_AN_FLU <-NULL
inter_sivep_20$POS_PCROUT<-NULL
inter_sivep_20$PCR_FLUASU <- NULL
inter_sivep_20$PCR_FLUBLI <- NULL
inter_sivep_20$TP_FLU_PCR <- NULL
inter_sivep_20$FLUASU_OUT <- NULL
inter_sivep_20$FLUBLI_OUT<- NULL
inter_sivep_20$PCR_VSR<- NULL
inter_sivep_20$PCR_ADENO <- NULL
inter_sivep_20$PCR_BOCA<- NULL
inter_sivep_20$PCR_METAP<- NULL
inter_sivep_20$PCR_OUTRO<- NULL
inter_sivep_20$PCR_PARA1<- NULL
inter_sivep_20$PCR_PARA2<- NULL
inter_sivep_20$PCR_PARA3<- NULL
inter_sivep_20$PCR_PARA4<- NULL
inter_sivep_20$PCR_RINO<- NULL
inter_sivep_20$PCR_RINO<- NULL
inter_sivep_20$DS_PCR_OUT <- NULL
inter_sivep_20$CLASSI_OUT<- NULL
inter_sivep_20$DT_ENCERRA<- NULL
inter_sivep_20$DT_RT_VGM<- NULL
inter_sivep_20$TOMO_OUT<- NULL
inter_sivep_20$OUT_ANIM<- NULL
inter_sivep_20$TP_TES_AN<- NULL
inter_sivep_20$DT_RES<- NULL
inter_sivep_20$DT_RES_AN<- NULL
inter_sivep_20$TP_FLU_AN<- NULL
inter_sivep_20$POS_AN_OUT<- NULL
inter_sivep_20$AN_ADENO<- NULL
inter_sivep_20$AN_OUTRO<- NULL
inter_sivep_20$AN_PARA1<- NULL
inter_sivep_20$AN_PARA2<- NULL
inter_sivep_20$AN_PARA3<- NULL
inter_sivep_20$AN_SARS2<- NULL
inter_sivep_20$AN_VSR<- NULL
inter_sivep_20$DS_AN_OUT<- NULL
inter_sivep_20$RES_AN <- NULL
inter_sivep_20$TP_AM_SOR <- NULL
inter_sivep_20$SOR_OUT <- NULL
inter_sivep_20$DT_CO_SOR<- NULL
inter_sivep_20$TP_SOR<- NULL
inter_sivep_20$OUT_SOR<- NULL
inter_sivep_20$RES_IGG<- NULL
inter_sivep_20$RES_IGA<- NULL
inter_sivep_20$RES_IGM<- NULL
inter_sivep_20$OBES_IMC <- NULL
inter_sivep_20$PAC_COCBO <- NULL
inter_sivep_20$PAC_DSCBO <- NULL
inter_sivep_20$HISTO_VGM <- NULL
inter_sivep_20$PAIS_VGM <- NULL


########### ANALISE COM DESFECHO (4-SRAG não especificado 5-SRAG por COVID-19)

### 0-19 anos ###
inter_sivep_20 <- subset(inter_sivep_20, inter_sivep_20$CLASSI_FIN
                      %in% c(4,5))
inter_sivep_20$CASO <-1
sum (inter_sivep_20$CASO) #### TOTAL DE OBITOS ###

## INTERNACAO #
inter_sivep_hosp <- subset(inter_sivep_20, inter_sivep_20$HOSPITAL == 1)
sum (inter_sivep_hosp$CASO)

## UTI ## 
inter_sivep_uti <- subset(inter_sivep_20, inter_sivep_20$UTI == 1)
sum (inter_sivep_uti$CASO)

## Suporte Ventilatorio #
inter_sivep_vent <- subset(inter_sivep_20, inter_sivep_20$SUPORT_VEN %in% c(1,2))
sum (inter_sivep_vent$CASO)

## Raio X ##
inter_sivep_rx <- subset(inter_sivep_20, inter_sivep_20$RAIOX_RES == 6) #Não realizado
sum (inter_sivep_rx$CASO)

rx_nao_realizado <- inter_sivep_rx %>%
  group_by(ID_MN_RESI, SG_UF, CS_ZONA) %>%
  summarise(OBITOS_MUNICIPIO = sum(CASO, na.rm = T)) %>%
  arrange(OBITOS_MUNICIPIO)


## Tomografia ##
inter_sivep_tomo <- subset(inter_sivep_20, inter_sivep_20$TOMO_RES == 6) #Não realizado
sum (inter_sivep_tomo$CASO)

## PCR ##
inter_sivep_pcr <- subset(inter_sivep_20, inter_sivep_20$PCR_RESUL == 4) #Não realizado
sum (inter_sivep_pcr$CASO)
