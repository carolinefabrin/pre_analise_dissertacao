library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)

inter_sivep_21 <- fread("bases/INFLUD21-24-05-2021.csv")

head(inter_sivep_21)

################# Base de Internação ############### 

inter_sivep_21$DT_INTERNA <- as.Date (inter_sivep_21$DT_INTERNA, format = '%d/%m/%Y' )
inter_sivep_21$DT_NASC <- as.Date (inter_sivep_21$DT_NASC, format = '%d/%m/%Y' )
inter_sivep_21$IDADE = as.numeric ((inter_sivep_21$DT_INTERNA - inter_sivep_21$DT_NASC)/365.25)


################### Base com dados por idade e somente residentes Brasil ############################ 

inter_sivep_21 <- filter(inter_sivep_21, IDADE < 19)
inter_sivep_21 <- subset(inter_sivep_21,inter_sivep_21$ID_PAIS == "BRASIL")

##############  Exclusao de colunas não necessárias para análise ########## 

inter_sivep_21$SEM_NOT <- NULL 
inter_sivep_21$SEM_PRI <- NULL 
inter_sivep_21$CO_REGIONA <- NULL 
inter_sivep_21$CO_UNI_NOT <- NULL
inter_sivep_21$COD_IDADE <- NULL
inter_sivep_21$CO_PAIS <- NULL
inter_sivep_21$CO_RG_RESI <- NULL
inter_sivep_21$CO_RG_INTE <- NULL
inter_sivep_21$CO_MU_INTE <- NULL
inter_sivep_21$CO_PS_VGM <- NULL
inter_sivep_21$LO_PS_VGM <- NULL
inter_sivep_21$DT_VGM <- NULL
inter_sivep_21$DT_UT_DOSE <- NULL
inter_sivep_21$DT_VAC_MAE <- NULL
inter_sivep_21$DT_DOSEUNI <- NULL
inter_sivep_21$DT_1_DOSE <- NULL
inter_sivep_21$DT_2_DOSE <- NULL
inter_sivep_21$DT_DIGITA <- NULL
inter_sivep_21$ID_REGIONA <- NULL
inter_sivep_21$CO_MUN_NOT <- NULL
inter_sivep_21$ID_RG_RESI <- NULL
inter_sivep_21$SURTO_SG <- NULL
inter_sivep_21$NOSOCOMIAL <- NULL
inter_sivep_21$AVE_SUINO <- NULL 
inter_sivep_21$ANTIVIRAL <- NULL
inter_sivep_21$TP_ANTIVIR <- NULL
inter_sivep_21$OUT_ANTIV <- NULL
inter_sivep_21$DT_ANTIVIR <- NULL
inter_sivep_21$ID_RG_INTE <- NULL
inter_sivep_21$DT_ENTUTI <- NULL
inter_sivep_21$DT_SAIDUTI <- NULL
inter_sivep_21$RAIOX_OUT <- NULL

inter_sivep_21$DT_COLETA <- NULL
inter_sivep_21$TP_AMOSTRA <- NULL
inter_sivep_21$OUT_AMOST <- NULL
inter_sivep_21$DT_PCR <- NULL
inter_sivep_21$POS_PCRFLU <-NULL
inter_sivep_21$POS_AN_FLU <-NULL
inter_sivep_21$POS_PCROUT<-NULL
inter_sivep_21$PCR_FLUASU <- NULL
inter_sivep_21$PCR_FLUBLI <- NULL
inter_sivep_21$TP_FLU_PCR <- NULL
inter_sivep_21$FLUASU_OUT <- NULL
inter_sivep_21$FLUBLI_OUT<- NULL
inter_sivep_21$PCR_VSR<- NULL
inter_sivep_21$PCR_ADENO <- NULL
inter_sivep_21$PCR_BOCA<- NULL
inter_sivep_21$PCR_METAP<- NULL
inter_sivep_21$PCR_OUTRO<- NULL
inter_sivep_21$PCR_PARA1<- NULL
inter_sivep_21$PCR_PARA2<- NULL
inter_sivep_21$PCR_PARA3<- NULL
inter_sivep_21$PCR_PARA4<- NULL
inter_sivep_21$PCR_RINO<- NULL
inter_sivep_21$PCR_RINO<- NULL
inter_sivep_21$DS_PCR_OUT <- NULL
inter_sivep_21$CLASSI_OUT<- NULL
inter_sivep_21$DT_ENCERRA<- NULL
inter_sivep_21$DT_RT_VGM<- NULL
inter_sivep_21$TOMO_OUT<- NULL
inter_sivep_21$OUT_ANIM<- NULL
inter_sivep_21$TP_TES_AN<- NULL
inter_sivep_21$DT_RES<- NULL
inter_sivep_21$DT_RES_AN<- NULL
inter_sivep_21$TP_FLU_AN<- NULL
inter_sivep_21$POS_AN_OUT<- NULL
inter_sivep_21$AN_ADENO<- NULL
inter_sivep_21$AN_OUTRO<- NULL
inter_sivep_21$AN_PARA1<- NULL
inter_sivep_21$AN_PARA2<- NULL
inter_sivep_21$AN_PARA3<- NULL
inter_sivep_21$AN_SARS2<- NULL
inter_sivep_21$AN_VSR<- NULL
inter_sivep_21$DS_AN_OUT<- NULL
inter_sivep_21$RES_AN <- NULL
inter_sivep_21$TP_AM_SOR <- NULL
inter_sivep_21$SOR_OUT <- NULL
inter_sivep_21$DT_CO_SOR<- NULL
inter_sivep_21$TP_SOR<- NULL
inter_sivep_21$OUT_SOR<- NULL
inter_sivep_21$RES_IGG<- NULL
inter_sivep_21$RES_IGA<- NULL
inter_sivep_21$RES_IGM<- NULL
inter_sivep_21$OBES_IMC <- NULL
inter_sivep_21$PAC_COCBO <- NULL
inter_sivep_21$PAC_DSCBO <- NULL
inter_sivep_21$HISTO_VGM <- NULL
inter_sivep_21$PAIS_VGM <- NULL


########### ANALISE COM DESFECHO (4-SRAG não especificado 5-SRAG por COVID-19)

### 0-19 anos ###
inter_sivep_21 <- subset(inter_sivep_21, inter_sivep_21$CLASSI_FIN
                         %in% c(4,5))
inter_sivep_21$CASO <-1
sum (inter_sivep_21$CASO) #### TOTAL DE OBITOS ###

## INTERNACAO #
inter_sivep_hosp <- subset(inter_sivep_21, inter_sivep_21$HOSPITAL == 1)
sum (inter_sivep_hosp$CASO)

## UTI ## 
inter_sivep_uti <- subset(inter_sivep_21, inter_sivep_21$UTI == 1)
sum (inter_sivep_uti$CASO)

## Suporte Ventilatorio #
inter_sivep_vent <- subset(inter_sivep_21, inter_sivep_21$SUPORT_VEN %in% c(1,2))
sum (inter_sivep_vent$CASO)

## Raio X ##
inter_sivep_rx <- subset(inter_sivep_21, inter_sivep_21$RAIOX_RES == 6) #Não realizado
sum (inter_sivep_rx$CASO)

rx_nao_realizado <- inter_sivep_rx %>%
  group_by(ID_MN_RESI, SG_UF, CS_ZONA) %>%
  summarise(OBITOS_MUNICIPIO = sum(CASO, na.rm = T)) %>%
  arrange(OBITOS_MUNICIPIO)


## Tomografia ##
inter_sivep_tomo <- subset(inter_sivep_21, inter_sivep_21$TOMO_RES == 6) #Não realizado
sum (inter_sivep_tomo$CASO)

## PCR ##
inter_sivep_pcr <- subset(inter_sivep_21, inter_sivep_21$PCR_RESUL == 4) #Não realizado
sum (inter_sivep_pcr$CASO)






library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)  
## Load sandwich package for robust estimator of glm
library(sandwich)
## Load lmtest package for coeftest of glm
library(lmtest)
library(sjPlot)
library(stargazer)



##Junção base 2020 com 2021
base_inter <- bind_rows (inter_sivep_20, inter_sivep_21)

base_inter <- base_inter %>% select(DT_SIN_PRI,
                                    DT_INTERNA,
                                    CO_MUN_RES,
                                    ID_MN_RESI,
                                    AMOSTRA,
                                    UTI,
                                    SUPORT_VEN,
                                    DT_RAIOX,
                                    DT_TOMO,
                                    EVOLUCAO,
                                    IDADE,
                                    CASO)


idh <- read_excel("bases/Atlas 2013_municipal, estadual e Brasil.xlsx", 
                  sheet = "MUN 91-00-10")

idh <- subset(idh, idh$ANO == 2010)
idh <- idh %>% select(CO_MUN_RES = Codmun6, IDHM)
idh <- mutate(idh, QUINTIL_IDH = ntile(idh$IDHM,5)) #Coluna com quantil

#Ajustando código das cidades satélites
##Código do DF é 530010

base_inter$CO_MUN_RES <- ifelse(substr(base_inter$CO_MUN_RES,1,2) == "53", 530010, base_inter$CO_MUN_RES)

base_inter <- merge(base_inter, idh, by = "CO_MUN_RES", all = T)

#Excluindo municípios sem idhm
base_inter <- subset(base_inter, !is.na(base_inter$IDHM))

#Excluindo pacientes sem municípios 
base_inter <- subset(base_inter, !is.na(base_inter$CO_MUN_RES))

#Excluindo pacientes sem evolução 
base_inter <- subset(base_inter, !is.na(base_inter$EVOLUCAO))


base_inter$OBITO <- ifelse(base_inter$EVOLUCAO == 2, 1,0)  #Coluna de óbito: 0 = NÃO ÕBITO, 1 = ÕBITO
base_inter$AMOSTRA <- ifelse(base_inter$AMOSTRA == 1, 1,0) #Coluna de coleta de amostra: 0 = NÃO COLETOU; 1 = COLETOU
base_inter$DT_RAIOX <- ifelse(is.na(base_inter$DT_RAIOX), 0,1) #Coluna Rx: 0 = NÃO FEZ; 1 = FEZ
base_inter$DT_TOMO <- ifelse(is.na(base_inter$DT_TOMO), 0,1) #Coluna Tomo: 0 = NÃO FEZ; 1 = FEZ
base_inter$UTI <- ifelse(base_inter$UTI == 1, 1,0) #Coluna de UTI: 0 = NÃO UTI; 1 = UTI

base_inter$QUINTIL_IDH <- as.factor(base_inter$QUINTIL_IDH)


#Dividindo UTI
base_uti <- subset(base_inter, base_inter$UTI == 1)

#Dividindo OBITO
base_obito <- subset(base_inter, base_inter$OBITO == 1)


#Dividindo idades
idade_0_11_inter <- subset(base_inter, base_inter$IDADE < 12)
idade_12_18_inter <- subset(base_inter, base_inter$IDADE >= 12)
idade_0_11_uti <- subset(base_uti, base_uti$IDADE < 12)
idade_12_18_uti <- subset(base_uti, base_uti$IDADE >= 12)
idade_0_11_obito <- subset(base_obito, base_obito$IDADE < 12)
idade_12_18_obito <- subset(base_obito, base_obito$IDADE >= 12)



###########################################################################
#Função para estimação
###########################################################################
#Estimando relações - http://rstudio-pubs-static.s3.amazonaws.com/5752_fc41dca85dd24539bc99868697de83d0.html
esimativa_rr <- function(variavel, base){
  glm.regre <- glm(variavel ~ QUINTIL_IDH, family="poisson"(link = "log"), data = base)
  ## Poisson model with SE estimated via robust variance estimator
  se_robusto <- coeftest(glm.regre, vcov = sandwich)
  ## Risk (intercept) and risk ratios
  rr <- exp(coef(glm.regre))
  resultado <- list(glm.regre, se_robusto, rr)
  return(resultado)
  
}

###########################################################################
#Internados
###########################################################################

idade_0_11_inter_amostra <- esimativa_rr(idade_0_11_inter$AMOSTRA, idade_0_11_inter)
idade_0_11_inter_raio_x <- esimativa_rr(idade_0_11_inter$DT_RAIOX, idade_0_11_inter)
idade_0_11_inter_tomo <- esimativa_rr(idade_0_11_inter$DT_TOMO, idade_0_11_inter)
idade_0_11_inter_vent <- esimativa_rr(idade_0_11_inter$SUPORT_VEN, idade_0_11_inter)
idade_0_11_inter_uti <- esimativa_rr(idade_0_11_inter$UTI, idade_0_11_inter)
idade_0_11_inter_obito <- esimativa_rr(idade_0_11_inter$OBITO, idade_0_11_inter)


idade_12_18_inter_amostra <- esimativa_rr(idade_12_18_inter$AMOSTRA, idade_12_18_inter)
idade_12_18_inter_raio_x <- esimativa_rr(idade_12_18_inter$DT_RAIOX, idade_12_18_inter)
idade_12_18_inter_tomo <- esimativa_rr(idade_12_18_inter$DT_TOMO, idade_12_18_inter)
idade_12_18_inter_vent <- esimativa_rr(idade_12_18_inter$SUPORT_VEN, idade_12_18_inter)
idade_12_18_inter_uti <- esimativa_rr(idade_12_18_inter$UTI, idade_12_18_inter)
idade_12_18_inter_obito <- esimativa_rr(idade_12_18_inter$OBITO, idade_12_18_inter)

stargazer(idade_0_11_inter_amostra[[2]],
          idade_0_11_inter_raio_x[[2]],
          idade_0_11_inter_tomo[[2]],
          idade_0_11_inter_vent[[2]],
          idade_0_11_inter_uti[[2]],
          idade_0_11_inter_obito[[2]],
          type="html",
          out="inter_0_11.doc",
          intercept.bottom = F,
          intercept.top = T,
          digits=2,ci = T)


stargazer(idade_12_18_inter_amostra[[2]],
          idade_12_18_inter_raio_x[[2]],
          idade_12_18_inter_tomo[[2]],
          idade_12_18_inter_vent[[2]],
          idade_12_18_inter_uti[[2]],
          idade_12_18_inter_obito[[2]],
          type="html",
          out="inter_12_18.doc",
          intercept.bottom = F,
          intercept.top = T,
          digits=2,ci = T)

###########################################################################
#UTI
###########################################################################

idade_0_11_uti_amostra <- esimativa_rr(idade_0_11_uti$AMOSTRA, idade_0_11_uti)
idade_0_11_uti_raio_x <- esimativa_rr(idade_0_11_uti$DT_RAIOX, idade_0_11_uti)
idade_0_11_uti_tomo <- esimativa_rr(idade_0_11_uti$DT_TOMO, idade_0_11_uti)
idade_0_11_uti_vent <- esimativa_rr(idade_0_11_uti$SUPORT_VEN, idade_0_11_uti)
idade_0_11_uti_obito <- esimativa_rr(idade_0_11_uti$OBITO, idade_0_11_uti)


idade_12_18_uti_amostra <- esimativa_rr(idade_12_18_uti$AMOSTRA, idade_12_18_uti)
idade_12_18_uti_raio_x <- esimativa_rr(idade_12_18_uti$DT_RAIOX, idade_12_18_uti)
idade_12_18_uti_tomo <- esimativa_rr(idade_12_18_uti$DT_TOMO, idade_12_18_uti)
idade_12_18_uti_vent <- esimativa_rr(idade_12_18_uti$SUPORT_VEN, idade_12_18_uti)
idade_12_18_uti_obito <- esimativa_rr(idade_12_18_uti$OBITO, idade_12_18_uti)

stargazer(idade_0_11_uti_amostra[[2]],
          idade_0_11_uti_raio_x[[2]],
          idade_0_11_uti_tomo[[2]],
          idade_0_11_uti_vent[[2]],
          idade_0_11_uti_obito[[2]],
          type="html",
          out="uti_0_11.doc",
          intercept.bottom = F,
          intercept.top = T,
          digits=2,ci = T)


stargazer(idade_12_18_uti_amostra[[2]],
          idade_12_18_uti_raio_x[[2]],
          idade_12_18_uti_tomo[[2]],
          idade_12_18_uti_vent[[2]],
          idade_12_18_uti_obito[[2]],
          type="html",
          out="uti_12_18.doc",
          intercept.bottom = F,
          intercept.top = T,
          digits=2,ci = T)

###########################################################################
#Obito
###########################################################################

idade_0_11_obito_amostra <- esimativa_rr(idade_0_11_obito$AMOSTRA, idade_0_11_obito)
idade_0_11_obito_raio_x <- esimativa_rr(idade_0_11_obito$DT_RAIOX, idade_0_11_obito)
idade_0_11_obito_tomo <- esimativa_rr(idade_0_11_obito$DT_TOMO, idade_0_11_obito)
idade_0_11_obito_vent <- esimativa_rr(idade_0_11_obito$SUPORT_VEN, idade_0_11_obito)
idade_0_11_obito_uti <- esimativa_rr(idade_0_11_obito$UTI, idade_0_11_obito)


idade_12_18_obito_amostra <- esimativa_rr(idade_12_18_obito$AMOSTRA, idade_12_18_obito)
idade_12_18_obito_raio_x <- esimativa_rr(idade_12_18_obito$DT_RAIOX, idade_12_18_obito)
idade_12_18_obito_tomo <- esimativa_rr(idade_12_18_obito$DT_TOMO, idade_12_18_obito)
idade_12_18_obito_vent <- esimativa_rr(idade_12_18_obito$SUPORT_VEN, idade_12_18_obito)
idade_12_18_obito_uti <- esimativa_rr(idade_12_18_obito$UTI, idade_12_18_obito)

stargazer(idade_0_11_obito_amostra[[2]],
          idade_0_11_obito_raio_x[[2]],
          idade_0_11_obito_tomo[[2]],
          idade_0_11_obito_vent[[2]],
          idade_0_11_obito_uti[[2]],
          type="html",
          out="obito_0_11.doc",
          intercept.bottom = F,
          intercept.top = T,
          digits=2,ci = T)


stargazer(idade_12_18_obito_amostra[[2]],
          idade_12_18_obito_raio_x[[2]],
          idade_12_18_obito_tomo[[2]],
          idade_12_18_obito_vent[[2]],
          idade_12_18_obito_uti[[2]],
          type="html",
          out="obito_12_18.doc",
          intercept.bottom = F,
          intercept.top = T,
          digits=2,ci = T)




#crianças internadas que morreram e não foram para UTI
obito_nao_uti <- subset(base_inter, base_inter$UTI == 0 & base_inter$OBITO ==1)
ggplot(obito_nao_uti, aes(QUINTIL_IDH, CASO)) +
  geom_col()
obito_nao_uti_sp <- subset(obito_nao_uti, substr(obito_nao_uti$CO_MUN_RES,1,2) == 35)
obito_nao_uti_idh_alto <- subset(obito_nao_uti, obito_nao_uti$QUINTIL_IDH == 5)

obito_nao_uti_idh_alto <- obito_nao_uti_idh_alto %>%
  group_by(ID_MN_RESI) %>%
  summarise(CASO = sum(CASO, na.rm = T))

ggplot(subset(obito_nao_uti_idh_alto, obito_nao_uti_idh_alto$CASO >10), aes(ID_MN_RESI, CASO)) +
  geom_col()+
  coord_flip()

glm.obito_nao_ut <- glm(OBITO ~ QUINTIL_IDH*UTI, family="poisson"(link = "log"), data = base_inter)
## Poisson model with SE estimated via robust variance estimator
coeftest(glm.obito_nao_ut, vcov = sandwich)
## Risk (intercept) and risk ratios
exp(coef(glm.obito_nao_ut))


