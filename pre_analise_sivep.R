library(readr)
library(data.table)
library(dplyr)


sivep_obitos <- fread("bases/INFLUD-26-04-2021.csv")

head(sivep_obitos)

### Base de Obitos ### (Incluso 2-Óbito)
obitos_sivep <- subset(sivep_obitos, sivep_obitos$EVOLUCAO == 2)
obitos_sivep$DT_EVOLUCA <- as.Date (obitos_sivep$DT_EVOLUCA, format = '%d/%m/%Y' )
obitos_sivep$DT_NASC <- as.Date (obitos_sivep$DT_NASC, format = '%d/%m/%Y' )
obitos_sivep$IDADE = as.numeric ((obitos_sivep$DT_EVOLUCA - obitos_sivep$DT_NASC)/365.25)

### Base com dados por idade ### 
obitos_sivep_idade <- filter(obitos_sivep, IDADE < 21)


## Exclusao de colunas não necessárias para análise ### 
obitos_sivep_idade$CO_REGIONA <- NULL 
obitos_sivep_idade$CO_UNI_NOT <- NULL
obitos_sivep_idade$COD_IDADE <- NULL
obitos_sivep_idade$CO_PAIS <- NULL
obitos_sivep_idade$CO_RG_RESI <- NULL
obitos_sivep_idade$CO_MUN_RES <- NULL
obitos_sivep_idade$CO_RG_INTE <- NULL
obitos_sivep_idade$CO_MU_INTE <- NULL
obitos_sivep_idade$CO_PS_VGM <- NULL
obitos_sivep_idade$LO_PS_VGM <- NULL
obitos_sivep_idade$DT_VGM <- NULL
obitos_sivep_idade$DT_UT_DOSE <- NULL
obitos_sivep_idade$DT_VAC_MAE <- NULL
obitos_sivep_idade$DT_DOSEUNI <- NULL
obitos_sivep_idade$DT_1_DOSE <- NULL
obitos_sivep_idade$DT_2_DOSE <- NULL
obitos_sivep_idade$DT_RAIOX <- NULL
obitos_sivep_idade$DT_TOMO <- NULL
obitos_sivep_idade$DT_DIGITA <- NULL
obitos_sivep_idade$ID_REGIONA <- NULL
obitos_sivep_idade$CO_MUN_NOT <- NULL
obitos_sivep_idade$ID_RG_RESI <- NULL
obitos_sivep_idade$SURTO_SG <- NULL
obitos_sivep_idade$NOSOCOMIAL <- NULL
obitos_sivep_idade$AVE_SUINO <- NULL 
obitos_sivep_idade$ANTIVIRAL <- NULL
obitos_sivep_idade$TP_ANTIVIR <- NULL
obitos_sivep_idade$OUT_ANTIV <- NULL
obitos_sivep_idade$DT_ANTIVIR <- NULL
obitos_sivep_idade$ID_RG_INTE <- NULL
obitos_sivep_idade$DT_ENTUTI <- NULL
obitos_sivep_idade$DT_SAIDUTI <- NULL
obitos_sivep_idade$RAIOX_OUT <- NULL
obitos_sivep_idade$RAIOX_RES <- NULL
obitos_sivep_idade$AMOSTRA <- NULL
obitos_sivep_idade$DT_COLETA <- NULL
obitos_sivep_idade$TP_AMOSTRA <- NULL
obitos_sivep_idade$OUT_AMOST <- NULL
obitos_sivep_idade$DT_PCR <- NULL
obitos_sivep_idade$POS_PCRFLU <-NULL
obitos_sivep_idade$POS_AN_FLU <-NULL
obitos_sivep_idade$POS_PCROUT<-NULL
obitos_sivep_idade$PCR_FLUASU <- NULL
obitos_sivep_idade$PCR_FLUBLI <- NULL
obitos_sivep_idade$TP_FLU_PCR <- NULL
obitos_sivep_idade$FLUASU_OUT <- NULL
obitos_sivep_idade$FLUBLI_OUT<- NULL
obitos_sivep_idade$PCR_VSR<- NULL
obitos_sivep_idade$PCR_ADENO <- NULL
obitos_sivep_idade$PCR_BOCA<- NULL
obitos_sivep_idade$PCR_METAP<- NULL
obitos_sivep_idade$PCR_OUTRO<- NULL
obitos_sivep_idade$PCR_PARA1<- NULL
obitos_sivep_idade$PCR_PARA2<- NULL
obitos_sivep_idade$PCR_PARA3<- NULL
obitos_sivep_idade$PCR_PARA4<- NULL
obitos_sivep_idade$PCR_RINO<- NULL
obitos_sivep_idade$PCR_RINO<- NULL
obitos_sivep_idade$PCR_RESUL<- NULL
obitos_sivep_idade$DS_PCR_OUT <- NULL
obitos_sivep_idade$CLASSI_OUT<- NULL
obitos_sivep_idade$DT_ENCERRA<- NULL
obitos_sivep_idade$DT_RT_VGM<- NULL
obitos_sivep_idade$TOMO_RES<- NULL
obitos_sivep_idade$TOMO_OUT<- NULL
obitos_sivep_idade$OUT_ANIM<- NULL
obitos_sivep_idade$TP_TES_AN<- NULL
obitos_sivep_idade$DT_RES<- NULL
obitos_sivep_idade$DT_RES_AN<- NULL
obitos_sivep_idade$TP_FLU_AN<- NULL
obitos_sivep_idade$POS_AN_OUT<- NULL
obitos_sivep_idade$AN_ADENO<- NULL
obitos_sivep_idade$AN_OUTRO<- NULL
obitos_sivep_idade$AN_PARA1<- NULL
obitos_sivep_idade$AN_PARA2<- NULL
obitos_sivep_idade$AN_PARA3<- NULL
obitos_sivep_idade$AN_SARS2<- NULL
obitos_sivep_idade$AN_VSR<- NULL
obitos_sivep_idade$DS_AN_OUT<- NULL
obitos_sivep_idade$RES_AN <- NULL
obitos_sivep_idade$TP_AM_SOR <- NULL
obitos_sivep_idade$SOR_OUT <- NULL
obitos_sivep_idade$DT_CO_SOR<- NULL
obitos_sivep_idade$TP_SOR<- NULL
obitos_sivep_idade$OUT_SOR<- NULL
obitos_sivep_idade$RES_IGG<- NULL
obitos_sivep_idade$RES_IGA<- NULL
obitos_sivep_idade$RES_IGM<- NULL
obitos_sivep_idade$PCR_SARS2<- NULL
obitos_sivep_idade$OBES_IMC <- NULL

##Exlusão obitos de pessoas não residentes no Brasil ## 
obitos_sivep_brasil <- subset(obitos_sivep_idade, 
                              obitos_sivep_idade$ID_PAIS == "BRASIL")

# Analise dos obitos por municipio de residencia
obitos_sivep_brasil$CASO <- 1
sum(obitos_sivep_brasil$CASO, na.rm = T) # TOTAL DE OBITOS POR SRAG DE 0-20 anos - BRASIL

obitos_municipio <- obitos_sivep_brasil %>%
  group_by(ID_MN_RESI, SG_UF, CS_ZONA)%>%
  summarise(OBITOS_MUNICIPIO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_MUNICIPIO)

### ZONA ###
obitos_municipio_zona <- obitos_sivep_brasil %>%
  group_by(CS_ZONA)%>%
  summarise(OBITOS_ZONA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ZONA)

## Obitos por Estado ##
obitos_estado <- obitos_sivep_brasil %>%
  group_by(SG_UF)%>%
  summarise(OBITOS_ESTADO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ESTADO)

## Obitos em capítais ###
obitos_capitais_brasil <- subset (obitos_sivep_brasil, obitos_sivep_brasil$ID_MN_RESI
                                  %in% c("RIO BRANCO","MACAPA", "MANAUS", "BELEM",
                                         "PORTO VELHO","BOA VISTA", "PALMAS",
                                         "SALVADOR", "FORTALEZA", "MACEIO", "SAO LUIS",
                                         "JOAO PESSOA", "RECIFE", "TERESINA", "NATAL", "ARACAJU",
                                         "GOIANIA", "CUIABA", "CAMPO GRANDE", "BRASILIA",
                                         "VITORIA", "BELO HORIZONTE", "RIO DE JANEIRO", "SAO PAULO",
                                         "CURITIBA", "FLORIANOPOLIS","PORTO ALEGRE"))

obitos_capitais <- obitos_capitais_brasil %>%
  group_by(ID_MN_RESI)%>%
  summarise(OBITOS_CAPITAIS = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_CAPITAIS)

## ANALISE POR SEXO BRASIL ## 
obitos_sexo <- obitos_sivep_brasil %>%
  group_by(CS_SEXO)%>%
  summarise(OBITOS_SEXO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_SEXO)

## ANALISE RAÇA BRASIL##
obitos_raca <- obitos_sivep_brasil %>%
  group_by(CS_RACA)%>%
  summarise(OBITOS_RACA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_RACA)

obitos_raca <- obitos_raca[!is.na(obitos_raca$CS_RACA),]
com_dados_sobre <- sum(!is.na(obitos_sivep_brasil$CS_RACA))
com_dados_sobre
sem_dados_sobre <- sum(is.na(obitos_sivep_brasil$CS_RACA))
sem_dados_sobre
sem_dados_sobre/(com_dados_sobre+sem_dados_sobre)*100

### ANALISE FATOR DE RISCO ### 
obitos_fator_risco <- obitos_sivep_brasil %>%
  group_by(FATOR_RISC)%>%
  summarise(OBITOS_FATOR_RISC = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_FATOR_RISC)

######## COMORBIDADES #############

obitos_comorb <- obitos_sivep_brasil %>%
  dplyr::select("CASO",
                "CARDIOPATI",
                "HEMATOLOGI",
                "SIND_DOWN",
                "HEPATICA",
                "ASMA",
                "DIABETES",
                "NEUROLOGIC",
                "PNEUMOPATI",
                "IMUNODEPRE",
                "RENAL",
                "OBESIDADE", 
                "OUT_MORBI")


obitos_comorb <- melt(obitos_comorb, id.vars = "CASO")

obitos_comorb <- obitos_comorb %>%
  group_by(variable, value) %>%
  summarise("QUANTIDADE" = sum(CASO,na.rm = T))

obitos_comorb$value <- as.factor(obitos_comorb$value)

obitos_comorb <- dcast(formula = variable ~ value, value.var =  "QUANTIDADE", obitos_comorb)

names(obitos_comorb) <- c("AGRAVO", "SIM", "NÃO", "IGNORADO", "SEM_DADOS")

