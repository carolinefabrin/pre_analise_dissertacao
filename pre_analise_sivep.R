library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(readxl)

sivep_obitos <- fread("bases/INFLUD-26-04-2021.csv")

head(sivep_obitos)

Obitos_Estado <- read_excel("bases/Obitos Estado.xlsx")

obitos_td_capitais <- read_excel("bases/obitos_td_capitais.xlsx")

obitos_cca_estado <- read_excel("bases/obitos_cca_estado.xlsx")

obitos_cca_capitais <- read_excel("bases/obitos_cca_capitais.xlsx")

raca_td <- read_excel("bases/raca_td.xlsx")

base_raca_srag <- read_excel("bases/base_raca_srag.xlsx")

base_estados_srag_0_19 <- read_excel("bases/base_estados_srag_0-19.xlsx")

base_capitais_srag_0_19 <- read_excel("bases/base_capitais_srag_0-19.xlsx")

base_estado_srag_cca <- read_excel("bases/base_estado_srag_cca.xlsx")

base_capitais_srag_cca <- read_excel("bases/base_capitais_srag_cca.xlsx")


################# Base de Obitos ############### (Incluso 2-Óbito)

obitos_sivep <- subset(sivep_obitos, sivep_obitos$EVOLUCAO == 2)
obitos_sivep$DT_EVOLUCA <- as.Date (obitos_sivep$DT_EVOLUCA, format = '%d/%m/%Y' )
obitos_sivep$DT_NASC <- as.Date (obitos_sivep$DT_NASC, format = '%d/%m/%Y' )
obitos_sivep$IDADE = as.numeric ((obitos_sivep$DT_EVOLUCA - obitos_sivep$DT_NASC)/365.25)


################### Base com dados por idade ############################ 

obitos_sivep_idade <- filter(obitos_sivep, IDADE < 20)


##############  Exclusao de colunas não necessárias para análise ########## 

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


###########    Exlusão obitos de pessoas não residentes no Brasil  ################ 

obitos_sivep_brasil <- subset(obitos_sivep_idade, 
                              obitos_sivep_idade$ID_PAIS == "BRASIL")


############ Analise dos obitos por municipio de residencia ################

obitos_sivep_brasil$CASO <- 1
sum(obitos_sivep_brasil$CASO, na.rm = T) #### TOTAL DE OBITOS POR SRAG DE 0-19 anos - BRASIL #######

obitos_municipio <- obitos_sivep_brasil %>%
  group_by(ID_MN_RESI, SG_UF, CS_ZONA)%>%
  summarise(OBITOS_MUNICIPIO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_MUNICIPIO)


################ ZONA DE RESIDENCIA ##############
obitos_municipio_zona <- obitos_sivep_brasil %>%
  group_by(CS_ZONA)%>%
  summarise(OBITOS_ZONA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ZONA)



############ ANALISE POR SEXO  ############# 
obitos_sexo <- obitos_sivep_brasil %>%
  group_by(CS_SEXO)%>%
  summarise(OBITOS_SEXO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_SEXO)



########## ANALISE RAÇA ##################
obitos_raca <- obitos_sivep_brasil %>%
  group_by(CS_RACA)%>%
  summarise(OBITOS_RACA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_RACA)

# DADOS EM CSV
write.csv(obitos_raca, "obitos_raca.csv", row.names = F)

obitos_raca <- obitos_raca[!is.na(obitos_raca$CS_RACA),]
com_dados_sobre <- sum(!is.na(obitos_sivep_brasil$CS_RACA))
com_dados_sobre
sem_dados_sobre <- sum(is.na(obitos_sivep_brasil$CS_RACA))
sem_dados_sobre
sem_dados_sobre/(com_dados_sobre+sem_dados_sobre)*100

##GRAFICO RAÇA ###
#POPULACAO_2020 = TOTAL DA POPULACAO DE 0-19 anos
raca_td$TAXA_MORTALIDADE <- raca_td$OBITOS*100 / raca_td$POPULACAO_2020

ggplot(raca_td, aes(x = RACA, y = TAXA_MORTALIDADE*100, fill=RACA ))+
  geom_col()+
  theme_bw()+
  xlab("População 0-19 anos")+
  ylab("Taxa Mortalidade") +
  labs(fill="Raça") 


########### ANALISE FATOR DE RISCO ########### 
obitos_fator_risco <- obitos_sivep_brasil %>%
  group_by(FATOR_RISC)%>%
  summarise(OBITOS_FATOR_RISC = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_FATOR_RISC)


########### COMORBIDADES #############

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

# DADOS EM CSV
write.csv(obitos_comorb, "obitos_comorb.csv", row.names = F)



########################## OBITOS POR ESTADO 0-19anos #################################

obitos_estado <- obitos_sivep_brasil %>%
  group_by(SG_UF)%>%
  summarise(OBITOS_ESTADO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ESTADO)

#GRAFICO ESTADO#
ggplot(obitos_estado, aes(x = SG_UF, y = OBITOS_ESTADO, 
    color= cut(OBITOS_ESTADO, breaks = c(0, 100, 200, 300, Inf)))) +
    geom_col()+
    theme(legend.position = "none")+
    xlab("Estados")+
    ylab("Obitos")+
    scale_color_manual(values = c('green', 'yellow', 'red', 'black'),
    limits = c('(0,100]', '(100,200]', '(200,300]','(300,Inf]'))

#DADOE EM CSV#
write.csv(obitos_estado, "obitos_estado.csv", row.names = F)


######### GRAFICO TAXA DE MORTALIDADE EM ESTADOS 0-19anos ########### 

#RASCUNHO###            Obitos_Estado$REGIAO   ifelse(Obitos_Estado$UF == "AC","AP","AM","PA","RO","RR","TO", "NORTE",
                        ifelse(Obitos_Estado$UF == "AL", "BA","CE", "MA", "PB", "PE", "PI", "RN", "SE", "NORDESTE",
                        ifelse(Obitos_Estado$UF == "DF", "MT", "MS", "GO","CENTRO-OESTE",
                        ifelse(Obitos_Estado$UF == "ES","MG","SP", "RJ", "SUDESTE",
                        ifelse(Obitos_Estado$UF == "PR", "SC", "RS", "SUL"))))

#GRAFICO#
ggplot(Obitos_Estado, aes(x = UF, y = TAXA_MORTALIDADE*100, 
      fill= cut(TAXA_MORTALIDADE*100, breaks = c(0, 0.5, 0.6, Inf)))) +
      geom_col()+
      theme(legend.position = "none")+
      xlab("Estados")+
      ylab("Taxa Mortalidade")+
      scale_fill_manual(values = c('lightblue2', 'lightblue3', 'lightblue4'),
      limits = c('(0,0.5]', '(0.5,0.6]', '(0.6,Inf]'))




####################### OBITOS EM CAPITAIS 0-19 anos ######################

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


#GRAFICO#
ggplot(obitos_capitais, aes(x = ID_MN_RESI, y = OBITOS_CAPITAIS, 
    color= cut(OBITOS_CAPITAIS, breaks = c(0, 25, 50, 75, Inf)))) +
    geom_col()+
    coord_flip()+ 
    theme(legend.position = "none")+
    xlab("Capitais")+
    ylab("Obitos")+
    scale_color_manual(values = c('green', 'yellow', 'red', 'black'),
                     limits = c('(0,25]', '(25,50]', '(50,75]','(75,Inf]'))


#Dados em csv#
write.csv(obitos_capitais, "obitos_capitais.csv", row.names = F)


###### GRAFICO TAXA DE MORTALIDADE CAPITAIS 0-19 anos ######### 

ggplot(obitos_td_capitais, aes(x = CAPITAL, y = TAXA_MORTALIDADE*100, 
    fill= cut(TAXA_MORTALIDADE*100, breaks = c(0, 0.5, 0.6, Inf)))) +
    geom_col()+
    coord_flip()+ 
    theme(legend.position = "none")+
    xlab("Capitais")+
    ylab("Taxa Mortalidade")+
    scale_fill_manual(values = c('lightblue2', 'lightblue3', 'lightblue4'),
    limits = c('(0,0.5]', '(0.5,0.6]', '(0.6,Inf]'))







########################### OBITOS DE 0-5anos #################################

##### Base com dados por idade ##### 
obitos_sivep_crianca <- filter(obitos_sivep_brasil, IDADE < 6)

#### TOTAL DE OBITOS POR SRAG 0-5 anos ####
sum(obitos_sivep_crianca$CASO, na.rm = T) 

###### ZONA DE RESIDENCIA  ######
obitos_munic_zona_cca <- obitos_sivep_crianca %>%
  group_by(CS_ZONA)%>%
  summarise(OBITOS_ZONA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ZONA)

##### ANALISE POR SEXO 0-5a #### 
obitos_sexo_cca <- obitos_sivep_crianca %>%
  group_by(CS_SEXO)%>%
  summarise(OBITOS_SEXO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_SEXO)

##### ANALISE RAÇA 0-5a ######
obitos_raca_cca <- obitos_sivep_crianca %>%
  group_by(CS_RACA)%>%
  summarise(OBITOS_RACA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_RACA)

# DADOS EM CSV
write.csv(obitos_raca_cca, "obitos_raca_cca.csv", row.names = F)

obitos_raca_cca <- obitos_raca_cca[!is.na(obitos_raca_cca$CS_RACA),]
com_dados_sobre <- sum(!is.na(obitos_sivep_crianca$CS_RACA))
com_dados_sobre
sem_dados_sobre <- sum(is.na(obitos_sivep_crianca$CS_RACA))
sem_dados_sobre
sem_dados_sobre/(com_dados_sobre+sem_dados_sobre)*100


##### ANALISE FATOR DE RISCO ###### 
obitos_fator_risco_cca <- obitos_sivep_crianca %>%
  group_by(FATOR_RISC)%>%
  summarise(OBITOS_FATOR_RISC = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_FATOR_RISC)


######## COMORBIDADES #############

obitos_comorb_cca <- obitos_sivep_crianca %>%
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


obitos_comorb_cca <- melt(obitos_comorb_cca, id.vars = "CASO")

obitos_comorb_cca <- obitos_comorb_cca %>%
  group_by(variable, value) %>%
  summarise("QUANTIDADE" = sum(CASO,na.rm = T))

obitos_comorb_cca$value <- as.factor(obitos_comorb_cca$value)

obitos_comorb_cca <- dcast(formula = variable ~ value, value.var =  "QUANTIDADE", obitos_comorb_cca)

names(obitos_comorb_cca) <- c("AGRAVO", "SIM", "NÃO", "IGNORADO", "SEM_DADOS")

# DADOS EM CSV
write.csv(obitos_comorb_cca, "obitos_comorb_cca.csv", row.names = F)


########################## OBITOS POR ESTADO 0-5 anos #################################
obitos_estado_cca <- obitos_sivep_crianca %>%
  group_by(SG_UF)%>%
  summarise(OBITOS_ESTADO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ESTADO)

#GRAFICO#
ggplot(obitos_estado_cca, aes(x = SG_UF, y = OBITOS_ESTADO, 
  color= cut(OBITOS_ESTADO, breaks = c(0, 50, 100, 150, Inf)))) +
  geom_col()+
  theme(legend.position = "none")+
  xlab("Estados")+
  ylab("Obitos")+
  scale_color_manual(values = c('green', 'yellow', 'red', 'black'),
  limits = c('(0,50]', '(50,100]', '(100,150]','(150,Inf]'))

#DADOE EM CSV#
write.csv(obitos_estado_cca, "obitos_estado_cca.csv", row.names = F)


#Dados csv
write.csv(obitos_estado_cca, "obitos_estado_cca.csv", row.names = F)


####### GRAFICO TAXA DE MORTALIDADE POR ESTADOS 0-5 anos ###### 

#Obs.: Populacao_1 = pop. de -0-3anos ; Populacao_2 = pop. de 4-5anos
obitos_cca_estado$POPULACAO_2020 <- obitos_cca_estado$POPULACAO_1 + obitos_cca_estado$POPULACAO_2
obitos_cca_estado$TAXA_MORTALIDADE <- obitos_cca_estado$OBITOS_ESTADO*100 / obitos_cca_estado$POPULACAO_2020

#GRAFICO#
ggplot(obitos_cca_estado, aes(x = UF, y = TAXA_MORTALIDADE*100, 
  fill= cut(TAXA_MORTALIDADE*100, breaks = c(0, 0.8, 1.3, Inf)))) +
  geom_col()+
  theme(legend.position = "none")+
  xlab("Estados")+
  ylab("Taxa Mortalidade")+
  scale_fill_manual(values = c('lightblue2', 'lightblue3', 'lightblue4'),
  limits = c('(0,0.8]', '(0.8,1.3]', '(1.3,Inf]'))


#DADOE EM CSV#
write.csv(obitos_cca_estado, "obitos_cca_estado.csv", row.names = F)


##############  OBITOS EM CAPITAIS 0-5 anos ##############

obitos_capitais_cca <- subset (obitos_sivep_crianca, obitos_sivep_crianca$ID_MN_RESI
                                  %in% c("RIO BRANCO","MACAPA", "MANAUS", "BELEM",
                                         "PORTO VELHO","BOA VISTA", "PALMAS",
                                         "SALVADOR", "FORTALEZA", "MACEIO", "SAO LUIS",
                                         "JOAO PESSOA", "RECIFE", "TERESINA", "NATAL", "ARACAJU",
                                         "GOIANIA", "CUIABA", "CAMPO GRANDE", "BRASILIA",
                                         "VITORIA", "BELO HORIZONTE", "RIO DE JANEIRO", "SAO PAULO",
                                         "CURITIBA", "FLORIANOPOLIS","PORTO ALEGRE"))

obitos_cap_cca <- obitos_capitais_cca %>%
  group_by(ID_MN_RESI)%>%
  summarise(OBITOS_CAPITAIS = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_CAPITAIS)


#GRAFICO#
ggplot(obitos_cap_cca, aes(x = ID_MN_RESI, y = OBITOS_CAPITAIS, 
  color= cut(OBITOS_CAPITAIS, breaks = c(0, 25, 50, 75, Inf)))) +
  geom_col()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab("Capitais")+
  ylab("Obitos")+
  scale_color_manual(values = c('green', 'yellow', 'red', 'black'),
                     limits = c('(0,25]', '(25,50]', '(50,75]','(75,Inf]'))


#Dados em csv#
write.csv(obitos_cap_cca, "obitos_cap_cca.csv", row.names = F)



####### GRAFICO TAXA DE MORTALIDADE CAPITAIS 0-5 anos ###### 

#Obs.: Populacao_1 = pop. de -0-3anos ; Populacao_2 = pop. de 4-5anos
obitos_cca_capitais$POPULACAO_2020 <- obitos_cca_capitais$POPULACAO_1 + obitos_cca_capitais$POPULACAO_2
obitos_cca_capitais$TAXA_MORTALIDADE <- obitos_cca_capitais$OBITOS*100 / obitos_cca_capitais$POPULACAO_2020


#Grafico#
ggplot(obitos_cca_capitais, aes(x = CAPITAL, y = TAXA_MORTALIDADE*100, 
  fill= cut(TAXA_MORTALIDADE*100, breaks = c(0, 1.5, 2, Inf)))) +
  geom_col()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab("Capitais")+
  ylab("Taxa Mortalidade")+
  scale_fill_manual(values = c('lightblue2', 'lightblue3', 'lightblue4'),
  limits = c('(0,1.5]', '(1.5,2]', '(2,Inf]'))


#Dados em csv#
write.csv(obitos_cca_capitais, "obitos_cca_capitais.csv", row.names = F)




########### ANALISE COM DESFECHO (4-SRAG não especificado 5-SRAG por COVID-19)

### 0-19 anos ###
obitos_srag <- subset(obitos_sivep_brasil, obitos_sivep_brasil$CLASSI_FIN
                      %in% c(4,5))
obitos_srag$CASO <-1
sum (obitos_srag$CASO) #### TOTAL DE OBITOS ###

############ Analise dos obitos por municipio de residencia ################
obitos_municipio_srag <- obitos_srag %>%
  group_by(ID_MN_RESI, SG_UF, CS_ZONA) %>%
  summarise(OBITOS_MUNICIPIO = sum(CASO, na.rm = T)) %>%
  arrange(OBITOS_MUNICIPIO)

################ ZONA DE RESIDENCIA ##############
obitos_municipio_srag_zona <- obitos_srag %>%
  group_by(CS_ZONA)%>%
  summarise(OBITOS_ZONA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ZONA)


############ ANALISE POR SEXO  ############# 
obitos_sexo_srag <- obitos_srag %>%
  group_by(CS_SEXO)%>%
  summarise(OBITOS_SEXO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_SEXO)



########## ANALISE RAÇA ##################
obitos_raca_srag <- obitos_srag %>%
  group_by(CS_RACA)%>%
  summarise(OBITOS_RACA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_RACA)

# DADOS EM CSV
write.csv(obitos_raca_srag, "obitos_raca_srag.csv", row.names = F)

obitos_raca_srag <- obitos_raca_srag[!is.na(obitos_raca_srag$CS_RACA),]
com_dados_sobre <- sum(!is.na(obitos_srag$CS_RACA))
com_dados_sobre
sem_dados_sobre <- sum(is.na(obitos_srag$CS_RACA))
sem_dados_sobre
sem_dados_sobre/(com_dados_sobre+sem_dados_sobre)*100

##GRAFICO RAÇA SRAG ###
#POPULACAO_2020 = TOTAL DA POPULACAO DE 0-19 anos
base_raca_srag$TAXA_MORTALIDADE <- base_raca_srag$OBITOS*100 / base_raca_srag$POPULACAO_2020

ggplot(base_raca_srag, aes(x = RACA, y = TAXA_MORTALIDADE*100, fill=RACA ))+
  geom_col()+
  theme_bw()+
  xlab("População 0-19 anos")+
  ylab("Taxa Mortalidade") +
  labs(fill="Raça") 


########### ANALISE FATOR DE RISCO ########### 
obitos_fator_risco_srag <- obitos_srag %>%
  group_by(FATOR_RISC)%>%
  summarise(OBITOS_FATOR_RISC = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_FATOR_RISC)


########### COMORBIDADES #############

obitos_comorb_srag <- obitos_srag %>%
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


obitos_comorb_srag <- melt(obitos_comorb_srag, id.vars = "CASO")

obitos_comorb_srag <- obitos_comorb_srag %>%
  group_by(variable, value) %>%
  summarise("QUANTIDADE" = sum(CASO,na.rm = T))

obitos_comorb_srag$value <- as.factor(obitos_comorb_srag$value)

obitos_comorb_srag <- dcast(formula = variable ~ value, value.var =  "QUANTIDADE", obitos_comorb_srag)

names(obitos_comorb_srag) <- c("AGRAVO", "SIM", "NÃO", "IGNORADO", "SEM_DADOS")

# DADOS EM CSV
write.csv(obitos_comorb_srag, "obitos_comorb_srag.csv", row.names = F)



########################## OBITOS POR ESTADO SRAG 0-19anos #################################

obitos_estado_srag <- obitos_srag %>%
  group_by(SG_UF)%>%
  summarise(OBITOS_ESTADO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ESTADO)

#GRAFICO ESTADO SRAG#
ggplot(obitos_estado_srag, aes(x = SG_UF, y = OBITOS_ESTADO, 
  color= cut(OBITOS_ESTADO, breaks = c(0, 100, 200, 300, Inf)))) +
  geom_col()+
  theme(legend.position = "none")+
  xlab("Estados")+
  ylab("Obitos")+
  scale_color_manual(values = c('green', 'yellow', 'red', 'black'),
                     limits = c('(0,100]', '(100,200]', '(200,300]','(300,Inf]'))

#DADOE EM CSV#
write.csv(obitos_estado_srag, "obitos_estado_srag.csv", row.names = F)


######### GRAFICO TAXA DE MORTALIDADE EM ESTADOS SRAG 0-19anos ########### 

#GRAFICO SRAG#
base_estados_srag_0_19$TAXA_MORTALIDADE <- base_estados_srag_0_19$OBITOS_ESTADO *100 / base_estados_srag_0_19$`POPULACAO_ 2020`

ggplot(base_estados_srag_0_19, aes(x = UF, y = TAXA_MORTALIDADE*100, 
  fill= cut(TAXA_MORTALIDADE*100, breaks = c(0, 0.5, 0.6, Inf)))) +
  geom_col()+
  theme(legend.position = "none")+
  xlab("Estados")+
  ylab("Taxa Mortalidade")+
  scale_fill_manual(values = c('lightblue2', 'lightblue3', 'lightblue4'),
  limits = c('(0,0.5]', '(0.5,0.6]', '(0.6,Inf]'))




####################### OBITOS EM CAPITAIS SRAG 0-19 anos ######################

obitos_capitais_srag <- subset (obitos_srag, obitos_srag$ID_MN_RESI
                                  %in% c("RIO BRANCO","MACAPA", "MANAUS", "BELEM",
                                         "PORTO VELHO","BOA VISTA", "PALMAS",
                                         "SALVADOR", "FORTALEZA", "MACEIO", "SAO LUIS",
                                         "JOAO PESSOA", "RECIFE", "TERESINA", "NATAL", "ARACAJU",
                                         "GOIANIA", "CUIABA", "CAMPO GRANDE", "BRASILIA",
                                         "VITORIA", "BELO HORIZONTE", "RIO DE JANEIRO", "SAO PAULO",
                                         "CURITIBA", "FLORIANOPOLIS","PORTO ALEGRE"))

obitos_cap_srag <- obitos_capitais_srag %>%
  group_by(ID_MN_RESI)%>%
  summarise(OBITOS_CAPITAIS = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_CAPITAIS)


#GRAFICO#
ggplot(obitos_cap_srag, aes(x = ID_MN_RESI, y = OBITOS_CAPITAIS, 
  color= cut(OBITOS_CAPITAIS, breaks = c(0, 25, 50, 75, Inf)))) +
  geom_col()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab("Capitais")+
  ylab("Obitos")+
  scale_color_manual(values = c('green', 'yellow', 'red', 'black'),
                     limits = c('(0,25]', '(25,50]', '(50,75]','(75,Inf]'))


#Dados em csv#
write.csv(obitos_cap_srag, "obitos_cap_srag.csv", row.names = F)


###### GRAFICO TAXA DE MORTALIDADE CAPITAIS SRAG 0-19 anos ######### 
base_capitais_srag_0_19$TAXA_MORTALIDADE <- base_capitais_srag_0_19$OBITOS_CAPITAIS *100 / base_capitais_srag_0_19$POPULACAO_2020

ggplot(base_capitais_srag_0_19, aes(x = CAPITAL, y = TAXA_MORTALIDADE*100, 
  fill= cut(TAXA_MORTALIDADE*100, breaks = c(0, 0.5, 0.6, Inf)))) +
  geom_col()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab("Capitais")+
  ylab("Taxa Mortalidade")+
  scale_fill_manual(values = c('lightblue2', 'lightblue3', 'lightblue4'),
                    limits = c('(0,0.5]', '(0.5,0.6]', '(0.6,Inf]'))



################################# 0-5 anos SRAG #################################

obitos_srag_cca <- subset(obitos_sivep_crianca, obitos_sivep_crianca$CLASSI_FIN
                          %in% c(4,5))

obitos_srag_cca$CASO <-1
sum (obitos_srag_cca$CASO)


############ Analise dos obitos por municipio de residencia ################
obitos_municipio_srag_cca <- obitos_srag_cca %>%
  group_by(ID_MN_RESI, SG_UF, CS_ZONA) %>%
  summarise(OBITOS_MUNICIPIO = sum(CASO, na.rm = T)) %>%
  arrange(OBITOS_MUNICIPIO)

################ ZONA DE RESIDENCIA ##############
obitos_municipio_srag_zona_cca <- obitos_srag_cca %>%
  group_by(CS_ZONA)%>%
  summarise(OBITOS_ZONA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ZONA)


############ ANALISE POR SEXO  ############# 
obitos_sexo_srag_cca <- obitos_srag_cca %>%
  group_by(CS_SEXO)%>%
  summarise(OBITOS_SEXO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_SEXO)



########## ANALISE RAÇA ##################
obitos_raca_srag_cca <- obitos_srag_cca %>%
  group_by(CS_RACA)%>%
  summarise(OBITOS_RACA = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_RACA)

# DADOS EM CSV
write.csv(obitos_raca_srag_cca, "obitos_raca_srag_cca.csv", row.names = F)

obitos_raca_srag_cca <- obitos_raca_srag_cca[!is.na(obitos_raca_srag_cca$CS_RACA),]
com_dados_sobre <- sum(!is.na(obitos_srag_cca$CS_RACA))
com_dados_sobre
sem_dados_sobre <- sum(is.na(obitos_srag_cca$CS_RACA))
sem_dados_sobre
sem_dados_sobre/(com_dados_sobre+sem_dados_sobre)*100

##GRAFICO RAÇA SRAG ###


########### ANALISE FATOR DE RISCO ########### 
obitos_fator_risco_srag_cca <- obitos_srag_cca %>%
  group_by(FATOR_RISC)%>%
  summarise(OBITOS_FATOR_RISC = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_FATOR_RISC)


########### COMORBIDADES #############

obitos_comorb_srag_cca <- obitos_srag_cca %>%
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


obitos_comorb_srag_cca <- melt(obitos_comorb_srag_cca, id.vars = "CASO")

obitos_comorb_srag_cca <- obitos_comorb_srag_cca %>%
  group_by(variable, value) %>%
  summarise("QUANTIDADE" = sum(CASO,na.rm = T))

obitos_comorb_srag_cca$value <- as.factor(obitos_comorb_srag_cca$value)

obitos_comorb_srag_cca <- dcast(formula = variable ~ value, value.var =  "QUANTIDADE", obitos_comorb_srag_cca)

names(obitos_comorb_srag_cca) <- c("AGRAVO", "SIM", "NÃO", "IGNORADO", "SEM_DADOS")

# DADOS EM CSV
write.csv(obitos_comorb_srag_cca, "obitos_comorb_srag_cca.csv", row.names = F)



########################## OBITOS POR ESTADO SRAG 0-5 anos #################################
obitos_estado_srag_cca <- obitos_srag_cca %>%
  group_by(SG_UF)%>%
  summarise(OBITOS_ESTADO = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_ESTADO)

#GRAFICO#
ggplot(obitos_estado_srag_cca, aes(x = SG_UF, y = OBITOS_ESTADO, 
  color= cut(OBITOS_ESTADO, breaks = c(0, 50, 100, 150, Inf)))) +
  geom_col()+
  theme(legend.position = "none")+
  xlab("Estados")+
  ylab("Obitos")+
  scale_color_manual(values = c('green', 'yellow', 'red', 'black'),
                     limits = c('(0,50]', '(50,100]', '(100,150]','(150,Inf]'))

#DADOE EM CSV#
write.csv(obitos_estado_srag_cca, "obitos_estado_srag_cca.csv", row.names = F)


####### GRAFICO TAXA DE MORTALIDADE POR ESTADOS 0-5 anos ###### 
#Obs.: Populacao_1 = pop. de -0-3anos ; Populacao_2 = pop. de 4-5anos
base_estado_srag_cca$POPULACAO_2020 <- base_estado_srag_cca$POPULACAO_1 + base_estado_srag_cca$POPULACAO_2
base_estado_srag_cca$TAXA_MORTALIDADE <- base_estado_srag_cca$OBITOS_ESTADO*100 / base_estado_srag_cca$POPULACAO_2020

#GRAFICO#
ggplot(base_estado_srag_cca, aes(x = UF, y = TAXA_MORTALIDADE*100, 
  fill= cut(TAXA_MORTALIDADE*100, breaks = c(0, 0.8, 1.3, Inf)))) +
  geom_col()+
  theme(legend.position = "none")+
  xlab("Estados")+
  ylab("Taxa Mortalidade")+
  scale_fill_manual(values = c('lightblue2', 'lightblue3', 'lightblue4'),
                    limits = c('(0,0.8]', '(0.8,1.3]', '(1.3,Inf]'))

#DADOE EM CSV#
write.csv(base_estado_srag_cca, "base_estado_srag_cca.csv", row.names = F)


##############  OBITOS EM CAPITAIS SRAG 0-5 anos ##############

obitos_capitais_srag_cca <- subset (obitos_srag_cca, obitos_srag_cca$ID_MN_RESI
                               %in% c("RIO BRANCO","MACAPA", "MANAUS", "BELEM",
                                      "PORTO VELHO","BOA VISTA", "PALMAS",
                                      "SALVADOR", "FORTALEZA", "MACEIO", "SAO LUIS",
                                      "JOAO PESSOA", "RECIFE", "TERESINA", "NATAL", "ARACAJU",
                                      "GOIANIA", "CUIABA", "CAMPO GRANDE", "BRASILIA",
                                      "VITORIA", "BELO HORIZONTE", "RIO DE JANEIRO", "SAO PAULO",
                                      "CURITIBA", "FLORIANOPOLIS","PORTO ALEGRE"))

obitos_cap_srag_cca <- obitos_capitais_srag_cca %>%
  group_by(ID_MN_RESI)%>%
  summarise(OBITOS_CAPITAIS = sum(CASO, na.rm = T))%>%
  arrange(OBITOS_CAPITAIS)


#GRAFICO#
ggplot(obitos_cap_srag_cca, aes(x = ID_MN_RESI, y = OBITOS_CAPITAIS, 
  color= cut(OBITOS_CAPITAIS, breaks = c(0, 25, 50, 75, Inf)))) +
  geom_col()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab("Capitais")+
  ylab("Obitos")+
  scale_color_manual(values = c('green', 'yellow', 'red', 'black'),
                     limits = c('(0,25]', '(25,50]', '(50,75]','(75,Inf]'))


#Dados em csv#
write.csv(obitos_cap_srag_cca, "obitos_cap_srag_cca.csv", row.names = F)



####### GRAFICO TAXA DE MORTALIDADE CAPITAIS SRAG 0-5 anos ###### 
#Obs.: Populacao_1 = pop. de -0-3anos ; Populacao_2 = pop. de 4-5anos
base_capitais_srag_cca$POPULACAO_2020 <- base_capitais_srag_cca$POPULACAO_1 + base_capitais_srag_cca$POPULACAO_2
base_capitais_srag_cca$TAXA_MORTALIDADE <- base_capitais_srag_cca$OBITOS*100 / base_capitais_srag_cca$POPULACAO_2020


#Grafico#
ggplot(base_capitais_srag_cca, aes(x = CAPITAL, y = TAXA_MORTALIDADE*100, 
  fill= cut(TAXA_MORTALIDADE*100, breaks = c(0, 1.5, 2, Inf)))) +
  geom_col()+
  coord_flip()+ 
  theme(legend.position = "none")+
  xlab("Capitais")+
  ylab("Taxa Mortalidade")+
  scale_fill_manual(values = c('lightblue2', 'lightblue3', 'lightblue4'),
                    limits = c('(0,1.5]', '(1.5,2]', '(2,Inf]'))


#Dados em csv#
write.csv(base_capitais_srag_cca, "base_capitais_srag_cca.csv", row.names = F)





















########### ANALISE COM DESFECHO (5-SRAG por COVID-19)

### 0-19 anos ###
obitos_srag_covid <- subset(obitos_sivep_brasil, obitos_sivep_brasil$CLASSI_FIN
                      == 5)
obitos_srag_covid$CASO <-1
sum (obitos_srag_covid$CASO)

### 0-5 anos ###
obitos_srag_covid_cca <- subset(obitos_sivep_crianca, obitos_sivep_crianca$CLASSI_FIN
                          == 5)

obitos_srag_covid_cca$CASO <-1
sum (obitos_srag_covid_cca$CASO)





