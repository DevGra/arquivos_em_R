# codigo para carregar ativos engenharia por ano e consolidar em um arquivo, tudo e com selecao de variaveis

library(plyr) #para renomer levels de fatores - mapvalues
library(openxlsx)

getwd()

#escolhe o diretorio de dados da RAIS
setwd("/Users/renatopedrosa/OneDrive/data/mte-rais/Engenharias/Data/UFs/2017/corrig2")

list.files()

temp = list.files(pattern="*.txt")
df=vector("list", length(temp))
#str(df[[1]])
#df[[1]][,9]

#carregando todos os arquivos em uma lista (lapply)

df<- lapply(temp, read.csv, encoding="latin1")

#str(df)
str(df[[26]])
names(df[[1]])

N<-length(df)

# remove index columns X XX se existirem, rodar quantas vezes for necessário

for (i in 1:N) {
  df[[i]]<-df[[i]][,-1]
}

names(df[[1]])
# troca level 000-1 por 0 na CNAE20 se for o caso (checar se é fator ou inteiro antes)
#str(df[[1]])

a<-c(which(colnames(df[[1]]) == "CNAE.2.0.Classe"))
for (i in 1:N) {
  df[[i]][,a[1]]<-mapvalues(df[[i]][,a[1]],c("000-1"),c("0"))
  df[[i]][,a[1]] <- as.integer(as.character(df[[i]][,a[1]]))
}

str(df[[26]])

notfactors <- c(which(colnames(df[[1]]) == "CNAE.2.0.Classe"))
notfactors <- c(notfactors, which(colnames(df[[1]]) =="CNAE.95.Classe"))
notfactors <- c(notfactors, which(colnames(df[[1]]) == "Qtd.Hora.Contr"))
notfactors <- c(notfactors, which(colnames(df[[1]]) == "Idade"))
notfactors <- c(notfactors, which(colnames(df[[1]]) == "Qtd.Dias.Afastamento"))
notfactors <- c(notfactors, which(colnames(df[[1]]) == "Vl.Remun.Dezembro.Nom"))
notfactors <- c(notfactors, which(colnames(df[[1]]) == "Vl.Remun.Dezembro..SM."))
notfactors <- c(notfactors, which(colnames(df[[1]]) == "Vl.Remun.Média.Nom"))
notfactors <- c(notfactors, which(colnames(df[[1]]) == "Vl.Remun.Média..SM."))


# Transformar em fatores usando SP como ref, deixar CNAE20  e variaveis numericas de fora

M<-length(names(df[[1]]))

for (i in 1:N) {
  for (j in 1:M) {
    if (j %in% notfactors) {}
    else {df[[i]][,j] <- factor(df[[i]][,j])}
}}

str(df[[1]])
#Criando um único dataframe para todas UFs
df_todas <- df[[1]]
str(df_todas)

for (i in 2:N) df_todas <- rbind(df_todas,df[[i]]) 

str(df_todas)
names(df_todas)

cnae_conversion<- unique(subset(df_todas,select=c("CNAE.2.0.Classe","CNAE.95.Classe")))
setwd("/Users/renatopedrosa/OneDrive/data/mte-rais/Docs/CNAE")
x<- table(cnae_conversion$CNAE.2.0.Classe,cnae_conversion$CNAE.95.Classe)
write.xlsx(cnae_conversion,"conversao_cnae20_cnae95.xlsx")

# Salvando arquivo consolidado do ano
getwd()
setwd("/Users/renatopedrosa/OneDrive/data/mte-rais/Engenharias/Data/Consolidado")

write.csv(df_todas,"Brasil2017.csv", fileEncoding = "latin1")


#Selecionando as variáveis
df_ativ <- subset(df_todas, select = c("Ano", "UF", "Município","CNAE.2.0.Classe",
                              "Natureza.Jurídica", "Tipo.Estab", "CBO.Ocupação.2002",  "CBOnome", "CBOgrupo",
                              "Vínculo.Ativo.31.12", "Tipo.Vínculo" , "Qtd.Hora.Contr", "Faixa.Hora.Contrat",
                              "Nacionalidade","Idade", "Faixa.Etária", "Escolaridade.após.2005",
                              "Raça.Cor","Sexo.Trabalhador",
                             "Faixa.Remun.Dezem..SM.","Vl.Remun.Dezembro..SM.","Vl.Remun.Dezembro.Nom"))

names(df_ativ)
str(df_ativ)

# renomeia as variaveis

newnames <- c("Ano", "UF", "Municipio", "CNAE20", "NatJur",
              "TipoEstab", "CBO", "CBOnome", "CBOgrupo", "Vinculo", 
              "TpVinc", "HrsContr",  "FxHrsContr", "Nacional", "Idade",
              "FxEtaria", "Escolaridade",  "RacaCor","Sexo", "FxRemDezSM", 
              "VlRemDezSM", "VlRemDezNom")


names(df_ativ) <- newnames

str(df_ativ)

#checando NAs
nas <- df_ativ[!complete.cases(df_ativ),]
str(nas)


# Corrigindo CBO: level 0000-1 é -1, trocando para 999999
# df_ativ$CBO <- mapvalues(df_ativ$CBO,c("0000-1"),c("999999"))

# ---- Mudando nomes dos níveis --------

levels_ativo <- c("Ativo")
levels(df_ativ$Vinculo) <- levels_ativo

levels(df_ativ$Escolaridade)
df_ativ$Escolaridade<- mapvalues(df_ativ$Escolaridade, 
                                 c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
                                 c("Analfabeto","<5anos","5anos", "6-9anos", "Fund compl", 
                                   "Med incompl", "Med compl", "Sup incompl", "Sup compl", "Mest", "Dout"))
# Ordenando levels se precisar
#df_ativ$Escolaridade<- factor(df_ativ$Escolaridade, 
#                                 levels = 
#                                 c("Analfabeto","<5anos","5anos", "6-9anos", "Fund compl", 
#                                   "Med incompl", "Med compl", "Sup incompl", "Sup compl", "Mest", "Dout"))

levels(df_ativ$Sexo)
df_ativ$Sexo <- mapvalues(df_ativ$Sexo, c("1","2"), c("Masc","Fem"))

levels_tipo_estab<- c("CNPJ","CEI")
levels(df_ativ$TipoEstab) <- levels_tipo_estab

levels(df_ativ$RacaCor)
df_ativ$RacaCor<- mapvalues(df_ativ$RacaCor,c("1","2","4","6","8","9","99"),
                             c("Indigena","Branca","Preta", "Amarela","Parda","NIdent","Ignorado"))
# Ordenando
#df_ativ$RacaCor<-factor(df_ativ$RacaCor, levels = c("Indigena","Branca","Preta","Amarela", "Parda","NIdent","Ignorado"))

levels(df_ativ$FxEtaria)
df_ativ$FxEtaria <- mapvalues(df_ativ$FxEtaria,c("1","2", "3","4","5","6","7","8","99"), 
                              c("10-14","15-17","18-24", "25-29", "30-39", "40-49", "50-64", "65+","NaoClass"))
#df_ativ$FxEtaria <- factor(df_ativ$FxEtaria,
#                              levels=c("18-24", "25-29", "30-39", "40-49", "50-64", "65+"))

levels(df_ativ$FxRemDezSM)
df_ativ$FxRemDezSM <- mapvalues(df_ativ$FxRemDezSM, 
                                c("1", "2", "3", "4",
                                  "5", "6", "7", "8", "9",
                                  "10", "11", "12", "99"),
                                c("até 0.5SM", "0.51-1SM", "1.01-1.5SM", "1.51-2SM", "2.01-3SM", 
                                  "3.01-4SM", "4.01-5SM", "5.01-7SM", "7.01-10SM", "10.01-15SM",
                                  "15.01-20SM", "20.01+", "NClass"))
#df_ativ$FxRemDezSM <- factor(df_ativ$FxRemDezSM, 
#                                levels =
#                                c("até 0.5SM", "0.51-1SM", "1.01-1.5SM", "1.51-2SM", "2.01-3SM", 
#                                  "3.01-4SM", "4.01-5SM", "5.01-7SM", "7.01-10SM", "10.01-15SM",
#                                  "15.01-20SM", "20.01+", "NClass"))

levels(df_ativ$FxHrsContr)
df_ativ$FxHrsContr <- mapvalues(df_ativ$FxHrsContr,  c("1", "2", "3", "4","5", "6"),
                                c("até 12", "13-15","16-20", "21-30", "31-40", "41-44"))
df_ativ$FxHrsContr <- factor(df_ativ$FxHrsContr,  
                                levels = c("até 12", "13-15","16-20", "21-30", "31-40", "41-44"))

# Nomeando Tipos de vinculo e incluindo grupos de TV
setwd("/Users/renatopedrosa/OneDrive/data/mte-rais/Docs/TipoVinculo")
list.files()
tv<- read.xlsx("TipoVinculo.xlsx")

levels(df_ativ$TpVinc)
df_ativ$GrupoTpVinc <- df_ativ$TpVinc

codTV <- as.character(tv$Cod)
nomeTV<- as.character(tv$Nome)
grupoTV<- as.character(tv$Grupo)

df_ativ$TpVinc <- mapvalues(df_ativ$TpVinc , codTV, nomeTV)
df_ativ$GrupoTpVinc <- mapvalues(df_ativ$GrupoTpVinc , codTV, grupoTV)

levels(df_ativ$TpVinc)
str(df_ativ)

#Nomer CNAE por seção e por classes engenharias (nosso agrupamento das seções)

df_ativ$CNAE20div<-df_ativ$CNAE20

df_ativ$CNAE20div<-floor(df_ativ$CNAE20div/1000)

df_ativ$CNAE20<-factor(df_ativ$CNAE20)
df_ativ$CNAE20div<-factor(df_ativ$CNAE20div)

df_ativ$CNAE20secao<-df_ativ$CNAE20div
df_ativ$CNAE20classe<-df_ativ$CNAE20div
df_ativ$CNAE20ggrupo<-df_ativ$CNAE20div
# Denominação CNAE

setwd("/Users/renatopedrosa/OneDrive/data/mte-rais/Docs/CNAE")
list.files()
cnae<-read.xlsx("cnae-conversao.xlsx")
str(cnae)
cnae$Divisao<-factor(cnae$Divisao)

divCod<-as.character(cnae$Divisao)
divNome<-as.character(cnae$Denom_div)
secNome<-as.character(cnae$nomeCNAE)
classe<-as.character(cnae$ClasseEngenharia)
ggrupo <- as.character(cnae$grandegrupo)

df_ativ$CNAE20div <- mapvalues(df_ativ$CNAE20div, divCod, divNome)
df_ativ$CNAE20secao<- mapvalues(df_ativ$CNAE20secao, divCod, secNome)
df_ativ$CNAE20classe<- mapvalues(df_ativ$CNAE20classe, divCod, classe)
df_ativ$CNAE20ggrupo <- mapvalues(df_ativ$CNAE20ggrupo, divCod, ggrupo)

str(df_ativ)
levels(df_ativ$CNAE20div)
levels(df_ativ$CNAE20secao)
levels(df_ativ$CNAE20classe)
levels(df_ativ$CNAE20ggrupo)

#Nomear levels de NatJur

getwd()
setwd("/Users/renatopedrosa/OneDrive/data/mte-rais/Docs/NatJur")
list.files()

#nova variavel para os nomes
df_ativ$NatJurNome<-df_ativ$NatJur

NatJurTab<-read.csv("NJDet.csv",sep=";")
str(NatJurTab)

NatJurTab$Codigo<-factor(NatJurTab$Codigo)
NatJurTab$Grupo<-factor(NatJurTab$Grupo)
cod<-as.character(NatJurTab$Codigo)
nome<-as.character(NatJurTab$Nome)

#Usando PLYR para renomear NatJurNome
df_ativ$NatJurNome<-
  mapvalues(df_ativ$NatJurNome,cod,nome)
levels(df_ativ$NatJurNome)
str(df_ativ)

#Adicionar variavel para grupos de nat jur
#Importar tabela
GrupoNJ <- read.csv("GrupoNJ.csv", sep=";")
str(GrupoNJ)
GrupoNJ$CodGrupo<- factor(GrupoNJ$CodGrupo)

#Nova coluna para nomes dos grupos
NatJurTab$NomeGrupo <- NatJurTab$Grupo
str(NatJurTab)

cod_gnj <- as.character(GrupoNJ$CodGrupo)
names_gnj <- as.character(GrupoNJ$NomeGrupo)

NatJurTab$NomeGrupo <- mapvalues(NatJurTab$NomeGrupo,cod_gnj,names_gnj)

cod_nj<-as.character(NatJurTab$Codigo)
grupo_nj<-as.character(NatJurTab$NomeGrupo)
df_ativ$GrupoNJ<-df_ativ$NatJur

str(df_ativ)
df_ativ$GrupoNJ<-mapvalues(df_ativ$GrupoNJ,cod_nj,grupo_nj)

levels(df_ativ$GrupoNJ)

getwd()
setwd("/Users/renatopedrosa/OneDrive/data/mte-rais/Engenharias/Data/Consolidado")

write.csv(df_ativ,"Brasil2017-selec.csv", fileEncoding = "latin1")

#end

