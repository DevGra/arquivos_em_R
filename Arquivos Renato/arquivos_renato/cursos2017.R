getwd()

# Path to this file
# Home
# setwd("/Users/renato/Dropbox/data/mec/CEnsSup/R_files/2017")
# laptop
#setwd("/Users/renatopedrosa/Dropbox/data/mec/CEnsSup/R_files/2017")

# path microdados CES2017
# laptop
setwd("/Users/renatopedrosa/Dropbox/data/mec/CEnsSup/microdados/2017/Microdados_Educacao_Superior_2017/DADOS")
# home
#setwd("/Users/renato/Dropbox/data/mec/CEnsSup/microdados/2017/Microdados_Educacao_Superior_2017/DADOS")

getwd()


list.files()

# load cursos
df_curso<- read.csv("DM_CURSO.CSV",sep="|",encoding="latin1")

str(df_curso)
View(df_curso)

# load docentes
#df_docente <- read.csv("DM_DOCENTE.CSV",sep="|",encoding="latin1")

# load IES
#df_IES <- read.csv("DM_IES.CSV",sep="|",encoding="latin1")

#load alunos
#df_ALUNO <- read.csv("DM_ALUNO.CSV",sep="|",encoding="latin1")

#selecting the relevant columns

names(df_curso)

df_curso_select <- subset(df_curso, select = c(CO_IES,TP_CATEGORIA_ADMINISTRATIVA,
                                               TP_ORGANIZACAO_ACADEMICA,CO_UF,CO_MUNICIPIO,
                                               NO_CURSO,TP_SITUACAO,CO_OCDE_AREA_GERAL,
                                               TP_GRAU_ACADEMICO,TP_MODALIDADE_ENSINO,
                                               TP_NIVEL_ACADEMICO,
                                               TP_ATRIBUTO_INGRESSO,
                                               NU_CARGA_HORARIA,
                                               DT_INICIO_FUNCIONAMENTO,
                                               QT_MATRICULA_TOTAL,QT_CONCLUINTE_TOTAL,
                                               QT_INGRESSO_TOTAL,QT_INGRESSO_VAGA_NOVA,
                                               QT_VAGA_TOTAL))

# head(df_curso_select)

# str(df_curso)

# names(df_curso_select)

# this makes the set of new names (must be in order)
newnames <- c("ies", "cat_adm","org_acad","uf","municipio","curso","situacao",
              "cod_ocde","grau_acad","mod_ens",
              "niv_acad",
              "atrib_ingr",
              "carg_hor",
              "data_func",
              "matr_total","concl_total","ingr_total",
              "ingr_vn","vaga_total")

# this attributes the new names to columns
colnames(df_curso_select) <- newnames
names(df_curso_select)

#this is the cat_adm column of the data frame
# df_curso_select$cat_adm
str(df_curso_select)
summary(df_curso_select)

# this changes the categorical column from integrer to factor
df_curso_select$ies <- factor(df_curso_select$ies)
df_curso_select$cat_adm <- factor(df_curso_select$cat_adm)
df_curso_select$org_acad <- factor(df_curso_select$org_acad)
df_curso_select$uf <- factor(df_curso_select$uf)
df_curso_select$municipio <- factor(df_curso_select$municipio)
df_curso_select$situacao <- factor(df_curso_select$situacao)
df_curso_select$cod_ocde <- factor(df_curso_select$cod_ocde)
df_curso_select$grau_acad <- factor(df_curso_select$grau_acad)
df_curso_select$mod_ens <- factor(df_curso_select$mod_ens)
df_curso_select$niv_acad <- factor(df_curso_select$niv_acad)
df_curso_select$atrib_ingr <- factor(df_curso_select$atrib_ingr)

# View(df_curso_select)

# ---- Mudando nomes dos níveis --------
# Adicionando alguns a serem utilizados

levels_cat_adm <- c("PúblFed","PúblEst","PúblMun", "PrivCFL", "PrivSFL", "Especial")
levels(df_curso_select$cat_adm) <- levels_cat_adm

levels_cat_adm

levels_org_acad <- c("Univ","CUniv", "Fac/Inst", "Ifet", "Cefet")
levels(df_curso_select$org_acad) <- levels_org_acad
levels(df_curso_select$org_acad)

levels_situacao <- c("Ativo","Extinto", "EmExt")
levels(df_curso_select$situacao) <- levels_situacao
levels(df_curso_select$situacao)

levels(df_curso_select$grau_acad) <- c("Bach", "Lic", "Tecn", "ABI","Seq")
levels(df_curso_select$grau_acad)

levels(df_curso_select$mod_ens) <- c("Pres", "EAD")
levels(df_curso_select$mod_ens)

levels(df_curso_select$niv_acad) <- c("Grad","Seq")
levels(df_curso_select$niv_acad)

levels(df_curso_select$atrib_ingr) <- c("Reg", "ABI", "BLInterd","3")
levels(df_curso_select$atrib_ingr)

levels_uf <- c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB",
               "PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF","EAD")
levels(df_curso_select$uf) <- levels_uf

levels_ocde <- c("Educação", "HumArt", "CSocAplic", "CExNat", "EngConstProd", "CAgric", "Saúde", "Serviços","ABI")
levels(df_curso_select$cod_ocde) <- levels_ocde

View(df_curso_select)

#------------ NAs ------------------
#Importante verificar onde ocorrem NAs

  nas <- df_curso_select[!complete.cases(df_curso_select),]
    nas$ies <- droplevels(nas$ies)
    nas$cat_adm <- droplevels(nas$cat_adm)
    nas$org_acad <- droplevels(nas$org_acad)
    nas$uf <- droplevels(nas$uf)
    nas$municipio <- droplevels(nas$municipio)
    nas$curso <- droplevels(nas$curso)
    nas$situacao <- droplevels(nas$situacao)
    nas$cod_ocde <- droplevels(nas$cod_ocde)
    nas$grau_acad <- droplevels(nas$grau_acad)
    nas$mod_ens <- droplevels(nas$mod_ens)
    nas$niv_acad <- droplevels(nas$niv_acad)
    nas$atrib_ingr <- droplevels(nas$atrib_ingr)
    nas$data_func <- droplevels(nas$data_func)

str(nas)
# View(nas)

# ---------------rodar quando atualizar ----------------------

# Vamos tentar corrigir isso ao máximo; onde não for possível, colocamos "M" (missing, mas não deixar NA, que atrapalha seleção)

# Caso UF/Municipio missing = EAD

na_uf <- df_curso_select[!complete.cases(df_curso_select$uf),]
na_municipio <- df_curso_select[!complete.cases(df_curso_select$municipio),]
na_ead <- df_curso_select[df_curso_select$mod_ens == "EAD",]

# View(na_uf)
# View(na_municipio)
# View(na_ead)
# iguais?

# unique(na_uf == na_municipio)
# unique(na_uf == na_ead)

# ok, são os mesmos, vamos colocar EAD nas duas variáveis onde é EAD

# Adicionando o level "EAD" (já foi)
# levels(df_curso_select$uf) <- c(levels(df_curso_select$uf), "EAD")
# levels(df_curso_select$municipio) <- c(levels(df_curso_select$municipio), "EAD")

df_curso_select[is.na(df_curso_select$uf),"uf"] <- "EAD"
df_curso_select[is.na(df_curso_select$mun),"municipio"] <- "EAD"

# rodar nas

# checando se ABI = area básica de ingresso <-> OCDE = NA

na_ocde <- df_curso_select[!complete.cases(df_curso_select$cod_ocde),]
ingr_abi <- df_curso_select[df_curso_select$atrib_ingr =="ABI",]

# testando os valores da comparação (não eliminar levels antes de fazer isso)

# unique(na_ocde == ingr_abi)

# são iguais

# str(na_ocde)
# str(ingr_abi)

# Vamos colocar "ABI" no lugar de NA em OCDE

# Adcionar level ABI para OCDE

levels(df_curso_select$cod_ocde) <- c(levels(df_curso_select$cod_ocde), "ABI")
levels(df_curso_select$cod_ocde)

# Trocar NA para "cod_ocde"

df_curso_select[is.na(df_curso_select$cod_ocde),"cod_ocde"] <- "ABI"

# rodar nas novamente

# grau_acad = "NA" para esses casos, mas podem haver outros, vamos checar se há algum outro NA nessa categoria

na_grau <- df_curso_select[!complete.cases(df_curso_select$grau_acad),]
na_grau$grau_acad <- droplevels(na_grau$grau_acad)
str(na_grau)

# Há outros casos
View(na_grau)

# Os casos em que atrib_ingr = 1, colocamos tbem "ABI"

levels(df_curso_select$grau_acad) <- c(levels(df_curso_select$grau_acad), "ABI")
levels(df_curso_select$grau_acad)

df_curso_select[(is.na(df_curso_select$grau_acad) & df_curso_select$atrib_ingr == "ABI"),"grau_acad"] <- "ABI"

# rodar nas

# Todos os "NA" estão em grau_acad, são cusos sequenciais -> vamos colocar "SEQ" no lugar NA em grau_acad

levels(df_curso_select$grau_acad) <- c(levels(df_curso_select$grau_acad), "Seq")
levels(df_curso_select$grau_acad)

df_curso_select[(is.na(df_curso_select$grau_acad) & df_curso_select$niv_acad == "Seq"),"grau_acad"] <- "Seq"

# checar nas

# summary(df_curso_select)

# Sub os nomes dos níveis

levels_cat_adm <- c("PúblFed","PúblEst","PúblMun", "PrivCFL", "PrivSFL", "Especial")
levels(df_curso_select$cat_adm) <- levels_cat_adm

levels_cat_adm

levels_org_acad <- c("Univ","CUniv", "Fac/Inst", "Ifet", "Cefet")
levels(df_curso_select$org_acad) <- levels_org_acad
levels(df_curso_select$org_acad)

levels_situacao <- c("Ativo","Extinto", "EmExt")
levels(df_curso_select$situacao) <- levels_situacao
levels(df_curso_select$situacao)

levels(df_curso_select$grau_acad) <- c("Bach", "Lic", "Tecn", "ABI", "Seq")
levels(df_curso_select$grau_acad)

levels(df_curso_select$mod_ens) <- c("Pres", "EAD")
levels(df_curso_select$mod_ens)

levels(df_curso_select$niv_acad) <- c("Grad","Seq")
levels(df_curso_select$niv_acad)

levels(df_curso_select$atrib_ingr) <- c("Reg", "ABI", "BLInterd","3")
levels(df_curso_select$atrib_ingr)

levels_uf <- c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB",
               "PE","AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF","EAD")
levels(df_curso_select$uf) <- levels_uf

levels_ocde <- c("Educação", "HumArt", "CSocAplic", "CExNat", "EngConstProd", "CAgric", "Saúde", "Serviços","ABI")
levels(df_curso_select$cod_ocde) <- levels_ocde

# rodar nas



# df_curso_select$uf

# levels <- as.numeric(df_curso_select$uf)
# levels

# grepl seleciona pattern nos valores de uma coluna
# selecionando apenas os cursos com "ENGENHARIA" no nome

eng <- df_curso_select[grepl("ENGENHARIA",df_curso_select$curso),]

# eliminar cursos de tecnologia

eng <- eng[eng$grau_acad != "Tecn",]

str(eng)
#View(eng)

# eliminando os fatores não utilizados
eng$cod_ocde <- droplevels(eng$cod_ocde)
eng$grau_acad <- droplevels(eng$grau_acad)
eng$niv_acad <- droplevels(eng$niv_acad)
eng$atrib_ingr <- droplevels(eng$atrib_ingr)
eng$situacao <- droplevels(eng$situacao)
eng$curso <- droplevels(eng$curso)
eng$uf <- droplevels(eng$uf)
eng$municipio <- droplevels(eng$municipio)
eng$mod_ens <- droplevels(eng$mod_ens)
eng$data_func <- droplevels(eng$data_func)
eng$ies <- droplevels(eng$ies)

names(eng)

# basic orperations with data frame

# is.data.frame(eng[1:10,]) # subsetting a row produces a data frame
# is.data.frame(eng[,2]) #subsetting a column does not produce a data.frame -> use subset or:

# to get a data frame, use drop = F
# eng[,4, drop=F]
# is.data.frame(eng[,4, drop=F])

#eng[is.na(eng),]


levels(eng$uf)

# operating dataframes
#eng$concl_total / eng$ingr_total
#is.data.frame(eng$concl_total / eng$ingr_total)

#----------------- Cruzamentos: Cross Tables

table_ca <- table(eng$cat_adm) #this gives the number of courses in each of the 6 adm categories
table_ca.uf <- table(eng$cat_adm,eng$uf)
class(table_ca)

# aggregate creates a datafile with sums of columns according to categories in another column
table_byuf <- aggregate(cbind(Ingr=eng$ingr_total, Matr=eng$matr_total, Concl=eng$concl_total), by=list(UF=eng$uf), FUN=sum)

table_byuf
str(table_byuf)

#View(table_byuf)


sp<-subset(eng, eng$uf=="SP")
sp
str(sp)
#View(sp)


#?aggregate
table_agreg_eng <- aggregate(cbind(Ingr=eng$ingr_total, Matr=eng$matr_total, Concl=eng$concl_total),by=list(Cat_adm=eng$cat_adm, Org_acad=eng$org_acad, UF=eng$uf), FUN=sum)
table_agreg_sp <- aggregate(cbind(Ingr=sp$ingr_total, Matr=sp$matr_total, Concl=sp$concl_total), by=list(Cat_adm=sp$cat_adm, Org_acad=sp$org_acad), FUN=sum)

getwd()

setwd( "/Users/renato/Dropbox/data/mec/CEnsSup/R_files/testes")
list.files()


write.csv(table_agreg_eng, file="tabela_agregados_eng.csv", fileEncoding = "latin1")
write.csv(table_agreg_sp, file="tabela_agregados_eng_sp.csv", fileEncoding = "latin1")


table_agreg_eng
table_agreg_sp
str(table_agreg_eng)
str(table_agreg_sp)

#?xtabs

#?daply
#?plyr



#eng_pres <- droplevels(subset(eng, eng$mod_ens == "1"))
#str(eng_pres)


# subsetting para SP: WARNING: remove NAs, nesse caso os cursos ? dist?ncia que n?o t?m UF atribu?da
#eng_sp <- droplevels(subset(eng, eng$uf=="35"))

#str(eng_sp)

#View(eng_sp)

# Criando tabelas (matrizes) 

# carregando PLYR

# library(plyr)

# Formato básico de tabelas
# WARNING: cálulas vazias (NA) criam problemas sários - ver abaixo exemplo UFs

#daply(eng_sp,.(cat_adm,org_acad), summarize,sum(matr_total))
# daply(eng,.(cat_adm,org_acad), summarize,sum(matr_total))

# daply(eng_1,.(cat_adm,org_acad), summarize,sum(matr_total))
# daply(eng_2,.(cat_adm,org_acad), summarize,sum(matr_total))

#sum(eng$matr_total)
#sum(eng$matr_total,na.rm = T)

#sum(eng_sp$matr_total)

#sum(eng[(eng$uf == "35"),]$matr_total,na.rm = T)

#sum(eng_sp[(eng_sp$situacao == "1"),]$matr_total)

# Warning: a primeira produz dados errados pois h? os cursos EAD sem atribui??o de UF
#table_1 <- daply(eng,.(uf,cat_adm), summarize,sum(matr_total))
#table_2 <- daply(eng_pres,.(uf,cat_adm), summarize,sum(matr_total))

#table_1
#table_2

# tabula matr?culas por cat_adm e por situ??o (ativo = 1, em extin??o = 3)
# daply(eng_sp,.(cat_adm,eng_sp$situacao), summarize,sum(matr_total))

#sum(eng_sp$matr_total)
#sum(eng_sp$matr_total,na.rm = T)

# Corrigindo data misses



#table1 <- daply(eng,.(uf,cat_adm), summarize,sum(matr_total))
#table2 <- daply(eng,.(cat_adm, org_acad), summarize,sum(matr_total))

#table1
#table2
# Troca NULL por zero

# table1[table1 == "NULL"] = 0
# table2[table2 == "NULL"] = 0

#table1
#table2

# escrevendo tabela em csv para importar com excel WARNING: se houver acentos precisa especifical o encoding = "latin1"
# Default: row.names = T, o que produz a primeira coluna corretamente

# write.table(table1, file="tabela.csv", sep=";", dec=".", fileEncoding = "latin1")

#esse m?todo ? melhor, n?o cria problema com a primeira coluna
#write.csv(table1, file="tabela.csv", fileEncoding = "latin1")

#labels(eng$curso)

#levels(eng$curso)

# list.files()

# WARNING: para recuperar a tabela no R, n?o esquecer o encoding!

# tabela <- read.csv("tabela.csv", sep=";", encoding = "latin1")
# str(tabela)


# --------------- Filtering

# seleciona cursos com mais de 2.000 matriculados

#filter <- eng$matr_total > 2000

#exibe na tela

#eng[filter,] 

# eng[eng$cat_adm == "P?bl Est" & eng$curso == "ENGENHARIA MEC?NICA",]

#str(table1)

# table1[10,1]
# table1["SP","P?blEst"]

#table1

#rownames(table1)
#colnames(table1)

# rownames(table2)
#colnames(table2)
#table2

#------ MATPLOT()

#table1
#View(table1)

#matplot(table1[,"P?blFed"],type="s")

#?matplot
#?plot

# ---- criando um DF de uma matrix VERY WEIRD!!!!

#table1.df <- as.data.frame(table1)
# class(table1.df)
#df<-lapply(table1.df, unlist)

#aplicar novamente as.data.frame
#df.df<- as.data.frame(df)

#df.df
#table1.df
#df
#str(table1.df)

#OK:
#str(df.df)

#class(table1.df)
#class(df)
# table1.df[2,]
#table1.df["SP",]
#colnames(table1.df)

# using rownames to create a new column for the UFs

#newcolumn <- rownames(table1.df)
#table1.df$uf <- newcolumn
#table1.df
#str(table1.df)


# ------------ GGPLOT2

library(ggplot2)
cursos <- df_curso_select
names(cursos)

str(cursos)
levels(cursos$cat_adm)
levels(cursos$cod_ocde)

# data and aesthetics
ggplot(data=cursos, aes(x=ingr_total , y=matr_total))

# add geometry: gráfico dispersão = geom_point
ggplot(data=cursos, aes(x=ingr_total , y=matr_total)) + 
  geom_point()

# add color by some factor to aesthetics

ggplot(data=cursos, aes(x=ingr_total , y=matr_total, 
                        colour = cod_ocde)) + 
  geom_point()

# add size of disk by org_acad

p <- ggplot(data=cursos, aes(x=ingr_total , y=matr_total, 
                        colour = cod_ocde,
                        size = concl_total)) + 
  geom_point()

# Using layers

# basic layer

p <- ggplot(data=cursos, aes(x=ingr_total , y=matr_total, 
                             colour = cod_ocde,
                             size = concl_total))

# Add geometry

p + geom_point()
p+ geom_line()

p+ geom_line() + geom_point()

#------Overriding Aesthetics

q <- ggplot(data=cursos, aes(x=ingr_total , y=matr_total, 
                             size = concl_total))
q + geom_point()

q + geom_point(aes(colour = cat_adm))

# Renaming axis
q + geom_point(aes(colour = cat_adm)) +
  xlab("Ingressos") + 
  ylab("Matrículas")
levels(cursos$cod_ocde)
cexnat <- cursos[cursos$cod_ocde == "CExNat",]
str(cexnat)
levels_org_acad
#View(cexnat)

r <- ggplot(data=cexnat, aes(x=cat_adm , y=concl_total)) +
  ggtitle("Concluintes por cat. adm. e org. acad. - Cursos de Saúde")

r + geom_point(aes(colour = org_acad, size = concl_total)) +
  xlab("Categoria Administrativa") + 
  ylab("Concluintes")

#--- Histogramas e densidades

s <- ggplot(data=cexnat, aes(x=matr_total, color = org_acad))
s + geom_histogram(binwidth=10)+
  xlim(0,200)

# add border

s + geom_histogram(binwidth=10, aes(fill = org_acad)) +
  xlim(0,200)

# addc colour

s + geom_histogram(binwidth=10, aes(fill = org_acad), colour = "Black") +
  xlim(0,200)

s + geom_density(aes(fill=org_acad)) + xlim(0,200)

s + geom_density(aes(fill=org_acad),position="stack")

#--- Stats transforms

class(cursos)
q <- ggplot(data=cursos,aes(x=ingr_total , y=matr_total, colour = cat_adm))

q + xlab("Ingressos") + 
  ylab("Matrículas") +
  geom_smooth()+
  xlim(10,7000) +
  ylim(10,8000)

q <- ggplot(data=cursos,aes(x=ingr_total , y=concl_total, colour = cat_adm))

q + xlab("Ingressos") + 
  ylab("Concluintes") +
  geom_smooth()+
  xlim(10,7000) +
  ylim(10,4000)

# basic barplot

p <- ggplot(data=cursos)
p +  geom_bar(aes(x=uf, y = ingr_total, color=uf, fill=uf),stat = "identity",width=0.7)

# eliminating EAD

p <- ggplot(data=subset(cursos,uf!="EAD"))
p + geom_bar(aes(x=uf, y = ingr_total, color=uf, fill=uf),stat = "identity",width=0.7)

# Choosing the x levels
p <- ggplot(data=cursos)
p +  geom_bar(aes(x=uf, y = ingr_total, color=uf, fill=uf),stat = "identity",width=0.7) +
  scale_x_discrete(limits=c("AC","MG","PR", "GO",  "SP"))

# Ordenando o eixo x - mais fácil criar uma tabela  para os dados agregados por UF usando "aggregate"

table_uf <- aggregate(
  cbind(Ingr=cursos$ingr_total, Matr=cursos$matr_total, Concl=cursos$concl_total), 
  by=list(UF = cursos$uf), 
  FUN=sum)
table_uf

#usar a biblioteca "forcats"
library(forcats)

# função "fct_reorder" - usar o sinal "-" para ordem decrescente (default é crescente)

table_uf$UF <- fct_reorder(table_uf$UF,-table_uf$Matr)
table_uf$UF

p <- ggplot(data=table_uf, aes(x=UF, y = Matr))
p +  geom_bar(stat = "identity",width=0.7)

