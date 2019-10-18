# load cursos
df_curso<- read.csv("files/DM_CURSO.CSV",sep="|",encoding="latin1")

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

#head(df_curso_select)

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

# exemplo de como renomear as colunas 
# nomes_coluna <- c("ies", "cat_adm","org_acad","uf","municipio","curso")
# colunas_renomeadas <- replace(nomes_coluna, c(1, 6),c("IES", "CURSO"))

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

#View(df_curso_select)

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
