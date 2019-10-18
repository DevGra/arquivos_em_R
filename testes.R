getwd()
setwd("c:/Users/cgt/Desktop/Joana_planilhas")
library(openxlsx)
library(dplyr)
library(expss)
library(stringr)
library(MASS)
library(rio)

planilha <- read.xlsx("vigente_detalhado_mod.xlsx") 

#-- transformando as variaveis da planilha em factor
for (i in 1:ncol(planilha)) {
  planilha[,i] <- factor(planilha[,i])
}

# - criando as tabelas
tabela <- table(planilha$classif_obj_fomen, planilha$ano_ref)
tabela1 <- table(planilha$area_conhecimento_filho, planilha$ano_ref)

# - escrevendo a planilha, com as abas passadas na lista
write.xlsx(list("ano_fomento" = tabela, "ano_are" = tabela1), "tabela.xlsx")

str(planilha)

names(planilha)
# - Selecionando as colunas da planilha
planilha_extr <- subset(planilha, select = c("classif_obj_fomen","grupo_fin_filho","ano_ref"))
View(planilha_extr)
#------
planilha_filho_x_ano <- subset(planilha, select = c("grupo_fin_filho", "ano_ref"))
View(planilha_filho_x_ano)

tabela_filho_x_ano <- table(filho = planilha_filho_x_ano$grupo_fin_filho, ano = planilha_filho_x_ano$ano_ref)
View(tabela_filho_x_ano)
write.xlsx(tabela_filho_x_ano, "tabela_filho_x_ano.xlsx")
# -----
planilha_classe_x_filho <- subset(planilha, select = c("classif_obj_fomen","grupo_fin_filho"))
View(planilha_classe_x_filho)
write.xlsx(planilha_classe_x_filho, "tabela_classe_x_filho.xlsx")

csv_classe_x_filho <- read.xlsx("tabela_classe_x_filho.xlsx")
View(csv_classe_x_filho)


#--- factor 
for (i in 1:ncol(csv_classe_x_filho)) {
  csv_classe_x_filho[,i] <- factor(csv_classe_x_filho[,i])
}
#pega o levels do factor
level_classe <- levels(csv_classe_x_filho$classif_obj_fomen)
level_filho <- levels(csv_classe_x_filho$grupo_fin_filho)

#tira o espaço em branco das variaveis
for (i in 1:length(level_classe)) {
  level_classe[i] <- str_trim(level_classe[i], side = "right")
  level_filho[i] <- str_trim(level_filho[i], side = "right")
}
# atribui o level tratado novamente ao dataframe 
levels(csv_classe_x_filho$classif_obj_fomen) <- level_classe
levels(csv_classe_x_filho$grupo_fin_filho) <- level_filho

names(csv_classe_x_filho) <- c("classe", "filho")

classe_x_filho_group <- csv_classe_x_filho %>%
                      group_by(csv_classe_x_filho$grupo_fin_filho, csv_classe_x_filho$classif_obj_fomen)

tb <- table(classe_x_filho_group)
df <- data.frame(tb)
names(df)

#------------- importando direto e testando a saida da tabel final --------------------

plan_original <- read.xlsx("vigente_detalhado_mod.xlsx")
names(plan_original)

fa <- subset(plan_original, select = c("classif_fomen","grupo_filho","ano_ref"))
#fa  <- distinct(fa, classif_fomen, grupo_filho, ano_ref, .keep_all = TRUE)

contados <- with(fa, ftable(fa$classif_fomen, fa$grupo_filho, fa$ano_ref ))
write.xlsx(contados, "tb_final_ftable.xlsx")
dim(contados)
df_selec <- fa %>%
            group_by(classif_fomen, grupo_filho) %>%
          count(ano_ref) %>%
            distinct(ano_ref, .keep_all = TRUE ) %>%
          arrange(classif_fomen) 

tbb <- tab_pivot()

names(df_selec) <- c("classe", "filho", "ano", "total")
class(df_selec$classe)

            
            



#df_selec <- tibble(df_selec)
classe <- as.character(levels(df_selec$classe))
df_selec <- distinct(df_selec, classe, filho, .keep_all = TRUE)
df_final <- cbind(df_selec, contados, deparse.level = 1)
class(classe)                                             
#-- transformando as variaveis da planilha em factor
for (i in 1:ncol(plan_original)) {
  plan_original[,i] <- factor(plan_original[,i])
}

tb_fa <- ftable(fa)
tb_fa[2,3:7]
nrow(tb_fa)
ncol(tb_fa)
fa$ano_ref <- as.character(fa$ano_ref)
fa$classif_fomen <- as.vector(fa$classif_fomen)
fa$grupo_filho<- as.vector(fa$grupo_filho)
#mat <- matrix(fa,nrow = 65,ncol = 9, byrow = TRUE)

lf2 <- aggregate(fa$ano1, by = list(fa$classif_fomen,fa$grupo_filho), count_row_if(fa$ano_ref == "2012"))

mdat <- matrix(c(1,2,3,4,5,6,7, 11,12,13,14,15,16,27), nrow = 7, ncol = 9, byrow = FALSE,
               dimnames = list(c(1:7),
                               c("Classe", "Filho", c(anos))))



tb_cf <- table(cf)
tb_fa <- table(fa)
write.ftable(tb_fa, "tb_final_ftable.xlsx")
class(tb_fa)

mxt_a <- as.matrix(fa$classif_fomen, fa$grupo_filho, fa$ano_ref)
mtx <- matrix(tbl_a)

tb_final <- matrix(fa)

#----------------------------------------- FIM ------

nlevels(fa$grupo_filho)

class(fa)

classe_x_filho <- subset(df, select = c("csv_classe_x_filho.classif_obj_fomen","csv_classe_x_filho.grupo_fin_filho"))
names(classe_x_filho) <- c("classe", "filho")

classe_x_filho  <- distinct(classe_x_filho, classe,filho, .keep_all = TRUE)
write.xlsx(classe_x_filho, "tabela_classe_x_filho.xlsx")

cl_fi_read <- read.xlsx("tabela_classe_x_filho.xlsx")


str(classe_x_filho_group)

tabela_teste <- table(planilha_extr$classif_obj_fomen , planilha_extr$grupo_fin_filho)
write.xlsx(tabela_teste, "tabela_teste.xlsx")


# ---------------------------------  TESTE --------------------------------------

names(planilha_extr)
mytable <- xtabs(~classe+filho+ano, data = planilha_extr)
tb_final <- ftable(mytable, row.vars = "0")
write.xlsx(mytable, "tabela_final.xlsx")
typeof(mytable)

df2 <- data.frame(ftable(xtabs(~classe+filho+ano, data=planilha_extr)))

?xtabs


tb_teste <- table(planilha_extr$classe, planilha_extr$filho, planilha_extr$ano)
format <- ftable(tb_teste)
write.xlsx(format, "tabela_teste_teste.xlsx")



#----- teste para geracao do df final de uma unica vez
anos <- levels(planilha$ano_ref)
classes <- levels(planilha$classif_obj_fomen)
classes <- str_trim(classes, side = "right")
filhos <- levels(planilha$grupo_fin_filho)
filhos <- str_trim(filhos, side = "right")
 
names(planilha_extr) <- c("classe", "filho", "ano")
i <- 1
arq <- NULL
for(a in anos){
  for(f in filhos){
     arq <- planilha_extr %>%
            select(classe, filho, ano) %>%
            filter(ufn =='São Paulo' & idhm > .8 & ano == a)
  }
  arq[i] <- arq
  i < i + 1
}

ano_2012 <- planilha_extr %>%
  select(classe, filho, ano) %>%
  filter(ano %in% c(anos)) %>%
  distinct(classe, filho, .keep_all = TRUE) %>%
  count(ano)
  
#--------- teste geracao ------------------------

ano <- planilha$ano_ref
unique(ano)

x<-planilha%>%
  group_by(ano_ref)%>%
  count(ano_ref)

y <- planilha%>%                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
  group_by(ano_ref, classif_obj_fomen)%>%
  count(processo_filho)

teste <- planilha%>%
  group_by(ano_ref, classif_obj_fomen)%>%
  summarise(ano_2012 = count(processo_filho))
  
  
count_if("2012", planilha$ano_ref)