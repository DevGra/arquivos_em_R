getwd()
setwd("c:/Users/cgt/Desktop/Joana_planilhas")
library(openxlsx)
library(dplyr)
library(expss)

planilha <- read.xlsx("vigente_detalhado_mod.xlsx") 

#-- transformando as variaveis da planilha em factor
for(i in 1:ncol(planilha)){
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
planilha_extr <- subset(planilha, select=c("ano_ref","grupo_fin_filho","classif_obj_fomen"))
View(planilha_extr)

tabela_teste <- table(planilha_extr$classif_obj_fomen , planilha_extr$grupo_fin_filho)
write.xlsx(tabela_teste, "tabela_teste.xlsx")
?table

# ---------------------------------  TESTE --------------------------------------

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