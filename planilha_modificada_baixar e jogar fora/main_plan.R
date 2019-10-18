getwd()
setwd('C:\\Users\\GRAZIANI\\Desktop\\FAPESP_RECENTE\\planilha_convenio')
getwd()
library(openxlsx)
library(dplyr)
library(stringr)

planilha <- read.xlsx("vigente_detalhado_mod.xlsx")
str(planilha)

#planilha <- planilha[,sapply(planilha[,1:7],str_trim,side="right")]

#-- transformando as variaveis da planilha em factor
for(i in 1:ncol(planilha)){
  planilha[,i] <- factor(planilha[,i])
  
}

# - criando as tabelas
tabela <- table(planilha$classif_obj_fomen, planilha$ano_ref)
tabela1 <- table(planilha$grupo_fin_filho, planilha$ano_ref)
tabela2 <- table(planilha$grupo_fin_filho, planilha$classif_obj_fomen)
tabela3 <- table(planilha$area_conhecimento_filho, planilha$ano_ref)

df_test <- data.frame(tabela)
colnames(df_test) <- c("Classe", "Ano", "Qtde_ano")

df_test2 <- data.frame(tabela1)
colnames(df_test2) <- c("Grupo_filho", "Ano", "Qtde_ano")

number_por_ano = df_test2 %>%
  group_by(Grupo_filho,Ano) %>%
  summarise(number_por_ano = sum(Qtde_ano))

# - Selecionando as colunas da planilha
planilha_extr <- subset(planilha, select=c("classif_obj_fomen","grupo_fin_filho","ano_ref"))
str(planilha_extr)

planilha_extr <- planilha_extr[1:2000,]


#pega o levels do factor
valores_obj_fomento <- levels(planilha_extr$classif_obj_fomen)
#tira o espaço em branco das variaveis
for(i in 1:length(valores_obj_fomento)){
  valores_obj_fomento[i] <- str_trim(valores_obj_fomento[i], side = "right")
}

#pega o levels do factor
valores_fin_filho <- levels(planilha_extr$grupo_fin_filho)
#tira o espaço em branco das variaveis
for(i in 1:length(valores_fin_filho)){
  valores_fin_filho[i] <- str_trim(valores_fin_filho[i], side = "right")
}

valores_ano <- levels(planilha_extr$ano_ref)


df = NULL
list_filhos = NULL
list_anos = NULL
list_class = NULL
list_col_names = NULL
ano = NULL
classe <- valores_obj_fomento
filhos <- valores_fin_filho
anos <- valores_ano
# classe <- c("AUXREG", "BFRH","DPCT","EMP","INFRA","PLP","PTE")
# filhos <- c("ANSP", "AuxOrg", "AuxPesq", "AuxPub", "AuxReun", "AuxVis", "BEPE", "BExt","BPais", "Capacit",
#             "CEPID","CPE","DPCT","EMU", "EnsPub-Aux", "EnsPub-Bol","FAPLIV","VITAE","INFRA", "JovemPesq",
#             "MidiaCien", "PAPI","PDIP", "PIPE-Aux","PIPE-AUX","PIPE-Bol","PITE", "POLPUB","PP-SUS","ProjEsp", 
#             "Reparo", "RTANSP","RTCOORD", "RTI","SPEC", "Tematico")
# anos <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018")
ano_ref_ret <- subset(planilha_extr, select=c("ano_ref"))
# convertendo dataframe para vetor
ano_ref_ret <- as.character(unlist(ano_ref_ret[, 1]))

str(ano_ref_ret)

for (c in classe){
  print(c)
  print("--------------------")
  list_class[c] <- c

  for (f in filhos){
      print(f)
      list_filhos[f] <- f
    #   if(f > 1){
    #   list_class[f] <- ""
    # }
 
    for (a in anos){
      #anos[a] <- as.numeric(as.character(anos[a]))
      print(a)
      Year <- sum(a == ano_ref_ret)
      list_anos[a] <- Year
      list_col_names[a]  <- a

    }
  }
}
  # if(c == 1){
  #   df = data.frame(list_class,list_filhos)
  #   
  # }
  # df = rbind(list_class,list_filhos)
  # #df = rbind(df, data.frame(list_class,list_filhos))

len_classe <- length(list_class)
len_filhos <- length(list_filhos)


df <- data.frame(list_class,list_filhos)

list_anos <- as.character(list_anos)
list_anos
list_col_names
vector_a<-unlist(list_col_names)
vector_b<-unlist(list_anos)
mtx <- rbind(c(vector_a), c(vector_b))
tb <- as.table(mtx)


?table



agrupados = planilha_extr %>%
  group_by(planilha_extr$classif_obj_fomen, planilha_extr$grupo_fin_filho) %>%
  summarise(agrupados = sum(planilha_extr$ano_ref))

# - escrevendo a planilha, com as abas passadas na lista
write.xlsx(list("ano_fomento" = tabela, "ano_are" = tabela1), "tabela.xlsx")
write.xlsx(tabela2, "tabela2_test.xlsx")

tabela <- data.frame(planilha$classif_obj_fomen, planilha$grupo_fin_filho)
resto <- planilha %>% group_by(planilha$classif_obj_fomen)

