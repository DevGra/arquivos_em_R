getwd()
setwd("C:\\Users\\cgt\\Desktop\\Joana_planilhas")

library(openxlsx)
library(dplyr)
library(tidyr)

# - carregando a planilha
plan_original <- read.xlsx("vigente_detalhado_mod.xlsx")
str(plan_original)

#-- transformando as variaveis da planilha em factor
for(i in 1:ncol(plan_original)){
  plan_original[,i] <- factor(plan_original[,i])
  
}
names(plan_original)

# - selecionando as variaveis 
fa <- subset(plan_original, select = c("classif_fomen","grupo_filho","ano_ref"))

# - agrupando as variaveis(fomen e filho) e contando suas ocorrencias por ano
df_selec <- fa %>%
  group_by(classif_fomen, grupo_filho) %>%
  count(ano_ref) 
  # distinct(ano_ref, .keep_all = TRUE) %>%
  # arrange(classif_fomen)

# - transformando os anos em coluna com sua respectiva contagem 
df <- df_selec %>%
  spread(ano_ref, n)

# - gravando a planilha(tb_final) e nomeando sua aba(grp_classe_filho_ano)
write.xlsx(list("grp_classe_filho_ano" = df), "tb_final.xlsx")


# --------------------------------------------------------------------- fim -------------------------------------------------------

set.seed(34756) # Set seed

data_header <- data.frame(Household_ID = runif(100), # Some dummy data
                          Sex = runif(100),
                          Age = runif(100),
                          Nationality = runif(100),
                          Year = runif(100),
                          Health = runif(100),
                          CoB = runif(100),
                          Income = runif(100),
                          Household_Size = runif(100),
                          Holidays = runif(100),
                          Marital_Status = runif(100),
                          Expenditure = runif(100))
data_header$Sex[rbinom(100, 1, 0.1) == 1] <- NA # Insert NA's

Sex <- data_header %>% drop_na(Sex)

# -----------------------------------------------------------------

# - exemplos de como trabalhar com as NAs - https://statistical-programming.com/r-replace-na-with-0/

#- cria uma matriz com alguns NAs
m <- matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10)
d <- as.data.frame(m)
# - substituindo os NAs da coluna por 99
d$V4[is.na(d$V4)] <- 99















