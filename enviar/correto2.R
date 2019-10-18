getwd()
setwd('C:\\Users\\GRAZIANI\\Desktop\\FAPESP_RECENTE\\planilha_convenio')
getwd()
library(openxlsx)
library(dplyr)
library(tidyr)

plan_original <- read.xlsx("vigente_detalhado_mod.xlsx")
str(plan_original)

#-- transformando as variaveis da planilha em factor
for(i in 1:ncol(plan_original)){
  plan_original[,i] <- factor(plan_original[,i])
  
}

#------------- importando direto e testando a saida da tabel final --------------------

fa <- subset(plan_original, select = c("classif_obj_fomen","grupo_fin_filho","ano_ref"))

names(fa) <- c("classif_fomen","grupo_filho","ano_ref")


df_selec <- fa %>%
  group_by(classif_fomen, grupo_filho) %>%
  count(ano_ref) %>%
  distinct(ano_ref, .keep_all = TRUE ) %>%
  arrange(classif_fomen)

df <- df_selec %>%
  spread(ano_ref, n)

str(df$classif_fomen)
df$classif_fomen <- levels(df$classif_fomen)


write.xlsx(df, "tb_final.xlsx")























