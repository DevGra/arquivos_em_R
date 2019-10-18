df = NULL
df_filhos = NULL
df_anos = NULL
ano = NULL
son = NULL
classe <- valores_obj_fomento
filhos <- valores_fin_filho
anos <- valores_ano
# classe <- c("AUXREG", "BFRH","DPCT","EMP","INFRA","PLP","PTE")
# filhos <- c("ANSP", "AuxOrg", "AuxPesq", "AuxPub", "AuxReun", "AuxVis", "BEPE", "BExt","BPais", "Capacit",
#             "CEPID","CPE","DPCT","EMU", "EnsPub-Aux", "EnsPub-Bol","FAPLIV","VITAE","INFRA", "JovemPesq",
#             "MidiaCien", "PAPI","PDIP", "PIPE-Aux","PIPE-AUX","PIPE-Bol","PITE", "POLPUB","PP-SUS","ProjEsp", 
#             "Reparo", "RTANSP","RTCOORD", "RTI","SPEC", "Tematico")
# anos <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018")

for (c in 1:length(classe)){
  Class = classe[c]
  for (f in 1:length(filhos)){
    #son = paste0(son, filhos[f], collapse = ", ")
    son = filhos[f]
    df_filhos = rbind(df_filhos, data.frame(son))
    for (a in 1:length(anos)){
      planilha_extr$ano_ref <- as.numeric(as.character(planilha_extr$ano_ref))
      Year = sum(planilha_extr$ano_ref)
      df_anos = rbind(df_anos, data.frame(Year))
    }
  }         
  df = rbind(df, data.frame(Class,df_filhos,df_anos))
}

# ----------------------------------------------------------------------------------------------

df = c(1,7)
df_filhos = NULL
df_anos = NULL
ano = NULL
son = NULL
list_anos = NULL
list_col_names = NULL
classe <- c("AUXREG", "BFRH","DPCT","EMP","INFRA","PLP","PTE")
filhos <- c("ANSP", "AuxOrg", "AuxPesq", "AuxPub", "AuxReun", "AuxVis", "BEPE", "BExt","BPais", "Capacit",
            "CEPID","CPE","DPCT","EMU", "EnsPub-Aux", "EnsPub-Bol","FAPLIV","VITAE","INFRA", "JovemPesq",
            "MidiaCien", "PAPI","PDIP", "PIPE-Aux","PIPE-AUX","PIPE-Bol","PITE", "POLPUB","PP-SUS","ProjEsp", 
            "Reparo", "RTANSP","RTCOORD", "RTI","SPEC", "Tematico")
anos <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018")

ano_ref <- c("2012","2012","2013","2013","2014","2014","2015","2015","2015","2016","2016","2017","2017","2018","2018","2018")


for (a in 1:length(anos)){
  ano_ref <- as.numeric(as.character(ano_ref))
  anos[a] <- as.numeric(as.character(anos[a]))
  list_col_names[[a + 1]]  <- anos[a]
  Year <- sum(anos[a] == ano_ref)
  list_anos[[a]] <- Year
  df = cbind(df, data.frame(list_anos[[a]])) 
  print(a)
  
}

colnames(df) <- c(list_col_names)
#df[,1] <- NULL
df 