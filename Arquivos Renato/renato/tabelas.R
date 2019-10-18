getwd()
setwd("/Users/renatopedrosa/Onedrive/data/cnpq/dadosabertos/")
list.files()

df_orig<-read.csv2("investimentos_cnpq_2018.csv",sep=",",encoding="latin1")
str(df_orig)

df <- df_orig[df_orig$Sigla.UF.Destino=="SP",]
linhas_new <- c("ApoioPartRealEventos", "ApoioPerCient", "ApoioPesqVis", "ApoioProjPesq", "BolsaApTecnico",
                "BolsaDesCientRegional", "BolsaDesCientTecnol", "BolsaDoutorado", "BolsaExtensaoPesquisa",
                "BolsaFixDoutores",  "BolsaGrad", "BolsaIC", "BolsaICJr", "BolsadInicTecnIndustrial",
                "BolsaMestrado","BolsaPesqEspecialistaVisit","BolsaPDoc","BolsasProdutividade", "BolsaExterior",
                "Estagio","Indefinido")

levels(df$Linha.de.Fomento)<-linhas_new

levels(df$Grande.Área) <- c("CAgrarias","CBiologicas","CSaude", "CExatasTerra", "CHumanas","CSociaisAplic",
                            "Engenharias", "Indefinido", "LingLetrasArtes", "Outra", "Tecnologias")

df$Linha.de.Fomento <- factor(df$Linha.de.Fomento)
df$Sigla.Instituição.Macro <- factor(df$Sigla.Instituição.Macro)
df$Sigla.UF.Destino <- factor(df$Sigla.UF.Destino)
df$Sigla.Instituição.Destino <- factor(df$Sigla.Instituição.Destino)
df$Instituição.Destino <- factor(df$Instituição.Destino)
str(df)

levels(df$Sigla.Instituição.Macro)

df_usp <- df[grepl("USP",df$Sigla.Instituição.Macro) | 
                 grepl("USP",df$Sigla.Instituição.Destino) |
                 grepl("USP",df$Instituição.Destino) |
                 df$Sigla.Instituição.Macro=="FAENQUIL",]
df_usp$Sigla.Instituição.Macro <- factor(df_usp$Sigla.Instituição.Macro)
#df_usp$Grande.Área <- factor(df_usp$Grande.Área)
#df_usp$Linha.de.Fomento <- factor(df_usp$Linha.de.Fomento)
str(df_usp)
IES<-c(rep("USP", nrow(df_usp)))
df_usp$IES<- factor(IES)

usp_grarea<-aggregate(df_usp$Valor.Pago, by = list(df_usp$Grande.Área), sum)
usp_fomento<-aggregate(df_usp$Valor.Pago, by = list(df_usp$Linha.de.Fomento), sum)
View(usp_fomento)
str(usp_fomento)

df_unesp <- df[df$Sigla.Instituição.Macro =="UNESP" | df$Sigla.Instituição.Macro=="FMVZ",]

df_unesp$Sigla.Instituição.Macro <- factor(df_unesp$Sigla.Instituição.Macro)
#df_unesp$Grande.Área <- factor(df_unesp$Grande.Área)
#df_unesp$Linha.de.Fomento <- factor(df_unesp$Linha.de.Fomento)

IES<-c(rep("UNESP", nrow(df_unesp)))
df_unesp$IES<- factor(IES)
str(df_unesp)

unesp_grarea<-aggregate(df_unesp$Valor.Pago, by = list(df_unesp$Grande.Área), sum)
unesp_fomento<-aggregate(df_unesp$Valor.Pago, by = list(df_unesp$Linha.de.Fomento), sum)


df_unicamp <- df[df$Sigla.Instituição.Macro =="UNICAMP",]
str(df_unicamp)

df_unicamp$Sigla.Instituição.Macro <- factor(df_unicamp$Sigla.Instituição.Macro)
#df_unicamp$Grande.Área <- factor(df_unicamp$Grande.Área)
#df_unicamp$Linha.de.Fomento <- factor(df_unicamp$Linha.de.Fomento)

IES<-c(rep("UNICAMP", nrow(df_unicamp)))
df_unicamp$IES<- factor(IES)
str(df_unicamp)

df_univ_sp <- rbind(df_usp,df_unesp,df_unicamp)
str(df_univ_sp)
df_univ_sp$Grande.Área<-factor(df_univ_sp$Grande.Área)

levels(df_univ_sp$Grande.Área)
setdiff(as.character(levels(factor(df_univ_sp[df_univ_sp$IES=="USP",]$Linha.de.Fomento))),
         as.character(levels(factor(df_univ_sp[df_univ_sp$IES=="UNICAMP",]$Linha.de.Fomento))))

setdiff(as.character(levels(factor(df_univ_sp[df_univ_sp$IES=="USP",]$Linha.de.Fomento))),
        as.character(levels(factor(df_univ_sp[df_univ_sp$IES=="UNESP",]$Linha.de.Fomento))))
setdiff(as.character(levels(factor(df_univ_sp[df_univ_sp$IES=="UNICAMP",]$Linha.de.Fomento))),
        as.character(levels(factor(df_univ_sp[df_univ_sp$IES=="USP",]$Linha.de.Fomento))))

nrow(df_univ_sp)
names(df_univ_sp)

l1<-df_univ_sp[df_univ_sp$IES=="UNICAMP",][1,]

l1<-list()
df_univ_sp[]

ga<-aggregate(df_univ_sp$Valor.Pago, by = list(df_univ_sp$Grande.Área,df_univ_sp$IES), sum)
lf<-aggregate(df_univ_sp$Valor.Pago, by = list(df_univ_sp$Linha.de.Fomento,df_univ_sp$IES), sum)

str(lf)


levels(lf$Group.1)
l1<-list("BolsaPesqEspecialistaVisit","UNICAMP",0)
l2<-list("BolsaPesqEspecialistaVisit","UNESP",0)

lf1<- rbind(lf,l1,l2)
lf2<-aggregate(lf1$x, by = list(lf1$Group.1,lf1$Group.2), sum)

valores<-as.vector(lf2$x)

mat <- matrix(valores,nrow=length(levels(lf2$Group.1)),ncol=length(levels(lf2$Group.2)))
colnames(mat)<- levels(lf2$Group.2)
rownames(mat)<- levels(lf2$Group.1)

mat<-addmargins(mat, margin = seq_along(dim(mat)), FUN = sum, quiet = FALSE)

write.xlsx(mat,"tabela_lf.xlsx",row.names=TRUE)


