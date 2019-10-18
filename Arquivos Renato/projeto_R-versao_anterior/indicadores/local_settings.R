library(crayon)
# definindo a cor padrão dos avisos de erro e warning
error <- red $ bold
warn <- magenta $ underline

#getwd()
#setwd("indicadores")

#DIR <- 'K:/projeto_R/inep'
#setwd(file.path("K:", "projeto_r", "inep"))
#PATH <- getwd()
# 
# DOWNLOAD_DIR <- sprintf("%s", paste(PATH, 'download', sep="/"))
# TRANSFORM_DIR <- sprintf("%s", paste(PATH, 'transform', sep="/"))
# SELECTED_DIR <- sprintf("%s", paste(PATH, 'selected', sep="/"))
# GRAPHIC_DIR <- sprintf("%s", paste(PATH, 'graphic', sep="/"))

# lista com os diretorios padrao do projeto
DIR <- list(DOWNLOAD_DIR = "C:/Users/cgt/Documents/carlos/projeto_R/indicadores/download",
             TRANSFORM_DIR = "C:/Users/cgt/Documents/carlos/projeto_R/indicadores/transform",
             SELECTED_DIR = "C:/Users/cgt/Documents/carlos/projeto_R/indicadores/selected",
             GRAPHIC_DIR = "C:/Users/cgt/Documents/carlos/projeto_R/indicadores/graphic"
             
        )
DIR
# salva as variaveis que serao globais, em em arquivo rdata que podera ser carregado em qualquer outro arquivo
save(DIR, file = "DIR.RData")

# para salvar uma imagem do workspace
#save.image(file="estruturacao.RData")
