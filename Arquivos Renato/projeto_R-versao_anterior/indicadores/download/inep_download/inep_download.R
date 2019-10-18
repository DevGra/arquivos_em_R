load(file = "DIR.RData")
source("librarys.R")

getwd()

#source("local_settings.R")


url <- 'http://portal.inep.gov.br/microdados'

#html <- read_html(url)

# classes <- html %>%
#   html_nodes('div') %>%
#   html_nodes("[class='list-download--two-columns-programs anchor__content']")
#   
#classes
# retitando apenas o atribute href dos links 
# data_anchor_cens_educ_sup <- classes[1] %>%
#   html_node(".list-download__content") %>%
#   html_node("ul") %>%
#   html_nodes("a") %>%
#   html_attr("href")

get_url_links <- function(url){
  link <- url
  
  #html <- xml2::read_html(link)
  html <- read_html(link)
  
  classes <- html %>%
    html_nodes('div') %>%
    html_nodes("[class='list-download--two-columns-programs anchor__content']")
  
  link <- classes[1] %>% 
    html_node(".list-download__content") %>%
    html_node("ul") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  
  ano <- classes[1] %>% 
    html_node(".list-download__content") %>%
    html_node("ul") %>%
    html_nodes("a li") %>% 
    html_text()
  
  
  # Combinando o nao com os links
  links <- tibble(ano = ano, link = link)
  #links <- list(ano = ano, link = link)
  
  return(links)
  
}

links <- get_url_links(url)
links$ano[23]
links$link[23]

length_list <- nrow(links)
length_list


i <- 23

while (i <= length_list) {
  
  #dir_down_for_inep <- sprintf("%s",paste(DOWNLOAD_DIR, 'inep_download',links$ano[i], 'inep.zip', sep='/'))
  dir_down_inep <- sprintf("%s",paste('inep_download',links$ano[i], sep = '/'))
  out_dir <- file.path(DIR$DOWNLOAD_DIR, dir_down_inep)
  
 
  if (i == 23) {
    print(i)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir)
      
      #download(links$link[i], dest = out_dir, mode = "wb")
      download(links$link[i], dest = sprintf("%s",paste(out_dir,'inep.zip', sep = '/')) , mode = "wb")
      # descompacta o zip 
      unzip(zipfile = sprintf("%s",paste(out_dir,'inep.zip', sep = '/')), exdir = out_dir)
      
      # para remover o arquivo baixado
      file_to_remove <- sprintf("%s",paste(out_dir,'inep.zip', sep = '/'))
      unlink(file_to_remove)
    
    } else {
      print("Este diretorio ja existe!")
    }
  }
  
  i <- i + 1
 
}

out_dir
library(plyr)
# get all the zip files
#zipF <- list.files(path = "/your/path/here/", pattern = "*.zip", full.names = TRUE)
zipF <- list.files(path = out_dir, pattern = "*.zip", full.names = TRUE)

# unzip all your files
ldply(.data = zipF, .fun = unzip, exdir = out_dir)

setwd(out_dir)
list.files()

# download(url, dest="dataset.zip", mode="wb") 
# unzip ("dataset.zip", exdir = "./")
# 
# 
# write.csv(table_agreg_eng, file="tabela_agregados_eng.csv", fileEncoding = "latin1")
# write.csv(table_agreg_sp, file="tabela_agregados_eng_sp.csv", fileEncoding = "latin1")



















