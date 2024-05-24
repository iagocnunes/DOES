# Projeto: Monitoramento do Diario Oficial do Espirito Santo
# Autor: Iago de Carvalho Nunes - contato: github.com/iagocnunes iagodcn@gmail.com
library(tidyverse)
library(lubridate)
library(httr2)
library(tm)
library(sjmisc)
library(chron)
library(data.table)

# Se certifique que o diretorio de trabalho da sessao nao possui arquivos '.pdf'
setwd("C:/Users/Fulano/Desktop/Pasta01/DOES")

# Data da busca
dia_inicial <- ("2024-04-30") # exemplo, formato ano/mes/dia
dia_final <- ("2024-05-07") # exemplo, formato ano/mes/dia

# Termos de busca
buscar_por <- paste0(c("Educação Infantil", "Sistema de educação"), collapse = "|") # exemplo
nao_incluir <- paste0(c("Secretaria de Estado da Educação"), collapse = "|") # exemplo


get_file <- function(date) {
  tryCatch({
    str_c("https://ioes.dio.es.gov.br/apifront/portal/edicoes/edicoes_from_data/", date, 
          ".json?&subtheme=false") %>%
      request() %>%
      req_perform() %>%
      resp_body_json(simplifyVector = TRUE) %>%
      getElement("itens") %>%
      pull(id) -> file
    for(file in file){
      download.file(str_c("https://ioes.dio.es.gov.br/portal/edicoes/download/", file), mode = "wb", 
                    destfile = str_c(date, "_", file, ".pdf"))}
  }, error=function(e){cat("ERROR: Sem diario neste dia, feriado!")})
}

n_days <- lubridate::interval(lubridate::ymd(dia_inicial),lubridate::ymd(dia_final))/lubridate::days(1)
days <- lubridate::ymd(dia_inicial) + lubridate::days(0:n_days)
days <- as.character(days[!is.weekend(days)])
for(day in days){
get_file(day)
}

file <- list.files(pattern=".pdf")
corp <- Corpus(URISource(file),
               readerControl = list(reader = readPDF))

cluster1A <- as.data.frame(str_split(buscar_por, "\\|"))
colnames(cluster1A) <- "V1"
cluster1A$V2 <- gsub(" ", "\\\\s*(.*?)\\\\s*", cluster1A$V1)
cluster1A$V3 <- as.character(paste0(cluster1A$V2, "|", cluster1A$V1))
cluster1X <- paste0(cluster1A[,3], collapse = "|")

cluster2A <- as.data.frame(str_split(nao_incluir, "\\|"))
colnames(cluster2A) <- "V1"
cluster2A$V2 <- gsub(" ", "\\\\s*(.*?)\\\\s*", cluster2A$V1)
cluster2A$V3 <- as.character(paste0(cluster2A$V2, "|", cluster2A$V1))
cluster2X <- paste0(cluster2A[,3], collapse = "|")

for(i in names(corp)){
  x1 <- as.data.frame(unlist(corp[[i]]))
  x1 <- x1 %>% setNames(c("V1X")) %>%
    filter(str_detect(V1X,
                      regex(cluster1X, ignore_case = T),
                      negate = F) & 
             str_detect(V1X,
                        regex(cluster2X, ignore_case = T),
                        negate = T))  %>%
    assign(value = .,
           x = paste0(c("DO", gsub("-|.pdf", "", i), "ES", "_out"), collapse = ""),
           envir = globalenv())
}

vy1A <- dplyr::bind_rows(mget(ls(pattern="_out")), .id = 'source')
rm(list = ls(pattern="_out"))
vy1A <- setDT(as.data.frame(vy1A), keep.rownames = T)
vy1A <- vy1A[,1:2]
vy1A$rn <- gsub("content", "", vy1A$rn)
vy1A$source <- gsub("_out", "", vy1A$source)
vy1A <- vy1A[,c("source","rn")] 
colnames(vy1A) <- c("DOES", "Pagina")
vy1A
