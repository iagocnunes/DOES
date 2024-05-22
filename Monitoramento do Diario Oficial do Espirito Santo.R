# Projeto: Monitoramento do Diario Oficial do Espirito Santo
# Autor: Iago de Carvalho Nunes - contato: github.com/iagocnunes iagodcn@gmail.com
library(tidyverse)
library(lubridate)
library(httr2)
library(tm)
library(sjmisc)
library(chron)

# Se certifique que o diretorio de trabalho da sessao nao possui arquivos '.pdf'
setwd("C:/Users/Fulano/Desktop/Pasta01/DOES")

# Termos da busca
buscar_por <- c("Gazeta") # exemplo
nao_incluir <- c("Gazeta do Povo") # exemplo

# Data da busca
dia_inicial <- ("2022-04-30") # exemplo, formato ano/mes/dia
dia_final <- ("2024-05-07") # exemplo, formato ano/mes/dia


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

monitor <- str_contains(corp, 
                        buscar_por, 
                        logic = 'or',
                        ignore.case = TRUE) & str_contains(corp,
                                                           nao_incluir,
                                                           logic = "not",
                                                           ignore.case = TRUE)

monitor # se = FALSE, o termo nao aparece; se = TRUE, o termo aparece no Diario

