# Projeto: Monitoramento do Diario Oficial do Espirito Santo
# Autor: Iago de Carvalho Nunes - contato: github.com/iagocnunes iagodcn@gmail.com
library(tidyverse)
library(httr2)
library(tm)
library(sjmisc)

get_file <- function(date) {
  str_c("https://ioes.dio.es.gov.br/apifront/portal/edicoes/edicoes_from_data/", date, 
        ".json?&subtheme=false") %>%
    request() %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    getElement("itens") %>%
    pull(id) %>% 
    str_c("https://ioes.dio.es.gov.br/portal/edicoes/download/", .) %>% 
    download.file(., mode = "wb", 
                  destfile = str_c(date, ".pdf"))
}

get_file(lubridate::today())

file <- list.files(pattern=paste0(as.character(lubridate::today()), ".pdf"))

corp <- Corpus(URISource(file),
               readerControl = list(reader = readPDF))

# Termos da busca
buscar_por <- c("Gazeta") # exemplos
nao_incluir <- c("Gazeta do Povo") # exemplos

monitor <- str_contains(corp, 
                        buscar_por, 
                        logic = 'or',
                        ignore.case = TRUE) & 
  str_contains(corp,
               nao_incluir,
               logic = "not",
               ignore.case = TRUE)

monitor # se = FALSE, o termo nao aparece; se = TRUE, o termo aparece no Diario
