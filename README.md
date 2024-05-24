Projeto: Monitoramento do Diário Oficial do Espírito Santo<br>
Autor: Iago de Carvalho Nunes - contato: github.com/iagocnunes iagodcn@gmail.com

## Pacotes de trabalho
Os pacotes necessários para executar essa atividade são:
````R
library(tidyverse) # ferramentas de ETL
library(lubridate) # Trabalhando com datas
library(httr2) # Trabalhando com HTTP
library(tm) # Trabalhando com mineração de texto
library(sjmisc) # ferramentas de ETL
library(chron) # Trabalhando com datas
library(data.table) # Trabalhando com objetos do tipo dataframe
````

## Baixando Diários
A primeira parte do script baixa os arquivos do DOES para uma pasta de sua escolha.  <br>
Se certifique que o diretório de trabalho da sessão não possui arquivos '.pdf' (preferencialmente, crie uma pasta nova e cole o caminho dela entre as aspas do comando ```setwd("")```.  <br>
Certifique-se que o caminho está com forward slash ("/"):
````R
setwd("C:/Users/Fulano/Desktop/Pasta01/DOES")
````
Em seguida, defina o intervado de tempo que iremos trabalhar:
````R
dia_inicial <- ("2024-04-30") # exemplo, formato ano/mes/dia
dia_final <- ("2024-05-07") # exemplo, formato ano/mes/dia
````
Defina os termos que serão buscados. <br>
Podem ser quantos termos forem necessários, mas certifique-se que estão acentuados conforme aparecem em documentos oficiais. <br>
Pode ser nomes de pessoas, empresas, CNPJs, entre outros. Caso for um CNPJ, certifique-se de incluir tanto um termo contendo traços e barras, quanto apenas com os números.:
````R
buscar_por <- paste0(c("Educação Infantil", "Sistema de educação"), collapse = "|") # exemplos
nao_incluir <- paste0(c("Secretaria de Estado da Educação", "Secretaria Municipal de Educação"), collapse = "|") # exemplos
````
Função para baixar os arquivos do intervalo definido:
````R
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
````
Baixando os arquivos:
````R
n_days <- lubridate::interval(lubridate::ymd(dia_inicial),lubridate::ymd(dia_final))/lubridate::days(1)
days <- lubridate::ymd(dia_inicial) + lubridate::days(0:n_days)
days <- as.character(days[!is.weekend(days)])
for(day in days){
get_file(day)
}
````
## Termos de busca
Aqui, criamos um objeto tipo ```Corpus``` com todos os arquivos baixados; e damos início a busca pelos termos:
````R
file <- list.files(pattern=".pdf")
corp <- Corpus(URISource(file),
               readerControl = list(reader = readPDF))

cluster1A <- as.data.frame(str_split(buscar_por, "\\|"), col.names =  "V1")
cluster1A$V2 <- gsub(" ", "\\\\s*(.*?)\\\\s*", cluster1A$V1)
cluster1A$V3 <- as.character(paste0(cluster1A$V2, "|", cluster1A$V1))
cluster1X <- paste0(cluster1A[,3], collapse = "|")

cluster2A <- as.data.frame(str_split(nao_incluir, "\\|"), col.names =  "V1")
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
````
Ao final, o objeto ```vy1A``` retorna um ```data.frame``` com o nome do DOES onde o termo foi encontrado e a respectiva página:
````R
vy1A
                 DOES Pagina
 1: DO20240430_9053ES      4
 2: DO20240430_9053ES    108
 3: DO20240430_9053ES    109
 4: DO20240430_9053ES    111
 5: DO20240430_9053ES    112
 6: DO20240430_9053ES    117
 7: DO20240502_9059ES     96
 8: DO20240502_9059ES    106
 9: DO20240503_9064ES    187
10: DO20240503_9064ES    193
11: DO20240506_9067ES      7
12: DO20240506_9067ES    158
13: DO20240506_9067ES    178
14: DO20240506_9067ES    179
15: DO20240506_9067ES    180
16: DO20240506_9067ES    191
17: DO20240506_9067ES    198
18: DO20240506_9067ES    199
19: DO20240506_9067ES    200
20: DO20240506_9067ES    201
21: DO20240506_9067ES    212
22: DO20240507_9070ES     83
                 DOES Pagina
````
DO20240430 = Diário Oficial 2024-04-30 [ano, mês, dia] <br>
9067ES = Identificador único do arquivo (caso haja mais de 1 DO por dia).
## Encontrando termos associados
O segundo script ("Monitoramento DOES_part2.R") constrói uma nuvem com as palavras mais recorrentes nas páginas em que aparecem os termos de busca. <br>
Resultado do nosso exemplo:<br>
<p align="left">
<a href="_blank">
<img src="https://github.com/iagocnunes/DOES/blob/main/img/cloudEducacaoInfantil_btw.png" width="600" alt="Ciências Sociais - UFES">
</a>
</p>
<p align="left">
<a href="_blank">
<img src="https://github.com/iagocnunes/DOES/blob/main/img/cloudSistemadeeducacao_btw.png" width="600" alt="Ciências Sociais - UFES">
</a>
</p>
