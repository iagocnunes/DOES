# Projeto: Monitoramento do Diario Oficial do Espirito Santo
# Autor: Iago de Carvalho Nunes - contato: github.com/iagocnunes iagodcn@gmail.com
###########################################################
## Encontrando as palavras relacionadas aos termos de busca
###########################################################
library(RColorBrewer)
library(wordcloud2)
library(webshot)
library(htmlwidgets)
webshot::install_phantomjs()

rm(cluster1X, cluster2A, i, x1, vy1A)
cluster1A <- cluster1A[,3]
stop_words <- read.csv("https://raw.githubusercontent.com/iagocnunes/DOES/main/stopwords-pt.txt", header=F)

for(i in names(corp)){
  x1 <- as.data.frame(unlist(corp[[i]]))
  for(z in cluster1A){
    x2 <- x1 %>% setNames(c("V1X")) %>%
      filter(str_detect(V1X,
                        regex(z, ignore_case = T),
                        negate = F) & 
               str_detect(V1X,
                          regex(nao_incluir, ignore_case = T),
                          negate = T))  %>%
      assign(value = .,
             x = paste0(c(gsub("[^a-zA-Z0-9]", "", iconv(sub("[^|]+\\|", "", z),from="utf-8",to="ASCII//TRANSLIT")), "_", "DO", gsub("-|.pdf", "", i), "ES", "_out"), collapse = ""),
             envir = globalenv())
  }
}

for(z in cluster1A){
  cluster1Z <- dplyr::bind_rows(mget(ls(pattern=gsub("[^a-zA-Z0-9]", "", iconv(sub("[^|]+\\|", "", z),from="utf-8",to="ASCII//TRANSLIT")))), .id = 'source')
  cluster1Z <- cluster1Z %>% 
    assign(value = .,
           x = paste0(c(gsub("[^a-zA-Z0-9]", "", iconv(sub("[^|]+\\|", "", z),from="utf-8",to="ASCII//TRANSLIT")), "_btw"), collapse = ""),
           envir = globalenv())
}

rm(list = ls(pattern="_out"))
rm(x1,x2)

list <- names(mget(ls(pattern="_btw")))

for(z in list){
  y <- get(z)
  y <- setDT(as.data.frame(y), keep.rownames = T)
  y <- as.data.frame(paste0(c(y$V1X), collapse = " "))
  y$doc_id <- z
  colnames(y) <- c("text", "doc_id")
  y <- y[,c("doc_id", "text")]
  y <- Corpus(DataframeSource(y))
  y <- tm_map(y, removeWords, stop_words$V1)
  y <- tm_map(y, stripWhitespace)
  y <- tm_map(y, removePunctuation, ucp = TRUE)
  y <- tm_map(y, removeNumbers, ucp = TRUE)
  TDM_clusters <- TermDocumentMatrix(y, 
                                     control = 
                                       list(tolower = TRUE))
  ft <- findFreqTerms(TDM_clusters, lowfreq = 10, highfreq = Inf)
  x6 <- as.matrix(TDM_clusters[ft,]) 
  x6 <- setDT(as.data.frame(x6), keep.rownames = T)
  x6 <- x6 %>% 
    anti_join(stop_words, by = join_by(rn == V1)) %>% 
    anti_join(as.data.frame(str_split(buscar_por, "\\s|\\|"), col.names ="V1"), by = join_by(rn == V1))
  colnames(x6) <- c("words", "freq")
  x6 <- x6[order(x6$freq, decreasing = T),]
  set.seed(1234)
  cloud <- wordcloud2(x6,
                      color=brewer.pal(8, "Dark2"))
  saveWidget(cloud, paste0(c("cloud", z, ".html"), collapse = ""), selfcontained = F)
  webshot(paste0(c("cloud", z, ".html"), collapse = ""), paste0(c("cloud", z, ".png"), collapse = ""), delay = 5, vwidth = 1500, vheight = 1000)
  rm(cloud, x6, ft, TDM_clusters, y)
  
}
