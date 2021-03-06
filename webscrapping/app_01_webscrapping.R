##################################################################
##################################################################
# WEB SCRAPPING com Wordcloud
##################################################################
##################################################################

##################################################################
# Instala��o dos pacotes (se for o caso)
##################################################################
#install.packages(c("rvest", "stringr", "tidyverse", "tm", "wordcloud", "SnowballC"))

##################################################################
# Carga de pacotes
##################################################################

# rvest - leitura do conte�do de p�ginas web
# stringr - manipula��o de strings
# tudyverse - grupo de pacotes para manipula��o de dados (tidyr, dplyr, ggplot2...)
# tm - ferramenta de text mining
# wordcloud - plot de nuvem de palavras
# SnowballC - extrator de radical das palavras baseado em C
library("xml2")
library("rvest") 
library("stringr") 
library("tidyverse") 
library("tm") 
library("wordcloud")
library("SnowballC")

##################################################################
# Fun��o scrap
# par�metros:
#   - site: string com o endere�o do site a ser baixado
##################################################################
scrap <- function(site){
   # Printa url da p�gina 
   print(paste("Baixando p�gina:", site))
  
   # Downloa da p�gina  Web
   # {rvest}: read_html
   html_source <- read_html(site)
   
   # NOTA: n�o mais necess�rio ap�s atualiza��o da p�gina R-Bloggers
   # Filtra todos os t�tulos 
   # {rvest}: html_nodes e html_attr
   # titles <- html_souce %>% 
   #           html_nodes("article") %>%
   #           html_nodes("a") %>%
   #           html_attr("title")
   
   # Filtra todos os t�tulos 
   # {rvest}: html_nodes, htm_text e html_attr
   titles <- html_source %>% 
      html_nodes("article") %>%
      html_nodes("header") %>%
      html_nodes("h3") %>%
      html_nodes("a") %>%
      html_text()
   
   # Remove todos os itens NA
   # {base}: is.na
   titles <- titles[!is.na(titles)]
   
   # NOTA: n�o mais necess�rio ap�s atualiza��o da p�gina R-Bloggers
   # Remove todos as ocorr�ncias do prefixo 'Permalink to '
   # {base}: gsub
   # titles <- gsub("Permalink to ", "", titles)
   
   # Retorna vetor com todos os t�tulos
   return(titles)
}
# Fim fun��o scrap ###############################################

# Define p�gina base
root <- "https://www.r-bloggers.com"

# Cria lista de todas as p�ginas que ser�o baixadas
# "https://www.r-bloggers.com"
# "https://www.r-bloggers.com/page/2"
# "https://www.r-bloggers.com/page/..."
all_pages <- c(root, paste0(root,"/page/",2:5))

# obtem lista de todos t�tulos
# {base}: lapply
# lapply(): aplica a fun��o scrap para cada item do vetor retornando uma lista
all_titles <- lapply(all_pages, scrap)

# Converte lista de t�tulos em vetor �nico
all_titles <- unlist(all_titles)

# Formata t�tulos
# converete para letras min�sculas, remove n�meros, remove lista de palavras n�o importantes 
# remove pontua��o, remove espa�os em branco
# tm: removeNumbers, removeWords, removePunctuation, stopwords
# stringr: str_trim
cleaned <- all_titles %>% tolower() %>%  removeNumbers() %>% removeWords(c(stopwords("en"), "data", "r")) %>% removePunctuation() %>% str_trim()

# Converte o vetor de frases em uma lista de documentos estruturados do tipo (tm::Corpus)
# Corpus(): converte para documentos estruturados
# VectorSource(): converte vetor de caracteres em um objeto Source (VectorSource, DataframeSource....)
# tm: Corpus, VectorSource
cleaned_corpus <- Corpus(VectorSource(cleaned))

# Obtem o radical das palavras aplicando a fun��o stemDocument em todas lista
# {tm}: tm_map
# {SnaowballC}: stemDocument
# tm_map(x, f): aplica fun��o f para toda lista x (tm::Corpus)
# stemDocument(): obtem radical de um vertor de textos ou documento struturado do tipo (tm::Corpus)
cleaned_corpus <- tm_map(cleaned_corpus, stemDocument)

# Converte a lista de documentos em um objeto TermDocumentMatrix (TermosxDocumento)
# cria uma matrix onde as linhas s�o os termos e as colunas os documentos
# {base}: sort, rowSums
# {tm}: TermDocumentMatrix
# TermDocumentMatrix(): Convert lista de documentos em uma matriz de termos x documentos (lista)
doc_object <- TermDocumentMatrix(cleaned_corpus) 


# Converte a lista de documentos em matriz de termos x documentos (matriz)
# {base}: as.matrix
doc_matrix <- as.matrix(doc_object)


# Obtem as linhas ordenados decrescentemente pela maior frequ�ncia
# {base}: sort, rowSums
counts <- sort(rowSums(doc_matrix),decreasing=TRUE) 

# Retorna as linhas onde os nomes das colunas do vetor contenha apenas caracteres de a-z
# {base}: grepl
counts <- counts[grepl("^[a-z]+$", names(counts))]

# Cria dataframe com as colunas word e freq (termo e frequ�ncia)
# {base}: data.frame
frame_counts <- data.frame(word = names(counts), freq = counts)

# Realiza a plotagem da nuvem de palavras
# wordcloud: wordcloud 
# wordcloud(): plot nuvem
wordcloud(words = frame_counts$word, freq = frame_counts$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.2, colors=brewer.pal(8, "Dark2"))


