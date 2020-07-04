##################################################################
##################################################################
# WEB SCRAPPING com Wordcloud
##################################################################
##################################################################

##################################################################
# Instalação dos pacotes (se for o caso)
##################################################################
#install.packages(c("rvest", "stringr", "tidyverse", "tm", "wordcloud", "SnowballC"))

##################################################################
# Carga de pacotes
##################################################################

# rvest - leitura do conteúdo de páginas web
# stringr - manipulação de strings
# tudyverse - grupo de pacotes para manipulação de dados (tidyr, dplyr, ggplot2...)
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
# Função scrap
# parâmetros:
#   - site: string com o endereço do site a ser baixado
##################################################################
scrap <- function(site){
   # Printa url da página 
   print(paste("Baixando página:", site))
  
   # Downloa da página  Web
   # {rvest}: read_html
   html_source <- read_html(site)
   
   # NOTA: não mais necessário após atualização da página R-Bloggers
   # Filtra todos os títulos 
   # {rvest}: html_nodes e html_attr
   # titles <- html_souce %>% 
   #           html_nodes("article") %>%
   #           html_nodes("a") %>%
   #           html_attr("title")
   
   # Filtra todos os títulos 
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
   
   # NOTA: não mais necessário após atualização da página R-Bloggers
   # Remove todos as ocorrências do prefixo 'Permalink to '
   # {base}: gsub
   # titles <- gsub("Permalink to ", "", titles)
   
   # Retorna vetor com todos os títulos
   return(titles)
}
# Fim função scrap ###############################################

# Define página base
root <- "https://www.r-bloggers.com"

# Cria lista de todas as páginas que serão baixadas
# "https://www.r-bloggers.com"
# "https://www.r-bloggers.com/page/2"
# "https://www.r-bloggers.com/page/..."
all_pages <- c(root, paste0(root,"/page/",2:5))

# obtem lista de todos títulos
# {base}: lapply
# lapply(): aplica a função scrap para cada item do vetor retornando uma lista
all_titles <- lapply(all_pages, scrap)

# Converte lista de títulos em vetor único
all_titles <- unlist(all_titles)

# Formata títulos
# converete para letras minúsculas, remove números, remove lista de palavras não importantes 
# remove pontuação, remove espaços em branco
# tm: removeNumbers, removeWords, removePunctuation, stopwords
# stringr: str_trim
cleaned <- all_titles %>% tolower() %>%  removeNumbers() %>% removeWords(c(stopwords("en"), "data", "r")) %>% removePunctuation() %>% str_trim()

# Converte o vetor de frases em uma lista de documentos estruturados do tipo (tm::Corpus)
# Corpus(): converte para documentos estruturados
# VectorSource(): converte vetor de caracteres em um objeto Source (VectorSource, DataframeSource....)
# tm: Corpus, VectorSource
cleaned_corpus <- Corpus(VectorSource(cleaned))

# Obtem o radical das palavras aplicando a função stemDocument em todas lista
# {tm}: tm_map
# {SnaowballC}: stemDocument
# tm_map(x, f): aplica função f para toda lista x (tm::Corpus)
# stemDocument(): obtem radical de um vertor de textos ou documento struturado do tipo (tm::Corpus)
cleaned_corpus <- tm_map(cleaned_corpus, stemDocument)

# Converte a lista de documentos em um objeto TermDocumentMatrix (TermosxDocumento)
# cria uma matrix onde as linhas são os termos e as colunas os documentos
# {base}: sort, rowSums
# {tm}: TermDocumentMatrix
# TermDocumentMatrix(): Convert lista de documentos em uma matriz de termos x documentos (lista)
doc_object <- TermDocumentMatrix(cleaned_corpus) 


# Converte a lista de documentos em matriz de termos x documentos (matriz)
# {base}: as.matrix
doc_matrix <- as.matrix(doc_object)


# Obtem as linhas ordenados decrescentemente pela maior frequência
# {base}: sort, rowSums
counts <- sort(rowSums(doc_matrix),decreasing=TRUE) 

# Retorna as linhas onde os nomes das colunas do vetor contenha apenas caracteres de a-z
# {base}: grepl
counts <- counts[grepl("^[a-z]+$", names(counts))]

# Cria dataframe com as colunas word e freq (termo e frequência)
# {base}: data.frame
frame_counts <- data.frame(word = names(counts), freq = counts)

# Realiza a plotagem da nuvem de palavras
# wordcloud: wordcloud 
# wordcloud(): plot nuvem
wordcloud(words = frame_counts$word, freq = frame_counts$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.2, colors=brewer.pal(8, "Dark2"))


