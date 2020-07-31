rm_accent <- function(str,pattern="all") {
  # rm_accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  
  # Verifica se o parâmetro str é não uma string e converte o valor para o tipo string
  if(!is.character(str)){}
      str <- as.character(str)
  }
  
  # Remove os acentos duplicados 
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  # Verifica se tem a ocorrência de "all" ou "todos" entre os padrões e substitui todos os caracteres acentuados
  if(any(c("all","todos")%in%pattern)) 
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  # Substitui todos elementos acentuados com os símbolos contidos no vetor de padrões
  for(i in which(accentTypes%in%pattern)){
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}
