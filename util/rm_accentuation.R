rm_accentuation <- function(str,pattern="all") {
  # Remove todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  #   str - vetor de strings que terão seus acentos retirados.
  #   patterns - vetor de strings acentos que serão retirados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Palavras aceitas: "all" - retira todos os acentos, mais "´", "`", "^", "~", "¨", "ç".
  
  # Verifica se o parâmetro str é não uma string e converte o valor para o tipo string
  if(!is.character(str)){}
      str <- as.character(str)
  }
  
  # Remove os acentos duplicados 
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  # símbolos acentuados
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  # símbolos sem acentuação
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  # Define tipos de acentos que poderão ser removidos
  accentTypes <- c("´","`","^","~","¨","ç")
  
  # Verifica se tem a ocorrência do pattern "all" e substitui todos os caracteres acentuados
  if(any(c("all","todos")%in%pattern)) 
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  # Substitui todos elementos acentuados com os símbolos contidos no vetor pattern
  for(i in which(accentTypes%in%pattern)){
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  # Retorna str com acentuação removida
  return(str)
}
