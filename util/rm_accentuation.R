rm_accent <- function(str,pattern="all") {
  # rm_accent - REMOVE ACENTOS DE PALAVRAS
  # Fun��o que tira todos os acentos e pontua��es de um vetor de strings.
  # Par�metros:
  # str - vetor de strings que ter�o seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos dever�o ser retirados.
  #            Para indicar quais acentos dever�o ser retirados, um vetor com os s�mbolos dever�o ser passados.
  #            Exemplo: pattern = c("�", "^") retirar� os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que s�o "�", "`", "^", "~", "�", "�")
  
  # Verifica se o par�metro str � n�o uma string e converte o valor para o tipo string
  if(!is.character(str)){}
      str <- as.character(str)
  }
  
  # Remove os acentos duplicados 
  pattern <- unique(pattern)
  
  if(any(pattern=="�"))
    pattern[pattern=="�"] <- "�"
  
  symbols <- c(
    acute = "������������",
    grave = "����������",
    circunflex = "����������",
    tilde = "������",
    umlaut = "�����������",
    cedil = "��"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("�","`","^","~","�","�")
  
  # Verifica se tem a ocorr�ncia de "all" ou "todos" entre os padr�es e substitui todos os caracteres acentuados
  if(any(c("all","todos")%in%pattern)) 
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  # Substitui todos elementos acentuados com os s�mbolos contidos no vetor de padr�es
  for(i in which(accentTypes%in%pattern)){
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}
