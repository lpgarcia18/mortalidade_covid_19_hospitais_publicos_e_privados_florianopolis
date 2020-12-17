# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(lubridate)
library(WeightIt)
library(survey)
library(margins)



# Bases -----------------------------------------------------------------
base <- read_csv("bases/base_notificados_sus_privado.csv")
dados_esc_renda <- read_csv("bases/dados_esc_renda.csv")



# Transformação  ----------------------------------------------------------
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)

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

  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))

  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)

  return(str)
}

dados_esc_renda$territorio <- rm_accent(dados_esc_renda$territorio)
dados_esc_renda$`13anos_esc` <- NULL
dados_esc_renda$`14anos_esc` <- NULL
dados_esc_renda$`15anos_esc` <- NULL
dados_esc_renda$`16anos_esc` <- NULL
dados_esc_renda$`17anos_esc` <- NULL
dados_esc_renda$pop <- NULL


base <- merge(base, dados_esc_renda, by = "territorio", all = T) #fazendo merge das bases e excluindo pacientes que não têm vinculação territorial
base$territorio <- NULL
base <- na.omit(base)
base <- subset(base, base$renda_media != 0) #Retirando CS Canto da Lagoa que está com a renda zerada

write.csv(base, "bases/base_notificados_sus_privado_ajust.csv", row.names = F)
