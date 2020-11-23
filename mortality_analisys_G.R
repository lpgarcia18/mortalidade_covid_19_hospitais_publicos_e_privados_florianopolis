# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(lubridate)
library(MatchIt)
#devtools::install_github('IQSS/Zelig')
library(Zelig)
library(margins)
library(cobalt)


# Bases -----------------------------------------------------------------
base <- read_csv("bases/base_notificados_sus_privado.csv", 
		 col_types = cols(
		 	notif_sus = col_factor(levels = c("0", "1")),
		 	sint_dor_garganta = col_factor(levels = c("NAO", "SIM")),
		 	sint_dispneia = col_factor(levels = c("NAO", "SIM")),
		 	sint_febre = col_factor(levels = c("NAO", "SIM")),
		 	sint_tosse = col_factor(levels = c("NAO", "SIM")),
		 	sexo = col_factor(levels = c("F", "M")),
		 	raca = col_factor(levels = c("Amarela", "Branca", "Indígena","Parda","Preta")),
		 	comorb_resp_cronica = col_factor(levels = c("NAO", "SIM")),
		 	comorb_card_cronica = col_factor(levels = c("NAO", "SIM")),
		 	comorb_dm = col_factor(levels = c("NAO", "SIM")),
		 	comorb_drc = col_factor(levels = c("NAO", "SIM")),
		 	comorb_imunossupressao = col_factor(levels = c("NAO", "SIM")),
		 	comorb_gesta_altorisco = col_factor(levels = c("NAO", "SIM")),
		 	comorb_dca_cromossomica = col_factor(levels = c("NAO", "SIM")),
		 	obito = col_factor(levels = c("0", "1"))))

base$territorio <- as.factor(base$territorio)

base <- na.omit(base)	
base <- subset(base, base$raca != "Indígena")
base$raca <- factor(base$raca, levels = c("Amarela", "Branca", "Parda", "Preta"))
base$mes_sint <- as.factor(base$mes_sint)

base %>%
	group_by(notif_sus) %>% summarise(idade = mean(idade),
					  obito = mean(obito ==1))

base$idade2 <- base$idade^2


# Carrega balanceamento feito pelo MatchIt genetic
load("BalIdade2.Rda")

bal.tab(m.out, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std") # Veja que idade^2 ficou um pouco desbalanceada, mas abaixo de 0.10

# Ajusta logit incluindo idade e idade^2
z.out <- zelig(obito ~
			notif_sus +
			territorio +
			sint_dor_garganta +
			sint_dispneia +
			sint_febre +
			sint_tosse +
			sexo +
			raca +
			comorb_resp_cronica +
			comorb_card_cronica +
			comorb_dm +
			comorb_imunossupressao +
			idade+I(idade^2),data = match.data(m.out), model = "logit")

summary(z.out) # Veja que diversas variáveis que já haviam sido incluídas no pareamento agora nao sao mais significativas.


# Ajusta logit apenas com tratamento e covariáveis significativas
z.out1 <- zelig(obito ~
	       	notif_sus +
	       	comorb_imunossupressao +
	       	idade+I(idade^2),data = match.data(m.out), model = "logit")

summary(z.out1) # note como o AIC caiu bastante, indicando que o menor modelo é preferível

# Compara as probabilidades de óbito no SUS com as da rede privada usando simulacoes

# Primeiro simula o SUS
x.out1 <- setx(z.out1, notif_sus = 1) 
s.out1 <- sim(z.out1, x = x.out1)  # Simula probabilidade de óbito da posteriori mantendo notif_sus sempre igual a TRUE
summary(s.out1)
plot(s.out1) 

# Agora simula na rede privada
x.out2 <- setx(z.out1, notif_sus = 0) 
s.out2 <- sim(z.out1, x = x.out2)  # Simula probabilidade de óbito da posteriori mantendo notif_sus sempre igual a FALSE
summary(s.out2)
plot(s.out2) 

# Comparando as duas simulacoes
s.out2 <- sim(z.out1, x = x.out1, x1 = x.out2)
summary(s.out2) 
plot(s.out2) # Note como as distribuicao das probabilidades de óbito estimadas para SUS e rede privada sao muito similares, apesar de que a diferenca (nao significativa) favorece a rede privada.
