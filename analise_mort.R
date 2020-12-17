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
library(arsenal)


# Bases -----------------------------------------------------------------
base <- read_csv("bases/base_notificados_sus_privado_ajust.csv", 
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


base$mes_sint <- as.factor(base$mes_sint)

base %>%
	group_by(notif_sus) %>% summarise(idade = mean(idade),
					  obito = mean(obito ==1))

#Construindo o indicador idade ^ 2
base$idade2 <- base$idade^2


#Análise descritiva
base$notif_sus <- as.character(base$notif_sus)
tabela_um <- tableby(notif_sus ~ ., data = base)
tab1 <- summary(tabela_um, title = "Tabela 1") %>% as.data.frame()
write.csv(tab1, "bases/tab_desc_tot.csv")
base$notif_sus <- as.factor(base$notif_sus)

# Carrega balanceamento feito pelo MatchIt genetic
m.out <- matchit(as.numeric(as.character(notif_sus))  ~
			renda_media +
		 	perc_mais_12anos_esc +
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
			idade+I(idade^2), data = base, method = "genetic")


tab_match <- bal.tab(m.out, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std") 

# Ajusta logit, incluindo idade e idade^2
z.out <- zelig(obito ~
			notif_sus +
			renda_media +
		 	perc_mais_12anos_esc +
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

summary(z.out) # Diversas variáveis que já haviam sido incluídas no pareamento agora não são mais significativas.


# Ajusta logit apenas com tratamento e covariáveis significativas
z.out1 <- zelig(obito ~
	       	notif_sus +
	       	comorb_imunossupressao +
	       	idade+I(idade^2),data = match.data(m.out), model = "logit")

summary(z.out1) # O AIC caiu bastante, indicando que o menor modelo é preferível

# Compara as probabilidades de óbito no SUS com as da rede privada usando simulações

# Primeiro simula o SUS
x.out1 <- setx(z.out1, notif_sus = 1) 
s.out1 <- Zelig::sim(z.out1, x = x.out1)  # Simula probabilidade de óbito da posteriori mantendo notif_sus sempre igual a TRUE
summary(s.out1)
plot(s.out1) 

# Agora simula na rede privada
x.out2 <- setx(z.out1, notif_sus = 0) 
s.out2 <- Zelig::sim(z.out1, x = x.out2)  # Simula probabilidade de óbito da posteriori mantendo notif_sus sempre igual a FALSE
summary(s.out2)
plot(s.out2) 

# Comparando as duas simulacoes
s.out3 <- Zelig::sim(z.out1, x = x.out1, x1 = x.out2)
summary(s.out3) 
plot(s.out3) # As distribuicao das probabilidades de óbito estimadas para SUS e rede privada são muito similares, a diferenca não é significativa

