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
library(WeightIt)
library(cobalt)



# Bases -----------------------------------------------------------------
#Casos
base <- read_csv("bases/base_pseudoanonimizada_sus-vs-privado.csv",
		 col_types = cols(fl_profsaude = col_factor(levels = c("NA", "0", "1")),
		 		 dt_nascimento = col_date(format = "%Y-%m-%d"),
		 		 dt_notificacao = col_date(format = "%Y-%m-%d"),
		 		 dt_inicio_sintomas = col_date(format = "%Y-%m-%d"),
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
		 		 data_obito = col_date(format = "%Y-%m-%d")))


base$obito <- ifelse(is.na(base$data_obito), 0, 1) %>% as.factor()
# base <- subset(base, base$obito == 1)
# base$obito <- NULL
base$data_obito <- NULL


base$cod_notificacao <- NULL
base$idade <- (base$dt_inicio_sintomas - base$dt_nascimento)/365
base$dt_nascimento <- NULL
base$sint_outros <- NULL

base %>%
	group_by(not_sus) %>% summarise(idade = mean(idade),
					obito = sum(obito ==1))

base$territorio <-base$unidade_ref
base$unidade_ref <- NULL
base$territorio <- as.factor(base$territorio)

base$idade <- as.numeric(base$idade)
base$fl_profsaude <- NULL
base <- na.omit(base)
base$dt_notificacao <- NULL
base$mes_sint <- month(base$dt_inicio_sintomas)
base$mes_sint <- as.factor(base$mes_sint)
base$dt_inicio_sintomas <- NULL
base <- as.data.frame(base)
base <- na.omit(base)
base$not_sus <- as.factor(base$not_sus)
base$obito <- as.factor(base$obito)

# Match -------------------------------------------------------------------
# Análise inicial do balanço das variáveis
SuperLearner::listWrappers()
W.out <- weightit(not_sus ~ 
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
		mes_sint +
		idade, data= base, method = "super", 
		SL.library = c("SL.lda"), estimand = "ATT")





summary(W.out)
bal.tab(W.out, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")


# ATT ---------------------------------------------------------------------
library("survey")
#W.out$weights <- rep(1,(sum(!is.na(W.out$weights))))
d.w <- svydesign(ids = ~1, weights = W.out$weights,
                     data = base)

fit <- svyglm(obito ~ 
	      	not_sus +
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
		mes_sint +
		idade, design = d.w, family= binomial())
summary(fit)

# Efeito parcial
library("margins")
PE <- margins(fit,design = d.w, type = "response")
summary(PE)
plot(PE)
