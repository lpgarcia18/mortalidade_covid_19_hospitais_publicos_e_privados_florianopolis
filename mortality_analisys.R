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
library(readxl)
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
					obito = sum(obito ==1))

base$idade <- base$idade^2


# Match -------------------------------------------------------------------
# Balanceamento das variáveis
SuperLearner::listWrappers()
W.out <- weightit(notif_sus ~ 
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
		idade, data= base, method = "super", 
		SL.library = c("SL.bayesglm",
			       "SL.gbm",
			       "SL.glm",
			       "SL.knn",
			       "SL.lda",
			       "SL.nnet",
			       "SL.nnls",
			       "SL.ranger",
			       "SL.xgboost"
			       ), 
		estimand = "ATT",stabilize = T)


summary(W.out)
bal.tab(W.out, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")


# Efeitos parciais ---------------------------------------------------------------------
# Fitando com os pesos
d.w <- svydesign(ids = ~1, weights = W.out$weights,
                     data = base)

fit <- svyglm(obito ~ 
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
		idade, design = d.w, family= binomial())
summary(fit)


# Efeito parcial
PE <- margins(fit,design = d.w, type = "response")
summary(PE)
plot(PE)


