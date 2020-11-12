# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(Matching)
library(rbounds)
library(lubridate)
library(caret)
library(mlr)


# Funcões -----------------------------------------------------------------
## Ler as tabelas dentro da planilha de excel
read_excel_allsheets <- function(filename, tibble = FALSE) {
	sheets <- readxl::excel_sheets(filename)
	x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
	if(!tibble) x <- lapply(x, as.data.frame)
	names(x) <- sheets
	x
}


#Retirar acentos
rm_accent <- function(str,pattern="all") {
	# Rotinas e funções úteis V 1.0
	# rm.accent - REMOVE ACENTOS DE PALAVRAS
	# Função que tira todos os acentos e pontuações de um vetor de strings.
	# Parâmetros:
	# str - vetor de strings que terão seus acentos retirados.
	# patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
	#            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
	#            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
	#            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
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


## Ajutar títulos das bases
ajustar_nomes <- function(x){
	x%>%
		stringr::str_trim() %>%      #Remove espaços em branco sobrando
		stringr::str_to_lower() %>%  #Converte todas as strings para minusculo
		rm_accent() %>%       #Remove os acentos com a funcao criada acima
		stringr::str_replace_all("[/' '.()]", "_") %>% #Substitui os caracteres especiais por "_"
		stringr::str_replace_all("_+", "_") %>%        #Substitui os caracteres especiais por "_"   
		stringr::str_replace_all("-", "_") %>%        #Substitui os caracteres especiais por "_"   
		stringr::str_replace("_$", "")                 #Substitui o caracter especiais por "_"
}

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

base$territorio <- ajustar_nomes(base$unidade_ref)
base$unidade_ref <- NULL
base$territorio <- as.factor(base$territorio)

base$idade <- as.numeric(base$idade)
base$fl_profsaude <- NULL
base <- na.omit(base)
base$dt_notificacao <- NULL
base$mes_sint <- month(base$dt_inicio_sintomas)
base$mes_sint <- as.factor(base$mes_sint)
base$dt_inicio_sintomas <- NULL
base$center_idade <- scale(base$idade, center = T, scale = T) 
base$idade <- NULL
base <- as.data.frame(base)
base <- na.omit(base)

# Pré-análise -------------------------------------------------------------
base_sem_obito <- base %>% dplyr::select(-obito)

base_sem_obito$not_sus <- as.factor(base_sem_obito$not_sus)


Tr <- base$not_sus %>% as.numeric() %>% cbind()
Y <- base$obito  %>% cbind()

var1 <- base$center_idade


# Generate the task
task <- makeClassifTask(data = base_sem_obito, target = "not_sus")

# Generate the learner
#lrns = listLearners("classif", properties = "prob")
classif_rf <- makeLearner("classif.ranger", predict.type = "prob")
classif_probit <- makeLearner("classif.probit", predict.type = "prob")
classif.h2o_glm <- makeLearner("classif.h2o.glm", predict.type = "prob")
classif_ada <- makeLearner("classif.ada", predict.type = "prob")
classif_gbm <- makeLearner("classif.gbm", predict.type = "prob")
classif_penalized <- makeLearner("classif.penalized", predict.type = "prob")
classif_glmnet <- makeLearner("classif.glmnet", predict.type = "prob")
lrns <- list(classif_rf, classif_probit,classif.h2o_glm, classif_ada, classif_gbm, classif_penalized, classif_glmnet)

#resampling strategy
rdesc <- makeResampleDesc("CV", iters = 5)

# Benchmarking
set.seed(1) 
bmr <- benchmark(lrns, task, rdesc, measures = list(auc,tp, tpr, mmce),keep.pred = T)

pred <- getBMRPredictions(bmr)


# Average treatment on the treated effect
rr1 <- Match(Y = Y, Tr = Tr, X = pred$base_sem_obito$classif.ranger$data$prob.1, estimand = "ATT", M = 1, ties = TRUE, replace = TRUE)
summary(rr1)

rr2 <- Match(Y = Y, Tr = Tr, X = pred$base_sem_obito$classif.h2o.glm$data$prob.1, estimand = "ATT", M = 1, ties = TRUE, replace = TRUE)
summary(rr2)


# Checking the balancing property
MatchBalance(Tr ~ territorio +
	     	sint_dor_garganta +
	     	sint_dispneia +
	     	sint_febre +
	     	sint_tosse +
	     	sexo +
	     	raca +
	     	comorb_resp_cronica +
	     	comorb_card_cronica +
	     	comorb_dm +
	     	comorb_drc +
	     	comorb_imunossupressao +
	     	comorb_dca_cromossomica +
	     	mes_sint +
	     	center_idade, match.out = rr1, nboots=0, data=base)

qqplot(var1[rr1$index.control], var1[rr1$index.treated])
abline(coef = c(0, 1), col = 2)


MatchBalance(Tr ~ territorio +
	     	sint_dor_garganta +
	     	sint_dispneia +
	     	sint_febre +
	     	sint_tosse +
	     	sexo +
	     	raca +
	     	comorb_resp_cronica +
	     	comorb_card_cronica +
	     	comorb_dm +
	     	comorb_drc +
	     	comorb_imunossupressao +
	     	comorb_dca_cromossomica +
	     	mes_sint +
	     	center_idade, match.out = rr2, nboots=0, data=base)

qqplot(var1[rr2$index.control], var1[rr2$index.treated])
abline(coef = c(0, 1), col = 2)



# Propensity score model - glm
base_sem_obito$not_sus <- as.numeric(as.character(base_sem_obito$not_sus))
glm1 <- glm(not_sus ~ ., family = binomial, data = base_sem_obito )
glm1 %>% summary()

# Average treatment on the treated effect
rr3 <- Match(Y = Y, Tr = Tr, X = glm1$fitted, estimand = "ATT", M = 1, ties = TRUE, replace = TRUE)
summary(rr3)
rr3_e <- Match(Y = Y, Tr = Tr, X = glm1$fitted, estimand = "ATE", M = 1, ties = TRUE, replace = TRUE)
summary(rr3_e)


# Checking the balancing property
MatchBalance(Tr ~ territorio +
	     	sint_dor_garganta +
	     	sint_dispneia +
	     	sint_febre +
	     	sint_tosse +
	     	sexo +
	     	raca +
	     	comorb_resp_cronica +
	     	comorb_card_cronica +
	     	comorb_dm +
	     	comorb_drc +
	     	comorb_imunossupressao +
	     	comorb_dca_cromossomica +
	     	mes_sint +
	     	center_idade, match.out = rr3, nboots=0, data=base_sem_obito)
qqplot(var1[rr3$index.control], var1[rr3$index.treated])
abline(coef = c(0, 1), col = 2)


# Sensitivity tests
psens(rr1, Gamma=1.7, GammaInc=.05)
hlsens(rr1, Gamma=1.7, GammaInc=.05, .1)

psens(rr2, Gamma=1.7, GammaInc=.05)
hlsens(rr2, Gamma=1.7, GammaInc=.05, .1)

psens(rr3, Gamma=1.7, GammaInc=.05)
hlsens(rr3, Gamma=1.7, GammaInc=.05, .1)