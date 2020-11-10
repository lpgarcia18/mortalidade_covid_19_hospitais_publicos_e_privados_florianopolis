# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(MatchIt)


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
		 		 raca = col_factor(levels = c("Amarela", "Branca", "Indígena","Parda","Preta","NA")),
		 		 comorb_resp_cronica = col_factor(levels = c("NAO", "SIM")), 
		 		 comorb_card_cronica = col_factor(levels = c("NAO", "SIM")), 
		 		 comorb_dm = col_factor(levels = c("NAO", "SIM")), 
		 		 comorb_drc = col_factor(levels = c("NAO", "SIM")), 
		 		 comorb_imunossupressao = col_factor(levels = c("NAO", "SIM")), 
		 		 comorb_gesta_altorisco = col_factor(levels = c("NAO", "SIM")), 
		 		 comorb_dca_cromossomica = col_factor(levels = c("NAO", "SIM")), 
		 		 data_obito = col_date(format = "%Y-%m-%d"), 
		 		 not_sus = col_factor(levels = c("0", "1"))))


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


## Dados demográficos
populacao <- read_excel_allsheets("bases/demografia/Estima_Pop_Genero_2000_a_2023/Estima_Pop_Genero_2000_a_2023_ULS.xlsx")
populacao <- map(populacao,`[`,"44075")#extraindo coluna de 2020
titulos_pop <- c("populacao", "homens", "mulheres")
populacao <- do.call(cbind,populacao)
names(populacao) <- titulos_pop

escolaridade <- read_excel_allsheets("bases/demografia/Estima_Escolaridade_2000_a_2023/Estima_Escolaridade_Pessoas_2000_2030_Unidade_Saude.xlsx")
escolaridade <- map(escolaridade,`[`,"44075")#extraindo coluna de 2020
titulos_esc <- names(escolaridade)
titulos_esc <- paste("escolaridade", titulos_esc)
titulos_esc <- ajustar_nomes(titulos_esc)
escolaridade <- do.call(cbind,escolaridade)
names(escolaridade) <- titulos_esc

cor_pele <- read_excel_allsheets("bases/demografia/Estima_Raça_Cor_2000_a_2023/Eatima_Raca_Cor_2000_a_2023_ULS.xlsx")
cor_pele <- map(cor_pele,`[`,"44075")#extraindo coluna de 2020
titulos_cp <- names(cor_pele)
titulos_cp <- sub(x = titulos_cp,pattern = "-",replacement = " ")
titulos_cp <- sub(x = titulos_cp,pattern = " ",replacement = "")
titulos_cp <- sub(x = titulos_cp,pattern = " ",replacement = "")
titulos_cp <-  paste("cor_pele", titulos_cp)
titulos_cp <- ajustar_nomes(titulos_cp)
cor_pele <- do.call(cbind,cor_pele)
names(cor_pele) <- titulos_cp
cor_pele <- cor_pele[-54,] #retirando o total

renda <- read_excel_allsheets("bases/demografia/Estima_Renda_2000_a_2023/Estima_Renda_ULS.xlsx")
renda <- map(renda,`[`,"44075")#extraindo coluna de 2020
titulos_renda <- names(renda)
titulos_renda <- ajustar_nomes(titulos_renda)
renda <- do.call(cbind,renda)
names(renda) <- titulos_renda
renda <- renda[-54,] #retirando o total

demografia <- cbind(populacao,escolaridade, cor_pele, renda)
demografia <- demografia[-c(1:4),]#retirando linhas sem informação

idade <- read_excel_allsheets("bases/demografia/Estima_Idade_2000_a_2023/Estima_Idade_0_a_100_ou_mais_de_2000_a_2030_ULS.xlsx")
nome_cs <- map(idade,`[`,5)#extraindo o nome dos cs
nome_cs <- do.call(cbind,nome_cs)
nome_cs <- names(nome_cs)
nome_cs <- ajustar_nomes(nome_cs)
nome_fe <- map(idade,`[`,4)#extraindo o nome das faixas estárias
nome_fe <- unlist(nome_fe[1]) %>% as.data.frame()
nome_fe <- nome_fe[-c(1:3),]
nome_fe <- as.character(nome_fe)
nome_fe <- paste0("idade_", nome_fe)
nome_fe[which(is.na(nome_fe))] <- "Total"
nome_fe <- ajustar_nomes(nome_fe)
idade <- map(idade,`[`,"...25")#extraindo coluna de 2020
idade <- do.call(cbind,idade)
names(idade) <- nome_cs
idade <- idade[-c(1:3),]
idade <- idade %>% t() %>% as.data.frame()
names(idade) <- nome_fe
idade$total <- NULL

demografia <- cbind(idade, demografia)
demografia$territorio <- row.names(demografia)
demografia[,-140] <- lapply(demografia[,-140], as.numeric) %>% as.data.frame()




# Merge das bases ---------------------------------------------------------

base <- merge(base, demografia, by = "territorio", all.x = T)

base$territorio <- as.factor(base$territorio)

base$idade <- as.numeric(base$idade)
base$fl_profsaude <- NULL
base <- na.omit(base)

# Pré-análise -------------------------------------------------------------

base %>%
	group_by(not_sus) %>%
	summarise(obito = n())

base <- base %>% dplyr::select(obito,
				not_sus,
			        territorio,
			       	sint_dor_garganta,
			       	sint_dispneia,
			       	sint_febre,
			       	sint_tosse,
			       	sexo,
			       	raca,
			       	comorb_resp_cronica,
			       	comorb_card_cronica,
			       	comorb_dm,
			       	comorb_drc,
			       	comorb_imunossupressao,
			       	comorb_dca_cromossomica,
			       	idade)

treat <- base %>% dplyr::select(not_sus,
			       territorio,
			       sint_dor_garganta,
			       sint_dispneia,
			       sint_febre,
			       sint_tosse,
			       sexo,
			       raca,
			       comorb_resp_cronica,
			       comorb_card_cronica,
			       comorb_dm,
			       comorb_drc,
			       comorb_imunossupressao,
			       comorb_dca_cromossomica,
			       idade)



Tr <- base$not_sus %>% as.character()
Y <- base$obito
X <- base %>% dplyr::select(territorio,
				     sint_dor_garganta,
				     sint_dispneia,
				     sint_febre,
				     sint_tosse,
				     sexo,
				     raca,
				     comorb_resp_cronica,
				     comorb_card_cronica,
				     comorb_dm,
				     comorb_drc,
				     comorb_imunossupressao,
				     comorb_dca_cromossomica,
				     idade)

glm1 <- glm(not_sus ~ ., family = binomial, data = treat )
glm1 %>% summary()


result_treat <- Matching::Match(Y = Y, Tr = Tr, X = glm1$fitted, estimand = "ATT", M = 1, ties = TRUE, replace = T)
summary(result_treat)


Matching::MatchBalance(Tr ~ territorio +
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
		       	idade, data = treat)

# Maching -----------------------------------------------------------------
matched_origem_not <- matchit(not_sus ~ territorio +
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
			 	idade + 
			 	idade_0_anos +
			 	idade_1_anos +
			 	idade_2_anos +
			 	idade_3_anos +
			 	idade_4_anos +
			 	idade_5_anos +
			 	idade_6_anos +
			 	idade_7_anos +
			 	idade_8_anos +
			 	idade_9_anos +
			 	idade_10_anos +
			 	idade_11_anos +
			 	idade_12_anos +
			 	idade_13_anos +
			 	idade_14_anos +
			 	idade_15_anos +
			 	idade_16_anos +
			 	idade_17_anos +
			 	idade_18_anos +
			 	idade_19_anos +
			 	idade_20_anos +
			 	idade_21_anos +
			 	idade_22_anos +
			 	idade_23_anos +
			 	idade_24_anos +
			 	idade_25_anos +
			 	idade_26_anos +
			 	idade_27_anos +
			 	idade_28_anos +
			 	idade_29_anos +
			 	idade_30_anos +
			 	idade_31_anos +
			 	idade_32_anos +
			 	idade_33_anos +
			 	idade_34_anos +
			 	idade_35_anos +
			 	idade_36_anos +
			 	idade_37_anos +
			 	idade_38_anos +
			 	idade_39_anos +
			 	idade_40_anos +
			 	idade_41_anos +
			 	idade_42_anos +
			 	idade_43_anos +
			 	idade_44_anos +
			 	idade_45_anos +
			 	idade_46_anos +
			 	idade_47_anos +
			 	idade_48_anos +
			 	idade_49_anos +
			 	idade_50_anos +
			 	idade_51_anos +
			 	idade_52_anos +
			 	idade_53_anos +
			 	idade_54_anos +
			 	idade_55_anos +
			 	idade_56_anos +
			 	idade_57_anos +
			 	idade_58_anos +
			 	idade_59_anos +
			 	idade_60_anos +
			 	idade_61_anos +
			 	idade_62_anos +
			 	idade_63_anos +
			 	idade_64_anos +
			 	idade_65_anos +
			 	idade_66_anos +
			 	idade_67_anos +
			 	idade_68_anos +
			 	idade_69_anos +
			 	idade_70_anos +
			 	idade_71_anos +
			 	idade_72_anos +
			 	idade_73_anos +
			 	idade_74_anos +
			 	idade_75_anos +
			 	idade_76_anos +
			 	idade_77_anos +
			 	idade_78_anos +
			 	idade_79_anos +
			 	idade_80_anos +
			 	idade_81_anos +
			 	idade_82_anos +
			 	idade_83_anos +
			 	idade_84_anos +
			 	idade_85_anos +
			 	idade_86_anos +
			 	idade_87_anos +
			 	idade_88_anos +
			 	idade_89_anos +
			 	idade_90_anos +
			 	idade_91_anos +
			 	idade_92_anos +
			 	idade_93_anos +
			 	idade_94_anos +
			 	idade_95_anos +
			 	idade_96_anos +
			 	idade_97_anos +
			 	idade_98_anos +
			 	idade_99_anos +
			 	idade_100_anos_ou_mais +
			 	idade_na +
			 	populacao +
			 	homens +
			 	mulheres +
			 	escolaridade_alfabetizados +
			 	escolaridade_nao_alfabetizados +
			 	escolaridade_s_instr_ou__1_ano +
			 	escolaridade_01_ano +
			 	escolaridade_02_anos +
			 	escolaridade_03_anos +
			 	escolaridade_04_anos +
			 	escolaridade_05_anos +
			 	escolaridade_06_anos +
			 	escolaridade_07_anos +
			 	escolaridade_08_anos +
			 	escolaridade_09_anos +
			 	escolaridade_10_anos +
			 	escolaridade_11_anos +
			 	escolaridade_12_anos +
			 	escolaridade_13_anos +
			 	escolaridade_14_anos +
			 	escolaridade_15_anos +
			 	escolaridade_16_anos +
			 	escolaridade_17_anos_ou_mais +
			 	escolaridade_nao_determinado +
			 	escolaridade_alfabetizacao_de_adultos +
			 	cor_pele_1branca +
			 	cor_pele_2preta +
			 	cor_pele_3amarela +
			 	cor_pele_4parda +
			 	cor_pele_5indigena +
			 	cor_pele_9ignorado +
			 	renda_tot_domic +
			 	renda_med_por_domic +
			 	renda_tot_resp +
			 	renda_med_resp +
			 	renda_tot_pess +
			 	renda_med_pess,
			 	data = base)




origem_not_match <- match.data(matched_origem_not) 


summary(origem_not_match)

origem_not_match$distance <- NULL
origem_not_match$weights <- NULL

mod1 <- glm(obito ~ .,
	    family = binomial(), data = origem_not_match)
mod1 %>% summary()
mod1 %>% plot()

with()