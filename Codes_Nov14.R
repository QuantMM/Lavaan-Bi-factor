##-------------------------
## Bifactor using lavaan
##------------------------- 2019-Oct-04

# setwd("-----------")
m.data <- read.csv("---------.csv", header = T, sep = ",", na.strings=c(""))

library(lavaan)
library(semTools)
library(mice)

###########################------------------------
##
## CFA Analysis
##
###########################------------------------

##-------------------------
## M-factor model:
##-------------------------
M.model <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
"

##Run cfa for M-factor
M.fit <- lavaan::cfa(M.model, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "FIML",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0 -> All factors (general and specific) must be orthogonal to each other
					verbose = T
					)
lavInspect(M.fit, what ='fit')
summary(M.fit, standardized = T, fit.measures = T)
#Extract factor scores: m_scores <- predict(M.fit)
inspect(M.fit, 'patterns') 


##-------------------------
## Internalizing Psycho: Model 25
##-------------------------
Inter <- "
	GIF =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	DAD =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
"

Inter.fit.na <- lavaan::cfa(Inter, data = m.data,
					estimator = "MLR",
					missing = "FIML",
					std.ov = TRUE,
					std.lv = TRUE,
					orthogonal = T,
					verbose = T
					)
lavInspect(Inter.fit.na, what ='fit')
summary(Inter.fit.na, standardized = T, fit.measures = T)

##-------------------------
## Internalizing Psycho: Model 26
##-------------------------
Inter2 <- "
	GIF =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
"

Inter.fit2 <- lavaan::cfa(Inter2, data = m.data,
					estimator = "MLR", missing = "ml",
					std.ov = TRUE, std.lv = TRUE, orthogonal = T, verbose = T)
summary(Inter.fit2, standardized = T, fit.measures = T)

###########################------------------------
##
## SEM Analysis 1
##
###########################------------------------

##-------------------------
## M-factor model + Internalizing Psycho:
##-------------------------

fit25 <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF25 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	DAD =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
	
	GIF25 ~ G + AX
"
Model1 <- lavaan::sem(fit25, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model1, standardized = T, fit.measures = T)
standardizedsolution(Model1)


fit26 <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF26 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd

	GIF26 ~ G + AX
"

Model2 <- lavaan::sem(fit26, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model2, standardized = T, fit.measures = T)
standardizedsolution(Model2)

###########################------------------------
##
## SEM Analysis 2: Interactions and/or mediator
##
###########################------------------------

##--------------
## Interaction (1) SDQ
##--------------
m.data$prod1	=	m.data$Pren_CESD1 * m.data$ZqPRS_0.0001
m.data$prod2	=	m.data$Pren_CESD2 * m.data$ZqPRS_0.0001
m.data$prod3	=	m.data$Pren_CESD5 * m.data$ZqPRS_0.0001
m.data$prod4	=	m.data$Pren_CESD7 * m.data$ZqPRS_0.0001
m.data$prod5	=	m.data$Pren_CESD11 * m.data$ZqPRS_0.0001
m.data$prod6	=	m.data$Pren_CESD20 * m.data$ZqPRS_0.0001
m.data$prod7	=	m.data$Pren_CESD3 * m.data$ZqPRS_0.0001
m.data$prod8	=	m.data$Pren_CESD6 * m.data$ZqPRS_0.0001
m.data$prod9	=	m.data$Pren_CESD14 * m.data$ZqPRS_0.0001
m.data$prod10	=	m.data$Pren_CESD18 * m.data$ZqPRS_0.0001
m.data$prod11	=	m.data$Pren_CESD4_rec * m.data$ZqPRS_0.0001
m.data$prod12	=	m.data$Pren_CESD8_rec * m.data$ZqPRS_0.0001
m.data$prod13	=	m.data$Pren_CESD12_rec * m.data$ZqPRS_0.0001
m.data$prod14	=	m.data$Pren_CESD16_rec * m.data$ZqPRS_0.0001
m.data$prod15	=	m.data$Pren_pregn_anx_q1 * m.data$ZqPRS_0.0001
m.data$prod16	=	m.data$Pren_pregn_anx_q2 * m.data$ZqPRS_0.0001
m.data$prod17	=	m.data$Pren_pregn_anx_q3 * m.data$ZqPRS_0.0001
m.data$prod18	=	m.data$Pren_pregn_anx_q4 * m.data$ZqPRS_0.0001

fit25_interaction <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF25 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	DAD =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
	
	INT_G_SDQ =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_SDQ =~ prod15+prod16+prod17+prod18
	
	GIF25 ~ G + AX + INT_G_SDQ + INT_AX_SDQ
"
Model3 <- lavaan::sem(fit25_interaction, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model3, standardized = T, fit.measures = T)
standardizedsolution(Model3)

fit26_interaction <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF26 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd

	INT_G_SDQ =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_SDQ =~ prod15+prod16+prod17+prod18
	
	GIF26 ~ G + AX + INT_G_SDQ + INT_AX_SDQ
"

Model4 <- lavaan::sem(fit26_interaction, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model4, standardized = T, fit.measures = T)
standardizedsolution(Model4)


##--------------
## Interaction (1) SDQ + mediator
##--------------
# summary(m.data$NE_18m); hist(m.data$NE_18m)
# summary(m.data$NE_36m); hist(m.data$NE_36m)

fit25_int_medi <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF25 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	DAD =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
	
	INT_G_SDQ =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_SDQ =~ prod15+prod16+prod17+prod18
	
	# direct effect
	GIF25 ~ a*G + b*AX + c*INT_G_SDQ + d*INT_AX_SDQ
	
    # mediator
	NE_36m ~ e*G + f*AX + g*INT_G_SDQ + h*INT_AX_SDQ
	GIF25 ~ i*NE_36m
	
	# indirect effect (a*b)
	ei := e*i
	fi := f*i
	gi := g*i
	hi := h*i
	
	# total effect
	total1 := a + (e*i)
	total2 := b + (f*i)
	total3 := c + (g*i)
	total4 := d + (h*i)
"
Model3_medi <- lavaan::sem(fit25_int_medi, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model3_medi, standardized = T, fit.measures = T)
standardizedsolution(Model3_medi)


Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
  GIF25 ~                                                               
    G          (a)    0.232    0.086    2.688    0.007    0.192    0.192
    AX         (b)    0.089    0.078    1.139    0.255    0.073    0.073
    INT_G_SDQ  (c)    0.216    0.094    2.291    0.022    0.179    0.179
    INT_AX_SDQ (d)    0.014    0.107    0.128    0.898    0.011    0.011
  NE_36m ~                                                              
    G          (e)    0.315    0.061    5.169    0.000    0.315    0.313
    AX         (f)    0.151    0.077    1.948    0.051    0.151    0.150
    INT_G_SDQ  (g)   -0.032    0.074   -0.432    0.666   -0.032   -0.032
    INT_AX_SDQ (h)    0.081    0.081    0.998    0.318    0.081    0.080
  GIF25 ~                                                               
    NE_36m     (i)    0.510    0.084    6.055    0.000    0.423    0.425

Defined Parameters:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ei                0.161    0.043    3.726    0.000    0.133    0.133
    fi                0.077    0.042    1.846    0.065    0.064    0.064
    gi               -0.016    0.038   -0.430    0.667   -0.013   -0.013
    hi                0.041    0.041    1.000    0.317    0.034    0.034
    total1            0.392    0.089    4.407    0.000    0.325    0.325
    total2            0.165    0.089    1.863    0.062    0.137    0.137
    total3            0.200    0.106    1.884    0.060    0.166    0.166
    total4            0.055    0.109    0.501    0.616    0.045    0.045


fit26_int_medi <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF26 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd

	INT_G_SDQ =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_SDQ =~ prod15+prod16+prod17+prod18
	
	# direct effect
	GIF26 ~ a*G + b*AX + c*INT_G_SDQ + d*INT_AX_SDQ
	
    # mediator
	NE_36m ~ e*G + f*AX + g*INT_G_SDQ + h*INT_AX_SDQ
	GIF26 ~ i*NE_36m
	
	# indirect effect (a*b)
	ei := e*i
	fi := f*i
	gi := g*i
	hi := h*i
	
	# total effect
	total1 := a + (e*i)
	total2 := b + (f*i)
	total3 := c + (g*i)
	total4 := d + (h*i)
"
Model4_medi <- lavaan::sem(fit26_int_medi, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model4_medi, standardized = T, fit.measures = T)
standardizedsolution(Model4_medi)


##--------------
## Interaction (2) MDD
##--------------
m.data$prod1	=	m.data$Pren_CESD1 * m.data$ZPRS_0.4
m.data$prod2	=	m.data$Pren_CESD2 * m.data$ZPRS_0.4
m.data$prod3	=	m.data$Pren_CESD5 * m.data$ZPRS_0.4
m.data$prod4	=	m.data$Pren_CESD7 * m.data$ZPRS_0.4
m.data$prod5	=	m.data$Pren_CESD11 * m.data$ZPRS_0.4
m.data$prod6	=	m.data$Pren_CESD20 * m.data$ZPRS_0.4
m.data$prod7	=	m.data$Pren_CESD3 * m.data$ZPRS_0.4
m.data$prod8	=	m.data$Pren_CESD6 * m.data$ZPRS_0.4
m.data$prod9	=	m.data$Pren_CESD14 * m.data$ZPRS_0.4
m.data$prod10	=	m.data$Pren_CESD18 * m.data$ZPRS_0.4
m.data$prod11	=	m.data$Pren_CESD4_rec * m.data$ZPRS_0.4
m.data$prod12	=	m.data$Pren_CESD8_rec * m.data$ZPRS_0.4
m.data$prod13	=	m.data$Pren_CESD12_rec * m.data$ZPRS_0.4
m.data$prod14	=	m.data$Pren_CESD16_rec * m.data$ZPRS_0.4
m.data$prod15	=	m.data$Pren_pregn_anx_q1 * m.data$ZPRS_0.4
m.data$prod16	=	m.data$Pren_pregn_anx_q2 * m.data$ZPRS_0.4
m.data$prod17	=	m.data$Pren_pregn_anx_q3 * m.data$ZPRS_0.4
m.data$prod18	=	m.data$Pren_pregn_anx_q4 * m.data$ZPRS_0.4


fit25_interaction <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF25 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	DAD =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
	
	INT_G_MDD =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_MDD =~ prod15+prod16+prod17+prod18
	
	GIF25 ~ G + AX + INT_G_MDD + INT_AX_MDD
"

Model5 <- lavaan::sem(fit25_interaction, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model5, standardized = T, fit.measures = T)


fit26_interaction <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF26 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd

	INT_G_MDD =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_MDD =~ prod15+prod16+prod17+prod18
	
	GIF26 ~ G + AX + INT_G_MDD + INT_AX_MDD
"
Model6 <- lavaan::sem(fit26_interaction, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model6, standardized = T, fit.measures = T)


##--------------
## Interactions (1) SDQ + (2) MDD
##--------------
m.data$prod1	=	m.data$Pren_CESD1 * m.data$ZqPRS_0.0001
m.data$prod2	=	m.data$Pren_CESD2 * m.data$ZqPRS_0.0001
m.data$prod3	=	m.data$Pren_CESD5 * m.data$ZqPRS_0.0001
m.data$prod4	=	m.data$Pren_CESD7 * m.data$ZqPRS_0.0001
m.data$prod5	=	m.data$Pren_CESD11 * m.data$ZqPRS_0.0001
m.data$prod6	=	m.data$Pren_CESD20 * m.data$ZqPRS_0.0001
m.data$prod7	=	m.data$Pren_CESD3 * m.data$ZqPRS_0.0001
m.data$prod8	=	m.data$Pren_CESD6 * m.data$ZqPRS_0.0001
m.data$prod9	=	m.data$Pren_CESD14 * m.data$ZqPRS_0.0001
m.data$prod10	=	m.data$Pren_CESD18 * m.data$ZqPRS_0.0001
m.data$prod11	=	m.data$Pren_CESD4_rec * m.data$ZqPRS_0.0001
m.data$prod12	=	m.data$Pren_CESD8_rec * m.data$ZqPRS_0.0001
m.data$prod13	=	m.data$Pren_CESD12_rec * m.data$ZqPRS_0.0001
m.data$prod14	=	m.data$Pren_CESD16_rec * m.data$ZqPRS_0.0001
m.data$prod15	=	m.data$Pren_pregn_anx_q1 * m.data$ZqPRS_0.0001
m.data$prod16	=	m.data$Pren_pregn_anx_q2 * m.data$ZqPRS_0.0001
m.data$prod17	=	m.data$Pren_pregn_anx_q3 * m.data$ZqPRS_0.0001
m.data$prod18	=	m.data$Pren_pregn_anx_q4 * m.data$ZqPRS_0.0001

m.data$prod1_md	=	m.data$Pren_CESD1 * m.data$ZPRS_0.4
m.data$prod2_md	=	m.data$Pren_CESD2 * m.data$ZPRS_0.4
m.data$prod3_md	=	m.data$Pren_CESD5 * m.data$ZPRS_0.4
m.data$prod4_md	=	m.data$Pren_CESD7 * m.data$ZPRS_0.4
m.data$prod5_md	=	m.data$Pren_CESD11 * m.data$ZPRS_0.4
m.data$prod6_md	=	m.data$Pren_CESD20 * m.data$ZPRS_0.4
m.data$prod7_md	=	m.data$Pren_CESD3 * m.data$ZPRS_0.4
m.data$prod8_md	=	m.data$Pren_CESD6 * m.data$ZPRS_0.4
m.data$prod9_md	=	m.data$Pren_CESD14 * m.data$ZPRS_0.4
m.data$prod10_md	=	m.data$Pren_CESD18 * m.data$ZPRS_0.4
m.data$prod11_md	=	m.data$Pren_CESD4_rec * m.data$ZPRS_0.4
m.data$prod12_md	=	m.data$Pren_CESD8_rec * m.data$ZPRS_0.4
m.data$prod13_md	=	m.data$Pren_CESD12_rec * m.data$ZPRS_0.4
m.data$prod14_md	=	m.data$Pren_CESD16_rec * m.data$ZPRS_0.4
m.data$prod15_md	=	m.data$Pren_pregn_anx_q1 * m.data$ZPRS_0.4
m.data$prod16_md	=	m.data$Pren_pregn_anx_q2 * m.data$ZPRS_0.4
m.data$prod17_md	=	m.data$Pren_pregn_anx_q3 * m.data$ZPRS_0.4
m.data$prod18_md	=	m.data$Pren_pregn_anx_q4 * m.data$ZPRS_0.4


fit25_interaction <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF25 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	DAD =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
	
	INT_G_SDQ =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_SDQ =~ prod15+prod16+prod17+prod18
	INT_G_MDD =~ prod1_md+prod2_md+prod3_md+prod4_md+prod5_md+prod6_md+prod7_md+prod8_md+prod9_md+prod10_md+prod11_md+prod12_md+prod13_md+prod14_md+prod15_md+prod16_md+prod17_md+prod18_md
	INT_AX_MDD =~ prod15_md+prod16_md+prod17_md+prod18_md
	
	GIF25 ~ G + AX + INT_G_SDQ + INT_AX_SDQ + INT_G_MDD + INT_AX_MDD
"
Model7 <- lavaan::sem(fit25_interaction, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model7, standardized = T, fit.measures = T)

fit26_interaction <- "
	G =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	Somat =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	NegAffect =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	AN =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	AX =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	GIF26 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	MOM =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m
	CHILD =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd

	INT_G_SDQ =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_SDQ =~ prod15+prod16+prod17+prod18
	INT_G_MDD =~ prod1_md+prod2_md+prod3_md+prod4_md+prod5_md+prod6_md+prod7_md+prod8_md+prod9_md+prod10_md+prod11_md+prod12_md+prod13_md+prod14_md+prod15_md+prod16_md+prod17_md+prod18_md
	INT_AX_MDD =~ prod15_md+prod16_md+prod17_md+prod18_md
	
	GIF26 ~ G + AX + INT_G_SDQ + INT_AX_SDQ + INT_G_MDD + INT_AX_MDD
"

Model8 <- lavaan::sem(fit26_interaction, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T, # TRUE:all covariances among latents are set to 0
)
summary(Model8, standardized = T, fit.measures = T)

