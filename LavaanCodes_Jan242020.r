##-------------------------
## Bifactor using lavaan
##------------------------- 2019-Nov-14

setwd("...")
m.data <- read.csv("...csv", header = T, sep = ",", na.strings=c(""))

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
	L1 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	L2 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	L3 =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	L4 =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	L5 =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
"

##-------------------------
## Internalizing Psycho: Model 25
##-------------------------
Inter <- "
	GIF =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	L11 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	L12 =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	L13 =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
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


###########################------------------------
##
## SEM Analysis 1
##
###########################------------------------

##-------------------------
## M-factor model + Internalizing Psycho:
##-------------------------

fit25 <- "
	L1 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	L2 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	L3 =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	L4 =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	L5 =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	L10 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	L11 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	L12 =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	L13 =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
	
	L10 ~ L1 + L5
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


###########################------------------------
##
## SEM Analysis 2: Interactions and/or mediator
##
###########################------------------------

##--------------
## Interaction: (1) SDQ + mediator
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


fit25_SDQ <- "
	L1 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	L2 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	L3 =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	L4 =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	L5 =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	L10 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	L11 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	L12 =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	L13 =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
	
	NE =~ NE_18m + NE_36m
	
	INT_G_SDQ =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_SDQ =~ prod15+prod16+prod17+prod18
	
	# direct effect
	L10 ~ a*L1 + b*L5 + c*INT_G_SDQ + d*INT_AX_SDQ
	
    # mediator
	NE ~ e*L1 + f*L5 + g*INT_G_SDQ + h*INT_AX_SDQ
	L10 ~ i*NE
	
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

Model_SDQ <- lavaan::sem(fit25_SDQ, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE)

summary(Model_SDQ, standardized = T, fit.measures = T)
standardizedsolution(Model_SDQ)


##--------------
## Interaction: (2) MDD + mediator
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


fit25_MDD <- "
	L1 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	L2 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	L3 =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	L4 =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	L5 =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	L10 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	L11 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	L12 =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	L13 =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m

	NE =~ NE_18m + NE_36m
	
	INT_G_MDD =~ prod1+prod2+prod3+prod4+prod5+prod6+prod7+prod8+prod9+prod10+prod11+prod12+prod13+prod14+prod15+prod16+prod17+prod18
	INT_AX_MDD =~ prod15+prod16+prod17+prod18
	
	# direct effect
	L10 ~ a*L1 + b*L5 + c*INT_G_MDD + d*INT_AX_MDD
	
	# mediator
	NE ~ e*L1 + f*L5 + g*INT_G_MDD + h*INT_AX_MDD
	L10 ~ i*NE

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

Model_MDD <- lavaan::sem(fit25_MDD, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T # TRUE:all covariances among latents are set to 0
)
summary(Model_MDD, standardized = T, fit.measures = T)

