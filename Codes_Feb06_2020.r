##-------------------------
## Bifactor using lavaan
##------------------------- 2019-Nov-14

setwd("...")
m.data <- read.csv("...", header = T, sep = ",", na.strings=c(""))

library(lavaan)
library(semTools)
library(mice)

##--------------
## Interaction terms: SDQ
##--------------
m.data$SDQprod1 = m.data$Pren_CESD1 * m.data$ZqPRS_0.0001
m.data$SDQprod2 = m.data$Pren_CESD2 * m.data$ZqPRS_0.0001
m.data$SDQprod3 = m.data$Pren_CESD5 * m.data$ZqPRS_0.0001
m.data$SDQprod4 = m.data$Pren_CESD7 * m.data$ZqPRS_0.0001
m.data$SDQprod5 = m.data$Pren_CESD11 * m.data$ZqPRS_0.0001
m.data$SDQprod6 = m.data$Pren_CESD20 * m.data$ZqPRS_0.0001
m.data$SDQprod7 = m.data$Pren_CESD3 * m.data$ZqPRS_0.0001
m.data$SDQprod8 = m.data$Pren_CESD6 * m.data$ZqPRS_0.0001
m.data$SDQprod9 = m.data$Pren_CESD14 * m.data$ZqPRS_0.0001
m.data$SDQprod10 = m.data$Pren_CESD18 * m.data$ZqPRS_0.0001
m.data$SDQprod11 = m.data$Pren_CESD4_rec * m.data$ZqPRS_0.0001
m.data$SDQprod12 = m.data$Pren_CESD8_rec * m.data$ZqPRS_0.0001
m.data$SDQprod13 = m.data$Pren_CESD12_rec * m.data$ZqPRS_0.0001
m.data$SDQprod14 = m.data$Pren_CESD16_rec * m.data$ZqPRS_0.0001
m.data$SDQprod15 = m.data$Pren_pregn_anx_q1 * m.data$ZqPRS_0.0001
m.data$SDQprod16 = m.data$Pren_pregn_anx_q2 * m.data$ZqPRS_0.0001
m.data$SDQprod17 = m.data$Pren_pregn_anx_q3 * m.data$ZqPRS_0.0001
m.data$SDQprod18 = m.data$Pren_pregn_anx_q4 * m.data$ZqPRS_0.0001

##--------------
## Interaction terms: MDD
##--------------
m.data$MDDprod1 = m.data$Pren_CESD1 * m.data$ZPRS_0.4
m.data$MDDprod2 = m.data$Pren_CESD2 * m.data$ZPRS_0.4
m.data$MDDprod3 = m.data$Pren_CESD5 * m.data$ZPRS_0.4
m.data$MDDprod4 = m.data$Pren_CESD7 * m.data$ZPRS_0.4
m.data$MDDprod5 = m.data$Pren_CESD11 * m.data$ZPRS_0.4
m.data$MDDprod6 = m.data$Pren_CESD20 * m.data$ZPRS_0.4
m.data$MDDprod7 = m.data$Pren_CESD3 * m.data$ZPRS_0.4
m.data$MDDprod8 = m.data$Pren_CESD6 * m.data$ZPRS_0.4
m.data$MDDprod9 = m.data$Pren_CESD14 * m.data$ZPRS_0.4
m.data$MDDprod10 = m.data$Pren_CESD18 * m.data$ZPRS_0.4
m.data$MDDprod11 = m.data$Pren_CESD4_rec * m.data$ZPRS_0.4
m.data$MDDprod12 = m.data$Pren_CESD8_rec * m.data$ZPRS_0.4
m.data$MDDprod13 = m.data$Pren_CESD12_rec * m.data$ZPRS_0.4
m.data$MDDprod14 = m.data$Pren_CESD16_rec * m.data$ZPRS_0.4
m.data$MDDprod15 = m.data$Pren_pregn_anx_q1 * m.data$ZPRS_0.4
m.data$MDDprod16 = m.data$Pren_pregn_anx_q2 * m.data$ZPRS_0.4
m.data$MDDprod17 = m.data$Pren_pregn_anx_q3 * m.data$ZPRS_0.4
m.data$MDDprod18 = m.data$Pren_pregn_anx_q4 * m.data$ZPRS_0.4

##--------------
## Covariates: 
##--------------
"PC1"              
"PC2"              
"PC3"
"Gender"
"MEDUC"
"age_mom_yr"
"meanPostCESD"
"Hamilton"

###########################------------------------
##
## SEM Analysis: including covariates DIRECTLY in structural model
##
###########################------------------------

FIT <- "
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
	
	# Interaction
	INT_G_SDQ =~ SDQprod1+SDQprod2+SDQprod3+SDQprod4+SDQprod5+SDQprod6+SDQprod7+SDQprod8+SDQprod9+SDQprod10+SDQprod11+SDQprod12+SDQprod13+SDQprod14+SDQprod15+SDQprod16+SDQprod17+SDQprod18
	INT_G_MDD =~ MDDprod1+MDDprod2+MDDprod3+MDDprod4+MDDprod5+MDDprod6+MDDprod7+MDDprod8+MDDprod9+MDDprod10+MDDprod11+MDDprod12+MDDprod13+MDDprod14+MDDprod15+MDDprod16+MDDprod17+MDDprod18
	
	# direct effect
	L10 ~ a*L1 + c*INT_G_SDQ + d*INT_G_MDD
	
    # mediator
	NE ~ e*L1 + g*INT_G_SDQ + h*INT_G_MDD
	L10 ~ i*NE
	
	# indirect effect (a*b)
	ei := e*i
	gi := g*i
	hi := h*i
	
	# total effect
	total1 := a + (e*i)
	total3 := c + (g*i)
	total4 := d + (h*i)
"

NewModel <- lavaan::sem(FIT, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE)

summary(Model_SDQ, standardized = T, fit.measures = T)
standardizedsolution(Model_SDQ)

## This model is wrong in terms of Lavaan syntax rule
Cov_FIT <- "
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
	
	# Interaction
	INT_G_SDQ =~ SDQprod1+SDQprod2+SDQprod3+SDQprod4+SDQprod5+SDQprod6+SDQprod7+SDQprod8+SDQprod9+SDQprod10+SDQprod11+SDQprod12+SDQprod13+SDQprod14+SDQprod15+SDQprod16+SDQprod17+SDQprod18
	INT_G_MDD =~ MDDprod1+MDDprod2+MDDprod3+MDDprod4+MDDprod5+MDDprod6+MDDprod7+MDDprod8+MDDprod9+MDDprod10+MDDprod11+MDDprod12+MDDprod13+MDDprod14+MDDprod15+MDDprod16+MDDprod17+MDDprod18
	
	# direct effect
	L10 ~ a*L1 + c*INT_G_SDQ + d*INT_G_MDD + PC1 + PC2 + PC3 + Gender + MEDUC + age_mom_yr + meanPostCESD + Hamilton
	
    # mediator
	NE ~ e*L1 + g*INT_G_SDQ + h*INT_G_MDD
	L10 ~ i*NE
	
	# indirect effect (a*b)
	ei := e*i
	gi := g*i
	hi := h*i
	
	# total effect
	total1 := a + (e*i)
	total3 := c + (g*i)
	total4 := d + (h*i)
"

Cov_model <- lavaan::sem(Cov_FIT, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE)
					
## Single indicator latent to incorporate covariates: variance fixed
## Errors occurred, mainly about "model is not identified"
Cov_FIT_single_indicator <- "
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
	
	CV1 =~ PC1 
	CV2 =~ PC2 
	CV3 =~ PC3 
	CV4 =~ Gender 
	CV5 =~ MEDUC 
	CV6 =~ age_mom_yr 
	CV7 =~ meanPostCESD
	CV8 =~ Hamilton
	
	PC1~~0*PC1
	PC2~~0*PC2
	PC3~~0*PC3
	Gender~~0*Gender 
	MEDUC ~~0*MEDUC 
	age_mom_yr~~0*age_mom_yr
	meanPostCESD~~0*meanPostCESD
	Hamilton~~0*Hamilton
	
	# Interaction
	INT_G_SDQ =~ SDQprod1+SDQprod2+SDQprod3+SDQprod4+SDQprod5+SDQprod6+SDQprod7+SDQprod8+SDQprod9+SDQprod10+SDQprod11+SDQprod12+SDQprod13+SDQprod14+SDQprod15+SDQprod16+SDQprod17+SDQprod18
	INT_G_MDD =~ MDDprod1+MDDprod2+MDDprod3+MDDprod4+MDDprod5+MDDprod6+MDDprod7+MDDprod8+MDDprod9+MDDprod10+MDDprod11+MDDprod12+MDDprod13+MDDprod14+MDDprod15+MDDprod16+MDDprod17+MDDprod18
	
	# direct effect
	L10 ~ a*L1 + c*INT_G_SDQ + d*INT_G_MDD + CV1 + CV2 + CV3 + CV4 + CV5 + CV6 + CV7 + CV8
	
    # mediator
	NE ~ e*L1 + g*INT_G_SDQ + h*INT_G_MDD
	L10 ~ i*NE
	
	# indirect effect (a*b)
	ei := e*i
	gi := g*i
	hi := h*i
	
	# total effect
	total1 := a + (e*i)
	total3 := c + (g*i)
	total4 := d + (h*i)
"
Cov_model2 <- lavaan::sem(Cov_FIT_single_indicator, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE)
					
## Single indicator latent to incorporate covariates: variance and loading fixed
## Worked well!
Cov_FIT_single_indicator_fixed_loadings <- "
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
	
	CV1 =~ 1*PC1 
	CV2 =~ 1*PC2 
	CV3 =~ 1*PC3 
	CV4 =~ 1*Gender 
	CV5 =~ 1*MEDUC 
	CV6 =~ 1*age_mom_yr 
	CV7 =~ 1*meanPostCESD
	CV8 =~ 1*Hamilton
	
	PC1~~0*PC1
	PC2~~0*PC2
	PC3~~0*PC3
	Gender~~0*Gender 
	MEDUC ~~0*MEDUC 
	age_mom_yr~~0*age_mom_yr
	meanPostCESD~~0*meanPostCESD
	Hamilton~~0*Hamilton
	
	# Interaction
	INT_G_SDQ =~ SDQprod1+SDQprod2+SDQprod3+SDQprod4+SDQprod5+SDQprod6+SDQprod7+SDQprod8+SDQprod9+SDQprod10+SDQprod11+SDQprod12+SDQprod13+SDQprod14+SDQprod15+SDQprod16+SDQprod17+SDQprod18
	INT_G_MDD =~ MDDprod1+MDDprod2+MDDprod3+MDDprod4+MDDprod5+MDDprod6+MDDprod7+MDDprod8+MDDprod9+MDDprod10+MDDprod11+MDDprod12+MDDprod13+MDDprod14+MDDprod15+MDDprod16+MDDprod17+MDDprod18
	
	# direct effect
	L10 ~ a*L1 + c*INT_G_SDQ + d*INT_G_MDD + CV1 + CV2 + CV3 + CV4 + CV5 + CV6 + CV7 + CV8
	
    # mediator
	NE ~ e*L1 + g*INT_G_SDQ + h*INT_G_MDD
	L10 ~ i*NE
	
	# indirect effect (a*b)
	ei := e*i
	gi := g*i
	hi := h*i
	
	# total effect
	total1 := a + (e*i)
	total3 := c + (g*i)
	total4 := d + (h*i)
"
Cov_model3 <- lavaan::sem(Cov_FIT_single_indicator_fixed_loadings, data = m.data,
					estimator = "MLR",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE)
					
## Conclusion:
## Cov_FIT vs. Cov_model3
## By including covariates to explain the variation of G2(second general factor),
## the direct effect of G1 turned out to be not significant