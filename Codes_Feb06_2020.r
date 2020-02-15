##-------------------------
## Bifactor using lavaan
##------------------------- 2019-Nov-14

setwd("...")
m.data <- read.csv("...", header = T, sep = ",", na.strings=c(""))

library(lavaan)
library(semTools)
library(mice)

library(plyr)
m.data <- rename(m.data, c("ZqPRS_0.0001"="SDQ", "ZPRS_0.4"="MDD"))
##--------------
## Interaction terms: SDQ
##--------------
m.data$SDQprod1 = m.data$Pren_CESD1 * m.data$SDQ
m.data$SDQprod2 = m.data$Pren_CESD2 * m.data$SDQ
m.data$SDQprod3 = m.data$Pren_CESD5 * m.data$SDQ
m.data$SDQprod4 = m.data$Pren_CESD7 * m.data$SDQ
m.data$SDQprod5 = m.data$Pren_CESD11 * m.data$SDQ
m.data$SDQprod6 = m.data$Pren_CESD20 * m.data$SDQ
m.data$SDQprod7 = m.data$Pren_CESD3 * m.data$SDQ
m.data$SDQprod8 = m.data$Pren_CESD6 * m.data$SDQ
m.data$SDQprod9 = m.data$Pren_CESD14 * m.data$SDQ
m.data$SDQprod10 = m.data$Pren_CESD18 * m.data$SDQ
m.data$SDQprod11 = m.data$Pren_CESD4_rec * m.data$SDQ
m.data$SDQprod12 = m.data$Pren_CESD8_rec * m.data$SDQ
m.data$SDQprod13 = m.data$Pren_CESD12_rec * m.data$SDQ
m.data$SDQprod14 = m.data$Pren_CESD16_rec * m.data$SDQ
m.data$SDQprod15 = m.data$Pren_pregn_anx_q1 * m.data$SDQ
m.data$SDQprod16 = m.data$Pren_pregn_anx_q2 * m.data$SDQ
m.data$SDQprod17 = m.data$Pren_pregn_anx_q3 * m.data$SDQ
m.data$SDQprod18 = m.data$Pren_pregn_anx_q4 * m.data$SDQ

##--------------
## Interaction terms: MDD
##--------------
m.data$MDDprod1 = m.data$Pren_CESD1 * m.data$MDD
m.data$MDDprod2 = m.data$Pren_CESD2 * m.data$MDD
m.data$MDDprod3 = m.data$Pren_CESD5 * m.data$MDD
m.data$MDDprod4 = m.data$Pren_CESD7 * m.data$MDD
m.data$MDDprod5 = m.data$Pren_CESD11 * m.data$MDD
m.data$MDDprod6 = m.data$Pren_CESD20 * m.data$MDD
m.data$MDDprod7 = m.data$Pren_CESD3 * m.data$MDD
m.data$MDDprod8 = m.data$Pren_CESD6 * m.data$MDD
m.data$MDDprod9 = m.data$Pren_CESD14 * m.data$MDD
m.data$MDDprod10 = m.data$Pren_CESD18 * m.data$MDD
m.data$MDDprod11 = m.data$Pren_CESD4_rec * m.data$MDD
m.data$MDDprod12 = m.data$Pren_CESD8_rec * m.data$MDD
m.data$MDDprod13 = m.data$Pren_CESD12_rec * m.data$MDD
m.data$MDDprod14 = m.data$Pren_CESD16_rec * m.data$MDD
m.data$MDDprod15 = m.data$Pren_pregn_anx_q1 * m.data$MDD
m.data$MDDprod16 = m.data$Pren_pregn_anx_q2 * m.data$MDD
m.data$MDDprod17 = m.data$Pren_pregn_anx_q3 * m.data$MDD
m.data$MDDprod18 = m.data$Pren_pregn_anx_q4 * m.data$MDD


###########################------------------------
##
## Test mediated effect
##
###########################------------------------

FIT1 <- "
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
	
	# Structural model
	L10 ~ a*L1 + ZqPRS_0.0001 + ZPRS_0.4 + b*INT_G_SDQ + c*INT_G_MDD + PC1 + PC2 + PC3 + Gender + MEDUC + age_mom_yr + meanPostCESD + Hamilton
	
    # mediator
	NE ~ d*L1 + e*INT_G_SDQ + f*INT_G_MDD
	L10 ~ g*NE
	
	# indirect effect (a*b)
	indir_L1_dg := d*g
	indir_SDQInt_eg := e*g
	indir_MDDInt_fg := f*g
	
	# total effect
	total_L1 := a + (d*g)
	total_SDQInt := b + (e*g)
	total_MDDInt := c + (f*g)
"

Fit1 <- lavaan::sem(FIT1, data = m.data,
					estimator = "ML",
					missing = "fiml",
					std.ov = TRUE,
					std.lv = TRUE,
					orthogonal = T,
					meanstructure = TRUE)
		# we included observed covariates in the structural model directly
		# lavaan used only 233 observations (out of 639), so did not impute missing values for 639 observations
		# so, compare the results with Fit2 below

lavaan WARNING: 379 cases were deleted due to missing values in 
                  exogenous variable(s), while fixed.x = TRUE.

lavaan 0.6-5.1458 ended normally after 226 iterations

  Estimator                                         ML               
  Optimization method                           NLMINB               
  Number of free parameters                        303               
                                                                     
                                                  Used       Total
  Number of observations                           233         639
  Number of missing patterns                        75

  Model Fit Test Statistic                    8507.863
  Degrees of freedom                              3909
  P-value (Chi-square)                           0.000


Fit1.boot <- lavaan::sem(FIT1, data = m.data,
					estimator = "ML",
					missing = "fiml",
					std.ov = TRUE,
					std.lv = TRUE,
					orthogonal = T,
					meanstructure = TRUE,
					se = "bootstrap", bootstrap=100)
		# but then, if we use bootstrap, what will happen?? i.e., for every bootstrap sample, 379 cases will be deleted??


FIT2 <- "
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
	
	# Structural model
	L10 ~ a*L1 + b*INT_G_SDQ + c*INT_G_MDD + CV1 + CV2 + CV3 + CV4 + CV5 + CV6 + CV7 + CV8
	
    # mediator
	NE ~ d*L1 + e*INT_G_SDQ + f*INT_G_MDD
	L10 ~ g*NE
	
	# indirect effect (a*b)
	indir_L1_dg := d*g
	indir_SDQInt_eg := e*g
	indir_MDDInt_fg := f*g
	
	# total effect
	total_L1 := a + (d*g)
	total_SDQInt := b + (e*g)
	total_MDDInt := c + (f*g)
"

Fit2 <- lavaan::sem(FIT2, data = m.data,
					estimator = "ML",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE)
					
		# FIT2 was modeld to include observed "covariates" in lavvan as single-indicator-latents,
		# This fits perfectly! So, time to try bootstrap
		Fit2.boot <- lavaan::sem(FIT2, data = m.data,
					estimator = "ML",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE,
					se = "bootstrap", bootstrap=500)

FIT3 <- "
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
	SDQ_main =~ 1*SDQ
	MDD_main =~ 1*MDD
	
	PC1~~0*PC1
	PC2~~0*PC2
	PC3~~0*PC3
	Gender~~0*Gender 
	MEDUC ~~0*MEDUC 
	age_mom_yr~~0*age_mom_yr
	meanPostCESD~~0*meanPostCESD
	Hamilton~~0*Hamilton
	SDQ~~0*SDQ
	MDD~~0*MDD
	
	# Interaction
	INT_G_SDQ =~ SDQprod1+SDQprod2+SDQprod3+SDQprod4+SDQprod5+SDQprod6+SDQprod7+SDQprod8+SDQprod9+SDQprod10+SDQprod11+SDQprod12+SDQprod13+SDQprod14+SDQprod15+SDQprod16+SDQprod17+SDQprod18
	INT_G_MDD =~ MDDprod1+MDDprod2+MDDprod3+MDDprod4+MDDprod5+MDDprod6+MDDprod7+MDDprod8+MDDprod9+MDDprod10+MDDprod11+MDDprod12+MDDprod13+MDDprod14+MDDprod15+MDDprod16+MDDprod17+MDDprod18
	
	# Structural model
	L10 ~ a*L1 + SDQ_main + MDD_main + b*INT_G_SDQ + c*INT_G_MDD + CV1 + CV2 + CV3 + CV4 + CV5 + CV6 + CV7 + CV8
	
    # mediator
	NE ~ d*L1 + e*INT_G_SDQ + f*INT_G_MDD
	L10 ~ g*NE
	
	# indirect effect (a*b)
	indir_L1_dg := d*g
	indir_SDQInt_eg := e*g
	indir_MDDInt_fg := f*g
	
	# total effect
	total_L1 := a + (d*g)
	total_SDQInt := b + (e*g)
	total_MDDInt := c + (f*g)
"

Fit3 <- lavaan::sem(FIT3, data = m.data,
					estimator = "ML",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE)
		# SDQ_main and MDD_main were included as single-indicator-latents
		# Fit okay; it's time to run bootstrap
		Fit3.boot <- lavaan::sem(FIT3, data = m.data,
					estimator = "ML",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE,
					se = "bootstrap", bootstrap=500)
		
FIT4 <- "
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
	SDQ_main =~ 1*SDQ
	MDD_main =~ 1*MDD
	
	PC1~~0*PC1
	PC2~~0*PC2
	PC3~~0*PC3
	Gender~~0*Gender 
	MEDUC ~~0*MEDUC 
	age_mom_yr~~0*age_mom_yr
	meanPostCESD~~0*meanPostCESD
	Hamilton~~0*Hamilton
	SDQ~~0*SDQ
	MDD~~0*MDD
	
	# Interaction
	INT_G_SDQ =~ SDQprod1+SDQprod2+SDQprod3+SDQprod4+SDQprod5+SDQprod6+SDQprod7+SDQprod8+SDQprod9+SDQprod10+SDQprod11+SDQprod12+SDQprod13+SDQprod14+SDQprod15+SDQprod16+SDQprod17+SDQprod18
	INT_G_MDD =~ MDDprod1+MDDprod2+MDDprod3+MDDprod4+MDDprod5+MDDprod6+MDDprod7+MDDprod8+MDDprod9+MDDprod10+MDDprod11+MDDprod12+MDDprod13+MDDprod14+MDDprod15+MDDprod16+MDDprod17+MDDprod18
	
	# Structural model
	L10 ~ a*L1 + b*SDQ_main + c*MDD_main + d*INT_G_SDQ + e*INT_G_MDD + CV1 + CV2 + CV3 + CV4 + CV5 + CV6 + CV7 + CV8
	
    # mediator
	NE ~ f*L1 + g*SDQ_main + h*MDD_main + i*INT_G_SDQ + j*INT_G_MDD
	L10 ~ k*NE
	
	# indirect effect (a*b)
	indir_L1_fk := f*k
	indir_SDQ_main_gk := g*k
	indir_MDD_main_hk := h*k
	indir_SDQInt_ik := i*k
	indir_MDDInt_jk := j*k
	
	# total effect
	total_L1 := a + (f*k)
	total_SDQMain := b + (g*k)
	total_MDDMain := c + (h*k)
	total_SDQInt := d + (i*k)
	total_MDDInt := e + (j*k)
"

Fit4 <- lavaan::sem(FIT4, data = m.data,
					estimator = "ML",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE)
		# SDQ_main and MDD_main ++ their mediated effects were included as single-indicator-latents general factor),
## the direct effect of G1 turned out to be not significant
