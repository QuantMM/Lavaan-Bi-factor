##-------------------------
##
## Bifactor using lavaan: Baseline model
## 
##------------------------- 2020-JULY-18
##
## Without mediation (Negative emotionality)

library(plyr)
library(lavaan)
library(semTools)
library(mice) ## if you encounter error, try to install packages, {devtools} and {tidyverse}
library(miceadds)
library(mitml)

setwd("C:/...")
m.data <- read.csv("...csv", header = T, sep = ",", na.strings=c(""))
m.data <- plyr::rename(m.data, c("ZqPRS_0.0001"="SDQ", "ZPRS_0.4"="MDD"))
boxplot(m.data)

##-------------------------
##
## Model Specification
##
##-------------------------

# Without mediation Model

fit <- "
	L1 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4
	L2 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
	L3 =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
	L4 =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
	L5 =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

	L10 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m + Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd  + SDQ_emo_dad_60m + SDQ_peer_dad_60m
	L11 =~ CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m + CBCL_anx_60m + CBCL_som_48m + CBCL_som_60m + CBCL_wit_48m + CBCL_wit_60m + PAPA_sepanx + PAPA_gad + PAPA_phob + PAPA_socphob + PAPA_overanx + PAPA_panic + PAPA_depdys + SDQ_emo_mom_60m + SDQ_emo_mom_72m + SDQ_peer_mom_60m + SDQ_peer_mom_72m
	L12 =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
	L13 =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m

	# Covariates
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
	INT_G_SDQ =~ SDQ.Pren_CESD1+SDQ.Pren_CESD2+SDQ.Pren_CESD5+SDQ.Pren_CESD7+SDQ.Pren_CESD11+SDQ.Pren_CESD20+SDQ.Pren_CESD3+SDQ.Pren_CESD6+SDQ.Pren_CESD14+SDQ.Pren_CESD18+SDQ.Pren_CESD4_rec+SDQ.Pren_CESD8_rec+SDQ.Pren_CESD12_rec+SDQ.Pren_CESD16_rec+SDQ.Pren_pregn_anx_q1+SDQ.Pren_pregn_anx_q2+SDQ.Pren_pregn_anx_q3+SDQ.Pren_pregn_anx_q4
	INT_G_MDD =~ MDD.Pren_CESD1+MDD.Pren_CESD2+MDD.Pren_CESD5+MDD.Pren_CESD7+MDD.Pren_CESD11+MDD.Pren_CESD20+MDD.Pren_CESD3+MDD.Pren_CESD6+MDD.Pren_CESD14+MDD.Pren_CESD18+MDD.Pren_CESD4_rec+MDD.Pren_CESD8_rec+MDD.Pren_CESD12_rec+MDD.Pren_CESD16_rec+MDD.Pren_pregn_anx_q1+MDD.Pren_pregn_anx_q2+MDD.Pren_pregn_anx_q3+MDD.Pren_pregn_anx_q4
	
	# Structural model
	L10 ~ a*L1 + b*SDQ_main + c*MDD_main + d*INT_G_SDQ + e*INT_G_MDD + CV1 + CV2 + CV3 + CV4 + CV5 + CV6 + CV7 + CV8
"

##-------------------------
##
## Missing value impuation for latent interaction terms
##
##-------------------------

## ------------------------
## Step 1: generate double-mean-centered product terms (with NAs)
## ------------------------
SDQnames <- c("SDQ","Pren_CESD1", "Pren_CESD2", "Pren_CESD5", "Pren_CESD7", "Pren_CESD11", "Pren_CESD20", "Pren_CESD3", "Pren_CESD6", "Pren_CESD14", "Pren_CESD18", "Pren_CESD4_rec", "Pren_CESD8_rec", "Pren_CESD12_rec", "Pren_CESD16_rec", "Pren_pregn_anx_q1", "Pren_pregn_anx_q2", "Pren_pregn_anx_q3", "Pren_pregn_anx_q4")
SDQsub <- subset(m.data, select=SDQnames)
SDQ_double_temp <- indProd(SDQsub, var1 = 1, var2 = 2:ncol(SDQsub), match = FALSE,
                meanC = TRUE, residualC = FALSE, doubleMC = TRUE)
SDQ_ex <- names(SDQ_double_temp) %in% c(SDQnames)
SDQ_double_mean <- SDQ_double_temp[!SDQ_ex]
boxplot(SDQ_double_mean)
	# this is compared with the regular product terms from earlier analyses, e.g., see Codes_Feb14_2020.r
	# boxplot(SDQ_double_mean) seems better, giving us prettier boxplots

MDDnames <- c("MDD","Pren_CESD1", "Pren_CESD2", "Pren_CESD5", "Pren_CESD7", "Pren_CESD11", "Pren_CESD20", "Pren_CESD3", "Pren_CESD6", "Pren_CESD14", "Pren_CESD18", "Pren_CESD4_rec", "Pren_CESD8_rec", "Pren_CESD12_rec", "Pren_CESD16_rec", "Pren_pregn_anx_q1", "Pren_pregn_anx_q2", "Pren_pregn_anx_q3", "Pren_pregn_anx_q4")
MDDsub <- subset(m.data, select=MDDnames)
MDD_double_temp <- indProd(MDDsub, var1 = 1, var2 = 2:ncol(MDDsub), match = FALSE,
                meanC = TRUE, residualC = FALSE, doubleMC = TRUE)
MDD_ex <- names(MDD_double_temp) %in% c(MDDnames)
MDD_double_mean <- MDD_double_temp[!MDD_ex]

# Dataset with double-mean-centered product terms for latent interactions
myData <- data.frame(cbind(m.data,SDQ_double_mean,MDD_double_mean))

## -------
## Data exploration
## -------
boxplot(myData)

	# standardize covariates:
	# as.vector() to keep the data object as data.frame (scale fuction creates class)
	myData$PC1 <- as.vector(scale(myData$PC1, center = TRUE, scale = TRUE))
	myData$PC2 <- as.vector(scale(myData$PC2, center = TRUE, scale = TRUE))
	myData$PC3 <- as.vector(scale(myData$PC3, center = TRUE, scale = TRUE))
	myData$Gender <- as.vector(scale(myData$Gender, center = TRUE, scale = TRUE))
	myData$MEDUC <- as.vector(scale(myData$MEDUC, center = TRUE, scale = TRUE))
	myData$age_mom_yr <- as.vector(scale(myData$age_mom_yr, center = TRUE, scale = TRUE))
	myData$meanPostCESD <- as.vector(scale(myData$meanPostCESD, center = TRUE, scale = TRUE))
	myData$Hamilton <- as.vector(scale(myData$Hamilton, center = TRUE, scale = TRUE))

boxplot(myData)

## -------
## FIML estimation using lavaan, before imputing data
## -------
start.time <- Sys.time()
fimlfit.SDQ <- lavaan::sem(fit, data = myData,
					estimator = "ML",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE,
					se = "bootstrap", bootstrap=1000)
end.time <- Sys.time()
(time.taken <- end.time - start.time)
