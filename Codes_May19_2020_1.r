##-------------------------
##
## Bifactor using lavaan
## 
##------------------------- 2020-May-19
##
## In this file, I tried double mean centering with semTools::indProd
## For missing value impuation, using {mice} but impute interaction terms as well (NOT impute main predictors first then create int with imputed main predictors)
## Added main effects of SDQ and MDD, but two separate models
## See "Analysis Summary_May16.pptx" file

library(plyr)
library(lavaan)
library(semTools)
library(mice) ## if you encounter error, try to install packages, {devtools} and {tidyverse}
library(miceadds)
library(mitml)

setwd("C:/...")
m.data <- read.csv("...")
m.data <- plyr::rename(m.data, c("ZqPRS_0.0001"="SDQ", "ZPRS_0.4"="MDD"))
boxplot(m.data)

##-------------------------
##
## Data setup
##
##-------------------------

L1names <- c("Pren_CESD1","Pren_CESD2","Pren_CESD5","Pren_CESD7","Pren_CESD11","Pren_CESD20","Pren_CESD3","Pren_CESD6","Pren_CESD14","Pren_CESD18","Pren_CESD4_rec","Pren_CESD8_rec","Pren_CESD12_rec","Pren_CESD16_rec","Pren_pregn_anx_q1","Pren_pregn_anx_q2","Pren_pregn_anx_q3","Pren_pregn_anx_q4")
L10names <- c("CBCL_emo_48m","CBCL_emo_60m","CBCL_anx_48m","CBCL_anx_60m","CBCL_som_48m","CBCL_som_60m","CBCL_wit_48m","CBCL_wit_60m","PAPA_sepanx","PAPA_gad","PAPA_phob","PAPA_socphob","PAPA_overanx","PAPA_panic","PAPA_depdys","SDQ_emo_mom_60m","SDQ_emo_mom_72m","SDQ_peer_mom_60m","SDQ_peer_mom_72m","Dominic_sepanx","Dominic_overanx","Dominic_phobia","Dominic_mdd","SDQ_emo_dad_60m","SDQ_peer_dad_60m")
genenames <- c("SDQ","MDD")
L1deleteID <- which( rowSums(is.na(m.data[,L1names])) == length(L1names) )
L10deleteID <- which( rowSums(is.na(m.data[,L10names])) == length(L10names) )
geneID <- which( rowSums(is.na(m.data[,genenames])) == length(genenames) )

intersect(L1deleteID,L10deleteID)
union(L1deleteID,L10deleteID)
union( union(L1deleteID,L10deleteID) ,geneID)

DeleteIDs <- union( union(L1deleteID,L10deleteID) ,geneID)
m.data <- data.frame(m.data[-DeleteIDs,])

##-------------------------
##
## Model Specification
##
##-------------------------

# Model 1. SDQ

SDQfit <- "
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

	PC1~~0*PC1
	PC2~~0*PC2
	PC3~~0*PC3
	Gender~~0*Gender 
	MEDUC ~~0*MEDUC 
	age_mom_yr~~0*age_mom_yr
	meanPostCESD~~0*meanPostCESD
	Hamilton~~0*Hamilton
	SDQ~~0*SDQ
	
	# Interaction
	INT_G_SDQ =~ SDQ.Pren_CESD1+SDQ.Pren_CESD2+SDQ.Pren_CESD5+SDQ.Pren_CESD7+SDQ.Pren_CESD11+SDQ.Pren_CESD20+SDQ.Pren_CESD3+SDQ.Pren_CESD6+SDQ.Pren_CESD14+SDQ.Pren_CESD18+SDQ.Pren_CESD4_rec+SDQ.Pren_CESD8_rec+SDQ.Pren_CESD12_rec+SDQ.Pren_CESD16_rec+SDQ.Pren_pregn_anx_q1+SDQ.Pren_pregn_anx_q2+SDQ.Pren_pregn_anx_q3+SDQ.Pren_pregn_anx_q4
	
	# Structural model
	L10 ~ a*L1 + b*SDQ_main + d*INT_G_SDQ + CV1 + CV2 + CV3 + CV4 + CV5 + CV6 + CV7 + CV8
	
    # mediator
	NE ~ f*L1 + g*SDQ_main + i*INT_G_SDQ
	L10 ~ k*NE
	
	# indirect effect (a*b)
	indir_L1_fk := f*k
	indir_SDQ_main_gk := g*k
	indir_SDQInt_ik := i*k
	
	# total effect
	total_L1 := a + (f*k)
	total_SDQMain := b + (g*k)
	total_SDQInt := d + (i*k)
"

# Model 2. MDD

MDDfit <- "
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
	MDD_main =~ 1*MDD
	
	PC1~~0*PC1
	PC2~~0*PC2
	PC3~~0*PC3
	Gender~~0*Gender 
	MEDUC ~~0*MEDUC 
	age_mom_yr~~0*age_mom_yr
	meanPostCESD~~0*meanPostCESD
	Hamilton~~0*Hamilton
	MDD~~0*MDD
	
	# Interaction
	INT_G_MDD =~ MDD.Pren_CESD1+MDD.Pren_CESD2+MDD.Pren_CESD5+MDD.Pren_CESD7+MDD.Pren_CESD11+MDD.Pren_CESD20+MDD.Pren_CESD3+MDD.Pren_CESD6+MDD.Pren_CESD14+MDD.Pren_CESD18+MDD.Pren_CESD4_rec+MDD.Pren_CESD8_rec+MDD.Pren_CESD12_rec+MDD.Pren_CESD16_rec+MDD.Pren_pregn_anx_q1+MDD.Pren_pregn_anx_q2+MDD.Pren_pregn_anx_q3+MDD.Pren_pregn_anx_q4
		
	# Structural model
	L10 ~ a*L1 + c*MDD_main + e*INT_G_MDD + CV1 + CV2 + CV3 + CV4 + CV5 + CV6 + CV7 + CV8
	
    # mediator
	NE ~ f*L1 + h*MDD_main + j*INT_G_MDD
	L10 ~ k*NE
	
	# indirect effect (a*b)
	indir_L1_fk := f*k
	indir_MDD_main_hk := h*k
	indir_MDDInt_jk := j*k
	
	# total effect
	total_L1 := a + (f*k)
	total_MDDMain := c + (h*k)
	total_MDDInt := e + (j*k)
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
nBoot <- 1000
start.time <- Sys.time()
fimlfit.SDQ <- lavaan::sem(SDQfit, data = myData,
					estimator = "ML",	# ML for maximum likelihood, ... see p.51 https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
					missing = "fiml",	# "ml" full information maximum likelihood approach (fiml); "listwise",
					std.ov = TRUE,	# TRUE:all observed variables are standardized before entering the analysis
					std.lv = TRUE,	# TRUE:the metric of each latent is determined by fixing residual variance to 1
					orthogonal = T,  # TRUE:all covariances among latents are set to 0
					meanstructure = TRUE,
					se = "bootstrap", bootstrap=nBoot)
end.time <- Sys.time()
(time.taken <- end.time - start.time) #9.362532 hours

start.time <- Sys.time()
fimlfit.MDD <- lavaan::sem(MDDfit, data = myData,
					estimator = "ML",
					missing = "fiml",
					std.ov = TRUE,
					std.lv = TRUE,
					orthogonal = T,
					meanstructure = TRUE,
					se = "bootstrap", bootstrap=1000)
end.time <- Sys.time()
(time.taken <- end.time - start.time) # 9.036253 hours

save.image(file="fimlfit_SDQ_MDD_nBoot1000_May21.RData")

## ------------------------
## Step 2: missing value imputation using mice()
## ------------------------

## check how many missing values in the created interaction terms
apply(myData, 2, function(x) sum(is.na(x)))
mice::md.pattern(myData)

## Imputation parameters:
## 1) m == how many datasets; 5, 50, 100
## 2) maxit = # of iter in each dataset; 20~30 seems sufficient, but 50 (fixed)

			## Trial 1
			TEMP <- myData[,1:18]
			imputed_TEMP1 <- mice(TEMP, m=3, maxit = 10, method = 'pmm', seed = 500)
			imputed_TEMP1_30 <- mice(TEMP, m=3, maxit = 30, method = 'pmm', seed = 500)
			imputed_TEMP1_50 <- mice(TEMP, m=3, maxit = 50, method = 'pmm', seed = 500)
			densityplot(imputed_TEMP1)
			densityplot(imputed_TEMP1, ~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4 + CBCL_emo_48m + CBCL_emo_60m)
			
				# This is to see maxit, to study convergence
				plot(imputed_TEMP1)
				plot(imputed_TEMP1_30)
				plot(imputed_TEMP1_50)

			## Trial 2
			imputed_TEMP2 <- mice(myData, m=3, maxit = 10, method = 'pmm', seed = 500)
			densityplot(imputed_TEMP2, ~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4)

## Generate imputed datasets for m=5, 50, 100
	start.time <- Sys.time()
imputed1 <- mice(myData, m=5, maxit = 50, method = 'pmm', seed = 500)
	end.time <- Sys.time()
	(time.taken <- end.time - start.time)
	# 13.41941 mins
	
	start.time <- Sys.time()
imputed2 <- mice(myData, m=50, maxit = 50, method = 'pmm', seed = 500)
	end.time <- Sys.time()
	(time.taken <- end.time - start.time)
	# 2.274807 hours
#imputed3 <- mice(myData, m=100, maxit = 50, method = 'pmm', seed = 500)

save.image(file="imputedDatasets_May17.RData")

## Data exploration
densityplot(imputed1, ~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 + Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18 + Pren_CESD4_rec +    Pren_CESD8_rec +  Pren_CESD12_rec +  Pren_CESD16_rec +  Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4 + CBCL_emo_48m + CBCL_emo_60m + CBCL_anx_48m)
densityplot(imputed1, ~ PC1 + PC2 + PC3 + Gender + MEDUC + age_mom_yr + meanPostCESD + Hamilton)
plot(imputed1, c("Pren_CESD1","Pren_CESD2","Pren_CESD5","Pren_CESD7"))	


## ------------------------
## Step 3: pooling
## ------------------------

			## Tried with(), but not working with lavaan::sem()
			pooledfit <- with(imputed_Data1, lavaan::sem(FIT3, estimator = "ML", std.ov = TRUE,	std.lv = TRUE, orthogonal = T, meanstructure = TRUE, se = "bootstrap", bootstrap=10))
			summary(pool(pooledfit))

load("imputedDatasets_May17.RData") 

## ------
## WITHOUT bootstrap in lavaan::sem()
## ------
# WhichData <- imputed1
WhichData <- imputed2
# WhichData <- imputed3
Model <- SDQfit
# Model <- MDDfit

## Setting up dataframe
ImputedDat <- miceadds::datlist_create(WhichData)

## Fitting models on imputed datasets
ALL <- lapply(
			ImputedDat,
			FUN = function(data){
					res <- lavaan::sem(Model, data = data,
												estimator = "ML",
												missing = "fiml",
												std.ov = TRUE,	
												std.lv = TRUE,	
												orthogonal = T, 
												meanstructure = TRUE)
					return(res)
				}
		)

ALL[[34]] <- NULL


## Extracting estimated parameters for mitml::testEstimates()
qhat <- lapply(
			ALL,
			FUN = function(ll){
					estall <- lavaan::parameterEstimates(ll)
					parnames <- paste0( estall$lhs , estall$op , estall$rhs, " (", estall$label, ")" )
					v1 <- estall$est
					names(v1) <- parnames
					return(v1)
				}
		)

se <- lapply(
			ALL,
			FUN = function(ll){
					estall <- lavaan::parameterEstimates(ll)
					parnames <- paste0( estall$lhs , estall$op , estall$rhs, " (", estall$label, ")" )
					v2 <- estall$se
					names(v2) <- parnames
					return(v2)
				}
		)
se2 <- lapply( se , FUN = function(ss){ ss^2 } ) # input variances

# Using mitml::testEstimates() to pool results
results <- mitml::testEstimates(qhat=qhat, uhat=se2)
round(results$estimates,4)

save.image(file="pooled_m50_withoutBoot_SDQ_May17.RData")

## ------
## WITH bootstrap in lavaan::sem()
## ------

load("imputedDatasets_May17.RData") 

nBoot <- 100
# WhichData <- imputed1
WhichData <- imputed2
# WhichData <- imputed3
Model <- SDQfit
# Model <- MDDfit

## Setting up dataframe
ImputedDat <- miceadds::datlist_create(WhichData)

## Fitting models on imputed datasets
	start.time <- Sys.time()
ALL <- lapply(
			ImputedDat,
			FUN = function(data){
					res <- lavaan::sem(Model, data = data,
												estimator = "ML",
												missing = "fiml",
												std.ov = TRUE,	
												std.lv = TRUE,	
												orthogonal = T, 
												meanstructure = TRUE,
												se = "bootstrap", bootstrap=nBoot)
					return(res)
				}
		)
	end.time <- Sys.time()
	(time.taken <- end.time - start.time)
	# 52.91525 mins

ALL[[34]] <- NULL

## Extracting estimated parameters for mitml::testEstimates()
qhat <- lapply(
			ALL,
			FUN = function(ll){
					estall <- lavaan::parameterEstimates(ll)
					parnames <- paste0( estall$lhs , estall$op , estall$rhs, " (", estall$label, ")" )
					v1 <- estall$est
					names(v1) <- parnames
					return(v1)
				}
		)

se <- lapply(
			ALL,
			FUN = function(ll){
					estall <- lavaan::parameterEstimates(ll)
					parnames <- paste0( estall$lhs , estall$op , estall$rhs, " (", estall$label, ")" )
					v2 <- estall$se
					names(v2) <- parnames
					return(v2)
				}
		)
se2 <- lapply( se , FUN = function(ss){ ss^2 } ) # input variances

# Using mitml::testEstimates() to pool results
results <- mitml::testEstimates(qhat=qhat, uhat=se2)
round(results$estimates,4)

save.image(file="pooled_m50_withBoot100_SDQ_May17.RData")