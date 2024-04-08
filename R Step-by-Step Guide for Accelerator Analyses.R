###--------------------------------------------------------------------------###
#---------File: GCRF Hub Step by Step Guide for Accelerator analysis-----------#
###--------------------------------------------------------------------------###

# Author: Dr. William E. Rudgard
# Acknowledgements: Huge thanks to Prof. Robin Evans for developing the code for 
# estimating standard errors of calculated marginal probabilities. Without
# his help this would not have been possible.

# Data overview

# The study population is: vulnerable adolescents living in South Africa.
# The study data combines the Young Carers and Mzantsi Wakho cohorts.
# Young Carers: Adolescents aged 10-17 years over two timepoints. 
# Baseline: 2010/2011, and follow-up: 2011/2012. Study location: Western Cape 
# and Mpumalanga provinces, SA.
# Mzantsi Wakho: Adolescents aged 11-19 years over two timepoints. Baseline: 
# 2014/2015, and follow-up: 2016/2017. Study location: Eastern Cape province, SA

#---Packages and Libraries
#install.packages(haven)
#install.packages("polycor")
#install.packages("jtools")

library(haven) # Import and export 'SPSS', 'Stata' and 'SAS' files
library(polycor) # Polychoric correlation
library(jtools) # Summarizing and visualizing regression models

#---Load data

# Read .dta
dta <- haven::read_dta("C:/Users/william.rudgard/OneDrive - Nexus365/Analysis - INSPIRE Acc/03_Data/20210608 Plos Medicine INSPIRE dta Formatted.dta")
dta <- as.data.frame(dta)

# Specify factor variables
names <- c("MOrp_i1", "Rural_i1", "POrp_i1", "LTFU_i12", "ScFr_i1", "ScFr_i2",
           "ScFr_i12", "ScMe_i1", "ScMe_i2", "ScMe_i12", "Food_i1", "Food_i2",
           "Food_i12", "Pov_i1", "Pov_i2", "Pov_i12", "PosP_i1", "PosP_i2",
           "PosP_i12", "MonP_i1", "MonP_i2", "MonP_i12", "AbSx_i1", "AbSx_i2",
           "AbPhy_i1", "AbPhy_i2", "AbEmo_i1", "AbEmo_i2", "TS_i1", "TS_i2",
           "ViPerp_i1", "ViPerp_i2", "ViVict_i1", "ViVict_i2", "Prov_i1",
           "Sex_i1", "House_i1", "HIV_i1")
dta[, names ] <- lapply(dta[, names ], factor)

# Check variable formats
#str(dta)

# Drop 223 participants lost to follow up between T1 and T2.
# 109 participants from MW and 114 participants from YC.
dta <- dta[ dta$LTFU_i12 == 1, ]

#---Data dictionary

# SDG OUTCOMES:
# Transactional sex: TS_i1 TS_i2 
# Sexual abuse: AbSx_i1 AbSx_i2 
# Physical abuse: AbPhy_i1 AbPhy_i2 
# Emotional abuse: AbEmo_i1 AbuEmo_i2
# Youth law breaking: ViPerp_i1 ViPerp_i2 
# Community violence victimisation: ViVict_i1 ViVict_i2

# HYPOTHESISED ACCELERATORS:
# Positive parenting: PosP_i1 PosP_i2 PosP_i12 PosP_c12
# Parental monitoring: MonP_i1 MonP_i2 MonP_i12 MonP_c12
# Food security: Food_i12
# Free schooling: ScFr_i12
# Free school meals: ScMe_i12
# Household non-food poverty: Pov_i12 

# COVARIATES: 
# Province: Prov_i1
# Age: Age_c1 
# Gender: Sex_i1
# Urban/rural: Rural_i1 
# Housing: House_i1 
# Maternal orphan: MOrp_i1 
# Paternal orphan: POrp_i1 
# Household size: HHsiz_c1 
# HIV status: HIV_i1 

#---STEP 1: Descriptive analysis of hypothesised accelerators and SDG outcomes

# Check 1: Are hypothesised accelerators correlated?
polycor::hetcor(dta[, c("PosP_i12",
                           "MonP_i12",
                           "Food_i12",
                           "ScFr_i12",
                           "ScMe_i12",
                           "Pov_i12"
                         # Add additional predictors here....
) ])
# This command uses Pearson product-moment correlations between numeric variables, 
# polyserial correlations between numeric and ordinal variables, and polychoric 
# correlations between ordinal variables.

# Investigate correlations with slightly higher correlation coefficients 
# using chi-squared test (I look at anything with correlation above 0.2)

# Free schooling and free school meals
table(dta$ScFr_i12, dta$ScMe_i12)
chisq.test(table(dta$ScFr_i12, dta$ScMe_i12))

# Food security and non-food poverty
table(dta$Food_i12, dta$Pov_i12)
chisq.test(table(dta$Food_i12, dta$Pov_i12))

# Make decision about whether to include variables or not.

# Check 2: How are hypothesised accelerators associatied with SDG outcomes?
for(var in c("AbSx_i2", "TS_i2", "AbPhy_i2", "AbEmo_i2", "ViPerp_i2", "ViVict_i2")){
  print(var)
  print(table(dta$PosP_i12, dta[, var ]))
  print(chisq.test(table(dta$PosP_i12, dta[, var ])))
  print(table(dta$MonP_i12, dta[, var ]))
  print(chisq.test(table(dta$MonP_i12, dta[, var ])))
  print(table(dta$Food_i12, dta[, var ]))
  print(chisq.test(table(dta$Food_i12, dta[, var ])))
  print(table(dta$ScFr_i12, dta[, var ]))
  print(chisq.test(table(dta$ScFr_i12, dta[, var ])))
  print(table(dta$ScMe_i12, dta[, var ]))
  print(chisq.test(table(dta$ScMe_i12, dta[, var ])))
  print(table(dta$Pov_i12, dta[, var ]))
  print(chisq.test(table(dta$Pov_i12, dta[, var ])))
}

#------------------------------------------------------------------------------#

# STEP 2: Single outcome regressions with hypothesised accelerators and covariates

# Switch factor variables in dta to numeric for regression
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
dta[, names ] <- lapply(dta[, names ], as.numeric.factor)

# Create dummy variables for categorical variables - this is necessary for later.
# For this analysis we only have one categorical variable - Province.
dta$Prov_i1EC <- ifelse(dta$Prov_i1 == 1, 1, 0)
dta$Prov_i1MP <- ifelse(dta$Prov_i1 == 2, 1, 0)
dta$Prov_i1WC <- ifelse(dta$Prov_i1 == 3, 1, 0)

#Check variable formats
#str(dta)

# Transactional sex (with no baseline control)
TS_model <- glm(
  # Outcome
  TS_i2 ~
  # Accelerators
  PosP_c12 + MonP_c12 + Food_i12 + ScFr_i12 + ScMe_i12 + Pov_i12 + 
  # T1 Control
  # Covariates
  Age_c1 + Sex_i1 + Rural_i1 + House_i1 + MOrp_i1 + POrp_i1 + HHsiz_c1 + HIV_i1 + Prov_i1MP + Prov_i1WC, data = dta, family = binomial(link = "logit"))
# Summarise model. Here exp(Est.) is equivalent to OR.
jtools::summ(TS_model, exp = TRUE)

# Sexual abuse (with baseline control)
AbSx_model <- glm(
  # Outcome
  AbSx_i2  ~
  # Accelerators
  PosP_c12 + MonP_c12 + Food_i12 + ScFr_i12 + ScMe_i12 + Pov_i12 + 
  # T1 Control
  AbSx_i1 +
  # Covariates
  Age_c1 + Sex_i1 + Rural_i1 + House_i1 + MOrp_i1 + POrp_i1 + HHsiz_c1 + HIV_i1 + Prov_i1MP + Prov_i1WC, data = dta, family = binomial(link = "logit"))
# Summarise model. Here exp(Est.) is equivalent to OR.
jtools::summ(AbSx_model, exp = TRUE)

# Physical abuse (with baseline control)
AbPhy_model <- glm(
  # Outcome
  AbPhy_i2  ~
  # Accelerators
  PosP_c12 + MonP_c12 + Food_i12 + ScFr_i12 + ScMe_i12 + Pov_i12 + 
  # T1 Control
  AbPhy_i1 +
  # Covariates
  Age_c1 + Sex_i1 + Rural_i1 + House_i1 + MOrp_i1 + POrp_i1 + HHsiz_c1 + HIV_i1 + Prov_i1MP + Prov_i1WC, data = dta, family = binomial(link = "logit"))
# Summarise model. Here exp(Est.) is equivalent to OR.
jtools::summ(AbPhy_model, exp = TRUE)

# Emotional abuse (with baseline control)
AbEmo_model <- glm(
  # Outcome
  AbEmo_i2  ~
  # Accelerators
  PosP_c12 + MonP_c12 + Food_i12 + ScFr_i12 + ScMe_i12 + Pov_i12 + 
  # T1 Control
  AbEmo_i1 +
  # Covariates
  Age_c1 + Sex_i1 + Rural_i1 + House_i1 + MOrp_i1 + POrp_i1 + HHsiz_c1 + HIV_i1 + Prov_i1MP + Prov_i1WC, data = dta, family = binomial(link = "logit"))
# Summarise model. Here exp(Est.) is equivalent to OR.
jtools::summ(AbEmo_model, exp = TRUE)

# Youth lawbreaking (with no baseline control)
ViPerp_model <- glm(
  # Outcome
  ViPerp_i2  ~
  # Accelerators
  PosP_c12 + MonP_c12 + Food_i12 + ScFr_i12 + ScMe_i12 + Pov_i12 + 
  # T1 Control
  # Covariates
  Age_c1 + Sex_i1 + Rural_i1 + House_i1 + MOrp_i1 + POrp_i1 + HHsiz_c1 + HIV_i1 + Prov_i1MP + Prov_i1WC, data = dta, family = binomial(link = "logit"))
# Summarise model. Here exp(Est.) is equivalent to OR.
jtools::summ(ViPerp_model, exp = TRUE)

# Community violence victimisation (with baseline control)
ViVict_model <- glm(
  # Outcome
  ViVict_i2  ~
  # Accelerators
  PosP_c12 + MonP_c12 + Food_i12 + ScFr_i12 + ScMe_i12 + Pov_i12 + 
  # T1 Control
  ViVict_i1 +
  # Covariates
  Age_c1 + Sex_i1 + Rural_i1 + House_i1 + MOrp_i1 + POrp_i1 + HHsiz_c1 + HIV_i1 + Prov_i1, data = dta, family = binomial(link = "logit"))
# Summarise model. Here exp(Est.) is equivalent to OR.
jtools::summ(ViVict_model, exp = TRUE)

#------------------------------------------------------------------------------#

# STEP 3: Benjamini and Hochberg procedure to control for False Discovery Rate

# Fot this step, we export results from STEP 5 to excel and use a pre-prepared 
# excel spreadsheet.

#------------------------------------------------------------------------------#
  
# STEP 4: Evaluate synergy effects of identified potential accelerators

# This step is applied on hypothesised accelerators that are estimated to be 
# independent predictors of multiple SDG outcomes after correcting for multiple 
# outcome testing using the Benjamini and Hochberg procedure, and are thus
# identified potential accelerators.

# In this analysis, we are therefore interested in calcuating the adjusted 
# probability of experiencing our outcomes given exposure to positive parenting,
# child monitoring, and food security by themselves or all together.

# To do this, we first estimate the adjusted probability of experiencing 
# our outcomes given exposure or not to our accelerators of interest. 
# For accelerators measured using a binary indicator no receipt is equivalent 
# to "0. No" and receipt is equivalent to "1. Yes". For continuous variables it
# requires the investigator to decide on two values of a scale which correspond
# to exposure or not. Here, for positive parenting, and parental monitoring and 
# supervision, we specify exposure as the highest attainable score on the scale
# and no exposure as the average score of these accelerators in the sample. 

# Unlike in Stata, in R the SEM command cannot estimate associations between
# hypothesised accelerators and SDG outcomes using a logit link. Hence, we 
# calculate adjusted probabilities from the single outcome regressions. These
# are mathematically equivalent to the ones that would be calculated from the
# multiple outcome model specified with SEM.

# Note that there is some discussion about whether controls should be fixed at 
# their mean values using the atmeans option in the Stata margins commmand. 
# Based on this paper published in the International Journal of Epidemiology I 
# choose not to take this approach. I would be happy to discuss reasons for this
# in more detail if it interested you.
# Muller and MacLehose (2014) Estimating predicted probabilities from logistic 
# regression: different methods correspond to different target populations 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4052139/

for(var in c("AbSx", "TS", "AbPhy", "AbEmo", "ViPerp", "ViVict")){
# Scenario 1: Experiencing no accelerators
print(paste("P(", var, ") for the scenario of No accelerators", sep = ""))
Base_dta <- model.matrix(get(paste0(var, "_model"))$formula, data = dta)
# Specify the values at which you would like to make the prediction.
Base_dta[ , "Food_i12"] <- 0
Base_dta[ , "PosP_c12"] <- 24.51
Base_dta[ , "MonP_c12" ] <- 19.46
p0 <- predict(get(paste0(var, "_model")), newdata = as.data.frame(Base_dta), type = "response"); print(paste("Marginal probability:", round(mean(p0),4), sep = " "))
Base_dta <- Base_dta * p0 * (1 - p0)
p0SE <- sqrt(sum(Base_dta %*% vcov(get(paste0(var, "_model"))) %*% t(Base_dta))/nrow(as.data.frame(Base_dta))^2)
p0LCI <- mean(p0) - 1.96 * p0SE
p0UCI <- mean(p0) + 1.96 * p0SE; print(paste("95% Conf. Interval:", round(p0LCI,4), round(p0UCI,4), sep = " "))
# Scenario 2: Experiencing all accelerators
print(paste("P(", var, ") for the scenario of All accelerators", sep = ""))
All_dta <- model.matrix(get(paste0(var, "_model"))$formula, data = dta)
# Specify the values at which you would like to make the prediction.
All_dta[ , "Food_i12"] <- 1
All_dta[ , "PosP_c12"] <- 32
All_dta[ , "MonP_c12" ] <- 24
p1 <- predict(get(paste0(var, "_model")), newdata = as.data.frame(All_dta), type = "response"); print(paste("Marginal probability:", round(mean(p1),4), sep = " "))
All_dta <- All_dta * p1 * (1-p1)
p1SE <- sqrt(sum(All_dta %*% vcov(get(paste0(var, "_model"))) %*% t(All_dta))/nrow(as.data.frame(All_dta))^2)
p1LCI <- mean(p1) - 1.96 * p1SE
p1UCI <- mean(p1) + 1.96 * p1SE; print(paste("95% Conf. Interval:", round(p1LCI,4), round(p1UCI,4), sep = " "))
# Contrast of scenarios 1 and 2
print(paste("Contrast of P(", var, ") comparing All vs. No", sep = ""))
print(paste("Probability difference:", round(mean(p1)-mean(p0),4), sep = " "))
p1p0SE <- sqrt(sum(Base_dta %*% vcov(get(paste0(var, "_model"))) %*% t(Base_dta))/nrow(as.data.frame(Base_dta))^2 + sum(All_dta %*% vcov(get(paste0(var, "_model"))) %*% t(All_dta))/nrow(as.data.frame(All_dta))^2 - 2*sum(Base_dta %*% vcov(get(paste0(var, "_model"))) %*% t(All_dta))/nrow(as.data.frame(All_dta))^2)
p1p0LCI <- (mean(p1)-mean(p0)) - 1.96 * p1p0SE
p1p0UCI <- (mean(p1)-mean(p0)) + 1.96 * p1p0SE; print(paste("95% Conf. Interval:", round(p1p0LCI,4), round(p1p0UCI,4), sep = " "))
}

# Estimating adjusted probabilies assumes proper model specification, namely,
# no uncontrolled confounding and no measurement error. Given that these 
# assumptions are fulfilled, outputted values represent the proportion of 
# observations with the outcome that we would expect to observe if all of the
# study population experience one or a combination of potential accelerators.

##------------------------------------End-------------------------------------##