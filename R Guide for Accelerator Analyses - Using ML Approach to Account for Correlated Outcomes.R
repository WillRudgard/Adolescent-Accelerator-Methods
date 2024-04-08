###--------------------------------------------------------------------------###

# Sensitivity analysis of of multiple outcome regression with 
# hypothesised accelerators, covariates, and correlated residuals.

#---Packages and Libraries
#install.packages("gee")
#install.packages("tab")

library(gee) # Generalized Estimation Equation Solver
library(tab) # Create Summary Tables for Statistical Reports

#---Load data

# Read .dta
dta <- haven::read_dta("C:/Users/william.rudgard/OneDrive - Nexus365/Analysis - INSPIRE Acc/02_Data/20210608 Plos Medicine INSPIRE dta Formatted.dta")
dta <- as.data.frame(dta)

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

# For reference of multilevel approaches for accouting for correlated outcomes
# read: 
# 1. Agresti, 2018 Introduction to categorical data analysis, Chapters 9 and 10
# 2. Goldstein, 2010 Multilevel Statistical Models, Chapters 4.4 and 6

# Prepare data

# Define a list with outcomes of interest
outcomes <- list("AbSx_i2", "AbPhy_i2", "AbEmo_i2")

# Rename outcome variables for reshaping data from wide to long (For now, I just use three outcomes).
for(n in 1:3){
  dta <- within(dta, assign(paste0("Violence_i",n), dta[,outcomes[[n]]]))
  print(table(dta[,paste0("Violence_i",n)]))
}

# Reshape data from wide to long so that each individual i has k rows 
# corresponding to outcome j1 to jk.
dta2 <- reshape(dta, varying=c("Violence_i1", "Violence_i2", "Violence_i3"), v.names="Violence_i", timevar="outcome", idvar="ID", direction="long")

# Define a list with protective factors and covariates of interest:
covariates <- list("PosP_c12", "MonP_c12", "Food_i12", "Pov_i12", "ScFr_i12", "ScMe_i12", "AbSx_i1", "AbPhy_i1", "AbEmo_i1", "Age_c1", "Sex_i1", "Rural_i1", "House_i1", "MOrp_i1", "POrp_i1", "HHsiz_c1", "Prov_i1")

# Generate binary indicators of outcomes j1 to jk in long data, and interaction
# with protective factors and covariates of interest.
for(ny in 1:3){ # Here, 3 corresponds to the total number of outcomes of interest, k
dta2[,outcomes[[ny]]] <- ifelse(dta2$outcome==ny & !is.na(dta2$outcome), 1, 0)
  for(ncov in 1:17){ # Here, 17 corresponds to the total number of protective factors and covariates of interest, k
  dta2 <- within(dta2, assign(paste0(covariates[[ncov]],"x",outcomes[[ny]]),as.numeric(dta2[,outcomes[[ny]]]) * as.numeric(dta2[,covariates[[ncov]]])))
  }
}

# Run marginal model with logit link and unstructured correlation
# Notice the inclusion of the categorical variable i.outcome
gee <- gee(Violence_i ~ 
  # Hypothesised accelerators
  PosP_c12xAbSx_i2 + MonP_c12xAbSx_i2 + Food_i12xAbSx_i2 + Pov_i12xAbSx_i2 + ScFr_i12xAbSx_i2 + ScMe_i12xAbSx_i2 +
  # T1 control
  AbSx_i1xAbSx_i2 +
  # Covariates
  Age_c1xAbSx_i2  + Sex_i1xAbSx_i2  + House_i1xAbSx_i2  + MOrp_i1xAbSx_i2  + POrp_i1xAbSx_i2 + HHsiz_c1xAbSx_i2  + Prov_i1xAbSx_i2 +
  # Hypothesised accelerators 
  PosP_c12xAbPhy_i2 + MonP_c12xAbPhy_i2 + Food_i12xAbPhy_i2 + Pov_i12xAbPhy_i2 + ScFr_i12xAbPhy_i2 + ScMe_i12xAbPhy_i2 +
  # T1 control
  AbPhy_i1xAbPhy_i2 +
  # Covariates
  Age_c1xAbPhy_i2 + Sex_i1xAbPhy_i2 + House_i1xAbPhy_i2 + MOrp_i1xAbPhy_i2 + POrp_i1xAbPhy_i2 + HHsiz_c1xAbPhy_i2 + Prov_i1xAbPhy_i2 +
  # Hypothesised accelerators
  PosP_c12xAbEmo_i2 + MonP_c12xAbEmo_i2 + Food_i12xAbEmo_i2 + Pov_i12xAbEmo_i2 + ScFr_i12xAbEmo_i2 + ScMe_i12xAbEmo_i2 +
  # T1 control
  AbEmo_i1xAbEmo_i2 + 
  # Covariates
  Age_c1xAbEmo_i2 + Sex_i1xAbEmo_i2 + House_i1xAbEmo_i2 + MOrp_i1xAbEmo_i2 + POrp_i1xAbEmo_i2 + HHsiz_c1xAbEmo_i2 + Prov_i1xAbEmo_i2 +
  outcome, id=ID, data=dta2, family=binomial(link="logit"), corstr="unstructured")

# Visualise results
tabgee <- tabgee(gee)

##------------------------------------End-------------------------------------##