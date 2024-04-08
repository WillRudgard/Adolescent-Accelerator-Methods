*------------------------------------------------------------------------------*

* Sensitivity analysis of of multiple outcome regression with 
* hypothesised accelerators, covariates, and correlated residuals.

use "C:\Users\william.rudgard\OneDrive - Nexus365\Analysis - INSPIRE Acc\02_Data\20210608 Plos Medicine INSPIRE dta Formatted.dta", clear

* For reference of multilevel approaches for accouting for correlated outcomes
* read: 
* 1. Agresti, 2018 Introduction to categorical data analysis, Chapters 9 and 10
* 2. Goldstein, 2010 Multilevel Statistical Models, Chapters 4.4 and 6

* Prepare data

* Drop 223 participants lost to follow up between T1 and T2.
* 109 participants from MW and 114 participants from YC.
drop if LTFU_i == 0

* Rename outcome variables for reshaping data from wide to long (For now, I just use three outcomes).
local outcomes "AbSx_i2" "AbPhy_i2" "AbEmo_i2"
forvalues n = 1/3{
local var : word `n' of "`outcomes'"
di "`var'"
gen Violence_i`n' = `var'
tab Violence_i`n', m
}

* Reshape data from wide to long so that each individual i has k rows 
* corresponding to outcome j1 to jk.
reshape long Violence_i, i(ID) j(outcome)

* Define a local with outcomes of interest:
local outcomes "AbSx_i2" "AbPhy_i2" "AbEmo_i2"
* Define a local with protective factors and covariates of interest:
local covariates "PosP_c12" "MonP_c12" "Food_i12" "Pov_i12" "ScFr_i12" "ScMe_i12" "AbSx_i1" "AbPhy_i1" "AbEmo_i1" "Age_c1" "Sex_i1" "Rural_i1" "House_i1" "MOrp_i1" "POrp_i1" "HHsiz_c1" "Prov_i1"
* Generate binary indicators of outcomes j1 to jk in long data, and interaction
* with protective factors and covariates of interest.
* REMEMBER locals must be executed at the same time as loops that use them.
forvalues ny = 1/3 /*Here, 3 corresponds to the total number of outcomes of interest, k*/{
local vary : word `ny' of "`outcomes'"
replace `vary' = 1 if outcome == `ny'
replace `vary' = 0 if outcome != `ny' & `vary' != .
	forvalues ncov = 1/17 /*Here, 17 corresponds to the total number of protective factors and covariates of interest, k*/{
	local varcov : word `ncov' of "`covariates'"
	gen `varcov'x`vary' = `varcov' * `vary' 
	}
}

* xtset data
xtset ID outcome

* Run marginal model with logit link and unstructured correlation
* Notice the inclusion of the categorical variable i.outcome
xtgee Violence_i ///
/*Hypothesised accelerators*/ c.PosP_c12xAbSx_i2 c.MonP_c12xAbSx_i2 i.Food_i12xAbSx_i2 i.Pov_i12xAbSx_i2 i.ScFr_i12xAbSx_i2 i.ScMe_i12xAbSx_i2 /*T1 control*/ i.AbSx_i1xAbSx_i2 /*Covariates*/ c.Age_c1xAbSx_i2 i.Sex_i1xAbSx_i2 i.House_i1xAbSx_i2 i.MOrp_i1xAbSx_i2 i.POrp_i1xAbSx_i2 c.HHsiz_c1xAbSx_i2 Prov_i1xAbSx_i2 ///
/*Hypothesised accelerators*/ c.PosP_c12xAbPhy_i2 c.MonP_c12xAbPhy_i2 i.Food_i12xAbPhy_i2 i.Pov_i12xAbPhy_i2 i.ScFr_i12xAbPhy_i2 i.ScMe_i12xAbPhy_i2 /*T1 control*/ i.AbPhy_i1xAbPhy_i2 /*Covariates*/ c.Age_c1xAbPhy_i2 i.Sex_i1xAbPhy_i2 i.House_i1xAbPhy_i2 i.MOrp_i1xAbPhy_i2 i.POrp_i1xAbPhy_i2 c.HHsiz_c1xAbPhy_i2 Prov_i1xAbPhy_i2 ///
/*Hypothesised accelerators*/ c.PosP_c12xAbEmo_i2 c.MonP_c12xAbEmo_i2 i.Food_i12xAbEmo_i2 i.Pov_i12xAbEmo_i2 i.ScFr_i12xAbEmo_i2 i.ScMe_i12xAbEmo_i2 /*T1 control*/ i.AbEmo_i1xAbEmo_i2 /*Covariates*/ c.Age_c1xAbEmo_i2 i.Sex_i1xAbEmo_i2 i.House_i1xAbEmo_i2 i.MOrp_i1xAbEmo_i2 i.POrp_i1xAbEmo_i2 c.HHsiz_c1xAbEmo_i2 Prov_i1xAbEmo_i2 ///
i.outcome, family(binomial) link(logit) eform corr(unstructured)


* Display adjusted correlation matrix between outcomes of interest
matrix list e(R)

* Calculate marginal probabilities given all combinations of identified potential accelerators
* Here it is important to select, which outcome is of interest using the if statement.
margins if outcome == 1, at(Food_i12xAbSx_i2 = (0 1) MonP_c12xAbSx_i2 = (24.51 32) PosP_c12xAbSx_i2 = (19.46 25))
margins if outcome == 2, at(Food_i12xAbPhy_i2 = (0 1) MonP_c12xAbPhy_i2 = (24.51 32) PosP_c12xAbPhy_i2 = (19.46 25))
margins if outcome == 3, at(Food_i12xAbEmo_i2 = (0 1) MonP_c12xAbEmo_i2 = (24.51 32)PosP_c12xAbEmo_i2 = (19.46 25))

**------------------------------------End-------------------------------------**
