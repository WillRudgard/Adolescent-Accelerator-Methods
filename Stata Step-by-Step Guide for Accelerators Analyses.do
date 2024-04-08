******************************************************************************** 
*----------File: GCRF Hub Step by Step Guide for Accelerator analysis----------*
********************************************************************************

* Data overview

* The study population is: vulnerable adolescents living in South Africa.
* The study data combines the Young Carers and Mzantsi Wakho cohorts.
* Young Carers: Adolescents aged 10-17 years over two timepoints. 
* Baseline: 2010/2011, and follow-up: 2011/2012. Study location: Western Cape 
* and Mpumalanga provinces, SA.
* Mzantsi Wakho: Adolescents aged 11-19 years over two timepoints. Baseline: 
* 2014/2015, and follow-up: 2016/2017. Study location: Eastern Cape province, SA

* Load data.
use "C:\Users\william.rudgard\OneDrive - Nexus365\Accelerators\INSPIRE\Data\INSPIRE Analysis DTA Formatted.dta" , clear

* Drop participants lost to follow up between T1 and T2.
* 109 participants from MW and 114 participants from YC.
*bysort Sample : tab T1andT2 
drop if LTFU_i12 == 0

* Data dictionary

* SDG OUTCOMES:
* Transactional sex: TS_i1 TS_i2 
* Sexual abuse: AbSx_i1 AbSx_i2 
* Physical abuse: AbPhy_i1 AbPhy_i2 
* Emotional abuse: AbEmo_i1 AbEmo_i2
* Youth law breaking: ViPerp_i1 ViPerp_i2 
* Community violence victimisation: ViVict_i1 ViVict_i2

* HYPOTHESISED ACCELERATORS:
* Positive parenting: PosP_i1 PosP_i2 PosP_i12 PosP_c12
* Parental monitoring: MonP_i1 MonP_i2 MonP_i12 MonP_c12
* Food security: Food_i12
* Free schooling: ScFr_i12
* Free school meals: ScMe_i12
* Household non-food poverty: Pov_i12 

* COVARIATES: 
* Province: Prov_i1
* Age: Age_c1 
* Gender: Sex_i1
* Urban/rural: Rural_i1 
* Housing: House_i1 
* Maternal orphan: MOrp_i1 
* Paternal orphan: POrp_i1 
* Household size: HHsiz_c1 
* HIV status: HIV_i1 

*------------------------------------------------------------------------------*

* STEP 1: Descriptive analysis of hypothesised accelerators and SDG outcomes

* Check 1: (for longitudinal data): Do outcomes change much over follow-up?
foreach var in AbSx_i TS_i AbPhy_i AbEmo_i ViPerp_i ViVict_i {
tab `var'1 `var'2  , row chi 
}

* If outcomes do not change much over follow-up then think whether it is 
* is possible that the root cause was experienced prior to the start of 
* the study. If this is the case then should you be evaluating this as an
* an outcome in this study?

* Check 2: Are hypothesised accelerators correlated?

* Check correlations across exposures of interest overall
tetrachoric PosP_i12 MonP_i12 Food_i12 ScFr_i12 ScMe_i12 Pov_i12 , pw stats( rho p )

* Investigate correlations with slightly higher correlation coefficients 
* using chi-squared test (I look at anything with correlation above 0.2)

* Free schooling and free school meals
tab ScFr_i12 ScMe_i12 , row m chi

* Food security and non-food poverty
tab Food_i12 Pov_i12 , row m chi

* Check 3: How are hypothesised accelerators associatied with SDG outcomes?
foreach var of varlist AbSx_i2 TS_i2 AbPhy_i2 AbEmo_i2 ViPerp_i2 ViVict_i2 {
tab PosP_i12 `var' , m row chi
tab MonP_i12  `var' , m row chi
tab Food_i12 `var' , m row chi
tab ScFr_i12 `var' , m row chi
tab ScMe_i12 `var' , m row chi
tab Pov_i12 `var' , m row chi
}

*------------------------------------------------------------------------------*

* STEP 2: Single outcome regressions with hypothesised accelerators and covariates

* Transactional sex (with no baseline control)
logit ///
/*Outcome*/ TS_i2 ///
/*Accelerators*/ c.PosP_c12 c.MonP_c12 i.Food_i12 ///
/*Accelerators*/ i.ScFr_i12 i.ScMe_i12 i.Pov_i12 ///
/*T1 Control*/ ///
/*Covariates*/ c.Age_c1 i.Sex_i1 i.Rural_i1 i.House_i1 i.MOrp_i1 ///
/*Covariates*/ i.POrp_i1 c.HHsiz_c1 i.HIV_i1 i.Prov_i1 ///
, or baselevels

{
* For Tabulating Odds Ratios:
lincom _b[ c.PosP_c12 ] , or
matrix coefTS1 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ c.MonP_c12 ] , or
matrix coefTS2 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Food_i12 ] , or
matrix coefTS3 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Pov_i12 ] , or
matrix coefTS4 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScFr_i12 ] , or
matrix coefTS5 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScMe_i12 ] , or
matrix coefTS6 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
matrix coefTSAll = ( coefTS1 \ coefTS2 \ coefTS3 \ coefTS4 \ coefTS5 \ coefTS6 )
}

* Sexual abuse (with baseline control)
logit ///
/*Outcome*/ AbSx_i2 ///
/*Accelerators*/ c.PosP_c12 c.MonP_c12 i.Food_i12 ///
/*Accelerators*/ i.ScFr_i12 i.ScMe_i12 i.Pov_i12 ///
/*T1 Control*/ AbSx_i1 ///
/*Covariates*/ c.Age_c1 i.Sex_i1 i.Rural_i1 i.House_i1 i.MOrp_i1 ///
/*Covariates*/ i.POrp_i1 c.HHsiz_c1 i.HIV_i1 i.Prov_i1 ///
, or baselevels
{
* For Tabulating Odds Ratios:
lincom _b[ c.PosP_c12 ] , or
matrix coefAbuS1 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ c.MonP_c12 ] , or
matrix coefAbuS2 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Food_i12 ] , or
matrix coefAbuS3 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Pov_i12 ] , or
matrix coefAbuS4 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScFr_i12 ] , or
matrix coefAbuS5 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScMe_i12 ] , or
matrix coefAbuS6 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
matrix coefAbuSAll = ( coefAbuS1 \ coefAbuS2 \ coefAbuS3 \ coefAbuS4 \ coefAbuS5 \ coefAbuS6 )
}

* Physical abuse (with baseline control)
logit ///
/*Outcome*/ AbPhy_i2 ///
/*Accelerators*/ c.PosP_c12 c.MonP_c12 i.Food_i12 ///
/*Accelerators*/ i.ScFr_i12 i.ScMe_i12 i.Pov_i12 ///
/*T1 Control*/ AbPhy_i1 ///
/*Covariates*/ c.Age_c1 i.Sex_i1 i.Rural_i1 i.House_i1 i.MOrp_i1 ///
/*Covariates*/ i.POrp_i1 c.HHsiz_c1 i.HIV_i1 i.Prov_i1 ///
, or baselevels

{
* For Tabulating Odds Ratios:
lincom _b[ c.PosP_c12 ] , or
matrix coefAbuP1 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ c.MonP_c12 ] , or
matrix coefAbuP2 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Food_i12 ] , or
matrix coefAbuP3 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Pov_i12 ] , or
matrix coefAbuP4 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScFr_i12 ] , or
matrix coefAbuP5 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScMe_i12 ] , or
matrix coefAbuP6 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
matrix coefAbuPAll = ( coefAbuP1 \ coefAbuP2 \ coefAbuP3 \ coefAbuP4 \ coefAbuP5 \ coefAbuP6 )
}

* Emotional abuse (with baseline control)
logit ///
/*Outcome*/ AbEmo_i2 ///
/*Accelerators*/ c.PosP_c12 c.MonP_c12 i.Food_i12 ///
/*Accelerators*/ i.ScFr_i12 i.ScMe_i12 i.Pov_i12 ///
/*T1 Control*/ AbEmo_i1 ///
/*Covariates*/ c.Age_c1 i.Sex_i1 i.Rural_i1 i.House_i1 i.MOrp_i1 ///
/*Covariates*/ i.POrp_i1 c.HHsiz_c1 i.HIV_i1 i.Prov_i1 ///
, or baselevels
{
* For Tabulating Odds Ratios:
lincom _b[ c.PosP_c12 ] , or
matrix coefAbuE1 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ c.MonP_c12 ] , or
matrix coefAbuE2 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Food_i12 ] , or
matrix coefAbuE3 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Pov_i12 ] , or
matrix coefAbuE4 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScFr_i12 ] , or
matrix coefAbuE5 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScMe_i12 ] , or
matrix coefAbuE6 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
matrix coefAbuEAll = ( coefAbuE1 \ coefAbuE2 \ coefAbuE3 \ coefAbuE4 \ coefAbuE5 \ coefAbuE6 )
}

* Youth lawbreaking (with no baseline control)
logit ///
/*Outcome*/ ViPerp_i2 ///
/*Accelerators*/ c.PosP_c12 c.MonP_c12 i.Food_i12 ///
/*Accelerators*/ i.ScFr_i12 i.ScMe_i12 i.Pov_i12 ///
/*T1 Control*/ ViPerp_i1 ///
/*Covariates*/ c.Age_c1 i.Sex_i1 i.Rural_i1 i.House_i1 i.MOrp_i1 ///
/*Covariates*/ i.POrp_i1 c.HHsiz_c1 i.HIV_i1 i.Prov_i1 ///
, or baselevels

{
* For Tabulating Odds Ratios:
lincom _b[ c.PosP_c12 ] , or
matrix coefViPerp1 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ c.MonP_c12 ] , or
matrix coefViPerp2 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Food_i12 ] , or
matrix coefViPerp3 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Pov_i12 ] , or
matrix coefViPerp4 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScFr_i12 ] , or
matrix coefViPerp5 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScMe_i12 ] , or
matrix coefViPerp6 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
matrix coefViPerpAll = ( coefViPerp1 \ coefViPerp2 \ coefViPerp3 \ coefViPerp4 \ coefViPerp5 \ coefViPerp6 )
}

* Community violence victimisation (with baseline control)
logit ///
/*Outcome*/ ViVict_i2 ///
/*Accelerators*/ c.PosP_c12 c.MonP_c12 i.Food_i12 ///
/*Accelerators*/ i.ScFr_i12 i.ScMe_i12 i.Pov_i12 ///
/*T1 Control*/ ///
/*Covariates*/ c.Age_c1 i.Sex_i1 i.Rural_i1 i.House_i1 i.MOrp_i1 ///
/*Covariates*/ i.POrp_i1 c.HHsiz_c1 i.HIV_i1 i.Prov_i1 ///
, or baselevels

{
* For Tabulating Odds Ratios:
lincom _b[ c.PosP_c12 ] , or
matrix coefViVict1 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ c.MonP_c12 ] , or
matrix coefViVict2 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Food_i12 ] , or
matrix coefViVict3 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.Pov_i12 ] , or
matrix coefViVict4 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScFr_i12 ] , or
matrix coefViVict5 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
lincom _b[ 1.ScMe_i12 ] , or
matrix coefViVict6 = ( r( estimate ) , r(lb) , r(ub) , r( p ) )
matrix coefViVictAll = ( coefViVict1 \ coefViVict2 \ coefViVict3 \ coefViVict4 \ coefViVict5 \ coefViVict6 )
}

* Bring all matrices together
matrix AllAll = ( coefTSAll , coefAbuSAll , coefAbuPAll , coefAbuEAll , coefViPerpAll , coefViVictAll )
* Display combined matrix
matrix list AllAll

*------------------------------------------------------------------------------*

* STEP 3: Multiple outcome regression with hypothesised accelerators and covariates

* Using Structural Equation Modelling - note that here we are not able correlate 
* our residuals.
gsem ///
/*Regress outcomes on main predictors (hypothesised accelerators)*/ ///
( AbSx_i2 <- c.PosP_c12 c.MonP_c12 i.Food_i12 i.ScFr_i12 i.ScMe_i12 i.Pov_i12 , family( binomial) link(logit) ) ///
( TS_i2 <- c.PosP_c12 c.MonP_c12 i.Food_i12 i.ScFr_i12 i.ScMe_i12 i.Pov_i12 , family( binomial) link(logit) ) ///
( AbPhy_i2 <- c.PosP_c12 c.MonP_c12 i.Food_i12 i.ScFr_i12 i.ScMe_i12 i.Pov_i12 , family( binomial) link(logit) ) ///
( AbEmo_i2 <- c.PosP_c12 c.MonP_c12 i.Food_i12 i.ScFr_i12 i.ScMe_i12 i.Pov_i12 , family( binomial) link(logit) ) /// 
( ViPerp_i2 <- c.PosP_c12 c.MonP_c12 i.Food_i12 i.ScFr_i12 i.ScMe_i12 i.Pov_i12 , family( binomial) link(logit) ) /// 
( ViVict_i2 <- c.PosP_c12 c.MonP_c12 i.Food_i12 i.ScFr_i12 i.ScMe_i12 i.Pov_i12 , family( binomial) link(logit) ) /// 
/*T1 control*/ ///
( AbSx_i2 <- i.AbSx_i1 , family( binomial ) link( logit ) ) /// 
/*( TS_i2 <- i.TS_i1 , family( binomial ) link( logit ) ) Because we evaluate EVER experiencing Transactional Sex (reported at T2) as our outcome we cannot adjust for this.*/ ///
( AbPhy_i2 <- i.AbPhy_i1 , family( binomial ) link( logit ) ) ///
( AbEmo_i2 <- i.AbEmo_i1 , family( binomial ) link( logit ) ) ///
( ViPerp_i2 <- i.ViPerp_i1 , family( binomial ) link( logit ) ) ///
/*( ViVict_i2 <- i.ViVict_i1 , family( binomial ) link( logit ) ) Because we evaluate EVER experiencing Community Violence (reported at T2) as our outcome we cannot adjust for this.*/ ///
/*Selected covariates*/ ///
( AbSx_i2 TS_i2 AbPhy_i2 AbEmo_i2 ViPerp_i2 ViVict_i2 <- c.Age_c1 i.Sex_i1 i.Rural_i1 i.House_i1 i.MOrp_i1 i.POrp_i1 c.HHsiz_c1 i.HIV_i1 i.Prov_i1 , family( binomial) link(logit) ) ///
, nocapslatent 

* Summarise odds ratios
estat eform AbSx_i2 TS_i2 AbPhy_i2 AbEmo_i2 ViPerp_i2 ViVict_i2

*------------------------------------------------------------------------------*

* STEP 4: Benjamini and Hochberg procedure to control for False Discovery Rate

* For this step, we export results from STEP 5 to a pre-prepared 
* excel spreadsheet saved in the "Benjamini Hochberg" file on the Adolescent 
* Accelerator Methods OSF webpage.

*------------------------------------------------------------------------------*

* STEP 5: Evaluate synergy effects of identified potential accelerators

* This step is applied on hypothesised accelerators that are estimated to be 
* independent predictors of multiple SDG outcomes after correcting for multiple 
* outcome testing using the Benjamini and Hochberg procedure.

* In this analysis, we are therefore interested in calcuating the adjusted 
* probability of experiencing our outcomes given exposure to positive parenting,
* child monitoring, and food security by themselves or all together.

* To do this, we first estimate the adjusted probability of experiencing 
* our outcomes given exposure or not to our accelerators of interest. 
* For accelerators measured using a binary indicator no receipt is equivalent 
* to "0. No" and receipt is equivalent to "1. Yes". For continuous variables it
* requires the investigator to decide on two values of a scale which correspond
* to exposure or not. Here, for positive parenting, and parental monitoring and 
* supervision, we specify exposure as the highest attainable score on the scale
* and no exposure as the average score of these accelerators in the sample. 

margins i.Food_i12 , at( PosP_c12 = ( 24.51 32 ) MonP_c12 = ( 19.46 24 ) ) post

* Estimating adjusted probabilities assumes proper model specification, namely,
* no uncontrolled confounding and no measurement error. Given that these 
* assumptions are fulfilled, outputted values represent the proportion of 
* observations with the outcome that we would expect to observe if we been able
* to force all of the study population to experience the specified combination 
* of accelerators.

* In statistical terms:
* a) Focusing on the outcome Emotional Abuse and a scenario in which all 
* accelerators are experienced. Here, Z=z refers to a given set of observed 
* values for the control vector Z:
* [Pr(TS_i2=1|Food_i12=1,PosP_c12=32,MonP_c12=24; Z=z)=4.31%] 

* b) Focusing on the outcome Emotional Abuse and a scenario in which only  
* food security and Positive Parenting are experienced. Note that now 
* MonP_i12 equals the population mean value of 19.46. Again,
* Z=z refers to a given set of observed values for the control vector.
* [Pr(TS_i2=1|Food_i12=1,PosP_c12=32,MonP_c12=19.46; Z=z)=6.72%] 

* Note that there is some discussion about whether controls should be fixed at 
* their mean values using the atmeans option in the Stata margins commmand. 
* Based on this paper published in the International Journal of Epidemiology I 
* choose not to take this approach. I would be happy to discuss reasons for this
* in more detail if it interested you.
* Muller and MacLehose (2014) Estimating predicted probabilities from logistic 
* regression: different methods correspond to different target populations 
* https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4052139/

* Following estimation of adjusted probabilities for each combination of our
* accelerators we can then calculate the DIFFERENCE in predicted probability 
* for specific combinations of accelerators.

* To estimate the difference in adjusted probabilities you use the lincom 
* command. This requires you to know what the margins command earlier has saved 
* each coefficient as. To do this you can specify coeflegend as an option.
* Note that when you do this you will not be shown the confidence intervals of
* the difference. Do not fret, you can get these by simply removing coeflegend
* as an option. Make sure to note down the names of each of the coefficients.  

* In this example I calculate the difference in adjusted probability of 
* experiencing transactional sex comparing exposure to all accelerators vs.
* exposure to no accelerators.
lincom ( ( _b[ 1._predict#4._at#1.Food_i12 ] ) - ( _b[ 1._predict#1._at#0.Food_i12 ] ) ) * 100

* Here the coefficient for experiencing no accelerators is named:
* _b[ 1._predict#1._at#0.Food_i12 ]
* Alternatively the coefficient for experiencing all accelerators is named:
* _b[ 1._predict#4._at#1.Food_i12 ]

* X._predict refers to the outcome of interest. In this case it is 
* 1._predict which refers to TS_i2. If it were to specify 2._predict it would 
* be refering to AbSx_i2.

* The X._at refers to the specific combination of continuous variables used in 
* an estimation. In the case of 4._at refers to the setting of PosP_c12
* and MonP_c12 to 32 and 24 respectively.

* The X.Food_i12 refers to the specification of food security, with 
* 1.Food_i12 referring to receipt and 0.Food_i12 referring to no receipt.

* Another useful reference for estimating predicted probabilities and 
* marginal effects in Stata is this Stata journal article by Richard Williams:
* https://www.stata-journal.com/article.html?article=st0260

*------------------------------------------------------------------------------*

* STEP 6: Sensitivity analysis of multiple outcome regression with hypothesised 
* accelerators, covariates, and correlated residuals.

* For reference of the mvprobit command read: Capellari and Jenkins (2003)

*type -ssc install mvprobit- to install necessary package (You only need to do this once)
*ssc install mvprobit

*---Run MV probit regression
mvprobit ///
/* EQ1 */ ( AbSx_i2 = /*Hypothesised accelerators*/ PosP_c12 MonP_c12 Food_i12 Pov_i12 ScFr_i12 ScMe_i12 /*T1 control*/ AbSx_i1 /*Covariates*/ Sex_i1 Age_c1 Rural_i1 House_i1 MOrp_i1 POrp_i1 HHsiz_c1 HIV_i1 Prov_i1 ) ///
/* EQ2 */ ( TS_i2 = /*Hypothesised accelerators*/ PosP_c12 MonP_c12 Food_i12 Pov_i12 ScFr_i12 ScMe_i12 /*T1 control*/ /*Covariates*/ Sex_i1 Age_c1 Rural_i1 House_i1 MOrp_i1 POrp_i1 HHsiz_c1 HIV_i1 Prov_i1 ) ///
/* EQ3 */ ( AbPhy_i2 = /*Hypothesised accelerators*/ PosP_c12 MonP_c12 Food_i12 Pov_i12 ScFr_i12 ScMe_i12 /*T1 control*/ AbPhy_i1 /*Covariates*/ Sex_i1 Age_c1 Rural_i1 House_i1 MOrp_i1 POrp_i1 HHsiz_c1 HIV_i1 Prov_i1 ) ///
/* EQ4 */ ( AbEmo_i2 = /*Hypothesised accelerators*/ PosP_c12 MonP_c12 Food_i12 Pov_i12 ScFr_i12 ScMe_i12 /*T1 control*/ AbEmo_i1 /*Covariates*/ Sex_i1 Age_c1 Rural_i1 House_i1 MOrp_i1 POrp_i1 HHsiz_c1 HIV_i1 Prov_i1 ) ///
/* EQ5 */ ( ViPerp_i2 = /*Hypothesised accelerators*/ PosP_c12 MonP_c12 Food_i12 Pov_i12 ScFr_i12 ScMe_i12 /*T1 control*/ ViPerp_i1 /*Covariates*/ Sex_i1 Age_c1 Rural_i1 House_i1 MOrp_i1 POrp_i1 HHsiz_c1 HIV_i1 Prov_i1 ) ///
/* EQ6 */ ( ViVict_i2 = /*Hypothesised accelerators*/ PosP_c12 MonP_c12 Food_i12 Pov_i12 ScFr_i12 ScMe_i12 /*T1 control*/ /*Covariates*/ Sex_i1 Age_c1 Rural_i1 House_i1 MOrp_i1 POrp_i1 HHsiz_c1 HIV_i1 Prov_i1 ) , ///
draws( 100 ) nolog

* In the output the athrho is the Fisher's z transformed rho 
* (https://journals.sagepub.com/doi/pdf/10.1177/1536867X1101100211)
* The rho is the correlation between the residuals of each of the two probits 
* referenced by the numbers after rho. For example rho21 is the correlation
* between the residuals of regression 2 and 1.

* The interpretation for a coefficient of 0.116 is approximately 12% of 
* unexplained variance is shared. For an example of an analysis using
* mvprobit: https://www.cogentoa.com/article/10.1080/23311975.2016.1265803

* Save estimation results given that model takes so long to run
estimates save "C:\Users\william.rudgard\OneDrive - Nexus365\Accelerators\INSPIRE\Estimates\mvprobit 100 draws.ster" , replace

* It is not possible to apply the margins command to mvprobit instead we have to
* recreate it ourselves when calculating adjusted probabilties.

* In this sensitivity analysis, we only caculate adjusted probability of our 
* outcomes given experiencing none or all of our identified potential 
* accelerators.

* To do this we first have to store a copy of our identified potential 
* accelerators:
clonevar PosPcopy = PosP_c12
clonevar MonPcopy = MonP_c12
clonevar Foodcopy = Food_i12

*---Predict given experience of no identified potential accelerators
* Replace all identified potential accelerators with "no experience" values
replace PosP_c12 = 19.46 /*mean value: 19.46*/
replace MonP_c12 = 24.51 /*mean value: 24.51*/
replace Food_i12 = 0

* Calculate adjusted probabilities for outcomes 1/t, with t corresponding to the
* total number of outcomes. 
* Here 1/6 corresponds to our outcomes of interest. If there were fewer or more
* outcomes, then replace 6 with the relevant number.
mvppred pmarg_noAcc_hat, pmarg
forvalues i=1/6{
sum pmarg_noAcc_hat`i'
}

*---Predict given experience of all identified potential accelerators
* Replace all identified potential accelerators with "experience" values
replace PosP_c12 = 25 /*max value: 25*/
replace MonP_c12 = 32 /*max value: 32*/
replace Food_i12 = 1

* Calculate adjusted probabilities for outcomes 1/t, with t corresponding to the
* total number of outcomes. 
mvppred pmarg_allAcc_hat, pmarg
forvalues i=1/6{
sum pmarg_allAcc_hat`i'
}

**------------------------------------End-------------------------------------**
