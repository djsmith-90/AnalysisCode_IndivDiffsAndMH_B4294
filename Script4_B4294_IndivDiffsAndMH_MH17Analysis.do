*** PROJECT: The role of individual differences in later mental health (B4294)
*** STATA VERSION 18
*** Script 4: Analysing mental health at age 17 outcomes
*** Created 7/12/2023 by Dan Major-Smith (based on script by Tapasya Bhardwaj)

** Set working directory
cd "X:\Studies\RSBB Team\Dan\B4294 - MSc Personality MH"

** Install any user-written packages
*ssc install missings, replace
*ssc install mimrgns, replace

** Create log file
capture log close
log using ".\Results\IndivDiffsAndMH_MH17_Results.log", replace

** Read in dataset
use ".\updatedDataset_6.12.2023\PersonalityMH_B4294_processed.dta", clear

* Or, if using synthetic datasets
*use ".\analysisCode_B4294\SyntheticData\syntheticData_B4294.dta", clear
*drop FALSE_DATA


******************************************************************************
***** Complete-case analyses

** Create CCA marker for all confounders
egen cca = rowmiss(sex-child_ACEs11)
recode cca (1/15 = 0) (0 = 1)
tab cca, m


*** Start with depression as outcome

** Extraversion
tab dep17 if cca == 1 & extra != .

* Unadjusted
logistic dep17 extra if cca == 1

* Adjusted
logistic dep17 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum extra if cca == 1 & dep17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


** Agreeableness
tab dep17 if cca == 1 & agree != .

* Unadjusted
logistic dep17 agree if cca == 1

* Adjusted
logistic dep17 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum agree if cca == 1 & dep17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


** Conscientiousness
tab dep17 if cca == 1 & consc != .

* Unadjusted
logistic dep17 consc if cca == 1

* Adjusted
logistic dep17 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum consc if cca == 1 & dep17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


** Emotional stability
tab dep17 if cca == 1 & emoStab != .

* Unadjusted
logistic dep17 emoStab if cca == 1

* Adjusted
logistic dep17 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum emoStab if cca == 1 & dep17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


** Openness
tab dep17 if cca == 1 & open != .

* Unadjusted
logistic dep17 open if cca == 1

* Adjusted (note: child_anx10 removed due to collinearity with outcome)
logistic dep17 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum open if cca == 1 & dep17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


** IQ/Cognitive ability
tab dep17 if cca == 1 & iq != .

* Unadjusted
logistic dep17 iq if cca == 1

* Adjusted
logistic dep17 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum iq if cca == 1 & dep17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))



*** Now with anxiety as outcome

** Extraversion
tab anx17 if cca == 1 & extra != .

* Unadjusted
logistic anx17 extra if cca == 1

* Adjusted
logistic anx17 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum extra if cca == 1 & anx17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


** Agreeableness
tab anx17 if cca == 1 & agree != .

* Unadjusted
logistic anx17 agree if cca == 1

* Adjusted
logistic anx17 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum agree if cca == 1 & anx17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


** Conscientiousness
tab anx17 if cca == 1 & consc != .

* Unadjusted
logistic anx17 consc if cca == 1

* Adjusted
logistic anx17 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum consc if cca == 1 & anx17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


** Emotional stability
tab anx17 if cca == 1 & emoStab != .

* Unadjusted
logistic anx17 emoStab if cca == 1

* Adjusted
logistic anx17 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum emoStab if cca == 1 & anx17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


** Openness
tab anx17 if cca == 1 & open != .

* Unadjusted
logistic anx17 open if cca == 1

* Adjusted
logistic anx17 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum open if cca == 1 & anx17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


** IQ/Cognitive ability
tab anx17 if cca == 1 & iq != .

* Unadjusted
logistic anx17 iq if cca == 1

* Adjusted (note: child_anx10 removed due to collinearity with outcome)
logistic anx17 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum iq if cca == 1 & anx17 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))



********************************************************************************
*** Now run multiple imputation (assuming MAR)

** NOTE: Have commented this out as is the same imputation model as in the age 24 script

*** Set up the imputation and register complete and missing variables
*missings report _all, percent

*mi set mlong
*mi register regular sex
*mi register imputed ageAtBirth home mum_edu dad_edu imd ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_anx10 child_dep10 child_depScore10 child_ACEs11 extra agree consc emoStab open iq dep17 anx17 dep24 anx24 behavDis13 iq8 pliks17 smk15_17 alc17 friends17 negCogStyles17 drugs17 anti17

*** Test imputation with a dry-run first
*mi impute chained ///
*	(regress) ageAtBirth extra agree consc emoStab open iq iq8 negCogStyles17 ///
*	(pmm, knn(5)) mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_depScore10 ///
*		alc17 pliks17 friends17 ///
*	(logit) ethnic child_anx10 child_dep10 child_ACEs11 dep17 anx17 dep24 anx24 ///
*		behavDis13 smk15_17 drugs17 anti17 ///
*	(mlogit) home mum_edu dad_edu ///
*	(ologit) imd ///
*	= i.sex, ///
*	add(50) burnin(10) rseed(91865) dryrun
	
	
*** Now run the actual imputation. On a standard laptop, running 50 imputations with a burn-in period of 10 takes about an hour. Use 'dots' option to show progess, 'augment' to avoid perfect prediction of categorical variables (see White et al., 2010), and 'savetrace' to check convergence
*mi impute chained ///
*	(regress) ageAtBirth extra agree consc emoStab open iq iq8 negCogStyles17 ///
*	(pmm, knn(5)) mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_depScore10 ///
*		alc17 pliks17 friends17 ///
*	(logit) ethnic child_anx10 child_dep10 child_ACEs11 dep17 anx17 dep24 anx24 ///
*		behavDis13 smk15_17 drugs17 anti17 ///
*	(mlogit) home mum_edu dad_edu ///
*	(ologit) imd ///
*	= i.sex, ///
*	add(50) burnin(10) rseed(91865) dots augment replace ///
*	savetrace("./Results/imp_trace_MH17.dta", replace)

	
*** Save this imputed dataset, so not have to run whole imputation again to access results
*save "./Results/imp_MH17.dta", replace

* As noted above, if already created the imputed datasets from the age 24 script ("imp_MH17.dta" for main imputations and "imp_trace_MH24" for trace plots), can use this year as imputation model is the same


** Check convergence and that imputation chains are well-mixed
* Read in the trace dataset
*use "./Results/imp_trace_MH17.dta", clear
use "./Results/imp_trace_MH24.dta", clear

sum 

* Save the mean value to add as a line in the plot - Do this for all outcomes and exposures
sum extra_mean
local mean_extra = r(mean)
display `mean_extra'

sum agree_mean
local mean_agree = r(mean)
display `mean_agree'

sum consc_mean
local mean_consc = r(mean)
display `mean_consc'

sum emoStab_mean
local mean_emoStab = r(mean)
display `mean_emoStab'

sum open_mean
local mean_open = r(mean)
display `mean_open'

sum iq_mean
local mean_iq = r(mean)
display `mean_iq'

sum dep17_mean
local mean_dep17 = r(mean)
display `mean_dep17'

sum anx17_mean
local mean_anx17 = r(mean)
display `mean_anx17'


* Convert the data from long to wide format (is necessary to create the plots)
reshape wide *mean *sd, i(iter) j(m)

* Set the iteration variable as the 'time' variable
tsset iter

* Make the plots - These all look relatively well-mixed and converged
tsline extra_mean*, yline(`mean_extra') legend(off) name(extra, replace)
tsline agree_mean*, yline(`mean_agree') legend(off) name(agree, replace)
tsline consc_mean*, yline(`mean_consc') legend(off) name(consc, replace)
tsline emoStab_mean*, yline(`mean_emoStab') legend(off) name(emoStab, replace)
tsline open_mean*, yline(`mean_open') legend(off) name(open, replace)
tsline iq_mean*, yline(`mean_iq') legend(off) name(iq, replace)
tsline dep17_mean*, yline(`mean_dep17') legend(off) name(dep17, replace)
tsline anx17_mean*, yline(`mean_anx17') legend(off) name(anx17, replace)

graph close _all


******************************************************************************
*** Now run the models on the imputed data and combine using Rubin's Rules
*use "./Results/imp_MH17.dta", clear
use "./Results/imp_MH24.dta", clear


** Check descriptive stats of observed vs imputed data for exposures and outcomes
mi convert wide

* Extraversion (imputed data very similar to observed)
gen miss_extra = 1
replace miss_extra = 0 if extra < .
tab miss_extra, m

sum extra
mi estimate: mean extra if miss_extra == 1

* Agreeableness (imputed data very similar to observed)
gen miss_agree = 1
replace miss_agree = 0 if agree < .
tab miss_agree, m

sum agree
mi estimate: mean agree if miss_agree == 1

* Conscientiousness (imputed data very similar to observed)
gen miss_consc = 1
replace miss_consc = 0 if consc < .
tab miss_consc, m

sum consc
mi estimate: mean consc if miss_consc == 1

* Emotional stability (imputed data similar to observed, but slightly lower)
gen miss_emoStab = 1
replace miss_emoStab = 0 if emoStab < .
tab miss_emoStab, m

sum emoStab
mi estimate: mean emoStab if miss_emoStab == 1

* Openness (imputed data similar to observed, but slightly lower)
gen miss_open = 1
replace miss_open = 0 if open < .
tab miss_open, m

sum open
mi estimate: mean open if miss_open == 1

* IQ (imputed data a couple of IQ points lower than observed)
gen miss_iq = 1
replace miss_iq = 0 if iq < .
tab miss_iq, m

sum iq
mi estimate: mean iq if miss_iq == 1

* Depression at 17 (imputed data similar to observed, but marginally higher rates of depression)
gen miss_dep = 1
replace miss_dep = 0 if dep17 < .
tab miss_dep, m

tab dep17
mi estimate: proportion dep17 if miss_dep == 1

* Anxiety at 17 (imputed data similar to observed, but marginally higher rates of anxiety)
gen miss_anx = 1
replace miss_anx = 0 if anx17 < .
tab miss_anx, m

tab anx17
mi estimate: proportion anx17 if miss_anx == 1



**** Reset the data by reading in the imputed data again and run analysis models
*use "./Results/imp_MH17.dta", clear
use "./Results/imp_MH24.dta", clear


*** Start with depression as outcome

** Extraversion

* Unadjusted
mi estimate, or: logistic dep17 extra

* Adjusted
mi estimate, or: logistic dep17 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum extra if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(extra = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Agreeableness

* Unadjusted
mi estimate, or: logistic dep17 agree

* Adjusted
mi estimate, or: logistic dep17 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum agree if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(agree = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Conscientiousness

* Unadjusted
mi estimate, or: logistic dep17 consc

* Adjusted
mi estimate, or: logistic dep17 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum consc if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(consc = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Emotional stability

* Unadjusted
mi estimate, or: logistic dep17 emoStab

* Adjusted
mi estimate, or: logistic dep17 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum emoStab if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(emoStab = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Openness

* Unadjusted
mi estimate, or: logistic dep17 open

* Adjusted
mi estimate, or: logistic dep17 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum open if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(open = (`lower_sd', `mean', `upper_sd')) predict(pr)


** IQ/Cognitive ability

* Unadjusted
mi estimate, or: logistic dep17 iq

* Adjusted
mi estimate, or: logistic dep17 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum iq if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(iq = (`lower_sd', `mean', `upper_sd')) predict(pr)



*** Now with anxiety as outcome

** Extraversion

* Unadjusted
mi estimate, or: logistic anx17 extra

* Adjusted
mi estimate, or: logistic anx17 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum extra if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(extra = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Agreeableness

* Unadjusted
mi estimate, or: logistic anx17 agree

* Adjusted
mi estimate, or: logistic anx17 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum agree if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(agree = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Conscientiousness

* Unadjusted
mi estimate, or: logistic anx17 consc

* Adjusted
mi estimate, or: logistic anx17 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum consc if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(consc = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Emotional stability

* Unadjusted
mi estimate, or: logistic anx17 emoStab

* Adjusted
mi estimate, or: logistic anx17 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum emoStab if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(emoStab = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Openness

* Unadjusted
mi estimate, or: logistic anx17 open

* Adjusted
mi estimate, or: logistic anx17 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum open if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(open = (`lower_sd', `mean', `upper_sd')) predict(pr)


** IQ/Cognitive ability

* Unadjusted
mi estimate, or: logistic anx17 iq

* Adjusted
mi estimate, or: logistic anx17 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum iq if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(iq = (`lower_sd', `mean', `upper_sd')) predict(pr)



******************************************************************************
**** NARMICE 'tipping point' analysis - Depression outcome

** As outcome data may be MNAR (i.e., people with depression or anxiety at age 24 may be less likely to attend this clinic, even when conditional on all other covariates), will run a NARMICE 'tipping point' analysis to see how robust these results are to violations of the MAR assumption. Will explore delta values (the conditional log-odds difference in the outcome) from 0 to 2, in steps of 0.5 - Will focus on positive values as would expect mental health issues to decrease, not increase, rates of participation.

* Set up a postfile to store results
capture postclose dep17_NARMICE
postfile dep17_NARMICE str10 exposure delta msp imor prev or lci uci p ///
	using "./Results/dep17_NARMICE.dta", replace
	
** Loop through delta values from 0 to 2, in steps of 0.5
forvalues i = 0(0.5)2 {
	
	di "Delta = " `i'
	
	** Read in dataset
	use ".\updatedDataset_6.12.2023\PersonalityMH_B4294_processed.dta", clear
	
	* Create missingness indicator and offset for depression at 24 outcome (am not making missingness variables for all variables with missing data, as will be too many and often highly-correlated, which could lead to imputation issues)
	gen miss_dep = 0
	replace miss_dep = 1 if dep17 == .
	
	gen offset = `i' * miss_dep
	tab offset
	
	* Set up the dataset and imputed variables
	mi set mlong
	mi register regular sex
	mi register imputed ageAtBirth home mum_edu dad_edu imd ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_anx10 child_dep10 child_depScore10 child_ACEs11 extra agree consc emoStab open iq dep17 anx17 dep24 anx24 behavDis13 iq8 pliks17 smk15_17 alc17 friends17 negCogStyles17 drugs17 anti17
	
	* Run MI (to save time, will reduce number of imputations to 25 and burn-in to 5)
	mi impute chained ///
		(logit, offset(offset)) dep17 ///
		(regress) ageAtBirth extra agree consc emoStab open iq iq8 negCogStyles17 ///
		(pmm, knn(5)) mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_depScore10 ///
		alc17 pliks17 friends17 ///
		(logit) ethnic child_anx10 child_dep10 child_ACEs11 anx17 dep24 anx24 ///
		behavDis13 smk15_17 drugs17 anti17 ///
		(mlogit) home mum_edu dad_edu ///
		(ologit) imd ///
		= i.sex, ///
		add(25) burnin(5) rseed(75423) dots augment replace
		
	* Estimate marginal difference in log-odds between observed and imputed depression
	mi estimate: logistic dep17 miss_dep
	local msp = e(b_mi)[1,1]
	
	* Exponentiate to get ignorable missingness odds ratio (IMOR)
	local imor = exp(`msp')
	
	* Estimate prevalence of depression
	mi estimate: proportion dep17
	
	matrix res = r(table)
	local prev = res[1,2] * 100
		
	** Perform adjusted model for each exposure and store parameters
	
	* Extraversion
	local exp = "extra"
	
	mi estimate, or: logistic dep17 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Agreeableness
	local exp = "agree"
	
	mi estimate, or: logistic dep17 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Conscientiousness
	local exp = "consc"
	
	mi estimate, or: logistic dep17 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Emotional stability
	local exp = "emoStab"
	
	mi estimate, or: logistic dep17 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Openness
	local exp = "open"
	
	mi estimate, or: logistic dep17 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* IQ/Cognitive ability
	local exp = "iq"
	
	mi estimate, or: logistic dep17 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

}

postclose dep17_NARMICE



*** Save and plot the results of this tipping point analysis
use "./Results/dep17_NARMICE.dta", clear

list

* Export to CSV
export delimited using "./Results/dep17_NARMICE.csv", replace


** Make plots for each of the exposures

** Extraversion

* Plot using delta/CSP
twoway (connected or delta if exposure == "extra", sort) ///
  (line lci delta if exposure == "extra", lpattern(dash)) ///
  (line uci delta if exposure == "extra", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.96(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "extra", sort) ///
  (line lci imor if exposure == "extra", lpattern(dash)) ///
  (line uci imor if exposure == "extra", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.96(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "extra", sort) ///
  (line lci prev if exposure == "extra", lpattern(dash)) ///
  (line uci prev if exposure == "extra", lpattern(dash)), ///
  xlabel(8(2)18, labsize(large)) ///
  ylabel(0.96(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(7.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep17_NARMICE_extra.pdf", replace

  
** Agreeableness

* Plot using delta/CSP
twoway (connected or delta if exposure == "agree", sort) ///
  (line lci delta if exposure == "agree", lpattern(dash)) ///
  (line uci delta if exposure == "agree", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "agree", sort) ///
  (line lci imor if exposure == "agree", lpattern(dash)) ///
  (line uci imor if exposure == "agree", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "agree", sort) ///
  (line lci prev if exposure == "agree", lpattern(dash)) ///
  (line uci prev if exposure == "agree", lpattern(dash)), ///
  xlabel(8(2)18, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(7.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep17_NARMICE_agree.pdf", replace


** Conscientiousness

* Plot using delta/CSP
twoway (connected or delta if exposure == "consc", sort) ///
  (line lci delta if exposure == "consc", lpattern(dash)) ///
  (line uci delta if exposure == "consc", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "consc", sort) ///
  (line lci imor if exposure == "consc", lpattern(dash)) ///
  (line uci imor if exposure == "consc", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "consc", sort) ///
  (line lci prev if exposure == "consc", lpattern(dash)) ///
  (line uci prev if exposure == "consc", lpattern(dash)), ///
  xlabel(8(2)18, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(7.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep17_NARMICE_consc.pdf", replace


** Emotional stability

* Plot using delta/CSP
twoway (connected or delta if exposure == "emoStab", sort) ///
  (line lci delta if exposure == "emoStab", lpattern(dash)) ///
  (line uci delta if exposure == "emoStab", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.9(0.02)0.98, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "emoStab", sort) ///
  (line lci imor if exposure == "emoStab", lpattern(dash)) ///
  (line uci imor if exposure == "emoStab", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.9(0.02)0.98, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "emoStab", sort) ///
  (line lci prev if exposure == "emoStab", lpattern(dash)) ///
  (line uci prev if exposure == "emoStab", lpattern(dash)), ///
  xlabel(8(2)18, labsize(large)) ///
  ylabel(0.9(0.02)0.98, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(7.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep17_NARMICE_emoStab.pdf", replace


** Openness

* Plot using delta/CSP
twoway (connected or delta if exposure == "open", sort) ///
  (line lci delta if exposure == "open", lpattern(dash)) ///
  (line uci delta if exposure == "open", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.98(0.02)1.1, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "open", sort) ///
  (line lci imor if exposure == "open", lpattern(dash)) ///
  (line uci imor if exposure == "open", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.98(0.02)1.1, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "open", sort) ///
  (line lci prev if exposure == "open", lpattern(dash)) ///
  (line uci prev if exposure == "open", lpattern(dash)), ///
  xlabel(8(2)18, labsize(large)) ///
  ylabel(0.98(0.02)1.1, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(7.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep17_NARMICE_open.pdf", replace
  

** IQ

* Plot using delta/CSP
twoway (connected or delta if exposure == "iq", sort) ///
  (line lci delta if exposure == "iq", lpattern(dash)) ///
  (line uci delta if exposure == "iq", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.96(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "iq", sort) ///
  (line lci imor if exposure == "iq", lpattern(dash)) ///
  (line uci imor if exposure == "iq", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.96(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "iq", sort) ///
  (line lci prev if exposure == "iq", lpattern(dash)) ///
  (line uci prev if exposure == "iq", lpattern(dash)), ///
  xlabel(8(2)18, labsize(large)) ///
  ylabel(0.96(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(7.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep17_NARMICE_iq.pdf", replace

graph close _all



******************************************************************************
**** NARMICE 'tipping point' analysis - Anxiety outcome

** As outcome data may be MNAR (i.e., people with depression or anxiety at age 24 may be less likely to attend this clinic, even when conditional on all other covariates), will run a NARMICE 'tipping point' analysis to see how robust these results are to violations of the MAR assumption. Will explore delta values (the conditional log-odds difference in the outcome) from 0 to 2, in steps of 0.5 - Will focus on positive values as would expect mental health issues to decrease, not increase, rates of participation.

* Set up a postfile to store results
capture postclose anx17_NARMICE
postfile anx17_NARMICE str10 exposure delta msp imor prev or lci uci p ///
	using "./Results/anx17_NARMICE.dta", replace
	
** Loop through delta values from 0 to 2, in steps of 0.5
forvalues i = 0(0.5)2 {
	
	di "Delta = " `i'
	
	** Read in dataset
	use ".\updatedDataset_6.12.2023\PersonalityMH_B4294_processed.dta", clear
	
	* Create missingness indicator and offset for anxiety at 24 outcome (am not making missingness variables for all variables with missing data, as will be too many and often highly-correlated, which could lead to imputation issues)
	gen miss_anx = 0
	replace miss_anx = 1 if anx17 == .
	
	gen offset = `i' * miss_anx
	tab offset
	
	* Set up the dataset and imputed variables
	mi set mlong
	mi register regular sex
	mi register imputed ageAtBirth home mum_edu dad_edu imd ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_anx10 child_dep10 child_depScore10 child_ACEs11 extra agree consc emoStab open iq dep17 anx17 dep24 anx24 behavDis13 iq8 pliks17 smk15_17 alc17 friends17 negCogStyles17 drugs17 anti17
	
	* Run MI (to save time, will reduce number of imputations to 25 and burn-in to 5)
	mi impute chained ///
		(logit, offset(offset)) anx17 ///
		(regress) ageAtBirth extra agree consc emoStab open iq iq8 negCogStyles17 ///
		(pmm, knn(5)) mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_depScore10 ///
		alc17 pliks17 friends17 ///
		(logit) ethnic child_anx10 child_dep10 child_ACEs11 dep17 dep24 anx24 ///
		behavDis13 smk15_17 drugs17 anti17 ///
		(mlogit) home mum_edu dad_edu ///
		(ologit) imd ///
		= i.sex, ///
		add(25) burnin(5) rseed(864581) dots augment replace
		
	* Estimate marginal difference in log-odds between observed and imputed depression
	mi estimate: logistic anx17 miss_anx
	local msp = e(b_mi)[1,1]
	
	* Exponentiate to get ignorable missingness odds ratio (IMOR)
	local imor = exp(`msp')
	
	* Estimate prevalence of depression
	mi estimate: proportion anx17
	
	matrix res = r(table)
	local prev = res[1,2] * 100
		
	** Perform adjusted model for each exposure and store parameters
	
	* Extraversion
	local exp = "extra"
	
	mi estimate, or: logistic anx17 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Agreeableness
	local exp = "agree"
	
	mi estimate, or: logistic anx17 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Conscientiousness
	local exp = "consc"
	
	mi estimate, or: logistic anx17 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Emotional stability
	local exp = "emoStab"
	
	mi estimate, or: logistic anx17 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Openness
	local exp = "open"
	
	mi estimate, or: logistic anx17 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* IQ/Cognitive ability
	local exp = "iq"
	
	mi estimate, or: logistic anx17 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx17_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

}

postclose anx17_NARMICE



*** Save and plot the results of this tipping point analysis
use "./Results/anx17_NARMICE.dta", clear

list

* Export to CSV
export delimited using "./Results/anx17_NARMICE.csv", replace


** Make plots for each of the exposures

** Extraversion

* Plot using delta/CSP
twoway (connected or delta if exposure == "extra", sort) ///
  (line lci delta if exposure == "extra", lpattern(dash)) ///
  (line uci delta if exposure == "extra", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.92(0.02)1, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "extra", sort) ///
  (line lci imor if exposure == "extra", lpattern(dash)) ///
  (line uci imor if exposure == "extra", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.92(0.02)1, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "extra", sort) ///
  (line lci prev if exposure == "extra", lpattern(dash)) ///
  (line uci prev if exposure == "extra", lpattern(dash)), ///
  xlabel(6(2)14, labsize(large)) xscale(range(5.5, 14.5)) ///
  ylabel(0.92(0.02)1, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(5.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx17_NARMICE_extra.pdf", replace

  
** Agreeableness

* Plot using delta/CSP
twoway (connected or delta if exposure == "agree", sort) ///
  (line lci delta if exposure == "agree", lpattern(dash)) ///
  (line uci delta if exposure == "agree", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.96(0.02)1.06, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "agree", sort) ///
  (line lci imor if exposure == "agree", lpattern(dash)) ///
  (line uci imor if exposure == "agree", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.96(0.02)1.06, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "agree", sort) ///
  (line lci prev if exposure == "agree", lpattern(dash)) ///
  (line uci prev if exposure == "agree", lpattern(dash)), ///
  xlabel(6(2)14, labsize(large)) xscale(range(5.5, 14.5)) ///
  ylabel(0.96(0.02)1.06, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(5.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx17_NARMICE_agree.pdf", replace


** Conscientiousness

* Plot using delta/CSP
twoway (connected or delta if exposure == "consc", sort) ///
  (line lci delta if exposure == "consc", lpattern(dash)) ///
  (line uci delta if exposure == "consc", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.92(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "consc", sort) ///
  (line lci imor if exposure == "consc", lpattern(dash)) ///
  (line uci imor if exposure == "consc", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.92(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "consc", sort) ///
  (line lci prev if exposure == "consc", lpattern(dash)) ///
  (line uci prev if exposure == "consc", lpattern(dash)), ///
  xlabel(6(2)14, labsize(large)) xscale(range(5.5, 14.5)) ///
  ylabel(0.92(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(5.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx17_NARMICE_consc.pdf", replace


** Emotional stability

* Plot using delta/CSP
twoway (connected or delta if exposure == "emoStab", sort) ///
  (line lci delta if exposure == "emoStab", lpattern(dash)) ///
  (line uci delta if exposure == "emoStab", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.88(0.02)0.96, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "emoStab", sort) ///
  (line lci imor if exposure == "emoStab", lpattern(dash)) ///
  (line uci imor if exposure == "emoStab", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.88(0.02)0.96, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "emoStab", sort) ///
  (line lci prev if exposure == "emoStab", lpattern(dash)) ///
  (line uci prev if exposure == "emoStab", lpattern(dash)), ///
  xlabel(6(2)14, labsize(large)) xscale(range(5.5, 14.5)) ///
  ylabel(0.88(0.02)0.96, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(5.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx17_NARMICE_emoStab.pdf", replace


** Openness

* Plot using delta/CSP
twoway (connected or delta if exposure == "open", sort) ///
  (line lci delta if exposure == "open", lpattern(dash)) ///
  (line uci delta if exposure == "open", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.96(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "open", sort) ///
  (line lci imor if exposure == "open", lpattern(dash)) ///
  (line uci imor if exposure == "open", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.96(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "open", sort) ///
  (line lci prev if exposure == "open", lpattern(dash)) ///
  (line uci prev if exposure == "open", lpattern(dash)), ///
  xlabel(6(2)14, labsize(large)) xscale(range(5.5, 14.5)) ///
  ylabel(0.96(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(5.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx17_NARMICE_open.pdf", replace
  

** IQ

* Plot using delta/CSP
twoway (connected or delta if exposure == "iq", sort) ///
  (line lci delta if exposure == "iq", lpattern(dash)) ///
  (line uci delta if exposure == "iq", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.96(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "iq", sort) ///
  (line lci imor if exposure == "iq", lpattern(dash)) ///
  (line uci imor if exposure == "iq", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.96(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "iq", sort) ///
  (line lci prev if exposure == "iq", lpattern(dash)) ///
  (line uci prev if exposure == "iq", lpattern(dash)), ///
  xlabel(6(2)14, labsize(large)) xscale(range(5.5, 14.5)) ///
  ylabel(0.96(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(5.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx17_NARMICE_iq.pdf", replace

graph close _all


log close


********************************************************************************
*** Make plots of age 17 and age 24 results from adjusted MI analyses

* Read in the csv file
import delimited using "./Results/ResultsForPlot.csv", clear

list


** Convert string vars to numeric to save order on graph

* Exposures
gen exp_num = 0
replace exp_num = 1 if exposure == "extra"
replace exp_num = 2 if exposure == "agree"
replace exp_num = 3 if exposure == "consc"
replace exp_num = 4 if exposure == "emoStab"
replace exp_num = 5 if exposure == "open"
replace exp_num = 6 if exposure == "iq"

label define exp_lb 1 "Extraversion" 2 "Agreeableness" 3 "Conscientiousness" ///
	4 "Emotional Stability" 5 "Openness to Experience" 6 "Cognitive ability"
label values exp_num exp_lb
tab exp_num

* Split apart the outcomes for each exposure
gen exp_split = .
replace exp_split = exp_num - 0.2 if outcome == "dep"
replace exp_split = exp_num + 0.2 if outcome == "anx"
tab exp_split

* Outcomes
gen out_num = 0
replace out_num = 1 if outcome == "dep"
replace out_num = 2 if outcome == "anx"

label define out_lb 1 "Depression" 2 "Anxiety"
label values out_num out_lb
tab out_num


*** Make a plot for each time-point, then combine together
twoway (scatter exp_split or if age == 24 & outcome == "dep", ///
		col(black) msize(small) msym(D)) ///
	(rspike lci uci exp_split if age == 24 & outcome == "dep", ///
		horizontal col(black)) ///
	(scatter exp_split or if age == 24 & outcome == "anx", ///
		col(red) msize(small) msym(D)) ///
	(rspike lci uci exp_split if age == 24 & outcome == "anx", ///
		horizontal col(red)), ///
	yscale(reverse) title("Mental health at age 24") ytitle("") ///
	xtitle("Odds ratio") xscale(range(0.9 1.1)) xlabel(0.9(0.02)1.1) ///
	xline(1, lcol(black) lpattern(shortdash) lwidth(thin)) ///
	ylabel(1 "Extraversion" 2 "Agreeableness" 3 "Conscientiousness" ///
		4 "Emotional Stability" 5 "Openness to Experience" 6 "Cognitive Ability", ///
		labsize(medium) angle(0)) ///
	legend(order(1 "Depression" 3 "Anxiety") cols(2) position(6)) ///
	name(age24, replace)
	
twoway (scatter exp_split or if age == 17 & outcome == "dep", ///
		col(black) msize(small) msym(D)) ///
	(rspike lci uci exp_split if age == 17 & outcome == "dep", ///
		horizontal col(black)) ///
	(scatter exp_split or if age == 17 & outcome == "anx", ///
		col(red) msize(small) msym(D)) ///
	(rspike lci uci exp_split if age == 17 & outcome == "anx", ///
		horizontal col(red)), ///
	yscale(reverse) title("Mental health at age 17") ytitle("") ///
	xtitle("Odds ratio") xscale(range(0.9 1.1)) xlabel(0.9(0.02)1.1) ///
	xline(1, lcol(black) lpattern(shortdash) lwidth(thin)) ///
	ylabel(1 "Extraversion" 2 "Agreeableness" 3 "Conscientiousness" ///
		4 "Emotional Stability" 5 "Openness to Experience" 6 "Cognitive Ability", ///
		labsize(medium) angle(0)) ///
	legend(order(1 "Depression" 3 "Anxiety") cols(2) position(6)) ///
	name(age17, replace)


** USe 'grc1leg' package to get single legend on combined graphs
*ssc install grc1leg, replace

grc1leg age24 age17, rows(1) imargin(tiny) ycommon xcommon
graph display, ysize(8) xsize(20)

graph export "./Results/ResultsCombined.pdf", replace

graph close _all