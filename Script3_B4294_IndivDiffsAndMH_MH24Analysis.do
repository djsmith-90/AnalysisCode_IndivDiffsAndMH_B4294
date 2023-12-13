*** PROJECT: The role of individual differences in later mental health (B4294)
*** STATA VERSION 18
*** Script 3: Analysing mental health at age 24 outcomes
*** Created 7/12/2023 by Dan Major-Smith (based on script by Tapasya Bhardwaj)

** Set working directory
cd "X:\Studies\RSBB Team\Dan\B4294 - MSc Personality MH"

** Install any user-written packages
*ssc install missings, replace
*ssc install mimrgns, replace

** Create log file
capture log close
log using ".\Results\IndivDiffsAndMH_MH24_Results.log", replace

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
tab dep24 if cca == 1 & extra != .

* Unadjusted
logistic dep24 extra if cca == 1

* Adjusted
logistic dep24 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum extra if cca == 1 & dep24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


** Agreeableness
tab dep24 if cca == 1 & agree != .

* Unadjusted
logistic dep24 agree if cca == 1

* Adjusted
logistic dep24 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum agree if cca == 1 & dep24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


** Conscientiousness
tab dep24 if cca == 1 & consc != .

* Unadjusted
logistic dep24 consc if cca == 1

* Adjusted
logistic dep24 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum consc if cca == 1 & dep24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


** Emotional stability
tab dep24 if cca == 1 & emoStab != .

* Unadjusted
logistic dep24 emoStab if cca == 1

* Adjusted
logistic dep24 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum emoStab if cca == 1 & dep24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


** Openness
tab dep24 if cca == 1 & open != .

* Unadjusted
logistic dep24 open if cca == 1

* Adjusted
logistic dep24 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum open if cca == 1 & dep24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


** IQ/Cognitive ability
tab dep24 if cca == 1 & iq != .

* Unadjusted
logistic dep24 iq if cca == 1

* Adjusted
logistic dep24 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum iq if cca == 1 & dep24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))



*** Now with anxiety as outcome

** Extraversion
tab anx24 if cca == 1 & extra != .

* Unadjusted
logistic anx24 extra if cca == 1

* Adjusted
logistic anx24 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum extra if cca == 1 & anx24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(extra = (`lower_sd', `mean', `upper_sd'))


** Agreeableness
tab anx24 if cca == 1 & agree != .

* Unadjusted
logistic anx24 agree if cca == 1

* Adjusted
logistic anx24 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum agree if cca == 1 & anx24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(agree = (`lower_sd', `mean', `upper_sd'))


** Conscientiousness
tab anx24 if cca == 1 & consc != .

* Unadjusted
logistic anx24 consc if cca == 1

* Adjusted
logistic anx24 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum consc if cca == 1 & anx24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(consc = (`lower_sd', `mean', `upper_sd'))


** Emotional stability
tab anx24 if cca == 1 & emoStab != .

* Unadjusted
logistic anx24 emoStab if cca == 1

* Adjusted
logistic anx24 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum emoStab if cca == 1 & anx24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(emoStab = (`lower_sd', `mean', `upper_sd'))


** Openness
tab anx24 if cca == 1 & open != .

* Unadjusted
logistic anx24 open if cca == 1

* Adjusted
logistic anx24 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum open if cca == 1 & anx24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(open = (`lower_sd', `mean', `upper_sd'))


** IQ/Cognitive ability
tab anx24 if cca == 1 & iq != .

* Unadjusted
logistic anx24 iq if cca == 1

* Adjusted
logistic anx24 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

* Predicted probabilities
sum iq if cca == 1 & anx24 != .
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
margins, at(iq = (`lower_sd', `mean', `upper_sd'))



********************************************************************************
*** Now run multiple imputation (assuming MAR)

** Set up the imputation and register complete and missing variables
missings report _all, percent

mi set mlong
mi register regular sex
mi register imputed ageAtBirth home mum_edu dad_edu imd ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_anx10 child_dep10 child_depScore10 child_ACEs11 extra agree consc emoStab open iq dep17 anx17 dep24 anx24 behavDis13 iq8 pliks17 smk15_17 alc17 friends17 negCogStyles17 drugs17 anti17

** Test imputation with a dry-run first
mi impute chained ///
	(regress) ageAtBirth extra agree consc emoStab open iq iq8 negCogStyles17 ///
	(pmm, knn(5)) mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_depScore10 ///
		alc17 pliks17 friends17 ///
	(logit) ethnic child_anx10 child_dep10 child_ACEs11 dep17 anx17 dep24 anx24 ///
		behavDis13 smk15_17 drugs17 anti17 ///
	(mlogit) home mum_edu dad_edu ///
	(ologit) imd ///
	= i.sex, ///
	add(50) burnin(10) rseed(91865) dryrun
	
	
** Now run the actual imputation. On a standard laptop, running 50 imputations with a burn-in period of 10 takes about an hour. Use 'dots' option to show progess, 'augment' to avoid perfect prediction of categorical variables (see White et al., 2010), and 'savetrace' to check convergence
mi impute chained ///
	(regress) ageAtBirth extra agree consc emoStab open iq iq8 negCogStyles17 ///
	(pmm, knn(5)) mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_depScore10 ///
		alc17 pliks17 friends17 ///
	(logit) ethnic child_anx10 child_dep10 child_ACEs11 dep17 anx17 dep24 anx24 ///
		behavDis13 smk15_17 drugs17 anti17 ///
	(mlogit) home mum_edu dad_edu ///
	(ologit) imd ///
	= i.sex, ///
	add(50) burnin(10) rseed(91865) dots augment replace ///
	savetrace("./Results/imp_trace_MH24.dta", replace)

	
** Save this imputed dataset, so not have to run whole imputation again to access results
save "./Results/imp_MH24.dta", replace


** Check convergence and that imputation chains are well-mixed
* Read in the trace dataset
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

sum dep24_mean
local mean_dep24 = r(mean)
display `mean_dep24'

sum anx24_mean
local mean_anx24 = r(mean)
display `mean_anx24'


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
tsline dep24_mean*, yline(`mean_dep24') legend(off) name(dep24, replace)
tsline anx24_mean*, yline(`mean_anx24') legend(off) name(anx24, replace)

graph close _all


******************************************************************************
*** Now run the models on the imputed data and combine using Rubin's Rules
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

* Depression at 24 (imputed data similar to observed)
gen miss_dep = 1
replace miss_dep = 0 if dep24 < .
tab miss_dep, m

tab dep24
mi estimate: proportion dep24 if miss_dep == 1

* Anxiety at 24 (imputed data similar to observed, but marginally lower rates of anxiety)
gen miss_anx = 1
replace miss_anx = 0 if anx24 < .
tab miss_anx, m

tab anx24
mi estimate: proportion anx24 if miss_anx == 1



**** Reset the data by reading in the imputed data again and run analysis models
use "./Results/imp_MH24.dta", clear


*** Start with depression as outcome

** Extraversion

* Unadjusted
mi estimate, or: logistic dep24 extra

* Adjusted
mi estimate, or: logistic dep24 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum extra if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(extra = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Agreeableness

* Unadjusted
mi estimate, or: logistic dep24 agree

* Adjusted
mi estimate, or: logistic dep24 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum agree if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(agree = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Conscientiousness

* Unadjusted
mi estimate, or: logistic dep24 consc

* Adjusted
mi estimate, or: logistic dep24 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum consc if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(consc = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Emotional stability

* Unadjusted
mi estimate, or: logistic dep24 emoStab

* Adjusted
mi estimate, or: logistic dep24 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum emoStab if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(emoStab = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Openness

* Unadjusted
mi estimate, or: logistic dep24 open

* Adjusted
mi estimate, or: logistic dep24 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum open if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(open = (`lower_sd', `mean', `upper_sd')) predict(pr)


** IQ/Cognitive ability

* Unadjusted
mi estimate, or: logistic dep24 iq

* Adjusted
mi estimate, or: logistic dep24 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum iq if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(iq = (`lower_sd', `mean', `upper_sd')) predict(pr)



*** Now with anxiety as outcome

** Extraversion

* Unadjusted
mi estimate, or: logistic anx24 extra

* Adjusted
mi estimate, or: logistic anx24 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum extra if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(extra = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Agreeableness

* Unadjusted
mi estimate, or: logistic anx24 agree

* Adjusted
mi estimate, or: logistic anx24 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum agree if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(agree = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Conscientiousness

* Unadjusted
mi estimate, or: logistic anx24 consc

* Adjusted
mi estimate, or: logistic anx24 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum consc if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(consc = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Emotional stability

* Unadjusted
mi estimate, or: logistic anx24 emoStab

* Adjusted
mi estimate, or: logistic anx24 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum emoStab if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(emoStab = (`lower_sd', `mean', `upper_sd')) predict(pr)


** Openness

* Unadjusted
mi estimate, or: logistic anx24 open

* Adjusted
mi estimate, or: logistic anx24 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

sum open if _mi_m == 0
local mean = r(mean)
local lower_sd = r(mean) - r(sd)
local upper_sd = r(mean) + r(sd)
di "1 SD lower = " `lower_sd' "; Mean = " `mean' "; 1 SD higher = " `upper_sd'
mimrgns, at(open = (`lower_sd', `mean', `upper_sd')) predict(pr)


** IQ/Cognitive ability

* Unadjusted
mi estimate, or: logistic anx24 iq

* Adjusted
mi estimate, or: logistic anx24 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11

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
capture postclose dep24_NARMICE
postfile dep24_NARMICE str10 exposure delta msp imor prev or lci uci p ///
	using "./Results/dep24_NARMICE.dta", replace
	
** Loop through delta values from 0 to 2, in steps of 0.5
forvalues i = 0(0.5)2 {
	
	di "Delta = " `i'
	
	** Read in dataset
	use ".\updatedDataset_6.12.2023\PersonalityMH_B4294_processed.dta", clear
	
	* Create missingness indicator and offset for depression at 24 outcome (am not making missingness variables for all variables with missing data, as will be too many and often highly-correlated, which could lead to imputation issues)
	gen miss_dep = 0
	replace miss_dep = 1 if dep24 == .
	
	gen offset = `i' * miss_dep
	tab offset
	
	* Set up the dataset and imputed variables
	mi set mlong
	mi register regular sex
	mi register imputed ageAtBirth home mum_edu dad_edu imd ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_anx10 child_dep10 child_depScore10 child_ACEs11 extra agree consc emoStab open iq dep17 anx17 dep24 anx24 behavDis13 iq8 pliks17 smk15_17 alc17 friends17 negCogStyles17 drugs17 anti17
	
	* Run MI (to save time, will reduce number of imputations to 25 and burn-in to 5)
	mi impute chained ///
		(logit, offset(offset)) dep24 ///
		(regress) ageAtBirth extra agree consc emoStab open iq iq8 negCogStyles17 ///
		(pmm, knn(5)) mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_depScore10 ///
		alc17 pliks17 friends17 ///
		(logit) ethnic child_anx10 child_dep10 child_ACEs11 dep17 anx17 anx24 ///
		behavDis13 smk15_17 drugs17 anti17 ///
		(mlogit) home mum_edu dad_edu ///
		(ologit) imd ///
		= i.sex, ///
		add(25) burnin(5) rseed(35761) dots augment replace
		
	* Estimate marginal difference in log-odds between observed and imputed depression
	mi estimate: logistic dep24 miss_dep
	local msp = e(b_mi)[1,1]
	
	* Exponentiate to get ignorable missingness odds ratio (IMOR)
	local imor = exp(`msp')
	
	* Estimate prevalence of depression
	mi estimate: proportion dep24
	
	matrix res = r(table)
	local prev = res[1,2] * 100
		
	** Perform adjusted model for each exposure and store parameters
	
	* Extraversion
	local exp = "extra"
	
	mi estimate, or: logistic dep24 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Agreeableness
	local exp = "agree"
	
	mi estimate, or: logistic dep24 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Conscientiousness
	local exp = "consc"
	
	mi estimate, or: logistic dep24 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Emotional stability
	local exp = "emoStab"
	
	mi estimate, or: logistic dep24 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Openness
	local exp = "open"
	
	mi estimate, or: logistic dep24 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* IQ/Cognitive ability
	local exp = "iq"
	
	mi estimate, or: logistic dep24 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post dep24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

}

postclose dep24_NARMICE



*** Save and plot the results of this tipping point analysis
use "./Results/dep24_NARMICE.dta", clear

list

* Export to CSV
export delimited using "./Results/dep24_NARMICE.csv", replace


** Make plots for each of the exposures

** Extraversion

* Plot using delta/CSP
twoway (connected or delta if exposure == "extra", sort) ///
  (line lci delta if exposure == "extra", lpattern(dash)) ///
  (line uci delta if exposure == "extra", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.92(0.02)1, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "extra", sort) ///
  (line lci imor if exposure == "extra", lpattern(dash)) ///
  (line uci imor if exposure == "extra", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.92(0.02)1, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "extra", sort) ///
  (line lci prev if exposure == "extra", lpattern(dash)) ///
  (line uci prev if exposure == "extra", lpattern(dash)), ///
  xlabel(10(2)26, labsize(large)) ///
  ylabel(0.92(0.02)1, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(10.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep24_NARMICE_extra.pdf", replace

  
** Agreeableness

* Plot using delta/CSP
twoway (connected or delta if exposure == "agree", sort) ///
  (line lci delta if exposure == "agree", lpattern(dash)) ///
  (line uci delta if exposure == "agree", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.98(0.02)1.06, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "agree", sort) ///
  (line lci imor if exposure == "agree", lpattern(dash)) ///
  (line uci imor if exposure == "agree", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.98(0.02)1.06, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "agree", sort) ///
  (line lci prev if exposure == "agree", lpattern(dash)) ///
  (line uci prev if exposure == "agree", lpattern(dash)), ///
  xlabel(10(2)26, labsize(large)) ///
  ylabel(0.98(0.02)1.06, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(10.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep24_NARMICE_agree.pdf", replace


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
  xlabel(10(2)26, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(10.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep24_NARMICE_consc.pdf", replace


** Emotional stability

* Plot using delta/CSP
twoway (connected or delta if exposure == "emoStab", sort) ///
  (line lci delta if exposure == "emoStab", lpattern(dash)) ///
  (line uci delta if exposure == "emoStab", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.9(0.02)1, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "emoStab", sort) ///
  (line lci imor if exposure == "emoStab", lpattern(dash)) ///
  (line uci imor if exposure == "emoStab", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.9(0.02)1, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "emoStab", sort) ///
  (line lci prev if exposure == "emoStab", lpattern(dash)) ///
  (line uci prev if exposure == "emoStab", lpattern(dash)), ///
  xlabel(10(2)26, labsize(large)) ///
  ylabel(0.9(0.02)1, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(10.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep24_NARMICE_emoStab.pdf", replace


** Openness

* Plot using delta/CSP
twoway (connected or delta if exposure == "open", sort) ///
  (line lci delta if exposure == "open", lpattern(dash)) ///
  (line uci delta if exposure == "open", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Delta for depression (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "open", sort) ///
  (line lci imor if exposure == "open", lpattern(dash)) ///
  (line uci imor if exposure == "open", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("IMOR for depression (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "open", sort) ///
  (line lci prev if exposure == "open", lpattern(dash)) ///
  (line uci prev if exposure == "open", lpattern(dash)), ///
  xlabel(10(2)26, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(10.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep24_NARMICE_open.pdf", replace
  

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
  xlabel(10(2)26, labsize(large)) ///
  ylabel(0.96(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for depression", size(vlarge)) ///
  xtitle("Prevalence of depression (%)", size(vlarge)) ///
  legend(off) xline(10.9) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/dep24_NARMICE_iq.pdf", replace

graph close _all



******************************************************************************
**** NARMICE 'tipping point' analysis - Anxiety outcome

** As outcome data may be MNAR (i.e., people with depression or anxiety at age 24 may be less likely to attend this clinic, even when conditional on all other covariates), will run a NARMICE 'tipping point' analysis to see how robust these results are to violations of the MAR assumption. Will explore delta values (the conditional log-odds difference in the outcome) from 0 to 2, in steps of 0.5 - Will focus on positive values as would expect mental health issues to decrease, not increase, rates of participation.

* Set up a postfile to store results
capture postclose anx24_NARMICE
postfile anx24_NARMICE str10 exposure delta msp imor prev or lci uci p ///
	using "./Results/anx24_NARMICE.dta", replace
	
** Loop through delta values from 0 to 2, in steps of 0.5
forvalues i = 0(0.5)2 {
	
	di "Delta = " `i'
	
	** Read in dataset
	use ".\updatedDataset_6.12.2023\PersonalityMH_B4294_processed.dta", clear
	
	* Create missingness indicator and offset for anxiety at 24 outcome (am not making missingness variables for all variables with missing data, as will be too many and often highly-correlated, which could lead to imputation issues)
	gen miss_anx = 0
	replace miss_anx = 1 if anx24 == .
	
	gen offset = `i' * miss_anx
	tab offset
	
	* Set up the dataset and imputed variables
	mi set mlong
	mi register regular sex
	mi register imputed ageAtBirth home mum_edu dad_edu imd ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_anx10 child_dep10 child_depScore10 child_ACEs11 extra agree consc emoStab open iq dep17 anx17 dep24 anx24 behavDis13 iq8 pliks17 smk15_17 alc17 friends17 negCogStyles17 drugs17 anti17
	
	* Run MI (to save time, will reduce number of imputations to 25 and burn-in to 5)
	mi impute chained ///
		(logit, offset(offset)) anx24 ///
		(regress) ageAtBirth extra agree consc emoStab open iq iq8 negCogStyles17 ///
		(pmm, knn(5)) mum_dep mum_anx dad_dep dad_anx child_SDQ9 child_depScore10 ///
		alc17 pliks17 friends17 ///
		(logit) ethnic child_anx10 child_dep10 child_ACEs11 dep17 anx17 dep24 ///
		behavDis13 smk15_17 drugs17 anti17 ///
		(mlogit) home mum_edu dad_edu ///
		(ologit) imd ///
		= i.sex, ///
		add(25) burnin(5) rseed(239684) dots augment replace
		
	* Estimate marginal difference in log-odds between observed and imputed depression
	mi estimate: logistic anx24 miss_anx
	local msp = e(b_mi)[1,1]
	
	* Exponentiate to get ignorable missingness odds ratio (IMOR)
	local imor = exp(`msp')
	
	* Estimate prevalence of depression
	mi estimate: proportion anx24
	
	matrix res = r(table)
	local prev = res[1,2] * 100
		
	** Perform adjusted model for each exposure and store parameters
	
	* Extraversion
	local exp = "extra"
	
	mi estimate, or: logistic anx24 extra ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Agreeableness
	local exp = "agree"
	
	mi estimate, or: logistic anx24 agree ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Conscientiousness
	local exp = "consc"
	
	mi estimate, or: logistic anx24 consc ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Emotional stability
	local exp = "emoStab"
	
	mi estimate, or: logistic anx24 emoStab ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* Openness
	local exp = "open"
	
	mi estimate, or: logistic anx24 open ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

	* IQ/Cognitive ability
	local exp = "iq"
	
	mi estimate, or: logistic anx24 iq ib1.sex ageAtBirth ib1.home ib1.mum_edu ib1.dad_edu ib1.imd ib0.ethnic mum_dep mum_anx dad_dep dad_anx child_SDQ9 ib0.child_anx10 ib0.child_dep10 child_depScore10 ib0.child_ACEs11
	
	matrix res = r(table)
	local or = res[1,1]
	local lci = res[5,1]
	local uci = res[6,1]
	local p = res[4,1]
	
	post anx24_NARMICE ("`exp'") (`i') (`msp') (`imor') (`prev') (`or') (`lci') (`uci') (`p')

}

postclose anx24_NARMICE



*** Save and plot the results of this tipping point analysis
use "./Results/anx24_NARMICE.dta", clear

list

* Export to CSV
export delimited using "./Results/anx24_NARMICE.csv", replace


** Make plots for each of the exposures

** Extraversion

* Plot using delta/CSP
twoway (connected or delta if exposure == "extra", sort) ///
  (line lci delta if exposure == "extra", lpattern(dash)) ///
  (line uci delta if exposure == "extra", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "extra", sort) ///
  (line lci imor if exposure == "extra", lpattern(dash)) ///
  (line uci imor if exposure == "extra", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "extra", sort) ///
  (line lci prev if exposure == "extra", lpattern(dash)) ///
  (line uci prev if exposure == "extra", lpattern(dash)), ///
  xlabel(8(2)22, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(9.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx24_NARMICE_extra.pdf", replace

  
** Agreeableness

* Plot using delta/CSP
twoway (connected or delta if exposure == "agree", sort) ///
  (line lci delta if exposure == "agree", lpattern(dash)) ///
  (line uci delta if exposure == "agree", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "agree", sort) ///
  (line lci imor if exposure == "agree", lpattern(dash)) ///
  (line uci imor if exposure == "agree", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "agree", sort) ///
  (line lci prev if exposure == "agree", lpattern(dash)) ///
  (line uci prev if exposure == "agree", lpattern(dash)), ///
  xlabel(8(2)22, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(9.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx24_NARMICE_agree.pdf", replace


** Conscientiousness

* Plot using delta/CSP
twoway (connected or delta if exposure == "consc", sort) ///
  (line lci delta if exposure == "consc", lpattern(dash)) ///
  (line uci delta if exposure == "consc", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "consc", sort) ///
  (line lci imor if exposure == "consc", lpattern(dash)) ///
  (line uci imor if exposure == "consc", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "consc", sort) ///
  (line lci prev if exposure == "consc", lpattern(dash)) ///
  (line uci prev if exposure == "consc", lpattern(dash)), ///
  xlabel(8(2)22, labsize(large)) ///
  ylabel(0.94(0.02)1.02, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(9.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx24_NARMICE_consc.pdf", replace


** Emotional stability

* Plot using delta/CSP
twoway (connected or delta if exposure == "emoStab", sort) ///
  (line lci delta if exposure == "emoStab", lpattern(dash)) ///
  (line uci delta if exposure == "emoStab", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.9(0.02)0.98, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta/CSP for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "emoStab", sort) ///
  (line lci imor if exposure == "emoStab", lpattern(dash)) ///
  (line uci imor if exposure == "emoStab", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.9(0.02)0.98, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "emoStab", sort) ///
  (line lci prev if exposure == "emoStab", lpattern(dash)) ///
  (line uci prev if exposure == "emoStab", lpattern(dash)), ///
  xlabel(8(2)22, labsize(large)) ///
  ylabel(0.9(0.02)0.98, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(9.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx24_NARMICE_emoStab.pdf", replace


** Openness

* Plot using delta/CSP
twoway (connected or delta if exposure == "open", sort) ///
  (line lci delta if exposure == "open", lpattern(dash)) ///
  (line uci delta if exposure == "open", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "open", sort) ///
  (line lci imor if exposure == "open", lpattern(dash)) ///
  (line uci imor if exposure == "open", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "open", sort) ///
  (line lci prev if exposure == "open", lpattern(dash)) ///
  (line uci prev if exposure == "open", lpattern(dash)), ///
  xlabel(8(2)22, labsize(large)) ///
  ylabel(0.98(0.02)1.08, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(9.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx24_NARMICE_open.pdf", replace
  

** IQ

* Plot using delta/CSP
twoway (connected or delta if exposure == "iq", sort) ///
  (line lci delta if exposure == "iq", lpattern(dash)) ///
  (line uci delta if exposure == "iq", lpattern(dash)), ///
  xlabel(0(0.5)2, labsize(large)) ///
  ylabel(0.98(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Delta for anxiety (log-odds)", size(vlarge)) ///
  legend(off) xline(0) name(delta, replace)
  
* Plot using IMOR
twoway (connected or imor if exposure == "iq", sort) ///
  (line lci imor if exposure == "iq", lpattern(dash)) ///
  (line uci imor if exposure == "iq", lpattern(dash)), ///
  xscale(log) xlabel(1(1)6, labsize(large)) ///
  ylabel(0.98(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("IMOR for anxiety (odds ratio)", size(vlarge)) ///
  legend(off) xline(1) name(msp, replace)
  
* Plot using prevalence
twoway (connected or prev if exposure == "iq", sort) ///
  (line lci prev if exposure == "iq", lpattern(dash)) ///
  (line uci prev if exposure == "iq", lpattern(dash)), ///
  xlabel(8(2)22, labsize(large)) ///
  ylabel(0.98(0.02)1.04, labsize(large)) ///
  ytitle("Odds ratio for anxiety", size(vlarge)) ///
  xtitle("Prevalence of anxiety (%)", size(vlarge)) ///
  legend(off) xline(9.8) name(prev, replace)
  
* Combine plots together
graph combine delta msp prev, rows(1) imargin(tiny) ycommon
graph display, ysize(8) xsize(20)

graph export "./Results/anx24_NARMICE_iq.pdf", replace

graph close _all



log close


