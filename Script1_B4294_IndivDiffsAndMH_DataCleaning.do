*** PROJECT: The role of individual differences in later mental health (B4294)
*** STATA VERSION 18
*** Script 1: Cleaning and processing datasets
*** Created 7/12/2023 by Dan Major-Smith (based on script by Tapasya Bhardwaj)

** Set working directory
cd "X:\Studies\RSBB Team\Dan\B4294 - MSc Personality MH"

** Install any user-written packages
*ssc install missings, replace

** Create log file
capture log close
log using ".\Results\IndivDiffsAndMH_cleaning.log", replace

** Read in dataset (NOTE: Synthetic datasets are generated after this script)
use ".\updatedDataset_6.12.2023\PersonalityMH_B4294.dta", clear


******************************************************************************
***** Data cleaning and processing

* Start by describing the data
describe

* Add value labels
numlabel, add

** Remove children not alive at 1 year of age and if withdrew consent
tab1 kz011b FKDQ1000, m

drop if kz011b == 2 | kz011b == .a
drop if FKDQ1000 == .b
drop if pb260 == .c

drop kz011b


*** Go through each variable and clean ready for analysis

** Confounders

* Child sex
tab kz021, m

rename kz021 sex
tab sex, m

* Mother age at birth
tab mz028b, m

replace mz028b = . if mz028b < 0
rename mz028b ageAtBirth
tab ageAtBirth, m

* Mother home ownership status - Code some values together
tab a006, m

replace a006 = . if a006 < 0
recode a006 (0 1 = 1) (2 5 = 2) (3 4 = 3) (6 = 4)
label define home_lb 1 "Owned/Mortaged" 2 "Council/HA" 3 "Private rent" 4 "Other"
numlabel home_lb, add
label values a006 home_lb
rename a006 home
tab home, m 

* Mother highest education level
tab c645a, m

replace c645a = . if c645a < 0
rename c645a mum_edu
tab mum_edu, m

* Father highest education level
tab c666a, m

replace c666a = . if c666a < 0
rename c666a dad_edu
tab dad_edu, m

* IMD status in pregancy
tab jan1993imd2010q5_M, m

replace jan1993imd2010q5_M = . if jan1993imd2010q5_M < 0
rename jan1993imd2010q5_M imd
tab imd, m

* Child ethnicity
tab c804, m

replace c804 = . if c804 < 0
rename c804 ethnic
recode ethnic (1 = 0) (2 = 1)
label define c804 0 "White" 1 "Non-white", modify
numlabel c804, add force
tab ethnic, m

* Mother EPDS depression score
tab b370, m

replace b370 = . if b370 < 0
rename b370 mum_dep
tab mum_dep, m

* Father EPDS depression score
tab pb260, m

replace pb260 = . if pb260 < 0
rename pb260 dad_dep
tab dad_dep, m

* Mother CCEI anxiety score
tab b351, m

replace b351 = . if b351 < 0
rename b351 mum_anx
tab mum_anx, m

* Father CCEI anxiety score
tab pb233, m

replace pb233 = . if pb233 < 0
rename pb233 dad_anx
tab dad_anx, m

* Offspring depression at age 10 (parent-reported)
tab kv8618, m

replace kv8618 = . if kv8618 < 0
rename kv8618 child_dep10
tab child_dep10, m

* Offspring anxiety at age 10 (parent-reported)
tab kv8617, m

replace kv8617 = . if kv8617 < 0
rename kv8617 child_anx10
tab child_anx10, m

* Offspring depression score at age 10 (child-reported)
tab fddp130, m

replace fddp130 = . if fddp130 < 0
rename fddp130 child_depScore10
tab child_depScore10, m

* Offspring strengths and difficulties score at age 9
tab ku710b, m

replace ku710b = . if ku710b < 0
rename ku710b child_SDQ9
tab child_SDQ9, m

* Offspring exposure to trauma/adverse child experiences up to age 11
tab clon145, m
tab clon152, m

replace clon145 = . if clon145 < 0
replace clon152 = . if clon152 < 0

tab clon145 clon152, m

gen child_ACEs11 = 0
replace child_ACEs11 = 1 if clon145 == 1 | clon152 == 1
replace child_ACEs11 = . if clon145 == . | clon152 == .
tab child_ACEs11, m


** Exposures

* Extraversion
sum fg7360

replace fg7360 = . if fg7360 < 0
rename fg7360 extra
sum extra

* Agreeableness
sum fg7361

replace fg7361 = . if fg7361 < 0
rename fg7361 agree
sum agree

* Conscientiousness
sum fg7362

replace fg7362 = . if fg7362 < 0
rename fg7362 consc
sum consc

* Emotional stability
sum fg7363

replace fg7363 = . if fg7363 < 0
rename fg7363 emoStab
sum emoStab

* Openness
sum fg7364

replace fg7364 = . if fg7364 < 0
rename fg7364 open
sum open

* Total IQ score
sum fh6280

replace fh6280 = . if fh6280 < 0
rename fh6280 iq
sum iq



** Outcomes

* Depression at age 24
tab FKDQ1000, m

replace FKDQ1000 = . if FKDQ1000 < 0
rename FKDQ1000 dep24
tab dep24, m

* Anxiety at age 24
tab FKDQ1030, m

replace FKDQ1030 = . if FKDQ1030 < 0
rename FKDQ1030 anx24
tab anx24, m

* Depression at age 17
tab FJCI603, m

replace FJCI603 = . if FJCI603 < 0
rename FJCI603 dep17
tab dep17, m

* Anxiety at age 17
tab FJCI602, m

replace FJCI602 = . if FJCI602 < 0
rename FJCI602 anx17
tab anx17, m


** Auxiliary variables (for multiple imputation)

* IQ at age 8
sum f8ws112

replace f8ws112 = . if f8ws112 < 0
rename f8ws112 iq8
sum iq8

* Any behavioural disorder at age 13 (parent-reported)
tab tb8623, m

replace tb8623 = . if tb8623 < 0
rename tb8623 behavDis13
tab behavDis13, m

* Number of PLIKS/psychotic experiences at age 17
tab FJPL170, m

replace FJPL170 = . if FJPL170 < 0
rename FJPL170 pliks17
tab pliks17, m

* Alcohol AUDIT score at age 17
sum FJAL4000

replace FJAL4000 = . if FJAL4000 < 0
rename FJAL4000 alc17
sum alc17

* Ever smoked at age 15 or 17
tab fh8410, m
tab FJSM050, m

replace fh8410 = . if fh8410 < 0
replace FJSM050 = . if FJSM050 < 0

tab fh8410 FJSM050, m

gen smk15_17 = 0
replace smk15_17 = 1 if fh8410 == 1 | FJSM050 == 1
replace smk15_17 = . if fh8410 == . & FJSM050 == .
tab smk15_17, m

* Ever tried any illegal drugs at age 17
foreach var of varlist FJSM050-FJDR6050 {
	tab `var', m
	replace `var' = . if `var' < 0
	tab `var', m
}

gen drugs17 = 0
replace drugs17 = 1 if FJSM050 == 1 | FJDR050 == 1 | FJDR5000 == 1 | FJDR5150 == 1 | FJDR5300 == 1 | FJDR5450 == 1 | FJDR5600 == 1 | FJDR5750 == 1 | FJDR5900 == 1 | FJDR6050 == 1
replace drugs17 = . if FJSM050 == . & FJDR050 == . & FJDR5000 == . & FJDR5150 == . & FJDR5300 == . & FJDR5450 == . & FJDR5600 == . & FJDR5750 == . & FJDR5900 == . & FJDR6050 == .
tab drugs17, m

* Number of close friends at age 17
tab FJPC050, m

replace FJPC050 = . if FJPC050 < 0
rename FJPC050 friends17
tab friends17, m

* Negative cognitive styles score at age 17
sum FJCQ1009

replace FJCQ1009 = . if FJCQ1009 < 0
rename FJCQ1009 negCogStyles17
sum negCogStyles17

* Any antisocial behaviors at age 17
drop FJAA650 FJAA1050

foreach var of varlist FJAA050-FJAA2400 {
	replace `var' = . if `var' < 0
	tab `var', m
}

gen anti17 = 0
replace anti17 = 1 if FJAA050 == 1 | FJAA150 == 1 | FJAA250 == 1 | FJAA350 == 1 | FJAA450 == 1 | FJAA550 == 1 | FJAA850 == 1 | FJAA950 == 1 | FJAA1150 == 1 | FJAA1450 == 1 | FJAA1550 == 1 | FJAA1700 == 1 | FJAA1800 == 1 | FJAA1900 == 1 | FJAA2000 == 1 | FJAA2250 == 1 | FJAA2400 == 1
replace anti17 = . if FJAA050 == . & FJAA150 == . & FJAA250 == . & FJAA350 == . & FJAA450 == . & FJAA550 == . & FJAA850 == . & FJAA950 == . & FJAA1150 == . & FJAA1450 == . & FJAA1550 == . & FJAA1700 == . & FJAA1800 == . & FJAA1900 == . & FJAA2000 == . & FJAA2250 == . & FJAA2400 == .
tab anti17, m


** Keep just relevant variables and reorder dataset
keep aln-child_depScore10 behavDis13-alc17 friends17 negCogStyles17 extra-open iq dep17 anx17 dep24 anx24 child_ACEs11 smk15_17 drugs17 anti17

order aln-child_depScore10 child_ACEs11 extra-iq dep17-anx24 behavDis13 iq8 pliks17 smk15_17 alc17 friends17 negCogStyles17 drugs17 anti17


** Make a variable to indicate which observations to include in imputation analyses (anyone with any exposure or outcome data)
gen mi_marker = 0
replace mi_marker = 1 if extra != . | agree != . | consc != . | emoStab != . | open != . | iq != . | dep17 != . | anx17 != . | dep24 != . | anx24 != .
tab mi_marker


** Similar variables, but now for complete confounder, exposure and outcome data at ages 17 and 24
gen cca17 = 0
replace cca17 = 1 if sex < . & ageAtBirth < . & home < . & mum_edu < . & dad_edu < . & imd < . & ethnic < . & mum_dep < . & mum_anx < . & dad_dep < . & dad_anx < . & child_SDQ9 < . & child_anx10 < . & child_dep10 < . & child_depScore10 < . & child_ACEs11 < . & extra < . & agree < . & consc < . & emoStab < . & open < . & iq < . & dep17 < . & anx17 < . 
tab cca17

gen cca24 = 0
replace cca24 = 1 if sex < . & ageAtBirth < . & home < . & mum_edu < . & dad_edu < . & imd < . & ethnic < . & mum_dep < . & mum_anx < . & dad_dep < . & dad_anx < . & child_SDQ9 < . & child_anx10 < . & child_dep10 < . & child_depScore10 < . & child_ACEs11 < . & extra < . & agree < . & consc < . & emoStab < . & open < . & iq < . & dep24 < . & anx24 < . 
tab cca24


** Look at descriptive statistics of confounders in full vs MI vs complete-case samples (with 'complete-case' meaning complete confounder, exposure and outcome at ages 17 and 24 data)

missings report sex-child_ACEs11, percent
missings report sex-child_ACEs11 if mi_marker == 1, percent

* Child sex
tab sex
tab sex if mi_marker == 1
tab sex if cca17 == 1
tab sex if cca24 == 1

* Mother age at birth
sum ageAtBirth
sum ageAtBirth if mi_marker == 1
sum ageAtBirth if cca17 == 1
sum ageAtBirth if cca24 == 1

* Mother home ownership status
tab home
tab home if mi_marker == 1
tab home if cca17 == 1
tab home if cca24 == 1

* Mother highest education level
tab mum_edu
tab mum_edu if mi_marker == 1
tab mum_edu if cca17 == 1
tab mum_edu if cca24 == 1

* Father highest education level
tab dad_edu
tab dad_edu if mi_marker == 1
tab dad_edu if cca17 == 1
tab dad_edu if cca24 == 1

* IMD status in pregancy
tab imd
tab imd if mi_marker == 1
tab imd if cca17 == 1
tab imd if cca24 == 1

* Child ethnicity
tab ethnic
tab ethnic if mi_marker == 1
tab ethnic if cca17 == 1
tab ethnic if cca24 == 1

* Mother EPDS depression score
sum mum_dep
sum mum_dep if mi_marker == 1
sum mum_dep if cca17 == 1
sum mum_dep if cca24 == 1

* Father EPDS depression score
sum dad_dep
sum dad_dep if mi_marker == 1
sum dad_dep if cca17 == 1
sum dad_dep if cca24 == 1

* Mother CCEI anxiety score
sum mum_anx
sum mum_anx if mi_marker == 1
sum mum_anx if cca17 == 1
sum mum_anx if cca24 == 1

* Father CCEI anxiety score
sum dad_anx
sum dad_anx if mi_marker == 1
sum dad_anx if cca17 == 1
sum dad_anx if cca24 == 1

* Offspring depression at age 10 (parent-reported)
tab child_dep10
tab child_dep10 if mi_marker == 1
tab child_dep10 if cca17 == 1
tab child_dep10 if cca24 == 1

* Offspring anxiety at age 10 (parent-reported)
tab child_anx10
tab child_anx10 if mi_marker == 1
tab child_anx10 if cca17 == 1
tab child_anx10 if cca24 == 1

* Offspring depression score at age 10 (child-reported)
sum child_depScore10
sum child_depScore10 if mi_marker == 1
sum child_depScore10 if cca17 == 1
sum child_depScore10 if cca24 == 1

* Offspring strengths and difficulties score at age 9
sum child_SDQ9
sum child_SDQ9 if mi_marker == 1
sum child_SDQ9 if cca17 == 1
sum child_SDQ9 if cca24 == 1

* Offspring exposure to trauma/adverse child experiences up to age 11
tab child_ACEs11
tab child_ACEs11 if mi_marker == 1
tab child_ACEs11 if cca17 == 1
tab child_ACEs11 if cca24 == 1


** Descriptive stats on exposures
missings report extra-iq, percent
missings report extra-iq if mi_marker == 1, percent

sum extra-iq
sum extra-iq if cca17 == 1
sum extra-iq if cca24 == 1


** Descriptive stats on outcomes
missings report dep17 anx17 dep24 anx24, percent
missings report dep17 anx17 dep24 anx24 if mi_marker == 1, percent

tab1 dep17 anx17 dep24 anx24
tab1 dep17 anx17 if cca17 == 1
tab1 dep24 anx24 if cca24 == 1


** Descriptive stats on auxiliary variables
missings report behavDis13-anti17, percent
missings report behavDis13-anti17 if mi_marker == 1, percent

* IQ at age 8
sum iq8
sum iq8 if mi_marker == 1

* Any behavioural disorder at age 13 (parent-reported)
tab behavDis13
tab behavDis13 if mi_marker == 1

* Number of PLIKS/psychotic experiences at age 17
tab pliks17
tab pliks17 if mi_marker == 1

* Alcohol AUDIT score at age 17
sum alc17
sum alc17 if mi_marker == 1

* Ever smoked at age 15 or 17
tab smk15_17
tab smk15_17 if mi_marker == 1

* Ever tried any illegal drugs at age 17
tab drugs17
tab drugs17 if mi_marker == 1

* Number of close friends at age 17
tab friends17
tab friends17 if mi_marker == 1

* Negative cognitive styles score at age 17
sum negCogStyles17
sum negCogStyles17 if mi_marker == 1

* Any antisocial behaviors at age 17
tab anti17
tab anti17 if mi_marker == 1


*** Reduce dataset down to just those with any exposure or outcome data, and save this file
keep if mi_marker == 1
drop mi_marker

drop cca17 cca24

save ".\updatedDataset_6.12.2023\PersonalityMH_B4294_processed.dta", replace

log close
