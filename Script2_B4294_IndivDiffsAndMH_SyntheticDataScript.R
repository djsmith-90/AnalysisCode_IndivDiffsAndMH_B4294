### Script for project 'The role of individual differences in later mental health' - ALSPAC B-number B4293
### Script 2: Generating synthetic datasets
### Created 7/12/2023 by Dan Major-Smith
### R version 4.3.1


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4294 - MSc Personality MH")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("synthpop")
library(synthpop)

#install.packages("readstata13")
library(readstata13)

#install.packages("haven")
library(haven)


##########################################################################################
#### Read in the processed dataset and generate synthetic datasets using 'synthpop' (https://www.synthpop.org.uk/get-started.html)

data_raw <- read.dta13("./updatedDataset_6.12.2023/PersonalityMH_B4294_processed.dta", nonint.factors = TRUE)
head(data_raw)
str(data_raw)


# Take a working copy by dropping some admin vars
dat <- data_raw %>%
  select(-c(aln, qlet))

head(dat)
glimpse(dat)


#### Now start the synthpop process

# Get information about variables in the dataset
codebook.syn(dat)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
dat_syn <- syn(dat, seed = 6789876)
head(dat_syn$syn)


# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, 5 observations have been dropped (0.07% of data)
replicated.uniques(dat_syn, dat)
dat_syn <- sdc(dat_syn, dat, rm.replicated.uniques = TRUE)


## Take a few unique true observations, and make sure not fully-replicated in synthetic dataset (based on the 'replicated.uniques' command from the 'synthpop' package)

# Make a dataset just of unique individuals using the observed data (as if two or more participants share exactly the same data, then it's impossible to link back to a unique individual)
sum(!(duplicated(dat) | duplicated(dat, fromLast = TRUE)))
dat_unique <- dat[!(duplicated(dat) | duplicated(dat, fromLast = TRUE)), ]

# Make a dataset just of unique individuals from the synthetic dataset
sum(!(duplicated(dat_syn$syn) | duplicated(dat_syn$syn, fromLast = TRUE)))
syn_unique <- dat_syn$syn[!(duplicated(dat_syn$syn) | duplicated(dat_syn$syn, fromLast = TRUE)), ]

# Select a random row from the observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 1), ])

# Combine observed row with the synthetic data, and see if any duplicates
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Repeat for a few more rows of observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 10), ])
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))


### Explore this synthetic dataset
dat_syn
summary(dat_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is fairly good). Save this as a PDF
compare(dat_syn, dat, stat = "counts", nrow = 5, ncol = 7)

pdf("./Results/Results_SynthPop/ComparingDescStats.pdf", height = 15, width = 18)
compare(dat_syn, dat, stat = "counts", nrow = 5, ncol = 7)
dev.off()


## Univariable analysis with depression at age 24 as outcome and IQ/cognitive ability as exposure to show that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.syn <- glm.synds(dep24 ~ iq, family = "binomial", data = dat_syn)
summary(model.syn)

# Results not exactly the same, but similar overall pattern of results (and store as PDF)
compare(model.syn, dat)

pdf("./Results/Results_SynthPop/ComparingUnadjustedModel.pdf", height = 8, width = 12)
compare(model.syn, dat)
dev.off()


## Next compare results of multivariable analyses (have excluded 'child_dep10' here, as get very different results in synthetic data compared to observed - Likely due to small number of cases at this point with depression)
model.syn2 <- glm.synds(dep24 ~ iq + sex + ageAtBirth + home + mum_edu + dad_edu + imd + ethnic +
                          mum_dep + mum_anx + dad_dep + dad_anx + child_ACEs11 +
                          child_anx10 + child_depScore10 + child_SDQ9, 
                        family = "binomial", data = dat_syn)
summary(model.syn2)

# Again, get relatively comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, dat)

pdf("./Results/Results_SynthPop/ComparingAdjustedModel.pdf", height = 8, width = 12)
compare(model.syn2, dat)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
dat_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(dat_syn$syn)), dat_syn$syn)
summary(dat_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
dat_syn_df <- dat_syn$syn
head(dat_syn_df)
glimpse(dat_syn_df)
summary(dat_syn_df)


## Now store this synthetic dataset in Stata format
write_dta(dat_syn_df, "./AnalysisCode_B4294/SyntheticData/syntheticData_B4294.dta")

