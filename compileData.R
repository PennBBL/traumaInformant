### This script merges the necessary files for analyses
###
### Ellyn Butler
### November 17, 2019

library('MatchIt')

# proband demographics
#probanddemo_df <- read.csv("/home/butellyn/age_prediction/data/n9498_demographics_go1_20161212.csv")

# envSES factor score
envSES_df <- read.csv("/home/butellyn/traumaInformant/data/n9498_go1_environment_factor_scores_tymoore_20150909.csv")
envSES_df <- envSES_df[,c("bblid", "envSES")]

# collateral demographics
kosha_df <- read.csv("/home/analysis/psycha1/pnc/PNC_GO1_GOASSESSDataArchiveNontext_DATA_2015-07-14_1157.csv")
names(kosha_df)[names(kosha_df) == "proband_bblid"] <- "bblid"
kosha_df <- kosha_df[kosha_df$interview_type == "MI", ]
kosha_df <- kosha_df[,c("bblid", "col_rel")]

# lifetime internalizing severity 
proband_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/proband_2019-08-15.csv")
proband_df <- proband_df[,c("bblid", "informant", "sex", "race", "ageAtClinicalAssess1", "fedu1", "medu1", "internal_bifactor")]
colnames(proband_df)[colnames(proband_df) == "internal_bifactor"] <- "internal_bifactor_P"
proband_df$informant <- NULL

collateral_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/collateral_2019-08-15.csv")
collateral_df <- collateral_df[,c("bblid", "informant", "internal_bifactor")]
colnames(collateral_df)[colnames(collateral_df) == "internal_bifactor"] <- "internal_bifactor_C"
collateral_df$informant <- NULL

# amygdala perfusion
perfusion_df <- read.csv("/home/butellyn/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
perfusion_df <- perfusion_df[,c("bblid", "scanid", "pcasl_jlf_cbf_R_Amygdala", "pcasl_jlf_cbf_L_Amygdala", "pcaslExclude", "pcaslRelMeanRMSMotion")]
perfusion_df <- perfusion_df[perfusion_df$pcaslExclude == 0,]

# sexual and physical assault
assault_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/GOA_itemwise_for_reporter_agreement.csv")
assault_df <- assault_df[,c("PROBAND_BBLID", "INTERVIEW_TYPE", "PTD003", "PTD004", "PTD020")]
assault_df <- assault_df[assault_df$INTERVIEW_TYPE %in% c("MI", "MP"),]
assault_df <- assault_df[assault_df$PTD003 != "." & assault_df$PTD004 != ".",]
assault_df[assault_df$PTD003 == 9, "PTD003"] <- 0
assault_df[assault_df$PTD004 == 9, "PTD004"] <- 0
colnames(assault_df)[colnames(assault_df) == "PROBAND_BBLID"] <- "bblid"
colnames(assault_df)[colnames(assault_df) == "INTERVIEW_TYPE"] <- "informant"
assault_df$informant <- as.character(assault_df$informant)
assault_df[assault_df$informant == "MI", "informant"] <- "collateral"
assault_df[assault_df$informant == "MP", "informant"] <- "proband"

assault_df_P <- assault_df[assault_df$informant == "proband",]
colnames(assault_df_P)[colnames(assault_df_P) == "PTD003"] <- "PTD003_P"
colnames(assault_df_P)[colnames(assault_df_P) == "PTD004"] <- "PTD004_P"
colnames(assault_df_P)[colnames(assault_df_P) == "PTD020"] <- "PTD020_P"
assault_df_P$informant <- NULL

assault_df_C <- assault_df[assault_df$informant == "collateral",]
colnames(assault_df_C)[colnames(assault_df_C) == "PTD003"] <- "PTD003_C"
colnames(assault_df_C)[colnames(assault_df_C) == "PTD004"] <- "PTD004_C"
colnames(assault_df_C)[colnames(assault_df_C) == "PTD020"] <- "PTD020_C"
assault_df_C$informant <- NULL

assault_df <- merge(assault_df_P, assault_df_C, by="bblid")
assault_df$PTD003_Agree <- NA
assault_df$PTD004_Agree <- NA

for (i in 1:nrow(assault_df)) {
	# PTD003
	if (assault_df[i, "PTD003_P"] == 1) {
		if (assault_df[i, "PTD003_C"] == 1) { assault_df[i, "PTD003_Agree"] <- "PYCY"
		} else { assault_df[i, "PTD003_Agree"] <- "PYCN" }
	} else {
		if (assault_df[i, "PTD003_C"] == 1) { assault_df[i, "PTD003_Agree"] <- "PNCY"
		} else { assault_df[i, "PTD003_Agree"] <- "PNCN" }
	}
	# PTD004
	if (assault_df[i, "PTD004_P"] == 1) {
		if (assault_df[i, "PTD004_C"] == 1) { assault_df[i, "PTD004_Agree"] <- "PYCY"
		} else { assault_df[i, "PTD004_Agree"] <- "PYCN" }
	} else {
		if (assault_df[i, "PTD004_C"] == 1) { assault_df[i, "PTD004_Agree"] <- "PNCY"
		} else { assault_df[i, "PTD004_Agree"] <- "PNCN" }
	}
}

# Collateral is not aware of at least one assault that occurred
assault_df$CollNotAwareAtLeastOne <- NA
for (i in 1:nrow(assault_df)) {
	if (assault_df[i, "PTD003_Agree"] == "PYCN" | assault_df[i, "PTD004_Agree"] == "PYCN") {
		assault_df[i, "CollNotAwareAtLeastOne"] <- "Yes"
	} else if (assault_df[i, "PTD003_Agree"] == "PYCY" | assault_df[i, "PTD004_Agree"] == "PYCY") {
		assault_df[i, "CollNotAwareAtLeastOne"] <- "No"
	}
}



############# Merging #############

# All datasets include no missing data for the following variables:
# sex, race, age, mother education, neighborhood crime factor
# score, proband-reported lifetime internalizing severity, collateral-
# reported lifetime internalizing severity, age of worst assault
# and physical and sexual assault knowledge

# Dataset #1: Match assaulted versus not
one_df <- merge(envSES_df, kosha_df)
one_df <- merge(one_df, proband_df)
one_df <- merge(one_df, collateral_df)
one_df <- merge(one_df, assault_df)
one_df$envSES <- round(one_df$envSES, digits=1)
one_df$ageAtClinicalAssess1 <- one_df$ageAtClinicalAssess1/12
one_df$ageAtClinicalAssess1 <- round(one_df$ageAtClinicalAssess1, digits=0)
one_df$Assaulted <- NA
for (i in 1:nrow(one_df)) {
	if (one_df[i, "PTD003_P"] == 1 | one_df[i, "PTD004_P"] == 1) {
		one_df[i, "Assaulted"] <- 1
	} else {
		one_df[i, "Assaulted"] <- 0
	}
}
one_df <- one_df[!is.na(one_df$medu1),]
one_df$race <- factor(one_df$race)


matchedinfo <- matchit(Assaulted ~ envSES + sex + race + ageAtClinicalAssess1 + medu1, data=one_df[,c("Assaulted", "envSES", "sex", "race", "ageAtClinicalAssess1", "medu1")])


# Dataset #2: Match assaulted versus not with amygdala perfusion included


# Dataset #3: Match know of assault versus not





