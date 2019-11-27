### This script merges the necessary files for analyses
###
### Ellyn Butler
### November 17, 2019 - November 23, 2019

library('MatchIt')

galton=FALSE
mymachine=TRUE

# proband demographics
#probanddemo_df <- read.csv("/home/butellyn/age_prediction/data/n9498_demographics_go1_20161212.csv")

# envSES factor score
if (galton == TRUE) {
	envSES_df <- read.csv("/home/butellyn/traumaInformant/data/n9498_go1_environment_factor_scores_tymoore_20150909.csv")
} else {
	envSES_df <- read.csv("/Users/butellyn/Documents/traumaInformant/data/n9498_go1_environment_factor_scores_tymoore_20150909.csv")
}
envSES_df <- envSES_df[,c("bblid", "envSES")]

# collateral demographics
if (galton == TRUE) {
	kosha_df <- read.csv("/home/analysis/psycha1/pnc/PNC_GO1_GOASSESSDataArchiveNontext_DATA_2015-07-14_1157.csv")
} else {
	kosha_df <- read.csv("/Users/butellyn/Documents/traumaInformant/data/PNC_GO1_GOASSESSDataArchiveNontext_DATA_2015-07-14_1157.csv")
}
names(kosha_df)[names(kosha_df) == "proband_bblid"] <- "bblid"
kosha_df <- kosha_df[kosha_df$interview_type == "MI", ]
kosha_df <- kosha_df[,c("bblid", "col_rel")]

# lifetime internalizing severity
if (galton == TRUE) {
	proband_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/proband_2019-08-15.csv")
} else {
	proband_df <- read.csv("/Users/butellyn/Documents/traumaInformant/data/proband_2019-08-15.csv")
}
proband_df <- proband_df[,c("bblid", "informant", "sex", "race", "ageAtClinicalAssess1", "fedu1", "medu1", "internal_bifactor")]
colnames(proband_df)[colnames(proband_df) == "internal_bifactor"] <- "internal_bifactor_P"
proband_df$informant <- NULL

if (galton == TRUE) {
	collateral_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/collateral_2019-08-15.csv")
} else {
	collateral_df <- read.csv("/Users/butellyn/Documents/traumaInformant/data/collateral_2019-08-15.csv")
}
collateral_df <- collateral_df[,c("bblid", "informant", "internal_bifactor")]
colnames(collateral_df)[colnames(collateral_df) == "internal_bifactor"] <- "internal_bifactor_C"
collateral_df$informant <- NULL

# amygdala perfusion
if (galton == TRUE) {
	perfusion_df <- read.csv("/home/butellyn/age_prediction/data/n1601_imagingclinicalcognitive_20190130.csv")
} else {
	perfusion_df <- read.csv("/Users/butellyn/Documents/traumaInformant/data/n1601_jlfAntsCTIntersectionPcaslValues_20170403.csv")
	perfusion_quality_df <- read.csv("/Users/butellyn/Documents/traumaInformant/data/n1601_PcaslQaData_20170403.csv")
	perfusion_df <- merge(perfusion_df, perfusion_quality_df)
}
perfusion_df <- perfusion_df[,c("bblid", "scanid", "pcasl_jlf_cbf_R_Amygdala", "pcasl_jlf_cbf_L_Amygdala", "pcaslExclude", "pcaslRelMeanRMSMotion")]
perfusion_df <- perfusion_df[perfusion_df$pcaslExclude == 0,]

# sexual and physical assault
if (galton == TRUE) {
	assault_df <- read.csv("/home/butellyn/parentchild_psychopathology/data/GOA_itemwise_for_reporter_agreement.csv")
} else {
	assault_df <- read.csv("/Users/butellyn/Documents/internalizingIRT/GOA_itemwise_for_reporter_agreement.csv")
}
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
		assault_df[i, "CollNotAwareAtLeastOne"] <- 1
	} else if (assault_df[i, "PTD003_Agree"] == "PYCY" | assault_df[i, "PTD004_Agree"] == "PYCY") {
		assault_df[i, "CollNotAwareAtLeastOne"] <- 0
	}
}



############# Merging #############

# All datasets include no missing data for the following variables:
# sex, race, age, mother education, neighborhood crime factor
# score, proband-reported lifetime internalizing severity, collateral-
# reported lifetime internalizing severity, age of worst assault
# and physical and sexual assault knowledge

##### Dataset #1: Match assaulted versus not
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
rownames(one_df) <- 1:nrow(one_df)

matchedinfo <- matchit(Assaulted ~ envSES + sex + race + ageAtClinicalAssess1 + medu1, data=one_df[,c("Assaulted", "envSES", "sex", "race", "ageAtClinicalAssess1", "medu1")])
one_df$matched <- 0
for (i in 1:nrow(one_df)) {
	if (i %in% rownames(matchedinfo$match.matrix)) {
		if (!is.na(matchedinfo$match.matrix[as.character(i), "1"])) {
			one_df[i, "matched"] <- 1
		}
	} else if (i %in% matchedinfo$match.matrix[,1]) {
		one_df[i, "matched"] <- 1
	}
}

one_df <- one_df[one_df$matched == 1,]
rownames(one_df) <- 1:nrow(one_df)
one_df$matched <- NULL

if (galton == TRUE) {
	print("Not sure where to put on Galton yet")
} else if (mymachine == TRUE) {
	write.csv(one_df, "/Users/butellyn/Documents/traumaInformant/data/matchedAssaultVsNot.csv")
}

##### Dataset #2: Match assaulted versus not with amygdala perfusion included
two_df <- merge(envSES_df, kosha_df)
two_df <- merge(two_df, proband_df)
two_df <- merge(two_df, collateral_df)
two_df <- merge(two_df, assault_df)
two_df <- merge(two_df, perfusion_df)
two_df$envSES <- round(two_df$envSES, digits=1)
two_df$ageAtClinicalAssess1 <- two_df$ageAtClinicalAssess1/12
two_df$ageAtClinicalAssess1 <- round(two_df$ageAtClinicalAssess1, digits=0)
two_df$Assaulted <- NA
for (i in 1:nrow(two_df)) {
	if (two_df[i, "PTD003_P"] == 1 | two_df[i, "PTD004_P"] == 1) {
		two_df[i, "Assaulted"] <- 1
	} else {
		two_df[i, "Assaulted"] <- 0
	}
}
two_df <- two_df[!is.na(two_df$medu1),]
two_df$race <- factor(two_df$race)
rownames(two_df) <- 1:nrow(two_df)

matchedinfo <- matchit(Assaulted ~ envSES + sex + race + ageAtClinicalAssess1 + medu1, data=two_df[,c("Assaulted", "envSES", "sex", "race", "ageAtClinicalAssess1", "medu1")])
two_df$matched <- 0
for (i in 1:nrow(two_df)) {
	if (i %in% rownames(matchedinfo$match.matrix)) {
		if (!is.na(matchedinfo$match.matrix[as.character(i), "1"])) {
			two_df[i, "matched"] <- 1
		}
	} else if (i %in% matchedinfo$match.matrix[,1]) {
		two_df[i, "matched"] <- 1
	}
}

two_df <- two_df[two_df$matched == 1,]
rownames(two_df) <- 1:nrow(two_df)
two_df$matched <- NULL

if (galton == TRUE) {
	print("Not sure where to put on Galton yet")
} else if (mymachine == TRUE) {
	write.csv(two_df, "/Users/butellyn/Documents/traumaInformant/data/matchedAssaultVsNotPerfusion.csv")
}

##### Dataset #3: Match know of assault versus not
three_df <- merge(envSES_df, kosha_df)
three_df <- merge(three_df, proband_df)
three_df <- merge(three_df, collateral_df)
three_df <- merge(three_df, assault_df)
three_df$envSES <- round(three_df$envSES, digits=1)
three_df$ageAtClinicalAssess1 <- three_df$ageAtClinicalAssess1/12
three_df$ageAtClinicalAssess1 <- round(three_df$ageAtClinicalAssess1, digits=0)
three_df$Assaulted <- NA
for (i in 1:nrow(three_df)) {
	if (three_df[i, "PTD003_P"] == 1 | three_df[i, "PTD004_P"] == 1) {
		three_df[i, "Assaulted"] <- 1
	} else {
		three_df[i, "Assaulted"] <- 0
	}
}
three_df <- three_df[!is.na(three_df$medu1),]
three_df$race <- factor(three_df$race)
three_df <- three_df[three_df$CollNotAwareAtLeastOne %in% c("No", "Yes"),]
rownames(three_df) <- 1:nrow(three_df)

matchedinfo <- matchit(CollNotAwareAtLeastOne ~ envSES + sex + race + ageAtClinicalAssess1 + medu1, data=three_df[,c("CollNotAwareAtLeastOne", "envSES", "sex", "race", "ageAtClinicalAssess1", "medu1")])
three_df$matched <- 0
for (i in 1:nrow(three_df)) {
	if (i %in% rownames(matchedinfo$match.matrix)) {
		if (!is.na(matchedinfo$match.matrix[as.character(i), "1"])) {
			three_df[i, "matched"] <- 1
		}
	} else if (i %in% matchedinfo$match.matrix[,1]) {
		three_df[i, "matched"] <- 1
	}
}

three_df <- three_df[three_df$matched == 1,]
rownames(three_df) <- 1:nrow(three_df)
three_df$matched <- NULL

if (galton == TRUE) {
	print("Not sure where to put on Galton yet")
} else if (mymachine == TRUE) {
	write.csv(three_df, "/Users/butellyn/Documents/traumaInformant/data/matchedKnowVsNot.csv")
}
