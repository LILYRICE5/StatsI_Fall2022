#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #LR - i do this

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/incumbents_subset.csv")

# At this point - I have run all code from above

# QUESTION 1 #
## 1.1: RUN REGRESSION WITH OUTCOME V = VOTESHARE, EXPLANATORY V = DIFFLOG

lm_vs_dl <- lm(voteshare ~ difflog, data = inc.sub)

summary(lm_vs_dl)

## 1.2: MAKE SCATTERPLOT OF THE TWO VARIABLES AND ADD REG LINE:

# First make plot:

plot(inc.sub$difflog, inc.sub$voteshare, main = "1.2(a): Plot of difflog
     vs voteshare",
     xlab = "difflog", ylab = "voteshare")

# then add regression line:

plot(inc.sub$difflog, inc.sub$voteshare, main = "1.2(b): Plot of difflog 
vs voteshare with 
Regression Line",
     xlab = "difflog", ylab = "voteshare")
abline(lm(voteshare ~ difflog, data = inc.sub), col = "blue")

?plot()

#1.3: SAVE RESIDUALS IN NEW OBJECT:

Res_lm_vs_dl <- lm_vs_dl$residuals

#1.4: WRITE PREDICTION EQUATION:
#prediction equation: Y = a + bX + e
# predicted voteshare1 = .579 + 0.042(difflog1)

#QUESTION 2
#2.1: RUN REG WHERE THE OUTCOME V IS PRESVOTE AND THE 
#EXPL V IS DIFFLOG

lm_pv_dl <- lm(presvote ~ difflog, data = inc.sub)

summary(lm_pv_dl)

#2.2: MAKE SCATTERPLOT + REG LINE

plot(inc.sub$difflog, inc.sub$presvote, main = "2.2(a): Plot of difflog
     vs presvote",
     xlab = "difflog", ylab = "presvote")

# then add regression line:

plot(inc.sub$difflog, inc.sub$presvote, main = "2.2(b): Plot of difflog 
vs presvote with 
Regression Line",
     xlab = "difflog", ylab = "presvote")
abline(lm(presvote ~ difflog, data = inc.sub), col = "blue")

?abline()

#2.3: SAVE RESIDS AS SEPARATE OBJECT

Res_lm_pv_dl <- lm_pv_dl$residuals

#2.4: WROTE PREDICTION EQUATION (IN .TEX FILE)

#QUESTION 3#
#3.1: RUN REG OUTCOME V IS VOTESHARE, EXPL VARIABLE
# IS PRESVOTE

lm_vs_pv <- lm(voteshare ~ presvote, data = inc.sub)

summary(lm_vs_pv)

#3.2: MAKE SCATTERPLOT OF BOTH VARIABLES + REG
# LINE

plot(inc.sub$presvote, inc.sub$voteshare, main = "3.2(a): Plot of presvote
     vs voteshare",
     xlab = "presvote", ylab = "voteshare")

# then add regression line:

plot(inc.sub$presvote, inc.sub$voteshare, main = "3.2(b): Plot of presvote 
vs voteshare with 
Regression Line",
     xlab = "presvote", ylab = "voteshare")
abline(lm(voteshare ~ presvote, data = inc.sub), col = "blue")

#QUESTION 4#
#4.1: RUN REGRESSION WHERE OUTCOME IS RESIDS FROM
#Q1 AND EXPLANATORY IS RESIDS FROM Q2

lm_rq1_rq2 <- lm(Res_lm_vs_dl~Res_lm_pv_dl)
summary(lm_rq1_rq2)

#4.2: PLOT BOTH VARIABLES:

plot(Res_lm_pv_dl, Res_lm_vs_dl, main = "4.2(a): Plot of Q1 Residuals
     vs Q2 Residuals",
     xlab = "Q1 Residuals: pv_dl", ylab = "Q2 Residuals: vs_dl ")

plot(Res_lm_pv_dl, Res_lm_vs_dl, main = "4.2(b): Plot of Q1 Residuals
     vs Q2 Residuals with Regression Line",
     xlab = "Q1 Residuals: pv_dl", ylab = "Q2 Residuals: vs_dl ")
abline(lm(Res_lm_vs_dl~Res_lm_pv_dl), col = "blue")

#QUESTION 5#
#5.1: RUN REGRESSION WHERE OUTCOME V IS VOTESHARE,
#2 EXPLANATORY VS ARE DIFFLOG AND PRESVOTE

lm_vs_dlpv <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(lm_vs_dlpv)

# PREDICTION EQUATION:
