#################
# Problem Set 4 #
#################

# Install and load packages: required on PS04 pdf

install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#######################
# QUESTION 1: ECONOMY #
#######################

# 1(A) = CREATE A NEW VARIABLE PROFESSIONAL BY RECODING THE VARIABLE TYPE SO THAT 
# PROFESSIONALS ARE CODED AS 1, BC AND WC AS 0 (HINT = IFELSE)

summary(Prestige$type)

?ifelse()

head(Prestige)

Professional <- ifelse( Prestige$type == "prof", 1, 0) #success - new variable created

summary(Professional)

Prestige_plus_Prof <- cbind(Prestige, Professional) # success - new variable added to dataset

head(Prestige_plus_Prof)
