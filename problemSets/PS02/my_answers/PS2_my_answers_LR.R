##############################
# PS02 my_answers Lily Rice #
##############################

###########
# Part 1 #
###########

# My first goal is to put the data into R Studio, I do so with the following steps:

# First I create vectors for each of the Variables (Class, Not Stopped, Bribe Requested,
# and Stopped/Warned):

Class <- c("Upper", "Lower")
NotStopped <- c(14,7)
BribeReq <- c(6,7)
Stopped <- c(7,1)

# Next, I created a data frame that includes all 3 vectors created above:

data <- data.frame(cbind(Class,NotStopped, BribeReq, Stopped))

print(data) #this displays the data.frame I have just created, shows data
# in rectangular form, I will add the margins to this data below so that I
# can see the column and row totals, which I will need to calculate expected
# values for each cell.

# Then I write a csv file for this data:

write.csv(data, 
          file = "/Users/lily/Documents/GitHub/StatsI_Fall2022/problemSets/PS02/my_answers/data.frame.csv",
          row.names = FALSE)

read.csv("data.frame.csv")

#####
#(a)# Calculate the x^2 (chi squared) test statistic by hand.
#####

# Step 1: Calculate Fe for all cells in the table (there will be 6 total)

## I calculated Fe for each cell in excel (and literally by hand), my table 
## of Fe calculated for each cell ca be found in the file "FeTable.png".

# Step 2: Calculate Chi Squared Test Statistic, this = the sum of all differences
# between Fe and Fo squared, divided by fe.

## I have calculated this by hand (with pen and paper) and have found
## the Chi Squared Test Statistic = 11.8548

#####
#(b)# Calculate the p-value from the test statistic you just
##### created in R. What do you conclude if alpha = 0.1?

# My chi squared test statistic = 11.8548; my degrees of freedom are
# 2 because ((rows-1)(columns-1)) = 2

pchisq(3.8269, df = 2, lower.tail = FALSE)

# This produces the p-value =  0.1475704

# if p > alpha, grounds to accept null hypothesis.
# for chi squared test, H0 (H null) = the variables are statistically 
# independent; and Ha (H alternative) = the variables are statistically
# dependent

# My calculation of the p value has found that p > alpha (of 0.1), which
# is grounds to reject the H0, that the variables are statistically
# independent.

#####
#(c)# Calculate the standardized residuals for each cell and
##### put them in the table.

# I calculated the standardized residual for each cell by hand (literally), 
# and created a csv file titled StandardizedResidualsPS02_LR.csv which has
# these standardized residuals entered into the table.  This file is saved
# in the my_answers folder, and I will enter this information below/in the 
# LaTex file for my answers to this problem set.

#####
#(d)# How might the standardized residuals help you interpret 
##### the results?

#The standardized residual allows us to better see if the difference between 
#the observed frequency and expected frequency is due to more than chance. 
#The standardized residual of a cell is the number of standard errors (se) 
#that Fo-Fe falls from the value we expect when H0 is true. 

#The standardized residuals we then found highlight what cells  are more/less 
#likely to occur if these variables were independent (H0 is true).  The higher
#(in absolute terms) a standardized residual is, is an indication of greater 
#chance of association between the two variables (or in other words, stronger 
#that the variables are not independent statistically). 

#With regards to our findings, these standardized residuals indicate that 
#there is greater chance of association between class and whether a bribe was 
#requested, and between class and whether the individual is warned/stopped, 
#because these cells have higher ()absolute values) standard residuals. 
#There is less of a chance of association statistically (greater chance of 
#independence, of H0 being true) between class and not being stopped.  

############
#Question 2#
############

# Step 1: load the data into R

women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

summary(women)
head(women)


# (a) I will now formulate my hypotheses:
## H0 : There is no association between whether a village had the reservation policy,
## (reserved variable) and the number of new or repaired drinking water facilities (water)
## in the villages.
## Ha : The number of new or repaired drinking water facilities in a village is correlated
## with whether or not they had the reservation policy (there is a relationship)
## between these two variables


# H0 (null hypothesis) = there is no correlation between whether a village had the
# reservation policy and the number of new/improved water facilities introduced in that GP
# Ha (alternative hypothesis) = there is a correlation between whether a village had
# the reservation policy, and the number of new/improved water facilities that were introduced.

lreg_water_reserved <- lm(women$water~women$reserved)
summary(lreg_water_reserved)


?subset
