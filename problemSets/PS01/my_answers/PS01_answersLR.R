#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", 
                      "package:utils", "package:datasets", "package:methods", "package:base")
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

lapply(c("stringr"),  pkgTest)

##I will now check which packages are loaded into this R session using the search() function

search()

# set working directory
## Note: this produces an error, I have set my WD another way, by using 
## the "new project" option in the top right corner
setwd("Home:/Documents:/GitHub/StatsI_Fall2022/problem_Sets/PS01/template")


#####################
# Problem 1
#####################

# I will first Answer Problem 1, Part 1:

# STEP 1 is to load the below data set
# This produces the object "y", and creates a vector for IQ denoted by the value "y"

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# I will not create a data.frame object for this vector because I am just looking at
# one vector, I do not need to tie it to another at this point

# First step to answering this question is calculating the sample mean (average IQ) 
# for this sample, y:

Mean_y <- sum(y)/length(y)

# This produces the value of 98.44, meaning the avg IQ in the sample of 25 students is 98.44
# This is the "sample mean", denoted by y bar in statistics

# To check that this is correct, I will use R's mean function:

mean(y)

# The output of this function is also 98.44, confirming I have calculated the sample mean
# correctly

# STEP 2 is to calculate the sample's standard deviation, denoted by StdDev_y
# to do so I must take the square root of the sum of all values minus the sample mean squared,
# divided by the sample size minus 1

n <- length(y)

nominator <- sqrt(sum((y-Mean_y)*(y-Mean_y)))

denominator <- sqrt(length(y)-1)

SD_y <- nominator/denominator

# I will use the stdv function in R to confirm that I have calculated the stdv correctly

sd(y)

# This produces the output of 13.09287 which is the same as my manually calculated Sample 
# Standard Deviation, confirming I calculated this correctly.

# STEP 3 is to calculate the 90% Confidence Interval (CI) for the avg ("Mean_y") student IQ in the 
# school.
# CI is the point estimate ("Mean_y") plus and minus the margin of error
# Because the Sample Size is small and we are assessing means, we will use a t-distribution
# to calculate the margin of error
# the formula I will be putting through R Studio is "Mean_y" + and - (t_0.05)("SE_y")
# "SE_y" is the estimated standard error, which I will calculate first below:

SE_y <- SD_y/sqrt(n)

# "t_0.05" is 1.711 (according to the student's t-distribution table)
# I will input the above values to the below equations to identify the upper and lower
# bands of the 90% confidence interval for the average IQ of students at the school:

Upper_CI_y <- Mean_y+1.711*SE_y
Lower_CI_y <- Mean_y-1.711*SE_y

# These inputs output the Upper_CI_y as 102.92 and the Lower_CI_y as 93.96.
# I will check this using the below formula:

t.test(y, conf.level = 0.9, alternative = "two.sided")

# Question 1 is to Calculate the Sample Mean (y bar) with 90% confidence interval
# Few terms defined for this data:
# Confidence Coefficient = .90
# I want to calculate the appropriate confidence interval for the
# mean IQ in the school using t-distribution
##NOTE: t distribution is used because this is a very small sample size

se <- sd(y)/sqrt(length(y)) #creating object (se) our standard error
# this is the formula that will calculate standard error, aka the standard
# deviation of the sampling distribution

# now I will create the object "t score"
t_score <- qt(.05, df = length(y)-1, lower.tail = FALSE)
CI_lower_t <- mean(y) - (se * t_score)
CI_upper_t <- mean(y) + (se * t_score)

# Now I will check my work using the t.test function

t.test(y, conf.level = 0.9, alternative = "two.sided")

#this t.test function has confirmed that I calculated the 90%
#confidence interval for the IQ of students in the school correctly
#because the confidence interval remains the same

# My findings for Q1 Part1 are summarised as : 90% of the time, when random sampling 
# from the population of students at the school, the average (or sample mean) 
# IQ will be between 93.96 and 102.92.

# The first attempt shows the more manual method of calculation with more detailed 
# description as to why each step was undertaken. The second attempt is more concise,
# showing the method but with less description as to why, and using more of the 
# preexisting R formulas rather than calculating from scratch.  The third, test, method is
# devoid of description as to why and is simply my test of the above methods.

#######
#Part2#
#######

# Next, I will use hypothesis testing to calculate whether the average IQ of students in the 
# school is higher than the average IQ score (100) among all schools in the country.
# Using the same sample, I will conduct the appropriate hypothesis test with alpha = .05

# STEP 1 of Hypothesis Testing = outline Assumptions:
## I assume the data is continuous, sample size is 25, sample was randomly selected.

# Step 2 = Formulate Hypotheses:
## Null Hypothesis H_0 : mu_y is < or = to 100 (meaning that schools population mean (mu_y) is less
## than or equal to country-wide students' mean.)
## Alternative Hypothesis H_a : is that mu_y > 100 (that the population mean for y is greater than 
## the average student IQ country-wide, 100).

# Step 3 = Calculate the Test Statistic "TS"
## Test Statistic = summarises how much our data differs from what we would have expected if  
## H_0 is true.
## I am going to use a t-table test statistic (TS) because I was working with a t-distribution previously
## (because the sample size is small)

TS <- (Mean_y-100)/SE_y

# Step 4 = Calculate p value, this is one sided because we are only concerned about whether or not
## mu is greater than the country-wide student IQ, (not if it is greater or less than).
## The degrees of freedom are n-1 also because we are using t-table (rationale for which explained
## above).  

p <- pt(abs(TS),df = n-1)

# Step 5 = make conclusions
## Because the p value = .722, and the level of test (alpha) = .05,(so the p value is
## greater than alpha) we fail to reject the null hypothesis that the students in the 
## school's average IQ is less than or equal to  the average IQ of students country-wide.

#####################
# Problem 2
#####################

# I will first open the expenditure file by clicking the open button > stats datasets > expenditure.txt file

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)

summary(expenditure)
head(expenditure)

# At this point I have loaded the expenditure data set, and created the object "expenditure" which refers to this 
# data set

# Next I need to create vectors for each variable in the data set (Y, X1, X2, and X3)

Y_HExpenditure <- c(expenditure$Y)
X1_Income <- c(expenditure$X1)
X2_FInsecurity <- c(expenditure$X2)
X3_UResidents <- c(expenditure$X3)

# Now I will create scatter plots for the relationships between each variable. These are 
# saved as .png files in my folder as well.

plot(expenditure$Y,expenditure$X1,
     main = "Scatterplot 1: Per Capita Expenditure on 
     Shelter/Housing (Y) vs Per Capita Personal 
     Income (X1) by State",
     ylab = "(X1) Personal Income",
     xlab = "(Y) Expenditure on Shelter/Housing")

# Scatterplot 1: It looks like there is a slight positive, linear relationship
# between Per Capita Income and Expenditure on Housing based on this graph, because 
# as y increases, x tends to increase as well. 

plot(expenditure$Y,expenditure$X2,
     main = "Scatterplot 2: Per Capita Expenditure on 
     Shelter/Housing (Y) vs Financially Insecure 
     Residents (X2) by State",
     ylab = "(X2) Financial Insecurity of Residents",
     xlab = "(Y) Expenditure on Shelter/Housing")

# Scatterplot 2: It looks like there is less correlation between Financial security of residents
# and expenditure on shelter/housing based on this graph (especially if compared to the 
# relationship observed in scatterplot 1), but there could still be a weak positive 
# correlation between the two variables.

plot(expenditure$Y,expenditure$X3,
     main = "Scatterplot 3: Per Capita Expenditure on 
     Shelter/Housing (Y) vs Proportion of 
     Urban Residents (X3) by State",
     ylab = "(X3) Proportion of Urban Residents",
     xlab = "(Y) Expenditure on Shelter/Housing")

# Scatterplot 3: It looks like there is a positive, relatively linear relationship
# between the proportion of urban residents and Expenditure on Housing in a state
# based on this graph, because as y increases, x tends to increase as well. 

plot(expenditure$X1,expenditure$X2,
     main = "Scatterplot 4: Per Capita Personal Income (X1)
     vs Proportion of Financially Insecure
     Residents (X2) by state",
     ylab = "(X2) Proportion of Financially Insecure Residents",
     xlab = "(X1) Per Capita Personal Income")

# Scatterplot 4: It looks like there is no correlation (or at the very least, 
# a very weak positive relationship) between Per Capita Income and Financially Insecure
# Residents in a state based on this graph.

plot(expenditure$X1,expenditure$X3,
     main = "Scatterplot 5: Per Capita Personal Income (X1)
     vs Proportion of Urban Residents (X3)
     by state",
     ylab = "(X3) Proportion of Urban Residents",
     xlab = "(X1) Per Capita Personal Income")

# Scatterplot 5: It looks like there is a strong (compared to the other graphs)
# positive, linear relationship between Per Capita Income and proportion of urban residents
# in a state based on this graph, because as y increases, x tends to increase as well, and 
# these points are closer to one another in a linear shape, with less outliers (though there
# are a few potential outliers observable).

plot(expenditure$X2,expenditure$X3,
     main = "Scatterplot 6: Proportion of Financially
     Insecure Residents (X2) vs Proportion of
     Urban Residents (X3) by state",
     ylab = "(X3) Proportion of Urban Residents",
     xlab = "(X2) Proportion of Financially Insecure Residents")

# Scatterplot 6: It looks like there is little to no observable correlation between the proportion
# of urban residents and proportion of financially insecure residents in a state based 
# on this graph, as there does not seem to be any linear relationship in any direction.
# In comparison to the other 5 Scatterplots, these two variables seem to have the least
# correlation with each other. 

# PART 2 OF Q 2: Plot relationship between Y and Region.  
# Then, On average, which region has the highest per capita expenditure on housing assistance?

# create object Region

Region <- expenditure$Region

# create a data.frame object binding the two variables (Y and Region)

DF_Y_Region <- data.frame(cbind(Y_HExpenditure,Region))

# Now, I create the box plot that shows the variation in State level expenditure 
# on housing in each Region

boxplot(DF_Y_Region$Y_HExpenditure~DF_Y_Region$Region,
        main = "Boxplot 1: State Per Capita Expenditure 
        on Housing Assistance/Shelters (Y)
        by Region",
        ylab = "(Y)Per Capita Expenditure ",
        xlab = "Region")

# On average, Region 4 (West) has the highest per capita expenditure on housing assistance.  This is 
# observable in the Boxplot produced above, as we can see the mean (black line) is close to 90, and higher
# than the means of all other regions.  

# PART 3 OF Q2: 
# Please plot the relationship between Y and X1? Describe this graph and the relationship. 

# Again, I will call the scatterplot created in part 1 of Y vs X1:

plot(expenditure$Y,expenditure$X1,
     main = "Scatterplot 1: Per Capita Expenditure on 
     Shelter/Housing (Y) vs Per Capita Personal
     Income (X1) by State",
     ylab = "(X1) Personal Income Per Capita",
     xlab = "(Y) Expenditure on Shelter / Housing")

#First go at creating this plot with Region addition

ggplot(expenditure,aes(x=Y_HExpenditure,y=X1_Income, col=Region))+geom_point()
                                                                  
# The problem with the above graph is that Region is differentiated by
# a gradation of just one colour. This is not clear enough, so I make the 
# colours bit clearer/more different from one another with the following inputs:
# First, I turn the Region section of the expenditure data into a Factor with 4 levels
# (1, 2, 3, and 4):

expenditure$Region <- as.factor(expenditure$Region)

# Then I produce Scatterplot 7 , which separates the initial scatterplot (Scatterplot 1)
# by Region,using 4 different colors to indicate the 4 different regions:

ggplot(expenditure,aes(x=Y_HExpenditure,y=X1_Income, col=Region))+ggtitle("Scatterplot 7: State Expenditure on
      Shelters/Housing Assistance (Y) vs Personal
      Income (X1), Differentiated by Region")+geom_point()

