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

#just exploring initial dataset and some functions
summary(Prestige$type)
?ifelse()
head(Prestige)

#creating new variable, "professional" by recoding variable "type" 
professional <- ifelse(Prestige$type == "prof", 1, 0) 
#success - new variable created

summary(professional)

#adding this new variable to the Prestige Dataset, new dataset called "Prestige_plus_prof
Prestige_plus_prof <- cbind(Prestige, professional) 
# success - new variable added to dataset

head(Prestige_plus_prof)

# 1(B) = RUN LINEAR MODEL WITH PRESTIGE AS OUTCOME; INCOME, PROFESSIONAL, AND THE INTERACTION 
# OF THE TWO AS PREDICTORS (NOTE: IS A CONTINOUS X DUMMY INTERACTION)

interact_reg <- lm(prestige ~ income + professional + income:professional, 
                   data = Prestige_plus_prof)

summary(interact_reg)

# 1(C) WRITE THE PREDICTION EQUATION BASED ON THE RESULT
## Answer: Yhat(i.e. prestige) = (21.142) + (0.003)(income) + (37.781)(professional) 
### + (-0.002)(income * professional)

# 1(D) INTERPRET THE COEFFICIENT FOR INCOME
## Answer: The coefficient of 0.003 for income means that for every one dollar increase in 
## income, there is an average increase in prestige of 0.003. 

# 1(E) INTERPRET THE COEFFICIENT FOR PROFESSIONAL
## Answer: The coefficient of 37.781 for professional means that for every one unit increase
## in professional (the predictor variable), there is on average an increase in prestige score of 37.781, holding the other variables constant.

# 1(F) WHAT IS THE EFFECT OF A $1000 INCREASE IN INCOME ON PRESTIGE SCORE FOR PROFESSIONAL occs?
y_1f1 <- 21.142 + (0.003*0) + (37.781*1) - (0.002*0*1)
y_1f2 <- 21.142 + (0.003*1000) + (37.781*1) - (0.002*1000*1)
Answer_1f <- y_1f2 - y_1f1 
## Answer: The marginal effect if a 1000 dollar increase in income on prestige when the variable of 
## of professional is 1 is 1. This means that when a professional's income increases by $1000, 
## their prestige score is predicted to increase by 1.

# 1(G) WHAT IS THE EFFECT OF CHANGING ONE'S OCCUPATIONS FROM NON-PROFESSIONAL TO PROF WHEN HER INCOME IS $6000? 
y_1g1 <- 21.142 + (0.003*6000) + (37.781*0) - (0.002*6000*0)
y_1g2 <- 21.142 + (0.003*6000) + (37.781*1) - (0.002*6000*1)
Answer_1g <- y_1g2 - y_1g1
## Answer: When income is $6000, the effect of chenging occupations from non-prof (0) to prof (1)
## is an on average increase in prestige score from 39.142 to 64.923. In other words, an on average 
## increase in prestige score of 25.781.

#################################
# QUESTION 2: POLITICAL SCIENCE # THE EFFECT OF YARD SIGNS ON VOTING PREFERENCES
#################################

# 2(A) USE THE RESULTS FROM A LINEAR REGRESSION TO DETERMINE WHETHER HAVING SIGNS IN A PRECINCT 
# AFFECTS VOTE SHARE (E.G. CONDUCT HYPOTHESIS TEST WITH ALPHA = .05)
## Conduct t-test because we want to identify individual coefficient (x1)
## First: identify Hypotheses: H0: beta(x1) = 0; Ha: beta(x1) =/= 0
## Second: Calculate Test Stat (t_x1) = b of x1 / se of x1 = 2.625

t_x1 <- 0.042/0.016
  
## Third: calc P val 

pval_x1 <- 2*pt(abs(t_x1), 131-2-1, lower.tail = F)

## this equals 0.013; alpha = 0.05, so pval_x1 < alpha, so we can reject the H0, the effect is significant
## We can reject the null hypothesis that having signs in a precinct does not affect voteshare

# 2(B) USE THE RESULTS TO DETERMINE WHETHER BEING NEXT TO PRECINCTS WITH THESE SIGNS AFFECTS
# VOTE SHARE (E.G. CONDUCT HYP TEST WITH ALPHA = .05)
## Again, conduct t-test but on 2nd variable now:
## First, Hypotheses. H0: beta x2 = 0 ; Ha: beta x2 =/= 0
## Second, Calculate Test Stat (t_x2) = b of x2 / se of x2 

t_x2 <- 0.042/0.013

## Third, calculate P val

pval_x2 <- 2*pt(abs(t_x2), 131-2-1, lower.tail = F)

## pval is 0.002; alpha is 0.05, so pval_x2 < alpha, so again can reject H0. The effect is significant.
## We can reject the null hypothesis that being a precinct beside one with signs does not have impact
## on voteshare.

# 2(C) INTERPRET THE COEFFICIENT FOR THE CONSTANT TERM SUBSTANTIVELY
## Answer: the coefficient for the constant term is 0.302. This is the value on the y-axes (voteshare) 
## when the value of both predictor/explanatory variables is 0. Basically, the voteshare (proportion of
## votes that go to Ken C.) is 0.302 in precincts with no signs posted that are also not adjacent to any 
## precincts with signs posted.

# 2(D) EVALUATE THE MODEL FIT FOR THIS REGRESSION. WHAT DOES THIS TELL US ABOUT THE IMPORTANCE
# OF YARD SIGNS VS OTHER FACTORS THAT ARE NOT MODELED?

## Do Overall F Test just to continue the check on model fit:
### Hypotheses: H0: all the betas = 0; Ha: at leas one beta =/= 0
### Step 2: Test Statistic: 

F_Stat <- ((0.094/(2-1))/((1-0.094)/(131-2)))

### Step 3: Calculate F p-value:

df1 <- 2-1
df2 <- 131-2-1

F_pval <- df(F_Stat, df1, df2)

### F p-value is 0.000178, very small -- we can realistically assume that at least one of the
### predictors is useful (has a linear relationship with the outcome variable voteshare). However,
### the R^2 is very low (0.094). This means that while both variables do seem to have a linear 
### relationship with the outcome, the proportion of variance in outcome (voteshare) that can be 
### explained by this model is very very low; thus other variables should be added as they may 
### have more impact.