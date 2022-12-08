####################
# Stats Final Exam #
####################
## Lily Rice ##
## 16304845 ##

##### Question 5 #####
# (B): Construct the 95% confidence interval for the effect of 
# democracy on FDI.

# What do I need: (1) to do hypothesis test:
# need to list hypotheses (done), need coefficient estimate (done),
# need to calculate Test Statistic, need to calculate P-value

t_value <- abs(qt((1-.975), 1000-2))
CI_lower <- 4.389 - (t_value*.4)
CI_higher <- 4.389 + (t_value*.4)

TS_dem <- 4.389/0.4
p_value_dem <- 2*pt(abs(TS_dem), 1000-3, lower.tail = F)

# (C): Plugged into the prediction equation then calculated the difference

FDI_Low_education <- -1.426 - (2*23936.45) - (4.879*10.99)
FDI_High_education <- -1.426 - (2*23936.45) - (4.879*12.89)
Diff_in_FDI <- FDI_Low_education - FDI_High_education

######
# Question 6 #
# (C): Calculating difference in arsenic based on change in diff100

arsenic_1 <- (0.42) + (2.49*1) - (3.99*0.4)
arsenic_2 <- (0.42) + (2.49*1) - (3.99*2.06)

diff_in_arsenic <- arsenic_2 - arsenic_1
