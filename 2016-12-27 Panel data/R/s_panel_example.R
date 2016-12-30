library(plyr)
library(dplyr)
library(tidyr)
library(xlsx)

# Reading data

data <- 
  read.xlsx("2016-12-27 Panel data/data/Panel_test_data.xlsx",  
            sheetIndex = 1, 
            header = TRUE) %>% 
  tbl_df() %>% 
  gather(Year, Value, 3:length(.)) %>% 
  spread(Variable, Value)



# Libraries for panel dsata analysis 
#The 'plm' package is needed in order to run panel data models.
#Installing this package will enable the 'plm' command
library(plm)

#Also install the known package 'systemfit' to solve simultaneous sets of equations
library(systemfit)


# 1. Simple regression NOT PANEL DATA
simple_ols <- lm(y ~ x1 + x2, data = data)
summary(simple_ols)

# Making data panel type 
p_data <- pdata.frame(data, index = c("Firm", "Year"), drop.index = TRUE, row.names = TRUE)




#################### 1.1.oneway only individual (no time dimension) effect ####################

#The specification for fixed effects estimations is 'within'
#Take the individual (not time) effect into account 
p_individ_effect <- plm(y ~ x1 + x2, data = p_data, model = "within", effect="individual")

# See the summary of the model
summary(p_individ_effect)

# See the individual fixed effect of each firm 
fixef(p_individ_effect, effect="individual", type = "dmean")


#################### 1.2.twoway: time + individual effect ####################

p_twoway <- plm(y ~ x1 + x2, data = p_data, model = "within", effect = "twoways")
summary(p_twoway)

# See the individual fixed effect of each firm 
fixef(p_twoway, effect="time", type = "dmean")

#Ask for the individual effects on the mean score

fixef(p_twoway, effect="individual", type = "dmean")


fixef(p_twoway)

#notice that the coefficients' estimates change
#The coefficients indicates how much Y changes in controlling for differences in time and between individuals



