#Setup the environment
rm(list = ls())
library(ggplot2)
library(car)
library(lmtest)
library(stats)
library(sandwich)
library(Hmisc)
library(stargazer)
library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)

# Function to print a nice (row sum) proportion table for single variable
print_single_var_prop_table <- function(var) {
  
  # How many columns are we looking at?
  numCol = length(unique(var))
  
  # Create proportion tables
  table = prop.table(table(var))
  
  # Coerce to percentages
  formattedTable <- apply(table*100, 1, function(u) sprintf("%.0f%%", u))
  table <- data.frame(as.list(formattedTable),row.names=c("All Voters"))
  
  # Plot a nice looking table
  kable(table, digits=2, align=sample(c("c"),numCol,replace=TRUE)) %>% 
    kable_styling(bootstrap_options = "striped", full_width = F, position="left")
  
}

# Function to print a nice (row sum) proportion table for a cross tabulation
print_prop_table <- function(var1,var2) {
  
  # How many columns are we looking at?
  numCol = length(unique(var2))
  numRow = length(unique(var1))
  
  # Create proportion tables
  table = prop.table(table(var1,var2),1)
  
  # Coerce to percentages
  formattedTable <- apply(table*100, 2, function(u) sprintf("%.0f%%", u))
  table <- 'rownames<-'(as.data.frame(formattedTable), rownames(table))
  
  # Plot a nice looking table
  kable(table, digits=2, align=sample(c("c"),numCol,replace=TRUE)) %>% 
    kable_styling(bootstrap_options = "striped", full_width = F, position="left")
  
}

# Function to print a nice summary table for a continuous variable
nice_summary_table <- function(var,descriptor) {
  
  meanVar <- mean(var)
  medianVar <- median(var)
  sdVar <- sd(var)
  minVar <- min(var)
  maxVar <- max(var)
  
  # Create proportion tables
  df <- data.frame(minVar,maxVar,meanVar,medianVar,sdVar)
  colnames(df) <- c("Min","Max","Mean","Median","St. Dev")
  rownames(df) <- descriptor
  
  # Plot a nice looking table
  kable(df, digits=2, align=c("c","c","c","c","c")) %>% 
    kable_styling(bootstrap_options = "striped", full_width = F, position="center")
  
}

# Read in the data
voters <- read.csv("us_public_opinion.csv")

# Check out the variables:  
str(voters)
summary(voters)

table(voters$gender)
table(voters$race_white)
table(voters$ideo5)
table(voters$pid3)

# Recode and clean values:
# Candidate ratings that are greater than 100 recoded to NA
voters$ftsanders[voters$ftsanders > 100] <- NA
voters$fthrc[voters$fthrc > 100] <- NA

# Remove the ideo5 "I don't know" cases.
voters$ideo5[voters$ideo5 == 6] <- NA

# Remove the pid3 "Other" and "Not Sure"
voters$ideo5[voters$pid3 == 4] <- NA
voters$ideo5[voters$pid3 == 5] <- NA

voters <- voters[complete.cases(voters),]

# a) Create a variable for age (use 2015 because this was conducted in January 2016)
voters$age <- 2015 - voters$birthyr

# b) Create a dummary variable for being female
voters$female <- voters$gender - 1

voters$ideo5 <- voters$ideo5 - 1

voters$adjustedAge <- voters$age - 18

# Create a variable to be the difference between a voter's rating of Sanders and Clinton
voters$diff <- voters$ftsanders - voters$fthrc


# Create factors from numeric variables
voters$ideolabel <- factor(voters$ideo5, labels = c("V.Liberal","Liberal","Moderate","Conservative","V.Conservative"))
voters$racelabel <- factor(voters$race_white, labels = c("Non-white","White"))
voters$racelabel <- relevel(voters$racelabel, "White")
voters$femalelabel <- factor(voters$female, labels = c("Men","Women"))
voters$pidlabel <- factor(voters$pid3, labels = c("Democrat","Republican","Independent"))

# Check for variance in age over partisanship
anova(lm(age~pid3, data=voters))

# Check for variance in age over ideology
anova(lm(age~ideo5, data=voters))

# Check independence of demographics and ideology
chisq.test(table(voters$racelabel,voters$ideolabel))
chisq.test(table(voters$femalelabel,voters$ideolabel))
chisq.test(table(voters$racelabel,voters$pidlabel))

chisq.test(table(voters$pidlabel,voters$ideolabel))

ageDiffModel <- lm(diff~adjustedAge, data=voters)

# T.test for gender versus candidate preference
t.test(diff~femalelabel,data=voters)

# T.test for gender versus candidate preference
t.test(diff~race_white,data=voters)
chisq.test(table(voters$femalelabel,voters$pidlabel))

# Build several explanatory models
model1 <- lm(diff~ideo5, data=voters)
model2 <- lm(diff~ideo5 + pidlabel, data=voters)
model3 <- lm(diff~ideo5 + pidlabel + adjustedAge + femalelabel + racelabel, data=voters)
model4 <- lm(diff~ideo5 + pidlabel + adjustedAge + femalelabel + racelabel + ideo5:pidlabel + ideo5:femalelabel + ideo5:racelabel, data=voters)

covModel1         <- vcovHC(model1, type = "HC1")
covModel2         <- vcovHC(model2, type = "HC1")
covModel3         <- vcovHC(model3, type = "HC1")
covModel4         <- vcovHC(model4, type = "HC1")

robust_se1    <- sqrt(diag(covModel1))
robust_se2    <- sqrt(diag(covModel2))
robust_se3    <- sqrt(diag(covModel3))
robust_se4    <- sqrt(diag(covModel4))

waldtest(model1, vcov = covModel1, test="F")
waldtest(model2, vcov = covModel2, test="F")
waldtest(model3, vcov = covModel3, test="F")
waldtest(model4, vcov = covModel4, test="F")

waldtest(model1,model2,model3,model4, test="F")

# Test for Heteroskedasticity
bptest(model4)

# Test For Normality of Residuals
shapiro.test(model4$residuals)

# Test for autocorrelation of residuals
durbinWatsonTest(model4)
