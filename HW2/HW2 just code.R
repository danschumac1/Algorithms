library(ggplot2)
library(tidyverse)
library(DescTools)
library(MASS)
library(car)

df_hearts <- read.csv('heartbpchol.csv', header = T)
head(df_hearts)
df_hearts$BP_Status = as.factor(df_hearts$BP_Status)
head(df_hearts)

# it isn't balanced but the tests stay the same for 1-way ANOVA
table(df_hearts$BP_Status) 

# The Variance looks pretty equal, Optimal is a little smaller.
boxplot(Cholesterol ~ BP_Status, data = df_hearts)

# what does this step do?
# we use the anova model before verify assumptions to take advantage of the LT test
# this generates anova output
aov.hearts = aov(Cholesterol ~ BP_Status, data = df_hearts)

# tests for equal var (use for 1-way ANOVA or 2-way Balanced*** maybe)
# p-val = 0.8332 can't reject H0: Equal variance amongst groups
LeveneTest(aov.hearts)

# visually test equal var Larger than 2 way just use visual
plot(aov.hearts)

# now we can interpret aov results
# with a p-val .00137 we reject H0 and accept Halt: at least 1 group has ***
summary(aov.hearts)

ScheffeTest(aov.hearts)

df_bupa <- read.csv('bupa.csv')
head(df_bupa)

#change drinkgroup to factor var
df_bupa$drinkgroup <- as.factor(df_bupa$drinkgroup)
head(df_bupa)

aov.mcv = aov(mcv ~ drinkgroup , data = df_bupa)
LeveneTest(aov.mcv)
summary(aov.mcv)

aov.alkphos = aov(alkphos ~ drinkgroup , data = df_bupa)
LeveneTest(aov.alkphos)
summary(aov.alkphos)

ScheffeTest(aov.mcv)

ScheffeTest(aov.alkphos)

df_psych <- read.csv('psych.csv')
df_psych$sex <- as.factor(df_psych$sex)
df_psych$rank <- as.factor(df_psych$rank)
head(df_psych,22)

# Make ANOVA model
# We have an unbalanced data set. so lets set up Sex based off of rank and rank based off of sex
aov.psych.sr <- aov(salary ~ sex * rank, data = df_psych)
aov.psych.rs <- aov(salary ~ rank * sex, data = df_psych)

#Check for Variance with 2-way, just use visual
par(mfrow = c(1,2)) ; plot(aov.psych.sr, c(1,2))
par(mfrow = c(1,2)) ; plot(aov.psych.rs, c(1,2))
#looks good!

# These are Type 1 tests sr = sex -> rank. rs is reverse
summary(aov.psych.sr)
summary(aov.psych.rs)
Anova(aov.psych.rs, type = 3)

aov.psych.no_interaction_term <- 
  aov(salary ~ sex + rank, data = df_psych)

summary(aov.psych.no_interaction_term)
Anova(aov.psych.no_interaction_term, type = 3)

# I have already checked for Normality above.

# I picked the aov.psych.no_interaction_term as my model because we are primarily concered about the interaction between sex and salary. This is a simpler model because it does not include the interaction effect. Simpler is good.

ScheffeTest(aov.psych.no_interaction_term)

df_cars <- read.csv('cars_new.csv')
df_cars$cylinders <- as.factor(df_cars$cylinders)
df_cars$origin <- as.factor(df_cars$origin)
df_cars$type <- as.factor(df_cars$type)
head(df_cars)

#our data is unbalanced
table(df_cars$cylinders);table(df_cars$origin);table(df_cars$type)

aov.cars <-
  aov(mpg_highway ~ cylinders + origin + type,
      data = df_cars)

# check variance
# This does not at all look like equal variance amongst groups. However we will continue as if it was because our teacher told us to.
par(mfrow = c(1,2)) ; plot(aov.cars, c(1,2))

#based off of the below results, origin is not significant. Lets redefine our model by dropping origin.
Anova(aov.cars, type = 3)

aov.cars <-
  aov(mpg_highway ~ cylinders + type,
      data = df_cars)

#this still does not look like equal variance to me.
par(mfrow = c(1,2)) ; plot(aov.cars, c(1,2))

summary(aov.cars)

Anova(aov.cars, type = 3)

# lets try building another model that has an interaction term but still leaves out origin.

aov.cars_ct <-
  aov(mpg_highway ~ cylinders * type,
      data = df_cars)

aov.cars_tc <-
  aov(mpg_highway ~ cylinders * type,
      data = df_cars)

#both type 1s and the type 3 test all show significant for both of our main effects and our interaction effect. Awesome!
summary(aov.cars_ct)
summary(aov.cars_tc)
Anova(aov.cars, type = 3)

# lets try building another model that has an interaction term but still leaves out origin.

aov.cars_ct <-
  aov(mpg_highway ~ cylinders * type,
      data = df_cars)

aov.cars_tc <-
  aov(mpg_highway ~ cylinders * type,
      data = df_cars)

#both type 1s and the type 3 test all show significant for both of our main effects and our interaction effect. Awesome!
summary(aov.cars_ct)
summary(aov.cars_tc)
Anova(aov.cars, type = 3)