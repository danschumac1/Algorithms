library(tidyverse)
library(ggpubr)
library(car)
library(DescTools)



df_births <- read.csv('birthweight.csv', header = T)

#lets check it out
df_births %>%  head()



# The data looks pretty close to normally distributed
boxplot(df_births$Weight,
        main = "Weight Distribution Boxplot",
        ylab = "Infant Birth Weight (Grams)"
)

# This is pretty spot on
qqnorm(
  df_births$Weight,
  main = 'Weight QQplot',
  ylab='Infant Birth Weight(Grams'); qqline(df_births$Weight,col =2)

# This is another sign for normal distribution. Can't reject H0: normal distribution
shapiro.test(df_births$Weight)

# We will continue assuming normality with evidence in the bag!




# It looks like the variance of not smokers is  higher than the non smokers. 
# Also on average the smoker's babies weigh less.
boxplot(Weight ~ MomSmoke,df_births)


df_smoker <- 
  filter(df_births, MomSmoke == 1)

# High p-val means cannot rej H0: NORMAL DISTRIBUTION
shapiro.test(df_smoker$Weight)

df_nonsmoker <- 
  filter(df_births, MomSmoke == 0)

# High p-val means cannot rej H0: NORMAL DISTRIBUTION
shapiro.test(df_nonsmoker$Weight)


# We proved every step of the way above that our data is normally distributed. 
# Therefor we will use the two-sample-t-test to check for a significant mean weight 
# difference in babies from smoker/nonsmoker parents.

# H0:   mu_smoke == mu_nonsmoker
# Halt: mu_smoke != mu_nonsmoker



# We can reject H0 and accept Halt: mu_smoke != mu_nonsmoker
t.test(df_births$MomSmoke, df_births$Weight, alternative = 'two.sided',var.equal = T)


# we need to check for equal variance
# Levene Test H0:     Equal var
#             Halt:   Non equal var

# Below we see that we cannot reject H0: Equal variance among groups.
# it holds up, it is okay to perform ANOVA

df_births$MomSmokef = as.factor(df_births$MomSmoke)
aov.births = aov(Weight ~ MomSmokef, data = df_births)
leveneTest(aov.births)



# Remember hypothesis 
#     H0:   mu_smoke == mu_nonsmoker
#     Halt: mu_smoke != mu_nonsmoker

# with a low p-val of 0.00233, we can reject H0 and accept Halt: mu_smoke != mu_nonsmoker
summary(aov.births)



# set them up as factor variables.
df_births$Black = as.factor(df_births$Black)
df_births$Married = as.factor(df_births$Married)
df_births$Boy = as.factor(df_births$Boy)
df_births$MomSmoke = as.factor(df_births$MomSmoke)
df_births$Ed  = as.factor(df_births$Ed )

aov.births_full <-
  aov(Weight ~ Black + MomSmoke,
      data = df_births)

# summary(aov.births_full)
Anova(aov.births_full, type = 3)

# I started with all the variables (Weight ~ Black + Married + Boy + MomSmoke + Ed)
# Out of the variables that were not significant, I cut out the one with the largest p-value. 
# then I reran the test without that value. 
# I repeated this process until all of my variables were significant. 
# I axed Ed then Married then Boy and was left with: (Weight ~ Black + MomSmoke)
# then I tested for an interaction effect (Weight ~ Black * MomSmoke)
# Since the interaction effect was not significant I converted back a model 
# Therefor, my final model is: (Weight ~ Black + MomSmoke) .



# My final model is (Weight ~ Black + MomSmoke)

# R^2 = SS(mod)/SS(Total)

# or

# r^2 = ( SS(Black) + SS(MomSmoke) ) / ( SS(Black) + SS(MomSmoke)+ SS(Residuals) )

# or

# r^2 = ( 3530450  + 2513301 ) / ( 3530450  + 2513301 + 70494249 )

# In conclusion

# r^2 = 0.0789

# The Model explains 7.90% of the variation.

# we can check this using R

LM <- lm(Weight ~ Black + MomSmoke, data = df_births)

results <- summary(LM)
print(paste('R Sqaured:', results$r.squared))

# this shows that our R^2 is 7.90%

# Normality looks good

par(mfrow=c(2,2))
plot(aov.births_full)




ScheffeTest(aov.births_full)

# Black babies are on average 293.94 grams lighter than white babies

# Smoke-Moms have babies that are on average 266.76 grams lighter than non-smoke-moms.
