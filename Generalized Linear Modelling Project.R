# Assignment ST6033

Gender <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

Severe <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,
            1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2)
Information <- c(1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2,
                 1,1,1,1,2,2,2,2,1,1,1,1,2,2,2,2)
Age <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,
         1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
n <- c(18,23,22,17,19,35,30,22,24,37,29,24,28,42,37,30,
       11,25,12,8,14,34,22,21,18,28,24,8,28,47,45,30)
Changed_Behaviour <- c(11,14,11,5,4,15,8,8,10,13,8,6,11,14,15,9,
                       6,13,7,8,7,15,8,5,12,15,7,1,13,21,11,6)
data = data.frame(Gender,Severe,Information,Age,n,Changed_Behaviour)
data
summary(data)

# Preliminary Analysis
# Plotting the proportions versus each factor

par(mfrow=c(2,2))
prop <- Changed_Behaviour/n
plot(Gender,prop)

# Female had more changed behaviour relating to hygiene than their males
plot(Severe, prop)
# The persons who believed the consequences of contracting swine flu were severe
# changed behaviour relating to hygiene almost as equally as persons who didnt
# believe the consequences of contracting swine flu were severe.

plot(Information,prop)
# The persons who believed the information available about swine flu were adequate
# had more behavioural change relating to hygiene than those who believed the
# information available about swine flu was inadequate.

plot(Age, prop)
# The persons changed behaviour relating to hygiene decreased with the increase
# in age. There is a possible case of an outlier for females aged 60+ who thought that the
# consequences of contracting swine flu were severe, and believed the information
# available about swine flu was adequate.

y <- cbind(Changed_Behaviour, n - Changed_Behaviour)

glmo <- glm(y~factor(Gender) + factor(Severe) + factor(Information) +
              factor(Age), family = binomial(link=logit))
summary(glmo)

# All the possible 2 way interactions
# 1) Gender + Severe
glm_GS <- glm(y ~ factor(Gender) + factor(Severe), family = binomial(link = logit))
summary(glm_GS)

#2) Gender + Information
glm_GI <- glm(y ~ factor(Gender) + factor(Information), family = binomial(link = logit))
summary(glm_GI)

#3) Gender + Age
glm_GA <- glm(y ~ factor(Gender) + factor(Age), family = binomial(link = logit))
summary(glm_GA)

#4) Severe +Information
glm_SI <- glm(y ~ factor(Severe) + factor(Information), family = binomial(link = logit))
summary(glm_SI)

#5) Severe + Age
glm_SA <- glm(y ~ factor(Severe) + factor(Age), family = binomial(link = logit))
summary(glm_SA)

#6) Information + Age
glm_IA <- glm(y ~ factor(Information) + factor(Age), family = binomial(link = logit))
summary(glm_IA)

#7) Gender + Severe + GS
glm_GSI <- glm(y ~ factor(Gender) + factor(Severe) + factor(Gender):factor(Severe), family = binomial(link = logit))
summary(glm_GSI)
# Looks not significant therefore will not consider it

#8) Gender + Information + GI
glm_GII <- glm(y ~ factor(Gender) + factor(Information) + factor(Gender):factor(Information), family = binomial(link = logit))
summary(glm_GII)

#9) Gender + Age + GA
glm_GAI <- glm(y ~ factor(Gender) + factor(Age) + factor(Gender):factor(Age), family = binomial(link = logit))
summary(glm_GAI)
# Not significant, hence will not be considered

#10) Severe + Information + SI
glm_SII <- glm(y ~ factor(Severe) + factor(Information) + factor(Severe):factor(Information), family = binomial(link = logit))
summary(glm_SII)

#11) Severe + Age + SA
glm_SAI <- glm(y ~ factor(Severe) + factor(Age) + factor(Severe):factor(Age), family = binomial(link = logit))
summary(glm_SAI)
# Not significant, hence we will not consider it

#12) Information + Age + IA
glm_IAI <- glm(y ~ factor(Information) + factor(Age) + factor(Information):factor(Age), family = binomial(link = logit))
summary(glm_IAI)
null <- glm(y~1,family=binomial(link="logit"))
summary(null)

# Gender
glm_G1 <- glm(y ~ factor(Severe) + factor(Information) + factor(Age), family = binomial(link="logit"))
glm_G2 <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Information):factor(Age),family = binomial(link="logit")  )
glm_G3 <- glm(y ~ factor(Information) + factor(Severe) + factor(Age) + factor(Severe):factor(Age),family = binomial(link="logit")  )
glm_G4 <- glm(y ~ factor(Age) + factor(Severe) + factor(Information) + factor(Severe):factor(Information),family = binomial(link="logit")  )

# Severe
glm_S1 <- glm(y ~ factor(Gender) + factor(Information) + factor(Age), family = binomial(link="logit")  )
glm_S2 <- glm(y ~ factor(Gender) + factor(Information) + factor(Age) + factor(Information):factor(Age),family = binomial(link="logit")  )
glm_S3 <- glm(y ~ factor(Information) + factor(Gender) + factor(Age) + factor(Gender):factor(Age),family = binomial(link="logit")  )
glm_S4 <- glm(y ~ factor(Age) + factor(Gender) + factor(Information) + factor(Gender):factor(Information),family = binomial(link="logit")  )

# Age
glm_A1 <- glm(y ~ factor(Gender) + factor(Severe) + factor(Information), family = binomial(link = logit))
glm_A2 <- glm(y ~ factor(Gender) + factor(Severe) + factor(Information) + factor(Severe):factor(Information), family = binomial(link = logit))
glm_A3 <- glm(y ~ factor(Severe) + factor(Gender) + factor(Information) + factor(Gender):factor(Information), family = binomial(link = logit))
glm_A4 <- glm(y ~ factor(Information) + factor(Gender) + factor(Severe) + factor(Gender): factor(Severe), family = binomial(link = logit))

# Information
glm_I1 <- glm(y ~ factor(Gender) + factor(Severe) + factor(Age), family = binomial(link = logit))
glm_I2 <- glm(y ~ factor(Gender) + factor(Severe) + factor(Age) + factor(Severe):factor(Age), family = binomial(link = logit))
glm_I3 <- glm(y ~ factor(Severe) + factor(Gender) + factor(Age) + factor(Gender):factor(Age), family = binomial(link = logit))
glm_I4 <- glm(y ~ factor(Age) + factor(Gender) + factor(Severe) + factor(Gender):factor(Severe), family = binomial(link = logit))


glm_full_GS <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Gender):factor(Severe), family = binomial(link="logit"))
glm_full_GI <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Gender):factor(Information), family = binomial(link="logit"))
glm_full_GA <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Gender):factor(Age), family = binomial(link="logit"))
glm_full_SI <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information), family = binomial(link="logit"))
glm_full_SI <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information), family = binomial(link="logit"))
glm_full_IA <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Age):factor(Information), family = binomial(link="logit"))
summary(glm_full_SI)

glm_full_SI_SA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age), family = binomial(link="logit"))
glm_full_SI_SG <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Gender), family = binomial(link="logit"))
glm_full_SI_GA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Gender):factor(Age), family = binomial(link="logit"))
glm_full_SI_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Information):factor(Age), family = binomial(link="logit"))

a=summary(glm_full_SI_SG_GI)

glm_full_SI_GI <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Gender):factor(Information), family = binomial(link="logit"))
glm_full_SI_SA_SG <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Severe):factor(Gender), family = binomial(link="logit"))
glm_full_SI_SA_SG <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Severe):factor(Gender), family = binomial(link="logit"))
glm_full_SI_SA_GA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Gender):factor(Age), family = binomial(link="logit"))
glm_full_SI_SA_GI <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Gender):factor(Information), family = binomial(link="logit"))
glm_full_SI_SA_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Information):factor(Age), family = binomial(link="logit"))
glm_full_SI_SG_GA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Gender) + factor(Gender):factor(Age), family = binomial(link="logit"))
glm_full_SI_SG_GI <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Gender) + factor(Gender):factor(Information), family = binomial(link="logit"))
glm_full_SI_SG_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Gender) + factor(Information):factor(Age), family = binomial(link="logit"))
 glm_full_SI_SG_GI <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Gender) + factor(Gender):factor(Information), family = binomial(link="logit"))
summary(glm_full_SI_GI_IA)

# Full model with 3 interactions
glm_full_SI_GA_GI <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Gender):factor(Age) + factor(Gender):factor(Information), family = binomial(link="logit"))
glm_full_SI_GA_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Gender):factor(Age) + factor(Information):factor(Age), family = binomial(link="logit"))
glm_full_SI_GA_GI <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Gender):factor(Age) + factor(Gender):factor(Information), family = binomial(link="logit"))
glm_full_SI_GI_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Gender):factor(Information) + factor(Information):factor(Age), family = binomial(link="logit"))
glm_full_SI_GI_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Gender):factor(Information) + factor(Information):factor(Age), family = binomial(link="logit"))
glm_full_SI_SA_SG_GA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Severe):factor(Gender) + factor(Gender):factor(Age), family = binomial(link="logit"))
summary(glm_full_SI_SA_SG_GA_GI)

# Full model with 4 interactions
glm_full_SI_SA_SG_GA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Severe):factor(Gender) + factor(Gender):factor(Age), family = binomial(link="logit"))
glm_full_SI_SA_SG_GI <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Severe):factor(Gender) + factor(Gender):factor(Information), family = binomial(link="logit"))
glm_full_SI_SA_SG_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Severe):factor(Gender) + factor(Information):factor(Age), family = binomial(link="logit"))
glm_full_SI_SG_GA_GI <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Gender) + factor(Gender):factor(Age) + factor(Gender):factor(Information), family = binomial(link="logit"))
glm_full_SI_SG_GA_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Gender) + factor(Gender):factor(Age) + factor(Information):factor(Age), family = binomial(link="logit"))
glm_full_SI_GA_GI_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Gender):factor(Age) + factor(Gender):factor(Information) + factor(Information):factor(Age) , family = binomial(link="logit"))

summary(glm_full_GS_GI_GA_SI_SA_IA)
# Full model with 5 interactions
glm_full_SI_SA_SG_GA_GI <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Severe):factor(Gender) + factor(Gender):factor(Age) + factor(Gender):factor(Information) , family = binomial(link="logit"))
glm_full_SI_SA_SG_GA_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Severe):factor(Gender) + factor(Gender):factor(Age) + factor(Information):factor(Age) , family = binomial(link="logit"))
glm_full_SI_SG_GA_GI_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Gender) + factor(Gender):factor(Age) + factor(Gender):factor(Information) + factor(Information):factor(Age) , family = binomial(link="logit"))
glm_full_SI_GI_GA_SA_IA <-glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Gender):factor(Information) + factor(Gender):factor(Age) + factor(Severe):factor(Age) + factor(Information):factor(Age) , family = binomial(link="logit"))
glm_full_SI_SG_GI_SA_IA <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information) + factor(Severe):factor(Gender) + factor(Gender):factor(Information) + factor(Severe):factor(Age) + factor(Information):factor(Age) , family = binomial(link="logit"))
summary(glm_full_SI_SG_GI_SA_IA)

# Full model with 6 interactions
glm_full_GS_GI_GA_SI_SA_IA <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Gender):factor(Severe) + factor(Gender):factor(Information) + factor(Gender):factor(Age) +  factor(Severe):factor(Information) + factor(Severe):factor(Age) + factor(Information):factor(Age) , family = binomial(link="logit"))

Deviance <- c(null$deviance, glm_GS$deviance,glm_GI$deviance,glm_GA$deviance,glm_SI$deviance,
              glm_SA$deviance,glm_IA$deviance,glm_GSI$deviance, glm_GII$deviance,
              glm_GAI$deviance, glm_SII$deviance, glm_SAI$deviance, glm_IAI$deviance,
              glmo$deviance)
d.f. <- c(null$df.residual, glm_GS$df.residual,glm_GI$df.residual,glm_GA$df.residual,
          glm_SI$df.residual,glm_SA$df.residual,glm_IA$df.residual, glm_GSI$df.residual,
          glm_GII$df.residual, glm_GAI$df.residual, glm_SII$df.residual, 
          glm_SAI$df.residual, glm_IAI$df.residual, glmo$df.residual)
Model <- c('Null','G+S','G+I','G+A','S+I','S+A','I+A','GS','GI','GA','SI','SA','IA','Full')

Deviance <- round(Deviance,2)
deviance_table <- data.frame(Model, Deviance ,d.f.)

# Applying diagonistics on the main model

h<-lm.influence(glm_full_SI)$hat
rpear <- residuals(glm_full_SI,"pearson")/sqrt(1-h)
rdev <- residuals(glm_full_SI,"deviance")/sqrt(1-h)
phat <- glm_full_SI$fitted.values
Changed_behaviour_fitted <- n*phat
Diagnostics.df <- data.frame(Changed_Behaviour, Changed_behaviour_fitted, n, phat,
                             h, rpear, rdev, D)
round(D,5)
par(mfrow=c(1,2))
plot(rpear, main="Index Plot of Pearson Residuals")
abline(h=0)
plot(rdev, main="Index Plot of Deviance Residuals")
abline(h=0)
# Rectangular Pattern
# Both the deviance and pearson residual plotting randomly about 0 with constant range
# No pattern, no problem with the systematic component
# No guarantee of symmetry in either of the graph

plot(glm_full_SI$linear.predictors, rpear, main="Plot of Pearson Residuals vs Linear
     Predictor")
# No pattern hence no problem with the systematic component
plot(glm_full_SI$linear.predictors, rdev, main="Plot of Deviance Residuals vs Linear
     Predictor")
# Checking for outliers
# From the graphs we can see that 1 predictor is above the +3 mark and should
# be investigated

# Checking for high leverage values
# 2(p+1)/n = 0.3125 
# 4 cases of high leverage detected, 10, 14 , 30, 31
Obslogit <- log((Changed_Behaviour/n)/1-(Changed_Behaviour/n))
Predlogit <- log((fitted.values(glm_full_SI))/1-(fitted.values(glm_full_SI)))
plot(Changed_Behaviour,Obslogit)

# Checking for high influence cases
D =rpear*rpear*h/(2*(1-h))
Newdiagnostic.df <- data.frame(Diagnostics.df,D)
plot(D)

# Overdispersion
summary(glm_full_SI)
# 32.582/24 = 1.35, we may have overdispersion due to high variability
glm_full_SI_over <- glm(y ~ factor(Severe) + factor(Information) + factor(Age) + factor(Gender) + factor(Severe):factor(Information), family = quasibinomial(link="logit"))
summary(glm_full_SI_over)
  
# Sparse data
