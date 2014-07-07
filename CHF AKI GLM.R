library(glmulti)

chfrifle3 <- glm(rifle3~log(PROCALCITONIN.d01)+LACTATE.d01+ WBC.d01+DM2+CR.d01+gender+ALB.d01+Htn+min_sodium+min_hb,data=globalnoesrd, family=binomial(link=logit))

multi.global.model <- glmulti(chfrifle3, # use the model with built as a starting point
                              level = 1,  #  just look at main effects
                              crit="aicc") # use AICc because it works better than AIC for small sample sizes)

summary(multi.global.model)