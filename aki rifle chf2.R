anonMASTER3 <- mutate(anonMASTER, devakirifle3=(d2max_cr/CR.d01)>=3)
anonmaster4 <- mutate(anonMASTER3, rifle3 = (devakirifle3=="TRUE" | rrtbin =="TRUE"))
data <- anonmaster4
chf <- subset(data, CHF=="CHF")
chfnoesrd<- subset(chf, ESRD_CKD5=="non ESRD.CKD5" )
glm.chf.aki.rifle3 <- glm(rifle3~log(PROCALCITONIN.d01)+LACTATE.d01+ WBC.d01+DM2+CR.d01+gender+ALB.d01+ESLD+PHtn+Htn,data=chfnoesrd, family=binomial(link=logit))
summary(glm.chf.aki.rifle3)
glmrifle3 <- glm.chf.aki.rifle3

int <- exp(confint(glm.chf.aki.rifle3))
odds <- exp(coef(glm.chf.aki.rifle3))
akioddstable <- cbind(odds,int)
akioddstable