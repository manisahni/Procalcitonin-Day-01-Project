2 week Survival Prediction Project: (Kill the APACHE)
========================================================

Our Aim is to develop a tool that competes with the APACHE score prediction system and allows clinicians to model scenarios that will predict probability of 2 week survival of patients. We are using admission day labs, clinical data about patient health (parts of their medical history) and some CPT codes that allow clinicians to enter or model clinical variable (such as if they are put on a ventilator, if they are started on RRT or if they require a central line). Currently I am trialing the first iteration of this model on CHF patients. But hopefully will expand to a more generalised model.



```r
chf <- read.csv("~/Dropbox/Procalcitonin CHF Project/chf.csv")
glm <- glm(two_week~LACTATE.d01+AGE+PROCALCITONIN.d01+min_sodium+min_hb+gender+devaki+rrtbin+COPD+CR.d01+AFIB+ALB.d01+ESLD+PHtn+Transplant.y+IHD+VentCPT+CatheterCPT, data=chf, family=binomial(link=logit), na.action=na.exclude)
```

```
## Error: object 'COPD' not found
```

```r
summary(glm)
```

```
## Error: object of type 'closure' is not subsettable
```

```r
library(pROC)
```

```
## Type 'citation("pROC")' for a citation.
## 
## Attaching package: 'pROC'
## 
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
```

```r
m.roc <- roc(chf$two_week, predict(glm, backtransform = TRUE))
```

```
## Error: no applicable method for 'predict' applied to an object of class
## "function"
```

```r
plot(m.roc)
```

```
## Error: object 'm.roc' not found
```

## Selecting the best model using the glmmulti package

Lets used the glm multi package to select the best model for the two week outome


```r
library(glmulti)



chf.surv <- glmulti(glm, # use the model with built as a starting point
                      level = 1,  #  just look at main effects
                      crit="aicc", method="g") # use AICc because it works better than AIC for small sample sizes)

summary(chf.surv)
```

After 820 generations:
Best model: two_week~1+ESLD+VentCPT+PHtn+IHD+Htn+min_hb+AGE+ALB.d01
Crit= 93.7236292379163
Mean crit= 100.789138411269
Improvements in best and average IC have bebingo en below the specified goals.
Algorithm is declared to have converged.
Completed.


Lets try out our  best model

```r
bestmodels <- glm(two_week~1+ESLD+VentCPT+PHtn+IHD+Htn+min_hb+AGE+ALB.d01 ,data=chf, family=binomial(link=logit), na.action=na.exclude)
```

```
## Error: object 'IHD' not found
```

```r
library(pROC)
m.roc <- roc(chf$two_week, predict(bestmodels, backtransform = TRUE))
```

```
## Error: object 'bestmodels' not found
```

```r
plot(m.roc)
```

```
## Error: object 'm.roc' not found
```
Not really as good as the whole model, I guess the genetic glm multi search didnt improve the model very much. We will chuck it. 

We will take a more reasoned clinical based approach, divide the predictors into 3 categories

Class 1) Admission Day Labs
Class 2) Patient History
Class 3) Procedures/Events in the Hospital


```r
admitlabs <- c("HCO3","PLATELETS.d01","LACTATE.d01","CR.d01","PROCALCITONIN.d01","ALB.d01","min_sodium","min_hb")
history <-c("gender","COPD","AFIB","ESLD","PHtn","AGE","Transplant.y","CHFTYPE","IHD")
interactions <- c("PLATELETS.d01:ESLD")
month.surv <- chf$event
fourteen <- chf$two_week
```

Categories 1 and 2 are immediately available to clinicians on the time of admission; events , procedures status may or may not be available to the clinician. Eg a patient may get admitted to the hospital ICU and then get intubated and get a central line in the on HOD subsequent 2 day. In this case the clinician will not have the VentStatus and the Central line status available at the time of admission but will have it in 2 days. They can thus use the model for restimation of th 2 week mortality category at the time.

Lets try to fit a model based on Predictor Class 1 and 2 only, since this information is immediately available to clinicians at the time of admission.


```r
 glm <- glm(event~ HCO3+ PLATELETS.d01+LACTATE.d01+AGE+PROCALCITONIN.d01+min_sodium+min_hb+gender+COPD+CR.d01+AFIB+ALB.d01+ESLD+PHtn+Transplant.y+IHD+PLATELETS.d01:ESLD, data=chf, family=binomial(link=logit), na.action=na.exclude)
```

```
## Error: object 'HCO3' not found
```

```r
library(pROC)
m.roc <- roc(chf$event, predict(glm, backtransform = TRUE))
```

```
## Error: no applicable method for 'predict' applied to an object of class
## "function"
```

```r
plot(m.roc)
```

```
## Error: object 'm.roc' not found
```

The following is the 2 week survival model based on clinical history and admit labs alone. The AUC on this model is reasonably good at 0.89, hopefully we can add on more terms to refine this model.


```r
glm <- glm(two_week~ HCO3+ PLATELETS.d01+LACTATE.d01+AGE+PROCALCITONIN.d01+min_sodium+min_hb+gender+COPD+CR.d01+AFIB+ALB.d01+ESLD+PHtn+Transplant.y+IHD+PLATELETS.d01:ESLD+CHFTYPE, data=chf, family=binomial(link=logit), na.action=na.exclude)
```

```
## Error: object 'HCO3' not found
```

```r
library(pROC)
m.roc <- roc(chf$two_week, predict(glm, backtransform = TRUE))
```

```
## Error: no applicable method for 'predict' applied to an object of class
## "function"
```

```r
plot(m.roc)
```

```
## Error: object 'm.roc' not found
```

Now we will add in event information that the physician may not have nessecarily have on admission. Lets suppose that we give wether the patient was Vent during this admission (represented by the VentCPT code). The AUC jumps to 0.9243


```r
glm <- glm(two_week~ HCO3+ PLATELETS.d01+LACTATE.d01+AGE+PROCALCITONIN.d01+min_sodium+min_hb+gender+COPD+CR.d01+AFIB+ALB.d01+ESLD+PHtn+Transplant.y+IHD+PLATELETS.d01:ESLD+CHFTYPE+VentCPT, data=chf, family=binomial(link=logit), na.action=na.exclude)
```

```
## Error: object 'HCO3' not found
```

```r
m.roc <- roc(chf$two_week, predict(glm, backtransform = TRUE))
```

```
## Error: no applicable method for 'predict' applied to an object of class
## "function"
```

```r
plot(m.roc)
```

```
## Error: object 'm.roc' not found
```

So far we have not incorporated any information on labs and any information to the model about what could have potentially changed beyond day 0-1 except for the VentCPT.
Suppose we know that the patient was started on RRT at some point during the admission.

```r
glm <- glm(two_week~ HCO3+ PLATELETS.d01+LACTATE.d01+AGE+PROCALCITONIN.d01+min_sodium+min_hb+gender+COPD+CR.d01+AFIB+ALB.d01+ESLD+PHtn+Transplant.y+IHD+PLATELETS.d01:ESLD+CHFTYPE+rifle3+VentCPT, data=chf, family=binomial(link=logit), na.action=na.exclude)
```

```
## Error: object 'HCO3' not found
```

```r
m.roc <- roc(chf$two_week, predict(glm, backtransform = TRUE))
```

```
## Error: no applicable method for 'predict' applied to an object of class
## "function"
```

```r
plot(m.roc)
```

```
## Error: object 'm.roc' not found
```


Here is the data for the 30 day survival AUC in this model.

```r
glm <- glm(event~ HCO3+ PLATELETS.d01+LACTATE.d01+AGE+PROCALCITONIN.d01+min_sodium+min_hb+gender+COPD+CR.d01+AFIB+ALB.d01+ESLD+PHtn+Transplant.y+IHD+PLATELETS.d01:ESLD+CHFTYPE+rifle3+VentCPT, data=chf, family=binomial(link=logit), na.action=na.exclude)
```

```
## Error: object 'HCO3' not found
```

```r
m.roc <- roc(chf$event, predict(glm, backtransform = TRUE))
```

```
## Error: no applicable method for 'predict' applied to an object of class
## "function"
```

```r
plot(m.roc)
```

```
## Error: object 'm.roc' not found
```
