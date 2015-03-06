Model for Healthcare Quality
========================================================

### Description

### Format

The variables in the dataset quality.csv are as follows:

* __MemberID__ numbers the patients from 1 to 131, and is just an identifying number.

* __InpatientDays__ is the number of inpatient visits, or number of days the person spent in the hospital.

* __ERVisits__ is the number of times the patient visited the emergency room.

* __OfficeVisits__ is the number of times the patient visited any doctor's office.

* __Narcotics__ is the number of prescriptions the patient had for narcotics.

* __DaysSinceLastERVisit__ is the number of days between the patient's last emergency room visit and the end of the study period (set to the length of the study period if they never visited the ER). 

* __Pain__ is the number of visits for which the patient complained about pain.

* __TotalVisits__ is the total number of times the patient visited any healthcare provider.

* __ProviderCount__ is the number of providers that served the patient.
MedicalClaims is the number of days on which the patient had a medical claim.

* __ClaimLines__ is the total number of medical claims.

* __StartedOnCombination__ is whether or not the patient was started on a combination of drugs to treat their diabetes (TRUE or FALSE).

* __AcuteDrugGapSmall__ is the fraction of acute drugs that were refilled quickly after the prescription ran out.

* __PoorCare__ is the outcome or dependent variable, and is equal to 1 if the patient had poor care, and equal to 0 if the patient had good care.


```r
# Week 3, Modeling the Expert


# Video 4

# Read in dataset
quality = read.csv("F:/SkyDrive/Studying/MIT_COURSES/Th AnalyticsEdge_15.071x/lecture/dataset/quality.csv")

# Look at structure
str(quality)
```

```
## 'data.frame':	131 obs. of  14 variables:
##  $ MemberID            : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ InpatientDays       : int  0 1 0 0 8 2 16 2 2 4 ...
##  $ ERVisits            : int  0 1 0 1 2 0 1 0 1 2 ...
##  $ OfficeVisits        : int  18 6 5 19 19 9 8 8 4 0 ...
##  $ Narcotics           : int  1 1 3 0 3 2 1 0 3 2 ...
##  $ DaysSinceLastERVisit: num  731 411 731 158 449 ...
##  $ Pain                : int  10 0 10 34 10 6 4 5 5 2 ...
##  $ TotalVisits         : int  18 8 5 20 29 11 25 10 7 6 ...
##  $ ProviderCount       : int  21 27 16 14 24 40 19 11 28 21 ...
##  $ MedicalClaims       : int  93 19 27 59 51 53 40 28 20 17 ...
##  $ ClaimLines          : int  222 115 148 242 204 156 261 87 98 66 ...
##  $ StartedOnCombination: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ AcuteDrugGapSmall   : int  0 1 5 0 0 4 0 0 0 0 ...
##  $ PoorCare            : int  0 0 0 0 0 1 0 0 1 0 ...
```

```r

# Table outcome
table(quality$PoorCare)
```

```
## 
##  0  1 
## 98 33
```

```r

# Baseline accuracy
98/131
```

```
## [1] 0.7481
```

```r

# Install and load caTools package
install.packages("caTools")
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(caTools)
```

```
## Error: there is no package called 'caTools'
```

```r

# Randomly split data
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
```

```
## Error: could not find function "sample.split"
```

```r
split
```

```
## function (x, f, drop = FALSE, ...) 
## UseMethod("split")
## <bytecode: 0x0000000005ded0f8>
## <environment: namespace:base>
```

```r

# Create training and testing sets
qualityTrain = subset(quality, split == TRUE)
```

```
## Error: comparison (1) is possible only for atomic and list types
```

```r
qualityTest = subset(quality, split == FALSE)
```

```
## Error: comparison (1) is possible only for atomic and list types
```

```r

# Logistic Regression Model
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
```

```
## Error: object 'qualityTrain' not found
```

```r
summary(QualityLog)
```

```
## Error: object 'QualityLog' not found
```

```r

# Make predictions on training set
predictTrain = predict(QualityLog, type = "response")
```

```
## Error: object 'QualityLog' not found
```

```r

# Analyze predictions
summary(predictTrain)
```

```
## Error: object 'predictTrain' not found
```

```r
tapply(predictTrain, qualityTrain$PoorCare, mean)
```

```
## Error: object 'qualityTrain' not found
```

```r



# Video 5

# Confusion matrix for threshold of 0.5
table(qualityTrain$PoorCare, predictTrain > 0.5)
```

```
## Error: object 'qualityTrain' not found
```

```r

# Sensitivity and specificity
10/25
```

```
## [1] 0.4
```

```r
70/74
```

```
## [1] 0.9459
```

```r

# Confusion matrix for threshold of 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)
```

```
## Error: object 'qualityTrain' not found
```

```r

# Sensitivity and specificity
8/25
```

```
## [1] 0.32
```

```r
73/74
```

```
## [1] 0.9865
```

```r

# Confusion matrix for threshold of 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)
```

```
## Error: object 'qualityTrain' not found
```

```r

# Sensitivity and specificity
16/25
```

```
## [1] 0.64
```

```r
54/74
```

```
## [1] 0.7297
```

```r



# Video 6

# Install and load ROCR package
install.packages("ROCR")
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(ROCR)
```

```
## Error: there is no package called 'ROCR'
```

```r

# Prediction function
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
```

```
## Error: could not find function "prediction"
```

```r

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
```

```
## Error: could not find function "performance"
```

```r

# Plot ROC curve
plot(ROCRperf)
```

```
## Error: object 'ROCRperf' not found
```

```r

# Add colors
plot(ROCRperf, colorize = TRUE)
```

```
## Error: object 'ROCRperf' not found
```

```r

# Add threshold labels
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0, 1, by = 0.1), text.adj = c(-0.2, 
    1.7))
```

```
## Error: object 'ROCRperf' not found
```


You can also embed plots, for example:


```r
plot(cars)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


