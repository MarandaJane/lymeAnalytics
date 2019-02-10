library(texreg)
data<-read.csv("finalData.csv")


fit1 <- lm(  lessHSPercent + aggregateMedianIncPercentage ~ aggregateCasesPerOneHundredThousand+ factor(STNAME), data)

fit2 <- lm( aggregateCases ~ hsOnlyPercent +aggregateMedianIncPercentage + factor(STNAME), data)

fit3<-  lm( aggregateCases ~ someCollegePercent + aggregateMedianIncPercentage+ factor(STNAME), data)

fit4<-  lm( aggregateCases ~ bachelorPercent + aggregateMedianIncPercentage +  factor(STNAME), data)



combinedEducation<-screenreg( list(edFit1, edFit2, edFit3, edFit4), omit.coef="factor", custom.model.names=c("Regression of lessThanHighSchoolDiploma", "Regression of highSchooDiplomaOnly with medianIncome", "Regression of someCollegeOrAssociateDegree with medianIncome", "Regression of bachelorDegreeOrHigher with medianIncome"), include.rsquared=FALSE, include.rmse=FALSE)
