library(texreg)
data<-read.csv("finalData.csv")

#Check individual education columns to test how good of a fit they are
edFit1 <- lm( lessHSPercent ~ aggregateCasesPerOneHundredThousand + factor(STNAME), data)

edFit2 <- lm( hsOnlyPercent ~ aggregateCasesPerOneHundredThousand  + factor(STNAME), data)

edFit3<-  lm( someCollegePercent ~ aggregateCasesPerOneHundredThousand  + factor(STNAME), data)

edFit4<-  lm( bachelorPercent ~ aggregateCasesPerOneHundredThousand  + factor(STNAME), data)


#Check combined education columns to test if education in general is a good fit
combinedEdFit<-  lm( normalizedData ~ lessHighSchoolDiploma+ highSchoolDiplomaOnly +  bachelorDegreeOrHigher + someCollegeOrAssociateDegree + bachelorDegreeOrHigher +  factor(STNAME), data)


#Check county(index) to test if it is a good fit
indexFit <- lm( normalizedData ~ Index + factor( STNAME), data)
screenreg( list(indexFit), omit.coef="factor", custom.model.names=c("Regression of Ed1"), include.rsquared=FALSE, include.rmse=FALSE)



#Plot education regressions

#edFit1
plot(data[,22], data[,32], ann=FALSE) 
abline(edFit1)    
abline(edFit1, lwd=3) 
abline(edFit1, lwd=3, col="red") 
title(main = "Less Than a High School Education",sub= NULL, xlab = "Cases of Lyme Disease By County", ylab = "% of Persons with Less Than a High School Education by County")

screenreg( list(edFit1), omit.coef="factor", custom.model.names=c("Regression of Ed1"), include.rsquared=FALSE, include.rmse=FALSE)

#edFit2
plot(data[,22], data[,34], ann=FALSE) 
abline(edFit2)  
abline(edFit2, lwd=3) 
abline(edFit2, lwd=3, col="red") 
title(main = "High School Diploma Only",sub= NULL, xlab = "Cases of Lyme Disease By County", ylab = "% of Persons with High School Diploma Only")


screenreg( list(edFit2), omit.coef="factor", custom.model.names=c("Regression of Ed1"), include.rsquared=FALSE, include.rmse=FALSE)


#edFit3
plot(data[,22], data[,36], ann=FALSE) 
abline(edFit3)  
abline(edFit3, lwd=3) 
abline(edFit3, lwd=3, col="red") 
title(main = "Some College or Associate's Degree",sub= NULL, xlab = "Cases of Lyme Disease By County", ylab = "% of Persons with Some College or Associate's Degree")


screenreg( list(edFit3), omit.coef="factor", custom.model.names=c("Regression of Ed1"), include.rsquared=FALSE, include.rmse=FALSE)


#edFit4
plot(data[,22], data[,38], ann=FALSE) 
abline(edFit4)  
abline(edFit4, lwd=3) 
abline(edFit4, lwd=3, col="red") 
title(main = "Bachelor's Degree or Higher",sub= NULL, xlab = "Cases of Lyme Disease By County", ylab = "% of Persons with Bachelor's Degree or Higher")




screenreg( list(edFit4), omit.coef="factor", custom.model.names=c("Regression of Ed1"), include.rsquared=FALSE, include.rmse=FALSE)


#combinedEdFit

plot(data[,30], data[,35]) 
abline(combinedEdFit)  
abline(combinedEdFit, lwd=3) 
abline(combinedEdFit, lwd=3, col="red") 


#Plot county regression
plot(data[,22], data[,1]) 
abline(indexFit)  
abline(indexFit, lwd=3) 
abline(indexFit, lwd=3, col="red") 






combinedEducation<-screenreg( list(edFit1, edFit2, edFit3, edFit4), omit.coef="factor", custom.model.names=c("Regression of lessThanHighSchoolDiploma", "Regression of highSchooDiplomaOnly", "Regression of someCollegeOrAssociateDegree", "Regression of bachelorDegreeOrHigher"), include.rsquared=FALSE, include.rmse=FALSE)
#aggregateCases: average number of 5 years
#aggregatePopulation: average population of 5 years
#NormalizedCases: (aggregateCases*100000) / aggregatePopulation




