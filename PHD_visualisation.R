rm(list=ls(all=TRUE))
setwd("C:\\Users\\SIDDARTH\\Desktop\\PHD\\TrainData")
library(glmnet)
library(caret)
library(MASS)
library(vegan)
library(data.table)
library(doParallel)
library(DMwR)
library(dummies)
library(e1071)
library(standardize)
library(ggplot2)
library(arules)
registerDoParallel(8)
one=read.csv("Train.csv",na.strings=c("","NA"))
two=read.csv("Train_Claim.csv",na.strings=c("","NA"))
three=read.csv("Train_Demographics.csv",na.strings=c("","NA"))
four=read.csv("Train_Policy.csv",na.strings=c("","NA"))
five=read.csv("Train_Vehicle.csv")

#Converting all the missing values to NA

#for Data of VEhicle
five$VehicleAttributeDetails=as.factor(ifelse(five$VehicleAttributeDetails=="???",NA,five$VehicleAttributeDetails))

#for Claim INformation
two$TypeOfCollission=as.factor(ifelse(two$TypeOfCollission=="?",NA,two$TypeOfCollission))
two$IncidentTime=as.factor(ifelse(two$IncidentTime=="-5",NA,two$IncidentTime))
two$PropertyDamage=as.factor(ifelse(two$PropertyDamage=="?",NA,two$PropertyDamage))
two$Witnesses=as.factor(ifelse(two$Witnesses=="MISSINGVALUE",NA,two$Witnesses))
two$PoliceReport=as.factor(ifelse(two$PoliceReport=="?",NA,two$PoliceReport))
two$AmountOfTotalClaim=(ifelse(two$AmountOfTotalClaim=="MISSEDDATA",NA,two$AmountOfTotalClaim))

#for policy  INFORMATIOn
four$PolicyAnnualPremium=(ifelse(four$PolicyAnnualPremium=="-1",NA,four$PolicyAnnualPremium))

colSums(is.na(five))
colSums(is.na(two))
colSums(is.na(four))
colSums(is.na(three))
str(five)
str(one)
str(two)
str(three)
str(four)
summary(five)
head(five)
#splitting the vehicle attributes into seprete fields

five_one=five[five$VehicleAttribute %in% c("VehicleID"),]
five_one$VehicleAttributeDetails=as.factor(as.character(five_one$VehicleAttributeDetails))
five_one=five_one[,-2]
str(five_one)       # we can delete Five_one i.e VEHICLE ID as iT OBVIOUSLY HAS all different factors

five_two=five[five$VehicleAttribute %in% c("VehicleMake"),] 
five_two$VehicleAttributeDetails2=as.factor(as.character(five_two$VehicleAttributeDetails))
str(five_two)
five_two=five_two[,c(-2,-3)]
five_three=five[five$VehicleAttribute %in% c("VehicleModel"),] 
five_three$VehicleAttributeDetails3=as.factor(as.character(five_three$VehicleAttributeDetails))
five_three=five_three[,c(-2,-3)]
five_four=five[five$VehicleAttribute %in% c("VehicleYOM"),]
five_four$VehicleAttributeDetails4=as.factor(as.character(five_four$VehicleAttributeDetails))
five_four=five_four[,c(-2,-3)]
str(five_four)


#merging all the csv files
merge1 <- merge(five_three,five_two,by="CustomerID")
merge2 <- merge(merge1,five_four,by="CustomerID")
merge3 <- merge(merge2,four,by="CustomerID")
merge4 <- merge(merge3,three,by="CustomerID")
merge5 <- merge(merge4,two,by="CustomerID")
total <- merge(merge5,one,by="CustomerID")
total$InsurancePolicyNumber=as.factor(total$InsurancePolicyNumber)
#deleting customer ID and insurance policy number 
total=total[,setdiff(names(total),c("CustomerID","InsurancePolicyNumber","Country","AmountOfPropertyClaim_disc","AmountOfTotalClaim_disc","AmountOfInjuryClaim_disc"))]
total$InsuredZipCode=as.factor(as.integer(total$InsuredZipCode/1000))
str(as.factor(as.integer(total$InsuredZipCode/1000)))
#trying to fill NA's of insured gender using insured relationship
total$InsuredGender=ifelse(total$InsuredRelationship=="wife","MALE",total$InsuredGender)
total$InsuredGender=ifelse(total$InsuredRelationship=="husband","FEMALE",total$InsuredGender)

total=centralImputation(total)

#filling NA's in type of collision
ggplot(total[!is.na(total$TypeOfCollission),], aes(x = TypeOfIncident, fill = TypeOfCollission)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "precent") + facet_grid(.~SeverityOfIncident) +
  theme(legend.position="none")

#filling NA's in property damage
total$AmountOfPropertyClaim_disc=discretize(total$AmountOfPropertyClaim,method = "interval",categories=27)
ggplot(total[!is.na(total$PropertyDamage),], aes(x = AmountOfPropertyClaim_disc, fill = PropertyDamage)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Training data only', y= "percent") + facet_grid(.~SeverityOfIncident) +
  theme(legend.position="none")

#filling amountoftotalclaim has correlation of 0.5 with propertyclaim and injuryclaim
total$AmountOfPropertyClaim_disc=discretize(total$AmountOfPropertyClaim,method = "interval",categories=6)
total$AmountOfTotalClaim_disc=discretize(total$AmountOfTotalClaim,method = "interval",categories=6)
total$AmountOfInjuryClaim_disc=discretize(total$AmountOfInjuryClaim,method = "interval",categories=6)
ggplot(total[!is.na(total$AmountOfTotalClaim),], aes(x = AmountOfPropertyClaim_disc, fill = AmountOfTotalClaim_disc)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Training data only', y= "percent") + facet_grid(.~SeverityOfIncident) +
  theme(legend.position="none")


colSums(is.na(total))
str(total)
head(total)


finaldata=centralImputation(total)
str(finaldata)

library(C50)

c5_tree <- C5.0( ReportedFraud~ . , finaldata,rules = T)
summary(c5_tree)

library(rpart)
rpart_tree <- rpart(ReportedFraud ~ . , data = finaldata,method="class")
summary(rpart_tree)
#feature engineering
#density plots
plot(density(total$Policy_Deductible))
plot(density(finaldata$PolicyAnnualPremium))
skewness(total$PolicyAnnualPremium)
plot(density(total$UmbrellaLimit))
plot(density(finaldata$InsuredAge))


#finding the number of days incident happened before accident
finaldata$DateOfIncident=as.Date(finaldata$DateOfIncident, format = "%Y-%m-%d")
finaldata$DateOfPolicyCoverage=as.Date(finaldata$DateOfPolicyCoverage, format = "%Y-%m-%d")
finaldata$incident_coverage=as.integer(finaldata$DateOfIncident-finaldata$DateOfPolicyCoverage)
finaldata$daysremainingforinsurance=finaldata$InsuredAge*365-finaldata$incident_coverage
range(finaldata$incident_coverage)
finaldata=finaldata[finaldata$incident_coverage>0,]# cause policy coverage cant be before policy commencement

total=centralImputation(total)
#data visualisation

#plotting the number of yes and no's
ggplot(total[!is.na(total$ReportedFraud),], aes(x =SeverityOfIncident , fill = ReportedFraud)) +
  geom_bar(stat='percent',position = 'fill') +
  labs(x = 'How many reported frauds?',y = "percent")


ggplot(total[!is.na(total$ReportedFraud),], aes(x =SeverityOfIncident , fill = ReportedFraud)) +
  geom_bar(stat='count',position = 'fill') 


#checking for percentage of men and women who default
p6 <- ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = InsuredGender, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~InsuredGender) +
  theme(legend.position="none")
plot(p6)
# so we see here same percent of men and women default

#insurance policy state
p7 <- ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = InsurancePolicyState, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~InsurancePolicyState) +
  theme(legend.position="none")
plot(p7)

# we can see that the percent of fraud is almost similar irrespective the states


#zipcode where the accidents occur usually
p7 <- ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = InsuredZipCode, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent")
plot(p7)
# we can see that frauds happen more frequently in some zipcode location than others , so it will b a significant factor

#checking the witnesses value
p7 <- ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = Witnesses, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~Witnesses) +
  theme(legend.position="none")
plot(p7)
#we can see the max percent of frauds happen during 3 witnesses

#police report
p7 <- ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = PoliceReport, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~PoliceReport) +
  theme(legend.position="none")
plot(p7)
# the percent of frauds are equal irrespective of police reports  #useless field

#number of vehicles
p7 <- ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = NumberOfVehicles, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~NumberOfVehicles) +
  theme(legend.position="none")
plot(p7)
# when the number of vehicles involved are 2 or 4 the fraud percent is higher

#bodily injuries
p7 <- ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = BodilyInjuries, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~BodilyInjuries) +
  theme(legend.position="none")
plot(p7)

#InsuredEducationLevel
p7 <- ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = InsuredEducationLevel, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~InsuredEducationLevel) +
  theme(legend.position="none")
plot(p7)

#InsuredHobbies
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = InsuredHobbies, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'InsuredHobbies',y='percent')
#people with chess and cross fit have higher fraud rate

#incidenttime
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = IncidentTime, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'IncidentTime',y='percent')

#severity of incident
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = SeverityOfIncident, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'SeverityOfIncident',y='percent')
#major damage had a huge amount of fraud cases

#IncidentAddress
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = IncidentAddress, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'SIncidentAddress',y='percent')
# v can see that some address had high cases of fraud cases

#AmountOfVehicleDamage
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = AmountOfVehicleDamage, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'AmountOfVehicleDamage',y='percent')
# v can see the amount of vehicle damage also has high influence on the fraud cases

#DateOfIncident
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = DateOfIncident, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'DateOfIncident',y='precent')
#certain dates have high influence

#incident-coverage
finaldata$incident_coverage_disc=discretize(finaldata$incident_coverage,method = "interval",categories=9)
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = incident_coverage_disc, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'DateOfIncident',y='percent')

#days remaining for insurance 
finaldata$daysremainingforinsurance_disc=discretize(finaldata$daysremainingforinsurance,method = "interval",categories=25)
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = daysremainingforinsurance_disc, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'daysremainingforinsurance',y='percent')
#fraud rates are high during the 2 bin when days are less and the initial period

#insured relationship
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = InsuredRelationship, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'DateOfIncident',y='percent')

#umbrella limit
finaldata$UmbrellaLimit_disc=discretize(finaldata$UmbrellaLimit,method = "interval",categories=20)
ggplot(finaldata[!is.na(finaldata$ReportedFraud),], aes(x = UmbrellaLimit_disc, fill = ReportedFraud)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'DateOfIncident',y='percent')
