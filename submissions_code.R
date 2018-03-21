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

setwd("C:\\Users\\SIDDARTH\\Desktop\\PHD\\TestData")
testone=read.csv("Test.csv",na.strings=c("","NA"))
testtwo=read.csv("Test_Claim.csv",na.strings=c("","NA"))
testthree=read.csv("Test_Demographics.csv",na.strings=c("","NA"))
testfour=read.csv("Test_Policy.csv",na.strings=c("","NA"))
testfive=read.csv("Test_Vehicle.csv")

two=rbind(two,testtwo)
three=rbind(three,testthree)
four=rbind(four,testfour)
five=rbind(five,testfive)
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
total <- merge(merge4,two,by="CustomerID")
total$InsurancePolicyNumber=as.factor(total$InsurancePolicyNumber)
#deleting customer ID and insurance policy number 
total=total[,setdiff(names(total),c("InsurancePolicyNumber","Country","AmountOfPropertyClaim_disc","AmountOfTotalClaim_disc","AmountOfInjuryClaim_disc"))]
total$InsuredZipCode=as.factor(as.integer(total$InsuredZipCode/10))
str((total$InsuredZipCode))
#trying to fill NA's of insured gender using insured relationship
total$InsuredGender=as.factor(ifelse(total$InsuredRelationship=="wife","2",total$InsuredGender))
total$InsuredGender=as.factor(ifelse(total$InsuredRelationship=="husband","1",total$InsuredGender))
str(total$InsuredGender)
colSums(is.na(total))

str(total)
#imputing police report 
total1=total[!is.na(total$PoliceReport),c("PoliceReport","Witnesses","AuthoritiesContacted","PropertyDamage")]
total_test1=total[is.na(total$PoliceReport),c("Witnesses","AuthoritiesContacted","PropertyDamage")]
total1=centralImputation(total1)
library(h2o)
h2o.init()
finaldata.h2o=as.h2o(total1)
str(total1)
finaltest.h2o=as.h2o(total_test1)
y.dep<-1
x.indep<-2:4
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = finaldata.h2o, ntrees = 51, max_depth = 2, seed = 1122,col_sample_rate_per_tree = 1)
pred=as.data.frame(h2o.predict(rforest.model, finaltest.h2o))
str(pred)
total[is.na(total$PoliceReport),"PoliceReport"]=pred[,1]
colSums(is.na(total))

summary(total)

#imputing TypeOfCollission

total2=total[!is.na(total$TypeOfCollission),c("TypeOfCollission","NumberOfVehicles","TypeOfIncident","PropertyDamage")]
total_test2=total[is.na(total$TypeOfCollission),c("NumberOfVehicles","TypeOfIncident","PropertyDamage")]
total2=centralImputation(total2)
table(total1$TypeOfCollission)
str(total2)
finaldata.h2o=as.h2o(total2)
str(total2)
finaltest.h2o=as.h2o(total_test2)
y.dep<-1
x.indep<-2:4
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = finaldata.h2o, ntrees = 51, max_depth = 2, seed = 1122,col_sample_rate_per_tree = 1)
pred=as.data.frame(h2o.predict(rforest.model, finaltest.h2o))
str(pred)
total[is.na(total$TypeOfCollission),"TypeOfCollission"]=pred[,1]
colSums(is.na(total))


#imputing property damage

total3=total[!is.na(total$PropertyDamage),c("PropertyDamage","TypeOfCollission","AmountOfPropertyClaim","NumberOfVehicles","TypeOfCollission")]
total_test3=total[is.na(total$PropertyDamage),c("TypeOfCollission","AmountOfPropertyClaim","NumberOfVehicles","TypeOfCollission")]
total3=centralImputation(total3)
table(total1$PropertyDamage)
str(total1)
finaldata.h2o=as.h2o(total3)
str(total3)
finaltest.h2o=as.h2o(total_test3)
y.dep<-1
x.indep<-2:5
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = finaldata.h2o, ntrees = 51,  max_depth = 2, seed = 1122,col_sample_rate_per_tree = 1)
pred=as.data.frame(h2o.predict(rforest.model, finaltest.h2o))
str(pred)
total[is.na(total$PropertyDamage),"PropertyDamage"]=pred[,1]
colSums(is.na(total))


#feature engineering
#density plots
plot(density(total$Policy_Deductible))
plot(density(total$PolicyAnnualPremium)) 
skewness(total$PolicyAnnualPremium)
plot(density(total$UmbrellaLimit))

#imputing the rest of the data
finaldata=centralImputation(total)

#finding the Age of the customer during the incident
finaldata$DateOfIncident=as.Date(finaldata$DateOfIncident, format = "%Y-%m-%d")
finaldata$DateOfPolicyCoverage=as.Date(finaldata$DateOfPolicyCoverage, format = "%Y-%m-%d")
finaldata$incident_coverage=as.integer(finaldata$DateOfIncident-finaldata$DateOfPolicyCoverage)
finaldata$daysremainingforinsurance=finaldata$InsuredAge*365+finaldata$incident_coverage
finaldata$Month_Incident <- as.factor(format(as.Date(finaldata$DateOfIncident), "%m"))
finaldata$year_Incident <- as.factor(format(as.Date(finaldata$DateOfIncident), "%Y"))
finaldata$Month_policy <- as.factor(format(as.Date(finaldata$DateOfPolicyCoverage), "%m"))
finaldata$year_policy <- as.factor(format(as.Date(finaldata$DateOfPolicyCoverage), "%Y"))
#total claim
finaldata$totalclaim=finaldata$AmountOfInjuryClaim+finaldata$AmountOfPropertyClaim+finaldata$AmountOfTotalClaim+finaldata$AmountOfVehicleDamage
str(finaldata)
#finaldata=finaldata[finaldata$incident_coverage>0,]# Removing rows where policy coverage is before policy commencement
library(lubridate)
a=wday(finaldata$DateOfIncident,label = F)
str(a)
finaldata$weekday=a
finaldata$weekday=ifelse(finaldata$weekday==c(1,7),0,1)
head(finaldata$weekday)
#converting into categorical and numerical varaibles
num_attr<-c("CustomerLoyaltyPeriod","Policy_Deductible","PolicyAnnualPremium","UmbrellaLimit","InsuredAge","CapitalGains","CapitalLoss","AmountOfInjuryClaim","AmountOfTotalClaim","AmountOfPropertyClaim","AmountOfVehicleDamage","incident_coverage","daysremainingforinsurance","totalclaim")
cat_attr<-setdiff(x=names(finaldata),y=num_attr)
cat_data<-data.frame(sapply(finaldata[,cat_attr],as.factor))
num_data<-data.frame(sapply(finaldata[,num_attr],as.numeric))

str(num_data)
#Treating the outliers
boxplot(num_data[,c(1:3)])
boxplot(num_data[,5:14])

#for ploicy annual premium
x <- unlist(finaldata[,"PolicyAnnualPremium"])
str(x)
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
num_data$PolicyAnnualPremium=x

#for amount property claim
x <- unlist(finaldata[,"AmountOfPropertyClaim"])
str(x)
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
num_data$AmountOfPropertyClaim=x

  #for amount vehicle damage
x <- unlist(finaldata[,"AmountOfVehicleDamage"])
str(x)
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
num_data$AmountOfVehicleDamage=x


#combining the numerical and categorical attributes
data2 = cbind(num_data, cat_data)

#splitting the data into final train and test
str(finaldata)
finaltrain=data2[(data2$CustomerID %in% one$CustomerID),]
final <- merge(one,finaltrain,by="CustomerID")
final=final[,setdiff(names(final),c("CustomerID","year_Incident"))]
finaltest=data2[(data2$CustomerID %in% testone$CustomerID),]
head(finaltest)
#saving the order of the customer ID for test data
order=as.character(finaltest$CustomerID)
finaltest=finaltest[,setdiff(names(finaltest),c("CustomerID","year_Incident"))]
head(finaltest)
str(final)

#correlation analysis
str(finaldata)
library(dplyr)
d_cor <- as.matrix(cor(data.matrix(finaldata)))
d_cor_melt <- arrange(melt(d_cor), -abs(value))
dplyr::filter(d_cor_melt, value > .5)
library(corrplot)
M <- cor(final[,num_attr])
corrplot(M, method = "circle")

  str(cat_attr)
a=data2[,cat_attr]
a=a[(a$CustomerID %in% one$CustomerID),]
a <- merge(a,one,by="CustomerID")
a=a[,setdiff(names(a),c("DateOfIncident","DateOfPolicyCoverage","CustomerID","year_Incident","weekday"))]
str(a)
rules= apriori(a)
options(digits=2)
inspect(rules[1:20])

total=centralImputation(total)
total=total[(total$CustomerID %in% one$CustomerID),]
total=merge(one,total,by="CustomerID")
total$CustomerID=as.numeric(total$CustomerID)
dt=total[,setdiff(names(total),c("DateOfIncident","DateOfPolicyCoverage","IncidentAddress","CustomerID","year_Incident"))]
str(dt)
library(C50)
c5_rules=C5.0(ReportedFraud~.,dt, rules=T)
C5imp(c5_rules,metric="usage")
summary(c5_rules)

a=createDataPartition(y=final$ReportedFraud,p=0.8,list = F)
finaltrain=final[a,]
str(final)
finaltest2=final[-a,]


#lasso ridge
x=data.matrix(finaltrain[,setdiff(names(finaltrain),"ReportedFraud")])


for (i in 0:100) {
  assign(paste("fit", i, sep=""), cv.glmnet(x,finaltrain$ReportedFraud, type.measure="auc", 
                                            alpha=i/100,family="binomial",nfolds=10,parallel=TRUE))
}

x.test = data.matrix(finaltest2[,setdiff(names(finaltest2),"ReportedFraud")])
y.test = finaltest2$ReportedFraud

accuracy=data.frame()
for(i in 0:100)
{ fit=paste0("fit",i)
glm_Test = predict(get(fit), data.matrix(finaltest2[,setdiff(names(finaltest2),"ReportedFraud")]), type="class")
cm_Glm = table(glm_Test, y.test)
accuracy=sum(diag(cm_Glm))/sum(cm_Glm)
}
alpha=data.frame(seq(0,1,by=0.01))
accuracy=cbind(alpha,accuracy)
accuracy
x=data.matrix(final[,setdiff(names(final),"ReportedFraud")])
Model = cv.glmnet(x,final$ReportedFraud, type.measure="auc", alpha=0, 
                  family="binomial",nfolds=10,parallel=TRUE)
pred_test=data.frame(predict(Model,data.matrix(finaltest),type="class"))
head(pred_test)
pred_test$CustomerID=order
pred_test=pred_test[order(match(pred_test$CustomerID,as.character(testone$CustomerID))),]
write.csv(x=pred_test,file="try.csv")




#NB
nbmodel=naiveBayes(finaltrain$ReportedFraud~.,finaltrain,laplace=0.4)
prednb=data.frame(predict(nbmodel,finaltest2))
confusionMatrix(prednb[,1],finaltest2$ReportedFraud)
prednb_train=predict(nbmodel,final)
#using the order of the customer ID and matching with the original submission file
prednb$CustomerID=order
prednb=prednb[order(match(prednb$CustomerID,as.character(testone$CustomerID))),]
write.csv(x=prednb,file="try.csv")



#h2o RANDOMFOREST
finaldata.h2o=as.h2o(final)
finaltrain.h2o=as.h2o(finaltrain)
finaltest.h2o=as.h2o(finaltest)
finaltest2.h2o=as.h2o(finaltest2)
y.dep<-1
x.indep<-2:45
rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = finaldata.h2o, ntrees = 51, mtries = -1, max_depth = 3, seed = 1122,col_sample_rate_per_tree = 1)
pred=as.data.frame(h2o.predict(rforest.model, finaltest2.h2o))
confusionMatrix(pred[,1],finaltest2$ReportedFraud)
predict.dl2_random <- as.data.frame(h2o.predict(rforest.model, finaltest.h2o))
predict.dl2_random$CustomerID=order
predict.dl2_random=predict.dl2_random[order(match(predict.dl2_random$CustomerID,as.character(testone$CustomerID))),]
write.csv(x=predict.dl2_random,file="try.csv")
predict.dl2_random_train=as.data.frame(h2o.predict(rforest.model, finaldata.h2o))
imp=h2o.varimp_plot(rforest.model)

tail(finaltest)
h2o.predict_leaf_node_assignment(rforest.model,finaldata.h2o)
h2o.gainsLift(rforest.model)

#gbm model
gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = finaldata.h2o, ntrees = 251, max_depth = 6, learn_rate = 0.11, seed = 1122,sample_rate = 1,col_sample_rate=1,min_split_improvement=1e-5)
h2o.auc(h2o.performance(gbm.model))
predict.dl2 <- as.data.frame(h2o.predict(gbm.model, finaltest2.h2o))
confusionMatrix(predict.dl2[,1],finaltest2$ReportedFraud)
predict.dl2 <- as.data.frame(h2o.predict(gbm.model, finaltest.h2o))
predict.dl2$CustomerID=order
predict.dl2=predict.dl2[order(match(predict.dl2$CustomerID,as.character(testone$CustomerID))),]
str(predict.dl2)
write.csv(x=predict.dl2,file="try.csv")
predict.dl2_train <- as.data.frame(h2o.predict(gbm.model, finaldata.h2o))

#xgboost
library(xgboost, quietly=TRUE)
a=createDataPartition(y=final$ReportedFraud,p=0.8,list = F)
final$ReportedFraud=ifelse(as.numeric(final$ReportedFraud)==1,1,0)
table(final$ReportedFraud)
finaltrain=final[a,]
str(finaltrain)
finaltest2=final[-a,]
xgb.data.train <- xgb.DMatrix(data.matrix(finaltrain[, colnames(finaltrain) != "ReportedFraud"]), label = finaltrain$ReportedFraud)
xgb.data.test <- xgb.DMatrix(data.matrix(finaltest2[, colnames(finaltest2) != "ReportedFraud"]), label = finaltest2$ReportedFraud)

xgb.model.speed <- xgb.train(data = xgb.data.train
                             , params = list( booster="gbtree",
                                              objective = "binary:logistic"
                                              , eta = 0.1
                                              , max.depth = 3
                                              , min_child_weight = 25
                                              , subsample = 1
                                              , colsample_bytree = 0.6
                                              , nthread = 2
                                              ,nfolds=3
                                              , eval_metric = "auc"
                                              
                             )
                             , watchlist = list(test = xgb.data.test)
                             , nrounds = 1000
                              , early_stopping_rounds = 400
                             , print_every_n = 20
)

print(xgb.model.speed)
predtest=predict(xgb.model.speed,data.matrix(finaltest2),ntreelimit = xgb.model.speed$bestInd)
predtest=ifelse(predtest<0.5,0,1)
confusionMatrix(predtest,finaltest2$ReportedFraud)
predtrain=(predict(xgb.model.speed,data.matrix(finaltrain), ntreelimit = xgb.model.speed$bestInd))
predtrain=ifelse(predtrain<0.6,0,1)
confusionMatrix(predtrain,finaltrain$ReportedFraud)

pred=data.frame(predict(xgb.model.speed,data.matrix(finaltest), ntreelimit = xgb.model.speed$bestInd))


pred$CustomerID=order
pred=pred[order(match(pred$CustomerID,as.character(testone$CustomerID))),]
pred$predict.xgb.model.speed..data.matrix.finaltest...ntreelimit...xgb.model.speed.bestInd.=ifelse(pred$predict.xgb.model.speed..data.matrix.finaltest...ntreelimit...xgb.model.speed.bestInd.<0.6,"Y","N")
write.csv(x=pred,file="try.csv")
pred_train=predict(xgb.model.speed,xgb.data.train, ntreelimit = xgb.model.speed$bestInd)
pred_train=ifelse(pred_train>0.6,"N","Y")
head(pred_train)



#stacking
train_Pred_All_Models = data.frame(nb = prednb_train,
                                   xgb = pred_train,
                                   rf = predict.dl2_random_train[,1],
                                   gbm=predict.dl2_train[,1])
train_Pred_All_Models = data.frame(sapply(train_Pred_All_Models, 
                                          as.factor))
train_Pred_All_Models = cbind(train_Pred_All_Models, ReportedFraud = (final$ReportedFraud))


x=data.matrix(train_Pred_All_Models[,setdiff(names(train_Pred_All_Models),"ReportedFraud")])


ensemble.h2o=as.h2o(train_Pred_All_Models)
str(train_Pred_All_Models)
y=5
x=1:4
ensemble_Model=h2o.randomForest(y=y.dep, x=x.indep, training_frame = ensemble.h2o, ntrees = 151,  max_depth = 3, seed = 1122,col_sample_rate_per_tree = 0.75)
summary(ensemble_Model)
test_Pred_All_Models = data.frame(nb = prednb[,1],
                                  xgb = pred[,1],
                                  rf = predict.dl2_random[,1],
                                  gbm=predict.dl2[,1]) 
test_Pred_All_Models = data.frame(sapply(test_Pred_All_Models, as.factor))
ensemble_test.h2o=as.h2o(test_Pred_All_Models)
ensemble_pred = as.data.frame(h2o.predict(ensemble_Model, ensemble_test.h2o))
write.csv(x=ensemble_pred,file = "try.csv")
