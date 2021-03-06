# 1
gerber = read.csv("gerber.csv")
table(gerber$voting)
gerberLogReg = glm(voting~civicduty+hawthorne+neighbors+self, data=gerber, family="binomial")
summary(gerberLogReg)
pred1 = predict(gerberLogReg, type="response")
table(gerber$voting, pred1>=0.3)
table(gerber$voting, pred1>=0.5)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors+sex, data=gerber, cp=0.0)
prp(CARTmodel3)
CARTmodel4 = rpart(voting ~control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~control+sex, data=gerber, cp=0.0)
pred4 = predict(CARTmodel4)
pred5 = predict(CARTmodel5)
prp(CARTmodel4, digit=6)
sexcontrolog = glm(voting~sex+control, data=gerber, family="binomial")
summary(sexcontrolog)
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(sexcontrolog, newdata=Possibilities, type="response")
sexcontrolog2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(sexcontrolog2)
predict(sexcontrolog2, newdata=Possibilities, type="response")

#2
letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio=0.5)
lettersTrain = subset(letters, split==TRUE)
lettersTest = subset(letters, split==FALSE)
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")
prediction = predict(CARTb, newdata=lettersTest, type="class")
table(lettersTest$isB, prediction)
library(randomForest)
set.seed(1000)
lettersForest = randomForest(isB~.-letter,data=lettersTrain)
PredictLetters = predict(lettersForest, newdata=lettersTest)
table(lettersTest$isB, PredictLetters)
letters$letter = as.factor( letters$letter )
set.seed(2000)
split = sample.split(letters$letter, SplitRatio=0.5)
lettersTrain = subset(letters, split==TRUE)
lettersTest = subset(letters, split==FALSE)
table(lettersTest$letter)
CARTletter = rpart(letter ~ . - isB, data=lettersTrain, method="class")
prediction = predict(CARTletter, newdata=lettersTest, type="class")
table(lettersTest$letter, prediction)
set.seed(1000)
lettersForest = randomForest(letter~.-isB,data=lettersTrain)
PredictLetters = predict(lettersForest, newdata=lettersTest)
table(lettersTest$letter, PredictLetters)

# 3
data(state)
statedata = data.frame(state.x77)
str(statedata)
lifexpReg = lm(Life.Exp~Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area, data=statedata)
summary(lifexpReg)
lifexpReg.pred = predict(lifexpReg)
lifexpReg.sse = sum((lifexpReg.pred-statedata$Life.Exp)^2)
lifexpReg.sse
lifexpReg2 = lm(Life.Exp~Population+Murder+HS.Grad+Frost, data=statedata)
summary(lifexpReg2)
lifexpReg2.pred = predict(lifexpReg2)
lifexpReg2.sse = sum((lifexpReg2.pred-statedata$Life.Exp)^2)
lifexpReg2.sse
CARTLifexp = rpart(Life.Exp ~ ., data=statedata)
prp(CARTLifexp)
CARTLifexp.pred=predict(CARTLifexp)
CARTLifexp.sse = sum((CARTLifexp.pred - statedata$Life.Exp)^2)
CARTLifexp.sse
CARTLifexp2 = rpart(Life.Exp ~ ., data=statedata,control=rpart.control(minbucket=5))
prp(CARTLifexp2)
CARTLifexp2.pred=predict(CARTLifexp2)
CARTLifexp2.sse = sum((CARTLifexp2.pred - statedata$Life.Exp)^2)
CARTLifexp2.sse
CARTLifexp3 = rpart(Life.Exp ~ Area, data=statedata,control=rpart.control(minbucket=5))
CARTLifexp3.pred=predict(CARTLifexp3)
CARTLifexp3.sse = sum((CARTLifexp3.pred - statedata$Life.Exp)^2)
CARTLifexp3.sse
set.seed(111)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01)
train(Life.Exp ~ Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area, data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )
CARTLifexp5 = rpart(Life.Exp ~ ., data=statedata,control=rpart.control(cp=0.12))
prp(CARTLifexp5)
CARTLifexp5.pred = predict(CARTLifexp5)
CARTLifexp5.sse = sum((CARTLifexp5.pred - statedata$Life.Exp)^2)
CARTLifexp5.sse

#4
census = read.csv("census.csv")
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio=0.6)
Train = subset(census, split==TRUE)
Test = subset(census, split==FALSE)
Over50kLog = glm(over50k~.,data=Train, family="binomial")
summary(Over50kLog)
pred1 = predict(Over50kLog, newdata=Test,type="response")
table(Test$over50k, pred1>0.5)
table(Test$over50k)
library(ROCR)
ROCRpred = prediction(pred1, Test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
library(rpart)
library(rpart.plot)
CART50k = rpart(over50k~., data=Train, method="class")
prp(CART50k)
prediction = predict(CART50k, newdata=Test, type="class")
table(Test$over50k,prediction)
predover50k = predict(CART50k, newdata=Test)
predover50k = predover50k[,2]
ROCRpred = prediction(predover50k, Test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
set.seed(1)
TrainSmall = Train[sample(nrow(Train),2000),]
library(randomForest)
incomeForest2 = randomForest(over50k~. -nativecountry,data=TrainSmall)
predictIncome = predict(incomeForest2, newdata=Test)
table(Test$over50k,predictIncome)
vu = varUsed(incomeForest2, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(incomeForest2$forest$xlevels[vusorted$ix]))
varImpPlot(incomeForest2)
library(caret)
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k~.,data = Train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
income50kCV=rpart(over50k~., data=Train,control=rpart.control(cp = 0.002))
PredictTest = predict(income50kCV,newdata=Test, type="class")
table(Test$over50k,PredictTest)
prp(income50kCV)