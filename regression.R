##backward stepwise
#Read data
LasVegas_dataset = read.table(file = '/Users/xhy0908/Downloads/LasVegasTripAdvisorReviews-Dataset.csv',header = T,sep=";")
View(LasVegas_dataset)

LasVegas_dataset = read.table(file = '/Users/xhy0908/Downloads/LasVegas_Modi.csv',header = T,sep=",")
str(LasVegas_dataset)

LasVegas_sel = subset(LasVegas_dataset,select = c(Nr..reviews,Free.internet,Nr..hotel.reviews
                                                  ,Helpful.votes,Score,Pool,Gym,Tennis.court,
                                                  Spa,Casino,Free.internet,Hotel.stars,Nr..rooms,Member.years))

#Descriptive Statistics
summary(LasVegas_dataset)
str(LasVegas_sel)
#we found that something wrong with Members.year
#find the wrong number and delete it
LasVegas_dataset['Member.years']
LasVegas_dataset_delete= LasVegas_dataset[-76,]
View(LasVegas_dataset_delete)
summary(LasVegas_dataset_delete)

#select a sebset for regression modeling at most 25
#LasVegas_sel = subset(LasVegas_dataset,select = c(Score,Pool,Gym,Tennis.court,Spa,Casino,Free.internet,Hotel.stars,User.continent,Nr..rooms ,User.country,Nr..reviews ))

#
install.packages("psych")
library(psych)
#a brief visual inspection of vars
pairs.panels(LasVegas_sel,col = 'red')

####Develop a model
# train data set & test data set
#80% fro train data and 20% for test data
set.seed(1)
train.size = 0.8
train.index = sample.int(length(LasVegas_sel$Score),round(length(LasVegas_sel$Score)*train.size))
train.sample = LasVegas_sel[train.index,]
test.sample = LasVegas_sel[-train.index,]

#backward
library(leaps)
fit = regsubsets(Score~., data=train.sample,nbest = 1, method = "backward")
fit.summary=summary(fit)

par(mfrow = c(2,2))
plot(fit.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(fit.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
p=which.max(fit.summary$adjr2)
points(p,fit.summary$adjr2[p], col="red",cex=2,pch=20)
plot(fit.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
p=which.min(fit.summary$cp)
points(p,fit.summary$cp[p],col="red",cex=2,pch=20)
p=which.min(fit.summary$bic)
plot(fit.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(p,fit.summary$bic[p],col="red",cex=2,pch=20)


#9 variables
plot(fit, scale = "Cp")
coef(fit,4)



#fetures selection
fit.summary$which[4,]


#build regression modle
best_fit = lm(Score ~ Free.internet + Pool  + Hotel.stars+ Gym , data = train.sample)
summary(best_fit)



best_fit = update(best_fit, .~. -Traveler.typeSolo)
summary(best_fit_test)

#predict train set score
train.sample$Pre.Score =
  predict(best_fit, newdata = subset(train.sample,select = c(Free.internet , Pool, Hotel.stars, Gym)))
str(train.sample$Pre.Score)
as.integer(train.sample$Pre.Score)
#test set
test.sample$Pre.Score =
  predict(best_fit, newdata = subset(test.sample,select = c(Free.internet , Pool, Hotel.stars, Gym)))

#check how good fit the train model
train.corr = round(cor(train.sample$Pre.Score,train.sample$Score),2)
train.RMSE = sqrt(mean((train.sample$Pre.Score - train.sample$Score)^2))
train.MAE = mean(abs(train.sample$Pre.Score - train.sample$Score))

#check the test set
test.corr = cor(test.sample$Pre.Score,test.sample$Score)
test.RMSE = sqrt(mean((test.sample$Pre.Score - test.sample$Score)^2))
test.MAE = mean(abs(test.sample$Pre.Score - test.sample$Score))
