#Read data
LasVegas_dataset = read.table(file = '/Users/xhy0908/Downloads/LasVegas_Modi.csv',header = T,sep=",")
str(LasVegas_dataset)

LasVegas_dataset = read.table(file = '/Users/xhy0908/Downloads/LasVegasTripAdvisorReviews-Dataset.csv',header = T,sep=";")
View(LasVegas_dataset)

##select non-factor variables
LasVegas_sel = subset(LasVegas_dataset,select = c(Score,Pool,Gym,Tennis.court,Spa,Casino,Free.internet,Nr..reviews,
                                                  Nr..hotel.reviews,Helpful.votes,Traveler.type,Hotel.stars,Nr..rooms,
                                                  Period.of.stay,User.continent,Member.years,Review.month,Review.weekday))

library(tree)
#define the classification
High=ifelse(LasVegas_sel$Score <=4,"No","Yes")

#train set and test set
set.seed(2)
train=sample(1:nrow(LasVegas_sel), 200)
LasVegas_sel.test=LasVegas_sel[-train,]
LasVegas_sel.train=LasVegas_sel[train,]

High.test=High[-train]


#add High variable to the dataframe
LasVegas_sel =data.frame(LasVegas_sel,High)

#make a tree
tree.Las =tree(High~.-Score ,LasVegas_sel,subset = train)
summary(tree.Las)

#plot tree
plot(tree.Las)
text(tree.Las,pretty=0,cex=0.6)

#predict
tree.pred=predict(tree.Las,LasVegas_sel.test,type="class")
table(tree.pred,High.test)

#modify
set.seed(3)
cv.Las=cv.tree(tree.Las,FUN=prune.misclass)
cv.Las