#Read data
LasVegas_dataset = read.table(file = '/Users/xhy0908/Downloads/LasVegas.csv',header = T,sep=";")
View(LasVegas_dataset)
#
library(tree)
#Descriptive Statistics
summary(LasVegas_dataset)

str(LasVegas_dataset)
#select a sebset for regression modeling
LasVegas_sel = subset(LasVegas_dataset,select = c(Score,Free.internet,Casino,Spa,Pool))
summary(LasVegas_sel)

High=ifelse(Score <=4,"No","Yes")
High

LasVegas_sel =data.frame(LasVegas_sel,High)

LasVegas_sel
tree.Las =tree(High~. -Score ,LasVegas_sel)
summary(tree.Las)
plot(tree.Las)
