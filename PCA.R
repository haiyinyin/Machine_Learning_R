LasVegas_dataset = read.table(file = '/Users/xhy0908/Downloads/LasVegas_Cluster.csv',header = T,sep=",")
str(LasVegas_dataset)

#LasVegas_sel = subset(LasVegas_dataset,select = c(Score, Nr..hotel.reviews, Helpful.votes ,Pool,Gym,Tennis.court,Spa,Casino,Free.internet,Hotel.stars,Traveler.type,Nr..rooms))
LasVegas_sel_num = subset(LasVegas_dataset,select = c(Score, Nr..hotel.reviews, Helpful.votes,Hotel.stars,Traveler.type,Nr..rooms))
str(LasVegas_sel_num)

apply(LasVegas_sel_num , 2, mean) 

pr.out=prcomp(LasVegas_sel_num , scale=TRUE)
names(pr.out)

biplot(pr.out, scale=0)