##Knn of pca and dc

set.seed(557) 
d<- ddc[sample(nrow(dpca)),]
test <- 1:411
traind<- d[-test,]
testd <- d[test,]

#test
accuracy <- rep(0, 25)
k <- 1:25
for(x in k){
  p <- knn(traind[,3:4], testd[,3:4],traind$y, k = x)
  accuracy[x] <-1- mean(p == testd$y)
}

plot(k, accuracy, type = 'b')
do=data.frame(cbind(k,accuracy))
g1=ggplot(do, aes(x=k, y=accuracy))+  ggtitle("Test data for DC") +
  labs(x="k",y="error")  + geom_line(color="red") + geom_point()
g1
#train
accuracyt <- rep(0, 25)
k <- 1:25
for(x in k){
  pt <- knn(traind[,3:4], traind[,3:4],traind$y, k = x)
  accuracyt[x] <-1- mean(pt == traind$y)
}

plot(k, accuracyt, type = 'b')
do=data.frame(cbind(k,accuracyt))
g2=ggplot(do, aes(x=k, y=accuracyt))+  ggtitle("Training data for DC") +
  labs(x="k",y="error")  + geom_line(color="red") + geom_point()
g2
multiplot(g1,g2)

ptt <- knn(traind[,3:4], testd[,3:4],traind$y, k = 1)
ptt=as.numeric(ptt)
a=as.numeric(testd$y)
g200=ggplot(testd, aes(x=testd$X_dc1, y=testd$X_dc2, color =testd$y, size=ptt))+  ggtitle("KNN for DC when k=1") +
  labs(x="pca1",y="pca2")   + geom_point()
g200

