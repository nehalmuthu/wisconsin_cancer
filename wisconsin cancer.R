#caTools
#caret 
#deplyr

cancer<-data[1:32]

set.seed(150)
split <-sample.split(cancer,SplitRatio = 0.75)

train <- subset(cancer, split == TRUE)
test <- subset(cancer, split == FALSE)



#train

y<-c(rep(0,nrow(train)))

for(i in 1:length(y)){
  if(train[i,2]=="M")
    y[i]=1 
  else 
    y[i]=0
}


model <- glm (y~ ., data = train[,3:32], family=binomial(link='logit'))

summary(model)


#test

y1<-c(rep(0,nrow(test)))

for(i in 1:length(y1)){
  if(test[i,2]=="M")
    y1[i]=1 
  else 
    y1[i]=0
}


fitted.results <- predict(model,newdata=subset(test,select=c(3:ncol(test))),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != y1)
print(paste('Accuracy',1-misClasificError))


  

