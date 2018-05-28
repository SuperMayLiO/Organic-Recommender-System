library(ggplot2)
library(dplyr)
library(reshape2)
library(recosystem)
library(caret)
library(mlbench)

data_mf <- read.csv("C:/Users/user/Desktop/Big Data/Final/data.csv",
                 stringsAsFactors=FALSE)

##########################libMF#################################
data_mf <- data_mf %>% select(會員卡號, 小分類名稱, 數量)
names(data_mf) <- c( "member","product", "quantity")
data_mf <- data_mf %>% group_by(member, product) %>% summarise(quantity=sum(quantity))
data_mf$product=as.factor(data_mf$product)

#change the data to the libmf convention
data_mf$member <- data_mf$member - 1
product_lookup <- 1:length(unique(data_mf$product)) - 1
names(product_lookup) <- unique(data_mf$product)
data_mf$product <- product_lookup[as.character(data_mf$product)]

#split train and test data
set.seed(1)
libMF_train_rows = sample.int(nrow(data_mf), nrow(data_mf) * 2 / 3)
libMF_train = data_mf[libMF_train_rows, ]
libMF_test = data_mf[-libMF_train_rows, ]

write.table(libMF_train, "C:/Users/user/Desktop/Big Data/Final/train.txt", row.names=FALSE, col.names=F)
write.table(libMF_test, "C:/Users/user/Desktop/Big Data/Final/test.txt", row.names=FALSE, col.names=F)
train_path <- "C:/Users/user/Desktop/Big Data/Final/train.txt"
test_path <- "C:/Users/user/Desktop/Big Data/Final/test.txt"

#10 folds cross validation
set.seed(1)
libMF_train=read.table(train_path)
folds <- createFolds(libMF_train$V3)
str(folds)
split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = data_mf)
unlist(lapply(split_up, nrow))

validate_err=NULL
for(i in 1:10){
  validate_set=split_up[[i]]
  train_set=split_up[-i]
  bind_train_set=NULL
  for(j in 1:9){
    bind_train_set=rbind(bind_train_set,train_set[[j]])}
  train_set=bind_train_set
  write.table(train_set, "C:/Users/user/Desktop/Big Data/Final/train.txt", row.names=FALSE, col.names=F)
  r <- Reco()
  opts <- r$tune(train_path, opts=list(dim=c(10, 20, 30),
                                       cost=c(0.01, 0.1, 0.5),
                                       lrate=c(0.01, 0.05, 0.1),
                                       nthread=1, niter=20))
  r$train(train_path, opts=c(opts$min, nthread=1, niter=20))
  write.table(validate_set, "C:/Users/user/Desktop/Big Data/Final/validate.txt", row.names=FALSE, col.names=F)
  validate_path <- "C:/Users/user/Desktop/Big Data/Final/validate.txt"
  # Prediction
  r$predict(validate_path, "C:/Users/user/Desktop/Big Data/Final/libmf_prediction.txt")
  validate_pred <- scan("C:/Users/user/Desktop/Big Data/Final/libmf_prediction.txt")
  validate_err[i]=sqrt(mean(( validate_pred -validate_set$quantity)^2))
  print(validate_err[i])
}

#pick final model
i=which.min(validate_err)
libMF_train=NULL
for(j in 1:9){
  libMF_train=rbind(libMF_train,split_up[-i][[j]])}
train_path <- "C:/Users/user/Desktop/Big Data/Final/train.txt"
write.table(libMF_train,train_path, row.names=FALSE, col.names=FALSE)

#use final model to predict
set.seed(1)
R <- Reco()
opts <- R$tune(train_path, opts=list(dim=c(10, 20, 30),
                                     cost=c(0.01, 0.1, 0.5),
                                     lrate=c(0.01, 0.05, 0.1),
                                     nthread=1, niter=20))

R$train(train_path, opts=c(opts$min, nthread=1, niter=20))

#True Value
true_quantity <- read.table(test_path, header=FALSE, sep=" ")$V3
true_table=read.table(test_path, header=FALSE, sep=" ")

#Prediction
R$predict(test_path, "C:/Users/user/Desktop/Big Data/Final/libmf_prediction.txt")
predMF <- scan("C:/Users/user/Desktop/Big Data/Final/libmf_prediction.txt")
length(true_quantity); length(predMF)

#libmf output
df_MF <- data.frame(member=true_table$V1,product=true_table$V2,quantity=true_table$V3,pred=predMF)
df_MF$product=sapply(df_MF$product,function(x) names(product_lookup)[product_lookup%in% x ])
write.csv(df_MF,"C:/Users/user/Desktop/Big Data/Final/libmf_output.csv")

#measure
sqrt(mean((true_quantity -predMF)^2))#2.494647
mean((true_quantity - predMF)^2)# 6.223265
#ggplot(df, aes(x=true_quantity, y=predMF)) + geom_point() + coord_fixed()
