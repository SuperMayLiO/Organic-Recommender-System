library(libFMexe)
library(ggplot2)
library(dplyr)
library(devtools)
library(caret)
library(mlbench)

data <- read.csv("C:/Users/user/Desktop/Big Data/Final/data.csv",
                 stringsAsFactors=FALSE)
#############################libFM####################################
data <- data %>% select(會員卡號, 小分類名稱, 數量)
names(data) <- c( "member","product", "quantity")
data <- data %>% group_by(member, product) %>% summarise(quantity=sum(quantity))
data$product=as.factor(data$product)


#split train and test data
set.seed(1)
libFM_train_rows = sample.int(nrow(data), nrow(data) * 2 / 3)
libFM_train = data[libFM_train_rows, ]
libFM_test = data[-libFM_train_rows, ]

#10 folds cross validation
folds <- createFolds(libFM_train$quantity)
str(folds)
split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = data)
unlist(lapply(split_up, nrow))

validate_err=NULL
for(i in 1:10){
  validate_set=split_up[[i]]
  train_set=split_up[-i]
  bind_train_set=NULL
  for(j in 1:9){
    bind_train_set=rbind(bind_train_set,train_set[[j]])}
  train_set=bind_train_set
  validate_pred=libFM(train_set, validate_set, quantity ~ member +product,task = "r", dim = 10, iter = 500)
  validate_err[i]=sqrt(mean(( validate_pred -validate_set$quantity)^2))
  print(validate_err[i])
}

#pick final model
i=which.min(validate_err)
libFM_train=NULL
for(j in 1:9){
  libFM_train=rbind(libFM_train,split_up[-i][[j]])}

#use final model to predict
predFM = libFM(libFM_train, libFM_test, quantity ~ member +product,
               task = "r", dim = 10, iter = 500)
#libfm output
df_FM <- data.frame(libFM_test, pred=predFM)
write.csv(df_FM,"C:/Users/user/Desktop/Big Data/Final/libfm_output.csv")

#measure
sqrt(mean((predFM - libFM_test$quantity)^2))#2.994784 #3.159529
mean((predFM - libFM_test$quantity)^2)#8.968729 #9.982621
#############################################
#predict member 6
member6=data.frame(member=6,product=unique(data$product),quantity=0)
member6_match=libFM_test %>% filter(member==6) %>% select(member,product,quantity)

member6$product=as.factor(member6$product)
member6$member=as.integer(member6$member)
member6$quantity=as.integer(member6$quantity)

semi_join(member6_match, member6, by = NULL, copy = FALSE)

predFM_member6 = libFM(libFM_train, member6, quantity ~ member +product,
               task = "r", dim = 10, iter = 500)
