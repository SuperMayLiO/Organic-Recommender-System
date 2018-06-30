#data
load(file="/Users/mayritaspring/Desktop/Github/Organic-Recommender-System/data/data.rda")
load("target.rda")

#load package
library(dplyr)

# result of NMF

orgcate=scan() # organic
530103
210102
510909
310704
310608
410210
210202
411203
410105
411313
221001


##a是原本資料併上日期順序
a=data
a=cbind(a, days=strptime(as.matrix(data[,4]), "%Y/%m/%d")- strptime("2008/12/31", "%Y/%m/%d"))
a[,16]=as.numeric(a[,16])

##這是購物袋
a=a[a[,6]!=10056355,]


target[target[,1] %in% as.matrix(cust[cust[,3] >200,1]),2]=0
## sequenceID:會員號  eventID:購買順序(日期的排序) 
## suqenceID 跟eventID要由小到大排序，第一層要sID排序，第二層e ID排序
## SIZE:品項?
org <- subset(a, 會員卡號%in%target[target[,2]==1,1]) #會員
trans <- org[which(org$小分類名稱.y %in% orgcate),3] #有機發票號 
trans <- unique(trans)
org[,17] <- org$小分類名稱.y %in% orgcate
or <- subset(org, V17%in%TRUE)
or <- org[org[,6]!=10056355,] #松青背心袋

library(arulesSequences)
library(plyr)
arudata=data.frame(sequenceID=or$會員卡號, 
                   eventID=strptime(as.matrix(or$交易日期), "%Y/%m/%d")- strptime("2008/12/31", "%Y/%m/%d")
)
arudata = data.frame(lapply(lapply(arudata,as.character),as.numeric))
arudata=unique(arudata)
arudata= arrange(arudata,sequenceID,eventID)
arudata$eventID <- as.numeric(as.character(arudata$eventID))
# write.table(arudata"D:\\arudata1.txt")


## 把sequential pattern mining需要的資料格式存到out.txt
sink("out2.txt")
for(i in 1:dim(arudata)[1]){
  k=or[which(arudata[i,1]==or$會員卡號 & arudata[i,2]==or$days),6]
  out=paste(arudata[i,1],arudata[i,2],length(k))
  for(j in 1:length(k)){
    out=paste(out,k[j])
  }
  cat(out,"\n")
}
sink()
##

data2 <- read_baskets(con = "out2.txt", info = c("sequenceID","eventID","size"))
rules<- cspade(data2, parameter = list(support = 0.004), control = list(verbose = TRUE))

head(as(data2,"data.frame"))
(result <- as(rules,"data.frame"))

r2 <- ruleInduction(rules, confidence = 0.5,
                    control = list(verbose = TRUE))

summary(r2)
r2
x=as(r2, "data.frame")
x[,1]=x[,1] %>% as.character



for(i in 1:dim(x)[1]){
 # x123=gregexpr("[0-9]+",x[i,1])
 # for(j in 1: length( x123[[1]])){
    while(gregexpr("\\{[0-9]+\\}",x[i,1])[[1]]>0){
    xx=gregexpr("\\{[0-9]+\\}",x[i,1])
    x1=substr(x[i,1], xx[[1]][1]+1,xx[[1]][1] +(attr(xx[[1]], "match.length")-1-1)[1]) 
    x2=data[data[,6]==x1,12] %>% as.matrix %>% .[1]
    x[i,1]=gsub(x1,x2,x[i,1])
  }
}

x[,2]=round(x[,2],4)
x[,3]=round(x[,3],4)
x[,4]=round(x[,4],4)

options(max.print=999999)
sink("asfasf.txt")
x
sink()


get_num <- function(x){
  s <- str_replace_all(s, "([[:punct:]]|<|>|=)", " ")
  s1 <- strsplit(s, ' ')[[1]]
  s1[sapply(s1, nchar) >= 1]
}

