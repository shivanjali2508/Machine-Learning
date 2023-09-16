getwd()
setwd("C:\\Users\\lords\\Documents\\R Practice - SJ")
dir()
univ = read.table('dataDemographics.csv', 
                  header=T,sep=',',
                  col.names=c("ID", "age",  "exp",	"inc",	
                              "zip",	"family",	
                              "edu",	"mortgage"))
dim(univ)
sum(is.na(univ))
sum(is.na(univ$age))
str(univ)

# Reading second Table
loanCalls <- read.table("dataLoanCalls.csv", header=T, sep=",",
                        col.names=c("ID", "infoReq", "loan"),
                        dec=".", na.strings="NA")
sum(is.na(loanCalls))

# Reading third Table
cc <- read.table("dataCC.csv", header=T, sep=",",
                 col.names=c("ID", "Month", "Monthly"),
                 dec=".", na.strings="NA")
sum(is.na(cc))

class(cc$ID)
class(cc$Month)
cc$ID <- as.factor(cc$ID)
cc$Month <- as.factor(cc$Month)
str(cc)

ccAvg <- data.frame(seq(1,5000), 
                    tapply(cc$Monthly, cc$ID, mean))
ccAvg
str(ccAvg)
names(ccAvg) <- c("ID", "ccavg")
str(ccAvg)

otherAccts <- read.table("dataOtherAccts.csv", header=T, sep=",",
                         col.names=c("ID", "Var", "Val"),
                         dec=".", na.strings="NA")
library(reshape) 
ID<-as.factor(otherAccts$ID)
Val<-as.factor(otherAccts$Val)

class(ID)
class(Val)


OtherAccountT<-data.frame(cast(otherAccts,ID~Var,value='Val'))
OtherAccountT

univcomp<-merge(univ,ccAvg,by.x="ID",by.y="ID",all=TRUE)
univcomp<-merge(univcomp,OtherAccountT,by.x="ID",by.y="ID",all=TRUE)
univcomp<-merge(univcomp,loanCalls,by.x="ID",by.y="ID",all=TRUE)
dim(univcomp)
univcomp
str(univcomp)

univcomp$cc<-as.factor(univcomp$cc)
univcomp$cd<-as.factor(univcomp$cd)
univcomp$online<-as.factor(univcomp$online)
univcomp$securities<-as.factor(univcomp$securities)
univcomp$infoReq<-as.factor(univcomp$infoReq)
str(univcomp)

library(DMwR2)

univ2<-knnImputation(univcomp,k=10,meth = 'median')
sum(is.na(univ2))
head(univ2,20)
str(univ2)

univ2$family <- ceiling(univ2$family)
univ2$edu <- ceiling(univ2$edu)

table(univcomp$age)
table(univcomp$exp)
table(univcomp$inc)
table(univcomp$ccavg)
table(univcomp$mortgage)
table(univcomp$loan)
table(univcomp$family)
table(univcomp$edu)


sapply(univ2, function(x) length(unique(x)))
attach(univ2)
univ2$family <- as.factor(family)
univ2$edu <- as.factor(edu)
univ2$loan <- as.factor(loan)
str(univ2)
names(univ2)
univ2Num <- subset(univ2, select=c(2,3,4,8,9))
head(univ2Num)
cor(univ2Num)

univ2<- univ2[,-c(1,3,5)]
dim(univ2)
str(univ2)
table(univ2$mortgage)

#age,inc,mortage,ccavg
install.packages("infotheo")
library(infotheo)
age <- discretize(univ2$age, disc="equalwidth", nbins=6) 
class(age)
head(age)
age<-as.factor(age$X)
bin_count<-table(age)
bin_count

inc=discretize(univ2$inc, disc="equalfreq", nbins=15) 
inc=as.factor(inc$X)
class(inc)
head(inc)
ccavg=discretize(univ2$ccavg, disc="equalfreq", nbins=10) 
ccavg=as.factor(ccavg$X)
class(ccavg)
head(ccavg)
mortgage=discretize(univ2$mortgage, disc="equalwidth", nbins=5)
mortgage=as.factor(mortgage$X)
class(mortgage)
head(mortgage)

head(univ2)
univ2<-subset(univ2,select=-c(1,2,6,5))
names(univ2)
univ2<-cbind(age,inc,ccavg,mortgage,univ2)
names(univ2)
head(univ2)
str(univ2)

rows<-seq(1,5000,1)
set.seed(123)
traindata<-sample(rows,3000)
set.seed(123)
remainingrows<-rows[-(traindata)]
testrows<-sample(remainingrows,1000)
set.seed(123)
evalrows<-rows[-c(traindata,testrows)]

length(traindata)
length(testrows)
length(evalrows)

dim(univ2)
train = univ2[traindata,]
dim(train)
str(train)
testrows=univ2[testrows,]
dim(testrows)
str(testrows)
evalrows=univ2[evalrows,]
dim(evalrows)
str(evalrows)

head(train,10)
summary(train)

head(train); head(testrows); head(evalrows)
summary(train)
summary(testrows)

rm(age,ccavg, mortgage, inc, univ)
table(train$loan)
names(train)
install.packages('C50')
library(C50)

dtC50 <- C5.0(loan ~ ., data = train, rules=TRUE)
summary(dtC50)
names(train)

#predicting train data
predict(dtC50, newdata=train, type="class")
a=table(train$loan, predict(dtC50,newdata=train, type="class"))
a

rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
rcTrain


predict(dtC50, newdata=testrows, type="class")
a=table(testrows$loan, predict(dtC50, newdata=testrows, type="class"))
rcTest=(a[2,2])/(a[2,1]+a[2,2])*100
rcTest


predict(dtC50, newdata=evalrows, type="class")
a=table(evalrows$loan, predict(dtC50, 
                           newdata=evalrows, type="class"))
rcEval=(a[2,2])/(a[2,1]+a[2,2])*100


cat("Recall in Training", rcTrain, '\n',
    "Recall in Testing", rcTest, '\n',
    "Recall in Evaluation", rcEval)



