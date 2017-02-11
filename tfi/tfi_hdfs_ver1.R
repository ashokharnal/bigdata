## Use R interface and NOT RStudio interface.
## 10th Feb, 2017
# Problem: Predict Revenue from Resturants
# Kaggle: Reference: https://www.kaggle.com/c/restaurant-revenue-prediction
# Date 6th Feb, 2017
# Objective:
#	a. How to issue hdfs commands from R
#	b. How to start h2o
#	c. How to connect h2o to hdfs
# 	d. How to use h2o from with R
#       e. How to perform PCA in h2o
#	f. How to develop model in h2o
#          on data in hdfs
# Pre-requisites:
#	A. hadoop must be started
#	B. YARN must be started


##1.
# Transfer files from Linux folder to hdfs folder
# 1.1 Create folder if it does not exist
system("hdfs dfs -mkdir -p /user/ashok/data_files/h2o/tfi/")
# 1.2 Then transfer files
system("hdfs dfs -put /cdata/tfi/*.csv /user/ashok/data_files/h2o/tfi/")
system("hdfs dfs -ls /user/ashok/data_files/h2o/tfi/")

## 2. Start h2o
# 2.1 Folder /user/ashok/h2o needs to be deleted before we start h2o
system("hdfs dfs -ls /user/ashok/h2o")     # Does it exist
system("hdfs dfs -rmr /user/ashok/h2o")    # Delete it

## 2.2 Start h2o from R
#   (After issung this command wait for some time..Takes time...R-prompt will come)
system("cd /home/ashok/h2o ; hadoop jar h2odriver.jar -nodes 1 -mapperXmx 1g -output h2o -timeout 6000 > /home/ashok/h2o_msg.txt", wait =F)

# 2.3 AFTER SOME TIME Call h2o library and other libraries
library(h2o)
library(lubridate)

## 3.
# Initialise/connect to h2o
h2o = h2o.init(ip="10.0.2.15",port=54321,startH2O=F)

## 4. Read data from hdfs
# 4.1 Design column types vector
num=rep(c('numeric'), each=38)
str=c('numeric', 'String','factor','factor','factor')
ctypes<-c(str,num)
# 4.2 Import file
filepath="hdfs://localhost:9000/user/ashok/data_files/h2o/tfi/train.csv"
train.hex<-h2o.importFile(path = filepath, destination_frame = "train.hex", header=T,col.types=ctypes)

## 5. Observe data
str(train.hex)
dim(train.hex)
head(train.hex)
h2o.cor(train.hex)
h2o.describe(train.hex)

##6. Date processing
# 6.1 First convert to r-dataframe
opendate<-as.data.frame(train.hex$'Open Date')
head(opendate)
class(opendate)
# 6.2 Use lubridate functions to extract date components
opendate$Open.Date<-mdy(opendate$Open.Date)
opendate$month<-month(opendate$Open.Date)
opendate$day<-day(opendate$Open.Date)
opendate$year<-year(opendate$Open.Date)
opendate$wday<-wday(opendate$Open.Date)
# 6.3 Convert date-dataframe to h2o frame
odate.hex<-as.h2o(opendate, destination_frame = "odate.hex")

## 7. Bind date and train dataframes
odate.hex$Open.Date<-NULL
train.hex<-h2o.cbind(odate.hex, train.hex)
head(train.hex)
train.hex$'Open Date'<-NULL
train.hex$Id<-NULL

# 8. Perform PCA 
# PCA will be done for numeric columns: c(1:4, 8:44)

train.pca=h2o.prcomp(training_frame = train.hex[,c(1:4,8:44)], k=16,transform = "STANDARDIZE")
x<-predict(train.pca, train.hex[,c(1:4,8:44)])
x$City<-train.hex$City
x$"City Group"<-train.hex$"City Group"
x$Type<-train.hex$Type
x$revenue<-train.hex$revenue


## 9. Split data frame
tr.split = h2o.splitFrame(train.hex, ratios = c(0.8))
train80=tr.split[[1]]
dim(train80)
valid20=tr.split[[2]]
dim(valid20)
class(train80)

## 10. Prepare for modeling
y='revenue'
x=names(train.hex[!(names(train.hex) %in% c('revenue'))])

model=h2o.gbm(x, y, train80, '007',distribution = c("gaussian"), quantile_alpha = 0.5, tweedie_power = 1.5,  ntrees = 1500, max_depth = 3, min_rows = 10, learn_rate = 0.002, learn_rate_annealing = 0.99, nfolds = 5, keep_cross_validation_predictions = TRUE, keep_cross_validation_fold_assignment = FALSE, col_sample_rate = 0.8, sample_rate = 0.8, stopping_metric="MSE")

# 10.1 Examine model
model
plot(model)

## 11. Predict validation data and find performance
pred=h2o.predict(model, valid20)
h2o.performance(model,valid20)

## 12.
# Before you leave do not forget to quit
#   So that h2o is stopped
q()

################## FINISH #########################################




