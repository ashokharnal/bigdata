# Objectives:
#           A. How to read a csv file
#           B. How to look at data-structure
#           C. How to access slices of data
#           D. Basic plotting
#           E. Statistical tests


# 1
# Set all libraries that we will use
library(dplyr)     # For glimpse() and sample_n
library(prettyR)   # For describe()
library(ggplot2)   # For plotting

# 2
# Set path to your data files (change it appropriately for your machine)
#   Note the use of forward slash (/) in the path statement
# 2.1
setwd("C:/Users/ashokharnal/Documents/bigDataLessons/3-dayclasses/data_exploration/bankmarketing")

# Just Check what path has been set
# 2.2
getwd()

#2.3
# List all files in current directory
dir()

# 2.4
#  Read bank-full.csv file into some variable 'bankdata'.
#    Fields are separated by semi-colon
bankdata<-read.table("bank-full.csv", header=TRUE,sep=";",quote="\"")

#******************
# 3. Have a look at data 
#******************

# 3.1
# See some data
head(bankdata)		# Display some starting rows
tail(bankdata)		# Display rows from tail-end

# 3.2
# Data dimesion
dim(bankdata)		# How many rows and columns
ncol(bankdata)		# Number of columns
nrow(bankdata)		# Number of rows

# 3.3
# Check data type of variable 'bankdata'
class(bankdata)

# 3.4
# What is the structure of 'bankdata'
str(bankdata)
glimpse(bankdata)

# 3.5
# Examine distribution of individual factors
table(bankdata$education)  # What to do with unknown
# 3.51 Divide age into three levels
bankdata$age_level=cut(bankdata$age,3,labels = c("young","middle","senior"))
# 3.52 See distribution of education vs age
#          And the fill up education, age-level wise
table(bankdata$education,bankdata$age_level)


# 3.6
# Row and col names
row.names(bankdata)	  # Rownames
names(bankdata)		    # Again column names
colnames(bankdata)	  # Column names

# 3.7
# See a data point at row 5, column 6
bankdata[5,6]
## Square-bracket operator
# Look at first five rows
bankdata[1:5,]
# Select just 'job' column
bankdata[,'job']
# OR, as
bankdata$job

#******************
# 4. Slice data
#******************

## Slice data
# Just look at all those rows data where job = management

# 4.1
## Which rows have values 'management'. Note double equal to (==)
bankdata[,'job']=='management'
# Get these values in a variable
myvalues<-bankdata[,'job']=='management'
bankdata[myvalues,]
head(bankdata[myvalues,])
# Or write directly as
bankdata[bankdata[,'job']=='management',]
OR
bankdata[bankdata$job=='management',]

# 4.2
# Look at those rows where age > 21
bankdata[bankdata[,'age']> 21,]		# Try first bankdata[,'age']> 21
# OR
bankdata[bankdata$age > 21,]
# Look at only job and marital columns
bankdata[,c(1,3)]

# 4.3
# Look rows 42 to 78 of job and marital columns
bankdata[c(42:78),c(1,3)]
# OR
bankdata[c(42:78),c('job','marital')]

# 4.4 
# Assign those who are "young" and whose education level is 'unknown;
#  education of "secondary"
#   Filter age_level wise and education wise
g<-bankdata[bankdata$age_level=="young" & bankdata$education == "unknown"    ,]
head(g)

# Create a logical vector as per filter
lv<-bankdata$age_level=="young" & bankdata$education == "unknown"
bankdata[lv,]$education <-"secondary"


#******************
# 5. Aggregate data
#******************

# 5.1
########## Explore individual variables #################
# Summarise. Note the differences between summary of
#   numerical and categorical data 
summary(bankdata)
describe(bankdata)

# 5.2
#   Aggregate is more general form of table.
#    class of aggregate is data.frame
#     Aggregate  age, marital status wise
aggregate(age ~ marital, mean , data=bankdata)
#   How balance depends upon job
aggregate(balance ~ job ,mean,data=bankdata)
#    Sort it
aggregate(balance ~ job ,mean,data=bankdata) %>% arrange(balance)
# g<-aggregate(balance ~ job ,mean,data=bankdata)
# ggplot(data=g,mapping = aes(x=job,y=balance)) + geom_bar(stat="identity")

# Advanced
aggregate(cbind(age,balance) ~ marital, mean , data=bankdata)
aggregate(age ~ marital+housing, mean , data=bankdata)
aggregate(cbind(balance,duration) ~ job+education+housing ,mean,data=bankdata)
aggregate(cbind(age,balance) ~ marital, mean , data=bankdata)
aggregate(.  ~ marital, mean , data=bankdata)




########## Some statistical operations #################
########## Explore multiple variables #################

# 6
# correlation between age, balance and duration
cor(bankdata[,c(1,6,12)])

# 6.1
# Chi-sqaure test
# Is there a relationship between 'has housing loan?' 
#   and 'has personal loan?'
# Get a contingency table
p<-table(bankdata$housing,bankdata$loan)
#   Better still give names to rows and columns
p<-table("Has housing loan?"=bankdata$housing,"Has personal loan?"=bankdata$loan)
p # Contingency table

# 6.2
# Apply chisquare test
chisq.test(p)

# 6.3
#  Compare two means
# Are the means of 'balance' different for those with 'yes' 
#  and with 'no' hosuing loan
sample1<-bankdata[bankdata$housing=="yes",]$balance
sample2<-bankdata[bankdata$housing=="no",]$balance
t.test(sample1,sample2)

# 6.4
# Test the hypothesis that population mean for balance  is 1300
t.test(bankdata$balance,mu=1300)


#******************
# 7. Plotting data
#******************

# 7.1
# Histogram of numerical variables, such as, age
#     Specify number of bins with breaks = 20
#      Also show labels
hist(bankdata$age, labels = TRUE)
# Have 10 bins
hist(bankdata$age,breaks=10, labels = TRUE)

# 7.2
# Get information about plotted histogram
histinfo<-hist(bankdata$age,breaks=10)
histinfo

# 7.3
# MAke a density, not frequency histogram
#   Read density points on y-axis
#     Sum of areas must total to 1
histinfo<-hist(bankdata$age,freq=FALSE)
histinfo

# 7.4
# Transform data and then plot
hist(log10(bankdata$age),breaks=20)

# 7.5
# Density plot
plot(density(bankdata$age))

# Superimpose normal distribution on histogram
#    dnorm is for density distribution function
# hist(bankdata$age,freq=FALSE)
# Note that in dnorm(), we have not specified 'x'. Default values are taken 
#   from the x-limits used from the previous plot. (see ?curve)
# curve(dnorm(x, mean=mean(bankdata$age), sd=sd(bankdata$age)), add=TRUE, col="darkblue", lwd=2) 

# 7.6
# Frequency of values in column education
table(bankdata$education)
# Draw a pie chart of education (levels) factors
pie(table(bankdata$education))
# Or draw a simple barplot of categorical data
pie(table(bankdata$education))
barplot(table(bankdata$education))
barplot(table(bankdata$marital, bankdata$housing))  # Stacked barplot
legend("topright", legend=c("a","b","c"))
legend("bottom", legend=c("single","married","divorced"), fill=c("lightgray","gray","darkgray"))

# 7.7
# Draw boxplots maritl status wise 
#   X-axis (independent) has categorical variable 
boxplot(age ~ marital,  data=bankdata)

# Draw scatter plot between age and balance
plot(bankdata$balance,bankdata$age)


# 7.8
# Or use ggplot() to draw a good bar-plot
ggplot(bankdata, aes(x = education)) + geom_bar()
# Bar plots of two categorical variables: education and job
ggplot(bankdata, aes(x = education,fill=job)) + geom_bar()
# Or barplot with proportion of jobs education wise
ggplot(bankdata, aes(x = education,fill=job)) + geom_bar(position="fill")

# 7.9
# Draw boxplots maritl status wise 
#   X-axis (independent) has categorical variable 
ggplot(bankdata,aes(x=marital,y=age))+geom_boxplot()
# You can draw a single boxplot() with imaginary
#   categorical variable on x-axis
ggplot(bankdata,aes(x=factor(0),y=age))+geom_boxplot()

# 7.10
# Plot density graph of counts of education (x-axis), job-wise
ggplot(bankdata, aes(x = education, color = job)) + geom_density()

# 8. gc() Garbage Collector output:
#    The max used column shows that you started with certain memory
#      sometime during startup R was using more memory,
#       and the gc trigger shows that garbage collection
#        will happen when you get above certain number of Ncells or
#           Vcells.
# 8.1 Remove/delete all objects from memory
rm(list = ls()) 
gc()

################# FINISHED #####################




