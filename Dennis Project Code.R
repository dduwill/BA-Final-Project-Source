
library(tidyr)
library(data.table)

train <- read.csv("~/Desktop/BA/final project/dataset/bank-additional/bank-additional-full.csv", sep=";", stringsAsFactors=FALSE)
test <- read.csv("~/Desktop/BA/final project/dataset/bank-additional/bank-additional.csv", sep=";", stringsAsFactors=FALSE)

#look at data
dim(train); str(train)  # cat 2:10,15,21
dim(test); str(test)    # cat 2:10,15,21

###### encode target variables ######
train$y<- ifelse(train$y == "yes",0,1)
test$y <- ifelse(test$y == "yes",0,1)

round(prop.table(table(train$y))*100) # 89%


#separate categorical and numerical
catcols <- c(2:10,15,21)
numcols <- setdiff(1:20,catcols)   # 找出2个data的不同， setdiff

# set train and test categorical columns
cat_train <- train[,catcols]
cat_test <- test[,catcols]

# set train and test numeric columns
num_train <- train[,numcols]
num_test <- test[,numcols]

# remove train and test to save memory
rm(train,test)

#load library
library (ggplot2)
library(plotly)
install.packages("devtools")
devtools::install_github("ropensci/plotly")

##########  Density ##########
#write a plot function
tr <- function(a){
              ggplot(data=num_train,aes(x=a,y=..density..))+
    geom_histogram(fill="blue",color="red",alpha=0.5,bins=100)+
    geom_density()
    ggplotly()
}

#variable age
tr(num_train$age)

#add target variable
num_train$y <- cat_train$y

########### Scatter ############

#create a scatter plot for age / duration
ggplot(data=num_train,aes(x=age,
                          y=duration))+
                      geom_point(aes(colour=y))+
                      scale_y_continuous("duration",breaks=seq(0,5000,200))


#variable duration
tr(num_train$duration)

#variable campaign
tr(num_train$campaign)

#variable pdays
tr(num_train$pdays)

#variable previous
tr(num_train$previous)

## visualize categorial variables
#dodged bar chart

all_bar <-function(i)
  {
  ggplot(cat_train,aes(x=i,fill=y))+ geom_bar(position="dodge",color="black")+scale_fill_brewer(palette = "Pastel1")+
    theme(axis.text.x = element_text(angle =60,hjust=1,size=10))
  }

#variables job
all_bar(cat_train$job)

# checking categories proportionate
prop.table(table(cat_train$job,cat_train$y),1)

#variables marital
all_bar(cat_train$marital)

# checking categories proportionate
prop.table(table(cat_train$marital,cat_train$y),1)

#variables education
all_bar(cat_train$education)

# checking categories proportionate
prop.table(table(cat_train$education,cat_train$y),1)


###############  Data Cleaning ################

table(is.na(num_train))  # check null data 453068 = 41188 *11, no null data
table(is.na(num_test))   # chechk null data 41190 = 4119 *10, no null data

# Correlation Caret
library(caret)
x <- cor(num_train)
ax <- findCorrelation(x, cutoff=0.7)  # set threhold =0.7
num_train <- num_train[,-ax,with=FALSE]








