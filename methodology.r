#PROLOGUE #################################################################################

setwd("C:/Users/Honza/Desktop/Empirical_paper/Lung_cancer") #set your working directory
data <- read.csv("survey_lung_cancerdata.csv") #load the data
#View(data) #take a look at the data
#install.packages("dplyr") #install dplyr, if you do not have it installed
library(dplyr) #load the package

#SECTION 1 ################################################################################

#Now some data manipulation will be performed, so that we know what is the dataset about.
dim(data) # dimension of the dataframe
glimpse(data) #take a glimpse at the data
data %>% colnames() #names of our variables
#This for cycle prints class of every variable (glimpse function already gave us this
#information, this is just another way to acquire it)
for (i in 1:ncol(data)){
  print(class(data[,i]))
}
#This is definitely troublesome.
#Many of these variables should be factors rather than integers (this is based on basic logic)
#this for cycle converts problematic integer variables to factors and prints their levels
for (i in 3:ncol(data)){
  data[,i] <- data[,i] %>% factor()
  print(levels(data[,i]))
}

data$LUNG_CANCER <- ifelse(data$LUNG_CANCER == "YES", 1,0) #converts YES or NO to numeric
data$LUNG_CANCER %>% #checks what class is the variable for cancer
  class()
data %>% #counts number of observations with and without cancer
  count(LUNG_CANCER)

#We are especially interested in impacts of smoking on lung cancer:
data %>%
  count(SMOKING, wt = LUNG_CANCER)



#Oh, shit, it seems that our dataset is not unbiased random sample from population of 
#lung-cancer patients, as 80% to 90% of lung cancer cases are associated with smoking.
#What now then? I believe, that we can continue to work nevertheless, because this is
#sample of people from population who fill out online questionnare. There are various possible
#explanations for why these people are not and even cannot be random sample of patients
#with lung cancer. However, I believe we can still use the data, only the predictive
#power of our model will not be general. It will be associated with specific subsample
#of cancer patients.

#SECTION 2 ################################################################################

#Here we will do some actual modelling. We choose generalized linear model due to binary
#nature of our response variable.
#As we have shown before, only 12.6% of our observations do not suffer from lung cancer.
#Therefore we choose logit, as it has fatter tails and is better at modelling outliers.
model <- glm(LUNG_CANCER ~ ., data = data, family = binomial(link = "logit")) #model
summary(model) #summary

#Some of our variables did not prove themselves to be significant.
#However, many of these variables  









#Tohle jsou jen nejaky moje poznamky, co pak smazu

install.packages("plm")
library("plm")
data("Cigar")
data <- Cigar
View(data)
View(Cigar)
library(dplyr)
data %>%
  filter(year == "90") %>%
  arrange(state)
data2 <- read.csv()








