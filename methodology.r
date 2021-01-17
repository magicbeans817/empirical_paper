#PROLOGUE #################################################################################

setwd("C:/Users/Honza/Desktop/Empirical_paper/Lung_cancer") #set your working directory
data <- read.csv("survey_lung_cancerdata.csv") #load the data
#data <- read.csv("survey_lung_cancer.csv") #load the data
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

#Let's create formula for our model:
flipitydopity <- c("GENDER","AGE", "SMOKING","YELLOW_FINGERS","ANXIETY","PEER_PRESSURE",
                   "CHRONIC.DISEASE","FATIGUE","ALLERGY","WHEEZING","ALCOHOL.CONSUMING",
                   "COUGHING","SHORTNESS.OF.BREATH","SWALLOWING.DIFFICULTY","CHEST.PAIN")

fmla <- as.formula(paste("LUNG_CANCER ~", paste(flipitydopity, collapse= "+")))
fmla

#The model
model1 <- glm(fmla, data = data, family = binomial(link = "logit")) #model
summary(model1) #summary

#Some of our variables did not prove themselves to be significant.
#However, many of these variables are connected to each other, therefore we will now consider
#adjusting our model and adding interactions between variables. We can also substract several
#variables as insignificant. For example, gender does not seem to play a significant role
#in matter of having cancer. Also anxiety might be connected to lung cancer, however it makes
#sense that this implication is not strong, asmuch larger share of population suffers
#from anxiety than from lung cancer.
#Also multicollinearity between e.g. smoking and yellow fingers


#Lets make a vector of variables we will exclude:
exclude <- c("ANXIETY","WHEEZING","SHORTNESS.OF.BREATH","CHEST.PAIN","GENDER","AGE")
flipitydopity2 <- flipitydopity[!flipitydopity %in% exclude]
flipitydopity2


#New formula:
fmla2 <- as.formula(paste("LUNG_CANCER ~", paste(flipitydopity2, collapse= "+")))
fmla2

#New model
model2 <- glm(fmla2, data = data, family = binomial(link = "logit")) #model with interactions
summary(model2)

#Lets add interactions:
interactions <- c()
#interactions <- c("SMOKING:YELLOW_FINGERS"#,#"SMOKING:PEER_PRESSURE",
                  #"SMOKING:ALCOHOL.CONSUMING","SMOKING:COUGHING"#,
                  #"SMOKING:SWALLOWING.DIFFICULTY"#,"CHRONIC.DISEASE:FATIGUE"
                  #)
flipitydopity3 <- c(flipitydopity2, interactions)
fmla3 <- as.formula(paste("LUNG_CANCER ~", paste(flipitydopity3, collapse= "+")))
model3 <- glm(fmla3, data = data, family = binomial(link = "logit")) #model with interactions
summary(model3)

#DAVIDE, TOHLE JE V PRDELI, TA DATA AZ DOTED ANI V JENDOM OHLEDU NEFUNGOVALA TAK,
#JAK BY MELA. a DO TOHO VSEHO JA MOC NEROZUMIM EKONOMETRII









#DONT RUN!!!!
#dalsi moje hovna, co mozna pouziju
predictions <- predict.glm(model2, data, type = "response" )
sort(predictions)
summary(predictions)

install.packages("MASS")
library(MASS)





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








