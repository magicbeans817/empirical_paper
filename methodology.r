#PROLOGUE #################################################################################

setwd("C:/Users/Honza/Desktop/Empirical_paper/empirical_paper") #set your working directory
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

#data$LUNG_CANCER <- ifelse(data$LUNG_CANCER == "YES", 1,0) #converts YES or NO to numeric
#data$LUNG_CANCER <- as.integer(data$LUNG_CANCER)

data$LUNG_CANCER %>% #checks what class is the variable for cancer
  class()
data %>% #counts number of observations with and without cancer
  count(LUNG_CANCER)

#We are especially interested in impacts of smoking on lung cancer:
data %>%
  count(SMOKING, wt = LUNG_CANCER)


#We can put these two together:
tabulka <- data %>%
  count(SMOKING, LUNG_CANCER)

data %>%
  count(SMOKING, YELLOW_FINGERS, LUNG_CANCER)

data %>%
  count(YELLOW_FINGERS, LUNG_CANCER)

#print the table, so we can put it in the paper
#magie na konvertovani matic, datatables a dataframes do texovych tabulek
#install.packages(xtable)
library(xtable)
tabulka <- as.matrix(tabulka)
pocet <- as.numeric(as.vector(tabulka[,"n"]))
tabulka1 <- matrix(pocet, nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("Non-smokers","Smokers"),c("Cancer-free","Cancer")))
tabulka1 <- as.data.frame(tabulka1)
tabulka1
print(xtable(tabulka1, caption = "Numbers of smokers and non-smokers in the dataset with and without cancer",
             digits = 0, type = "latex"), file = "table1.tex")


#Oh no, it seems that our dataset is not unbiased random sample from population of 
#lung-cancer patients, as 80% to 90% of lung cancer cases are associated with smoking.
#What now then? I believe, that we can continue to work nevertheless, because we create the model
#so that it has strong predictive power, not explanatory power. Better papers explaining factors
#behind lung cancer have estimated. However, predicting lung cancer seems much more interesting and yet
#not so mainstream (although it is becoming mainstream)

#SECTION 2 ################################################################################

#Here we will do some actual modelling. We choose generalized linear model due to binary
#nature of our response variable.
#As we have shown before, only 12.6% of our observations do not suffer from lung cancer.
#Therefore we choose logit, as it has fatter tails and is better at modelling outliers.

#Let's create formula for our model:
flipitydopity <- c("GENDER","AGE", "SMOKING","ANXIETY","PEER_PRESSURE","YELLOW_FINGERS",
                   "CHRONIC.DISEASE","FATIGUE","ALLERGY","WHEEZING","ALCOHOL.CONSUMING",
                   "COUGHING","SHORTNESS.OF.BREATH","SWALLOWING.DIFFICULTY","CHEST.PAIN")

fmla <- as.formula(paste("LUNG_CANCER ~", paste(flipitydopity, collapse= "+")))
fmla

#The model
model1 <- glm(fmla, data = data, family = binomial(link = "logit")) #model
summary(model1) #summary

'''
Some of our variables did not prove themselves to be significant.
However, some of these variables may connected to each other, therefore we will now consider
adjusting our model and adding interactions between variables. We can also substract several
variables as insignificant. For example, gender does not seem to play a significant role
in matter of having cancer. Also anxiety might be connected to lung cancer, however it makes
sense that this implication is not strong, as much larger share of population suffers
from anxiety than from lung cancer. The implication may be strong from only one side.
'''


#Lets make a vector of variables we will exclude:
exclude <- c("ANXIETY","WHEEZING","SHORTNESS.OF.BREATH","CHEST.PAIN","GENDER","AGE","ALCOHOL.CONSUMING")
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
interactions <- c("SMOKING:COUGHING","CHRONIC.DISEASE:FATIGUE")
flipitydopity3 <- c(flipitydopity2, interactions)
fmla3 <- as.formula(paste("LUNG_CANCER ~", paste(flipitydopity3, collapse= "+")))
model3 <- glm(fmla3, data = data, family = binomial(link = "logit")) #model with interactions
summary(model3)

#Interesting... what if we take out smoking?
exclude2 <- c("SMOKING")
flipitydopity4 <- c(flipitydopity2, interactions)
flipitydopity4 <- flipitydopity4[!flipitydopity4 %in% exclude2]
fmla4 <- as.formula(paste("LUNG_CANCER ~", paste(flipitydopity4, collapse= "+")))
model4 <- glm(fmla4, data = data, family = binomial(link = "logit")) #model with interactions
summary(model4)


data$model4_pred <- predict(model4, data, type = "response")
data$binary <- ifelse(data$LUNG_CANCER == "YES",1,0)

#install.packages("ResourceSelection")
library(ResourceSelection) #we will check Hosmer-Lemeschov test for goodness of fit
hl <- hoslem.test(data$binary, data$model4_pred, g=10)
hl

x <- c()
for (i in 5:15) {
  print(hoslem.test(data$binary, data$model4_pred, g=i)$p.value)
  x <- rbind(hoslem.test(data$binary, data$model4_pred, g=i)$p.value,x)
  #x[i-4,1] <- hoslem.test(data$binary, data$model4_pred, g=i)$p.value
}
x
x <- rev(x)
x

hl_pvalues <- matrix(c(5:15,x), byrow = FALSE, nrow = length(x), ncol = 2,
                     dimnames = list(c(5:15),c("Number of groups", "p-values of HL test")))
hl_pvalues


#install.packages("pROC") #package pro AUC a ROC
library(pROC)

ROC <- roc(data$LUNG_CANCER, data$model4_pred)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)

#according to this, we can make the threshold
treshold <- 0.35
#we can add new column with diagnosis as a string
data <- data %>%
  mutate(DIAGNOSIS = ifelse(model4_pred < treshold, "NO","YES"))
data$DIAGNOSIS <- data$DIAGNOSIS %>% as.factor()
data$DIAGNOSIS %>% is.factor()

#install.packages(caret)
#install.packages(e1071)
library(caret)
library(e1071)
conmat <- confusionMatrix(data$DIAGNOSIS, data$LUNG_CANCER, positive = "YES")
conmat

#print the tables
print(xtable(summary(model4), caption = "Summary of logit model",
             digits = 3, type = "latex"), file = "summary_estimation.tex")

#print the tables
print(xtable(hl_pvalues, caption = "Hosmer-Lemeschov test, p-values",
             digits = 3, type = "latex"), file = "hltest_pvalues.tex")

#print the tables
print(xtable(confmat, caption = "Confusion Matrix for classification threshold = 0.35", #nefunguje, napisu to texu sam
             digits = 3, type = "latex"), file = "confusion_matrix.tex")




