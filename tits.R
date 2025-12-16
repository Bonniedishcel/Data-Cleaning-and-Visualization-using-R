train <- titanic_data
train
#1b)
Ccounts <- table(train$Survived, train$PClass)
#confusion Matrix
Ccounts
counts <- table(train$Survived, train$Sex)
#confusion Matrix
counts
# survival for male and female
female <- nrow(train[train$Sex=='female',])
male <- nrow(train[train$Sex=='male',])
slice <- c(female, male)
divi <- c("Female", "Male")

survival_percent <- c(counts[2]/(counts[1]+counts[2]), counts[4]/(counts[3]+counts[4]))
# Survival Percent for Female,   Male
survival_percent

#1c) survival by class of ticket
Ccounts <- table(train$Survived, train$PClass)
#confusion Matrix
Ccounts
Class_survival_percent <- c(Ccounts[2]/(Ccounts[1]+Ccounts[2]), Ccounts[4]/(Ccounts[3]+Ccounts[4]), Ccounts[6]/(Ccounts[5]+Ccounts[6]))
Class_survival_percent

#1d)Pclass and gender
Ccounts <- table(train$Survived, train$PClass,train$Sex)
Ccounts
Class_gendersurvival_percent <- c(Ccounts[2]/(Ccounts[1]+Ccounts[2]),
                            Ccounts[4]/(Ccounts[3]+Ccounts[4]),
                            Ccounts[6]/(Ccounts[5]+Ccounts[6]),
                            Ccounts[8]/(Ccounts[7]+Ccounts[8]),
                            Ccounts[10]/(Ccounts[9]+Ccounts[10]),
                            Ccounts[12]/(Ccounts[11]+Ccounts[12])
                            
                            )
Class_gendersurvival_percent
#1e) refer first script

#1f
Ccounts <- table(train$Survived,train$Age)
Ccounts
# code to be done 
# Class 1 is Classy and others NonClassy
CustomerType <- factor(train$PClass,
                       levels = c("1st","2nd","3rd"),
                       labels = c("Classy","Non Classy","Non Classy"))
CustomerType
