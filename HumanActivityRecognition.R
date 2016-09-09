remove(train_test)
remove(train_train)
remove(test_test)
remove(inTrain)
remove(colnames_test)
remove(colnames_train)
remove(prediccion1_1)
remove(prediccion1_2)
remove(drops)
remove(colcnts)
remove(cnt)
remove(tree1)
remove(tree2)
remove(nsv)
remove(aaa)
remove(bbb)
remove(ccc)

library(AppliedPredictiveModeling)
library(caret)
library(ggplot2)
library(plotly)
library(rattle)
library(rpart.plot)
library(randomForest)
library(rgl)
library(plot3D)
library(gridExtra)
library(grid)

#######################################################################
####################1. Getting the data################################
#######################################################################
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

traininghr <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testinghr <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))



########################################################################
############2. Configuring training and testing dataset#################
########################################################################

colnames_train <- colnames(traininghr)
colnames_test <- colnames(testinghr)

names(traininghr)
names(testinghr)

#Partitioning the training and testing set
inTrain <- createDataPartition(y=traininghr$classe, p=0.7, list=FALSE)
train_train<- traininghr[inTrain, ]
train_test<- traininghr[-inTrain, ]
dim(train_train)
dim(train_test)



######################################################################
##############3. Cleaning and treating the data#######################
######################################################################

#3.1 Configuring functions to drop NA's####################
###########################################################
# Count the number of non-NAs in each col.
contarNAs <- function(x) {
        as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))
}

# Build vector of missing data or NA columns to drop.
columnaNA <- contarNAs(train_train)
drops <- c()
for (colNA in 1:length(columnaNA)) {
        if (columnaNA[colNA] < nrow(train_train)) {
                drops <- c(drops, colnames_train[colNA])
        }
}

#3.2 Dropping Na's columns from traininghr#################
###########################################################

# Drop NA data and unneccesary variables for prediction
train_train<- train_train[,!(names(train_train) %in% drops)]
train_train<- train_train[,8:length(colnames(train_train))]

train_test<- train_test[,!(names(train_test) %in% drops)]
train_test<- train_test[,8:length(colnames(train_test))]

# Show remaining columns.
colnames(train_train)
colnames(train_test)

#Verifying near zero variance
nsv <- nearZeroVar(train_train, saveMetrics=TRUE)
nsv


#3.3 Dropping Na's columns from testinghr##################
###########################################################
# Drop NA data and unneccesary variables for prediction
test_test<-testinghr

test_test<- test_test[,!(names(test_test) %in% drops)]
test_test<- test_test[,8:length(colnames(test_test))]


########################################################################
##############4. Additional exploratory data############################
########################################################################

str(train_train)
summary(train_train)

######################################################################
#2D analysis
######################################################################

#####################################################################
#Analyzing with a heath map

ccc<-as.matrix(train_train[c(1:50), c(1:52)]) #Matrix from train_train
heatmap(ccc, cexCol=0.5, cexRow = 0.5)
?heatmap

#Transforming all variables to numeric
aaa<-as.data.frame(train_train %>%  mutate_each_( funs(as.numeric(.)), names( .[,sapply(., is.numeric)] )))
str(aaa)
bbb<-as.matrix(aaa[c(1:50),c(1:52)]) #Matrix from aaa
heatmap(bbb)


#####################################################################
#Common graphics

#Analyzing acceleration
bp1<-qplot(classe, total_accel_belt, data=train_train, geom="boxplot")
bp2<-qplot(classe, total_accel_arm, data=train_train, geom="boxplot")
bp3<-qplot(classe, total_accel_forearm, data=train_train, geom="boxplot")
bp4<-qplot(classe, total_accel_dumbbell, data=train_train, geom="boxplot")
grid.arrange(bp1, bp2, bp3, bp4, ncol=2)

# par(mfrow = c(1, 1))  
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
boxplot(train_train$classe, train_train$total_accel_arm)




######################################################################
#3D analysis
######################################################################

######################################################################
#With rgl package
#Creating a numeric vector of classe
classe_rgl<-as.numeric(train_train$classe)


#About belt
par3d(windowRect = c(100, 100, 612, 612))
mfrow3d(2, 2)

gyros_belt<-plot3d(train_train$gyros_belt_x, train_train$gyros_belt_y, 
                   train_train$gyros_belt_z, col=classe_rgl)

accel_belt<-plot3d(train_train$accel_belt_x, train_train$accel_belt_y, 
                   train_train$accel_belt_z, col=classe_rgl)

magnet_belt<-plot3d(train_train$magnet_belt_x, train_train$magnet_belt_y, 
                    train_train$magnet_belt_z, col=classe_rgl)

rollpitchyaw_belt<-plot3d(train_train$roll_belt, train_train$pitch_belt, 
                          train_train$yaw_belt, col=classe_rgl)

#About arm
par3d(windowRect = c(100, 100, 612, 612))
mfrow3d(2, 2)
gyros_arm<-plot3d(train_train$gyros_arm_x, train_train$gyros_arm_y, 
                  train_train$gyros_arm_z , col=classe_rgl)
accel_arm<-plot3d(train_train$accel_arm_x, train_train$accel_arm_y,
                  train_train$accel_arm_z, col=classe_rgl)
magnet_arm<-plot3d(train_train$magnet_arm_x, train_train$magnet_arm_y,
                   train_train$magnet_arm_z, col=classe_rgl)


#About forearm
par3d(windowRect = c(100, 100, 612, 612))
mfrow3d(2, 2)
gyros_forearm<-plot3d(train_train$gyros_forearm_x, train_train$gyros_forearm_y,
                       train_train$gyros_forearm_z, col = classe_rgl)

accel_forearm<-plot3d(train_train$accel_forearm_x, train_train$accel_forearm_y,
                      train_train$accel_forearm_z, col=classe_rgl)

magnet_forearm<-plot3d(train_train$magnet_forearm_x, train_train$magnet_forearm_y,
                       train_train$magnet_forearm_z, col=classe_rgl)


#To introduce legends
#legend3d("topright", legend = paste(unique(classe_rgl)))
#legend3d("topright", legend = paste('Type', c('A', 'B', 'C', 'D', 'E')), pch = 16, col = rainbow(3), cex=1, inset=c(0.02))



##########################################################################
#With plot3d package
with(train_train, scatter3D(gyros_belt_x, gyros_belt_y, 
                            gyros_belt_z, col=classe_rgl, pch=16, cex=0.5,
                            theta=10, d=2))



######################################################################
########################5. Model######################################
######################################################################

#################################################################
###5.1 Applying predictive trees

tree1<-train(classe~., data=train_train, method="rpart", 
             trControl=trainControl(method = "cv", number = 10))
print(tree1)
print(tree1$finalModel)
grid.newpage()
ff<-fancyRpartPlot(tree1$finalModel)
ff


#Testing the model 5.1 
prediccion1_1<-predict(tree1, train_test)
print(confusionMatrix(prediccion1_1, train_test$classe), digits=4)
#train_test$prediccion1_1<-prediccion1_1
#table(train_test$prediccion1_1, train_test$classe)

#Prediction in test_test sample
prediccion1_2<-predict(tree1, test_test)
test_test$prediccion1_2<-prediccion1_2
table(test_test$prediccion1_2)

#################################################################
###5.2 Applying predictive trees with preprocessing and cross validation 
tree2<-train(classe~., data=train_train, method="rpart", preProcess=c("center", "scale"), 
            trControl=trainControl(method = "cv", number = 4))
print(tree2)
print(tree2$finalModel)
fancyRpartPlot(tree2$finalModel)


#Testing the model 5.2
prediccion2_1<-predict(tree2, train_test)
print(confusionMatrix(prediccion2_1, train_test$classe), digits=4)
#mytest$prediccion2_1<-prediccion2_1


#################################################################
###5.3 Applying predictive trees


