#loading STEPAIc and CAR libraries
install.packages("car")
library("car")
library(MASS)

#importing the dataset from a csv file to dataframe.
cars_data <- read.csv("CarPrice_Assignment.csv")

#Viewing the data in cars_data
summary(cars_data)

#removing the carid from the dataset as it is has no significance in our predication
cars_data <- cars_data[-1]

#separating the carname from the column excluding model names
library(tidyr)
cars_data <- separate(cars_data,CarName,into = c("CarModel","ModelNumber"),sep = " ")

#deleting the ModelNumber column from dataset
cars_data <- cars_data[,!names(cars_data) %in% c("ModelNumber")]

#on observing the data, we have 2 rendundencies like nissan and Nissan, volkswagen is also written
#as vw. hence replacing both with the same name
library(stringr)
cars_data$CarModel <- str_replace_all(cars_data$CarModel,"Nissan","nissan")
cars_data$CarModel <- str_replace_all(cars_data$CarModel,"vw","volkswagen")
cars_data$CarModel <- str_replace_all(cars_data$CarModel,"maxda","mazda")
cars_data$CarModel <- str_replace_all(cars_data$CarModel,"porcshce","porsche")
cars_data$CarModel <- str_replace_all(cars_data$CarModel,"toyouta","toyota")
cars_data$CarModel <- str_replace_all(cars_data$CarModel,"vokswagen","volkswagen")

#checking for NA values in dataset
sum(sapply(cars_data, is.na))


#there are no NA values present in the dataset

#removing the duplicates if present in dataframe
cars_data <- unique.data.frame(cars_data)

#converting all the categorical variables into numeric to see the significance of the variables
levels(cars_data$fueltype) <- c(0,1)
cars_data$fueltype <- as.numeric(levels(cars_data$fueltype))[cars_data$fueltype]

levels(cars_data$aspiration) <- c(0,1)
cars_data$aspiration <- as.numeric(levels(cars_data$aspiration))[cars_data$aspiration]

levels(cars_data$enginelocation) <- c(0,1)
cars_data$enginelocation <- as.numeric(levels(cars_data$enginelocation))[cars_data$enginelocation]

levels(cars_data$doornumber) <- c(0,1)
cars_data$doornumber <- as.numeric(levels(cars_data$doornumber))[cars_data$doornumber]

#converting multi-level categorical variables into numeric and binding the columns to the actual dataset
dummyvariable_carbody <- data.frame(model.matrix(~carbody+0,data = cars_data))
cars_data <- cbind(cars_data[,!names(cars_data) %in% c("carbody")],dummyvariable_carbody)
 
dummyvariable_carModel <- data.frame(model.matrix(~CarModel+0,data = cars_data))
cars_data <- cbind(cars_data[,!names(cars_data) %in% c("CarModel")],dummyvariable_carModel)

dummyvariable_drivewheel <- data.frame(model.matrix(~drivewheel+0,data = cars_data))
cars_data <- cbind(cars_data[,!names(cars_data) %in% c("drivewheel")],dummyvariable_drivewheel)

dummyvariable_enginetype <- data.frame(model.matrix(~enginetype+0,data = cars_data))
cars_data <- cbind(cars_data[,!names(cars_data) %in% c("enginetype")],dummyvariable_enginetype)

dummyvariable_cylindernumber <- data.frame(model.matrix(~cylindernumber+0,data = cars_data))
cars_data <- cbind(cars_data[,!names(cars_data) %in% c("cylindernumber")],dummyvariable_cylindernumber)

dummyvariable_fuelsystem <- data.frame(model.matrix(~fuelsystem+0,data = cars_data))
cars_data <- cbind(cars_data[,!names(cars_data) %in% c("fuelsystem")],dummyvariable_fuelsystem)

#symboling value is converted to numeric datatype 
cars_data$symboling <- as.numeric(cars_data$symboling)

#deriving a new metric for stroke ratio which is derived by dividing bore-ratio to stroke
cars_data$strokeRatio <- cars_data$boreratio/cars_data$stroke
cars_data <- cars_data[,!names(cars_data) %in% c("boreratio","stroke")]

#Now building the model to predict the price of car
#setting the seed
set.seed(100)

#creating the training data and test datar
trainingdata_size <- sample(1:nrow(cars_data),0.7*nrow(cars_data))
training_data <- cars_data[trainingdata_size,]
test_data <- cars_data[-trainingdata_size,]

#creating the first model  
model_first <- lm(formula = price~.,cars_data)
summary(model_first)

#In the model_1, we have NA values which means in the given dataset which means there 
#are no observations with that combination of levels of the factors.
#using STEPAIC method to eliminate insignificant variables
step_model <- stepAIC(model_first,direction="both")
step_model


#now removning the variables one by one which are insignificant

model_second <- lm(formula = price~fueltype+aspiration+enginelocation+wheelbase
              +carlength+carwidth+carheight+curbweight+enginesize
              +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                carbodyhatchback+CarModelalfa.romero+CarModelaudi
              +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
              +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
              +CarModelporsche+CarModelsaab+CarModelsubaru+CarModelvolkswagen
              +enginetypedohc+enginetypedohcv+enginetypeohcv+cylindernumberfive
              +cylindernumberfour+cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
              +strokeRatio,cars_data)
summary(model_second)

#calculating the vif of model_2 to check for multi collinearity
vif(model_second)

#fueltype has high VIF hence removing that variable and checking 
model_third <- lm(formula = price~aspiration+enginelocation+wheelbase
                   +carlength+carwidth+carheight+curbweight+enginesize
                   +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                     carbodyhatchback+CarModelalfa.romero+CarModelaudi
                   +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
                   +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
                   +CarModelporsche+CarModelsaab+CarModelsubaru+CarModelvolkswagen
                   +enginetypedohc+enginetypedohcv+enginetypeohcv+cylindernumberfive
                   +cylindernumberfour+cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
                   +strokeRatio,cars_data)
summary(model_third)
vif(model_third)

#eliminating curbweight from the mdoel as it has high VIF
model_fourth <- lm(formula = price~aspiration+enginelocation+wheelbase
                  +carlength+carwidth+carheight+enginesize
                  +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                    carbodyhatchback+CarModelalfa.romero+CarModelaudi
                  +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
                  +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
                  +CarModelporsche+CarModelsaab+CarModelsubaru+CarModelvolkswagen
                  +enginetypedohc+enginetypedohcv+enginetypeohcv+cylindernumberfive
                  +cylindernumberfour+cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
                  +strokeRatio,cars_data)
summary(model_fourth)
vif(model_fourth)

#remooving cylindernumberfour and checking 
model_5 <- lm(formula = price~aspiration+enginelocation+wheelbase
                   +carlength+carwidth+carheight+enginesize
                   +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                     carbodyhatchback+CarModelalfa.romero+CarModelaudi
                   +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
                   +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
                   +CarModelporsche+CarModelsaab+CarModelsubaru+CarModelvolkswagen
                   +enginetypedohc+enginetypedohcv+enginetypeohcv+cylindernumberfive
                   +cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
                   +strokeRatio,cars_data)
summary(model_5)
vif(model_5)

#removing carlength, adding cylindernumberfour and checking the model 
model_6 <- lm(formula = price~aspiration+enginelocation+wheelbase
              +cylindernumberfour+carwidth+carheight+enginesize
              +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                carbodyhatchback+CarModelalfa.romero+CarModelaudi
              +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
              +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
              +CarModelporsche+CarModelsaab+CarModelsubaru+CarModelvolkswagen
              +enginetypedohc+enginetypedohcv+enginetypeohcv+cylindernumberfive
              +cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
              +strokeRatio,cars_data)
summary(model_6)
vif(model_6)

#removing cykindernumberfour 
model_7 <- lm(formula = price~aspiration+enginelocation+wheelbase
              +carwidth+carheight+enginesize
              +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                carbodyhatchback+CarModelalfa.romero+CarModelaudi
              +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
              +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
              +CarModelporsche+CarModelsaab+CarModelsubaru+CarModelvolkswagen
              +enginetypedohc+enginetypedohcv+enginetypeohcv+cylindernumberfive
              +cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
              +strokeRatio,cars_data)
summary(model_7)
vif(model_7)

#enginetypedohcv has high p value hence removing the variable
model_8 <- lm(formula = price~aspiration+enginelocation+wheelbase
              +carwidth+carheight+enginesize
              +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                carbodyhatchback+CarModelalfa.romero+CarModelaudi
              +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
              +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
              +CarModelporsche+CarModelsaab+CarModelsubaru+CarModelvolkswagen
              +enginetypedohc+enginetypeohcv+cylindernumberfive
              +cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
              +strokeRatio,cars_data)
summary(model_8)
vif(model_8)

#removing engine size and checking the model
model_9 <- lm(formula = price~aspiration+enginelocation+wheelbase
              +carwidth+carheight
              +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                carbodyhatchback+CarModelalfa.romero+CarModelaudi
              +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
              +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
              +CarModelporsche+CarModelsaab+CarModelsubaru+CarModelvolkswagen
              +enginetypedohc+enginetypeohcv+cylindernumberfive
              +cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
              +strokeRatio,cars_data)
summary(model_9)
vif(model_9)

#removing CarModelvolkswagen 
model_10 <- lm(formula = price~aspiration+enginelocation+wheelbase
              +carwidth+carheight
              +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                carbodyhatchback+CarModelalfa.romero+CarModelaudi
              +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
              +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
              +CarModelporsche+CarModelsaab+CarModelsubaru
              +enginetypedohc+enginetypeohcv+cylindernumberfive
              +cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
              +strokeRatio,cars_data)
summary(model_10)
vif(model_10)

#removing the carwidth 
model_11 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +carheight
               +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelporsche+CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv+cylindernumberfive
               +cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
               +strokeRatio,cars_data)
summary(model_11)
vif(model_11)

#removing car height from model
model_12 <- lm(formula = price~aspiration+enginelocation+wheelbase
               
               +compressionratio+peakrpm+highwaympg+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelporsche+CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv+cylindernumberfive
               +cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
               +strokeRatio,cars_data)
summary(model_12)
vif(model_12)

#removing highwayrpm
model_13 <- lm(formula = price~aspiration+enginelocation+wheelbase
               
               +compressionratio+peakrpm+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelporsche+CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv+cylindernumberfive
               +cylindernumbersix+fuelsystem2bbl+fuelsystem4bbl
               +strokeRatio,cars_data)
summary(model_13)
vif(model_13)

#removing cylindernumbersix from model
model_14 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+peakrpm+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModelchevrolet+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelporsche+CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv+cylindernumberfive
               +fuelsystem2bbl+fuelsystem4bbl
               +strokeRatio,cars_data)
summary(model_14)
vif(model_14)
#removing CarModelchevrolet back to model 
model_15 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+peakrpm+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelporsche+CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv+cylindernumberfive
               +fuelsystem2bbl+fuelsystem4bbl
               +strokeRatio,cars_data)
summary(model_15)
vif(model_15)

#removing strokeRatio 
model_16 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+peakrpm+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelporsche+CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv+cylindernumberfive
               +fuelsystem2bbl+fuelsystem4bbl
               +strokeRatio,cars_data)
summary(model_16)
vif(model_16)

# removing cylindernumberfive
model_17 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+peakrpm+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelporsche+CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv
               +fuelsystem2bbl+fuelsystem4bbl
               +strokeRatio,cars_data)
summary(model_17)
vif(model_17)

#removing strokeRatio
model_18 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+peakrpm+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelporsche+CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv
               +fuelsystem2bbl+fuelsystem4bbl
               ,cars_data)
summary(model_18)
vif(model_18)

#removing peakrpm
model_19 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelporsche+CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv
               +fuelsystem2bbl+fuelsystem4bbl
               ,cars_data)
summary(model_19)
vif(model_19)
#removng CarModelporsche
model_20 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelsaab+CarModelsubaru
               +enginetypedohc+enginetypeohcv
               +fuelsystem2bbl+fuelsystem4bbl
               ,cars_data)
summary(model_20)
vif(model_20)

#adding porsche carmodel and removing CarModelsubaru
model_21 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModeldodge
               +CarModeljaguar+CarModelmitsubishi+CarModelpeugeot+CarModelplymouth
               +CarModelsaab+CarModelporsche
               +enginetypedohc+enginetypeohcv
               +fuelsystem2bbl+fuelsystem4bbl
               ,cars_data)
summary(model_21)
vif(model_21)
#removing CarModelmitsubishi
model_22 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModeldodge
               +CarModeljaguar+CarModelpeugeot+CarModelplymouth
               +CarModelsaab+CarModelporsche
               +enginetypedohc+enginetypeohcv
               +fuelsystem2bbl+fuelsystem4bbl
               ,cars_data)
summary(model_22)
vif(model_22)

#removing CarModelplymouth
model_23 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick+CarModeldodge
               +CarModeljaguar+CarModelpeugeot
               +CarModelsaab+CarModelporsche
               +enginetypedohc+enginetypeohcv
               +fuelsystem2bbl+fuelsystem4bbl
               ,cars_data)
summary(model_23)
vif(model_23)

#removing CarModeldodge
model_24 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelpeugeot
               +CarModelsaab+CarModelporsche
               +enginetypedohc+enginetypeohcv
               +fuelsystem2bbl+fuelsystem4bbl
               ,cars_data)
summary(model_24)
vif(model_24)

#removing enginetypedohc
model_25 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelpeugeot
               +CarModelsaab+CarModelporsche
               +enginetypeohcv
               +fuelsystem2bbl+fuelsystem4bbl
               ,cars_data)
summary(model_25)
vif(model_25)

#removing fuelsystem4bbl 
model_26 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelalfa.romero+CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelpeugeot
               +CarModelsaab+CarModelporsche
               +enginetypeohcv
               +fuelsystem2bbl
               ,cars_data)
summary(model_26)
vif(model_26)
#removing CarModelalfa.romero
model_27 <- lm(formula = price~aspiration+enginelocation+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelpeugeot
               +CarModelsaab+CarModelporsche
               +enginetypeohcv
               +fuelsystem2bbl
               ,cars_data)
summary(model_27)
vif(model_27)

#removing enginelocation
model_28 <- lm(formula = price~aspiration+wheelbase
               +compressionratio+carbodyconvertible+
                 carbodyhatchback+CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelpeugeot
               +CarModelsaab+CarModelporsche
               +enginetypeohcv
               +fuelsystem2bbl
               ,cars_data)
summary(model_28)
vif(model_28)
#removing compressionratio
model_29 <- lm(formula = price~aspiration+wheelbase
               +carbodyconvertible+
                 carbodyhatchback+CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelpeugeot
               +CarModelsaab+CarModelporsche
               +enginetypeohcv
               +fuelsystem2bbl
               ,cars_data)
summary(model_29)
vif(model_29)

#removing compression ratio is making carbodyhatchback insignificant 
#so adding back the compression ratio and removing carbody hatchback
model_30 <- lm(formula = price~aspiration+wheelbase
               +carbodyconvertible+compressionratio+
                 CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelpeugeot
               +CarModelsaab+CarModelporsche
               +enginetypeohcv
               +fuelsystem2bbl
               ,cars_data)
summary(model_30)
vif(model_30)
#removing CarModelpeugeot
model_31 <- lm(formula = price~aspiration+wheelbase
               +carbodyconvertible+compressionratio+
                 CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelsaab+CarModelporsche
               +enginetypeohcv
               +fuelsystem2bbl
               ,cars_data)
summary(model_31)
vif(model_31)

#now removing compressionratio
model_32 <- lm(formula = price~aspiration+wheelbase
               +carbodyconvertible+
                 CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelsaab+CarModelporsche
               +enginetypeohcv
               +fuelsystem2bbl
               ,cars_data)
summary(model_32)
vif(model_32)

#reoving CarModelsaab
model_33 <- lm(formula = price~aspiration+wheelbase
               +carbodyconvertible+
                 CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelporsche
               +enginetypeohcv
               +fuelsystem2bbl
               ,cars_data)
summary(model_33)
vif(model_33)

#predicting the results with test data
predict_1 <- predict(model_33,test_data)
test_data$testprice <- predict_1

#calculating r square for actual and predicted value
r<- cor(test_data$price,test_data$testprice)
rsquared <- cor(test_data$price,test_data$testprice)^2
# predicted rsquared--0.8
#actual rsquared--0.87
#RSE of 0.07

#removing carmodelporsche 
model_34 <- lm(formula = price~aspiration+wheelbase
               +carbodyconvertible+CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+
               +enginetypeohcv
               +fuelsystem2bbl
               ,cars_data)
summary(model_34)
vif(model_34)

#removing carmodelporsche makes rsquared drop by huge value hence adding back the variable
#removing fuelsystem2bbl
model_35 <- lm(formula = price~aspiration+wheelbase
               +carbodyconvertible+
                 CarModelaudi
               +CarModelbmw+CarModelbuick
               +CarModeljaguar+CarModelporsche
               +enginetypeohcv
               ,cars_data)
summary(model_35)
vif(model_35)
predict_2 <- predict(model_35,test_data)
test_data$testprice2 <- predict_2

#calculating r square for actual and predicted value
r<- cor(test_data$price,test_data$testprice2)
rsquared <- cor(test_data$price,test_data$testprice)^2
rsquared
#predicted rsquare value--0.8
#actual r square value--0.86
#no significance of keeping fuelsystem2bbl

#final model is model_35
final_model <- model_35
final_prdiction <- predict(model_35,test_data)
test_data$final_prdcted_price <- final_prdiction

predicted_r_squared <- cor(test_data$price,test_data$final_prdcted_price)^2
actual_r_squared <- 0.86
plot(test_data$price,test_data$final_prdcted_price)
#based on the model created, we have RES of 7% which is acceptable