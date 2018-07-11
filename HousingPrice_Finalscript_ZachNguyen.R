#####################################
### Section 1: Data Preparation #####
#####################################

# Importing the data
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

test_id <- test$Id
test$Id <- NULL
train$Id <- NULL

test$SalePrice <- NA
full <- rbind(train,test)

# Loading libraries

library(dplyr)            ## Used for Data manipulation, data wrangling
library(tidyr)            ## Used for Data manipulation, data wrangling
library(VIM)              ## Used to visualization of missing variables
library(mice)             ## Used for imputation of missing variables
library(psych)            ## Used to get Descriptive statistics
library(ggplot2)          ## Used to plot graphs
library(gridExtra)        ## Used to arrange visualization
library(corrplot)         ## Used for drawing correlation plots
library(caret)            ## Used for regression
library(randomForest)     ## Used to conduct random forest
library(e1071)            ## Used for statistical analysis

#######################################################
### Section 2: Data Cleaning, Wrangling, Imputation ###
#######################################################

# A look at the missing variable

miss <- full %>% 
          select(everything()) %>% 
          summarize_all(funs(sum(is.na(.))))
sort(miss[1,], decreasing = TRUE)


# Visualize the missingness of the data

var_with_missing <- colnames(full[colSums(is.na(full)) >0])   
var_with_missing <- var_with_missing[-35]
aggr(full[,var_with_missing], 
     combined = TRUE, bars = TRUE, sortVars = TRUE, cex.axis = 0.69)

# Find and clean some obvious incorrect data

# Find any year value above 2010 and replace with suitable values
full %>% 
  select(GarageYrBlt, YrSold, YearBuilt, YearRemodAdd) %>% 
  filter_all(any_vars(.>2010))      
full[which(full$GarageYrBlt  > 2010), "GarageYrBlt"] <- 2007    

# Find illogical year value (Sold before Built, Remodeled before Built, Garage Built before Built)
full %>% 
  select(GarageYrBlt, YrSold, YearBuilt, YearRemodAdd) %>% 
  filter(YrSold < YearBuilt | YearRemodAdd < YearBuilt | GarageYrBlt < YearBuilt)      
full[which(full$YearBuilt > full$YrSold), "YrSold"] <-  2008
full[which(full$YearBuilt > full$YearRemodAdd), "YearRemodAdd"] <- 2002 

# Find properties with missing garage year built, set them to year built
which(!is.na(full$GarageType) & is.na(full$GarageYrBlt))
full[c(2127,2577), c("GarageType", "GarageYrBlt", "YearBuilt")]
full[c(2127,2577), "GarageYrBlt"] <- full[c(2127,2577), "YearBuilt"]

# Find Predictors catagories with too few data point and assign to nearest neighbors
which(full$MSSubClass == 150)
full[2819, "MSSubClass"] <- 160

which(full$TotRmsAbvGrd == 13 | full$TotRmsAbvGrd == 14 | full$TotRmsAbvGrd == 15)
full[c(636,1903,2550), "TotRmsAbvGrd"] <- 12

which(full$Fireplaces == 4)
full[2711, "Fireplaces"] <- 3

which(full$GarageCars == 5)
full[1829, "GarageCars"] <- 4

# Fill in missing data which are Missing Not At Random (MNAR)
full.MNAR <- full

# Group MNAR vars, where NA indicate the subject is not there
var_MNAR <- c("Alley", "BsmtQual", "BsmtCond","BsmtExposure","BsmtFinType1",
                   "BsmtFinType2","FireplaceQu","GarageType", "GarageYrBlt",
                   "GarageFinish","GarageQual","GarageCond","PoolQC","Fence",
                   "MiscFeature")     

# Create a function to fill in missing data
NA_to_NotAvailable <- function (data, predictor) {
                        levels(data[,predictor]) <- c(levels(data[,predictor]), "Not Available")
                        data[,predictor][is.na(data[,predictor])] <- "Not Available"
                        return(data[,predictor])
                        } 

for (i in 1:length(var_MNAR)) {
                      full.MNAR[,var_MNAR[i]] <- NA_to_NotAvailable(full, var_MNAR[i])
                      }

# Visualize data after MNAR variables imputation
aggr(full.MNAR[,var_with_missing], 
     combined = TRUE, bars = TRUE, sortVars = TRUE, cex.axis = 0.69)  

miss <- full.MNAR %>% 
  select(everything(), -SalePrice) %>% 
  summarize_all(funs(sum(is.na(.))))
sort(miss[1,], decreasing = TRUE)

# Use mice to impute data Missing At Random (MAR)

# Group the remaining MAR variables
var_MAR <- c("MasVnrType", "MSZoning", "Utilities", "Functional", 
                      "Exterior1st", "Exterior2nd", "Electrical", 
                      "KitchenQual", "SaleType")

# Convert these into factors for mice imputation
full.MNAR <- full.MNAR %>% 
          mutate_at(var_MAR, funs(factor(.)))       

# We use cart method since imputation is fairly linear
imputed.data <- mice(full.MNAR[,!(names(full.MNAR) %in% "SalePrice")], m=1, method='cart')
full.complete <- mice::complete(imputed.data, action = 1, include = FALSE)

# Visualize Lot Front and Mass Veneer which has the most imputation. 
# We also use log transformation to linearize graph and aid visualization.
xyplot(imputed.data, 
       LotFrontage ~ log10(LotArea), pch = 20, grid = TRUE,
       xlab = "Area of Lot (Log10 Sq Ft)",
       ylab = "Length of Street Lot (Ft)",
       main = "Imputation of Length of Street Lot") 

xyplot(imputed.data,
       MasVnrArea ~ MasVnrType,
       xlab = "Type of Mason Veneer",
       ylab = "Area of Mason Veneer",
       main = "Imputation of Length of Street Lot")     

# Finalize our data set after imputation
train.complete <- full.complete[1:1460,]
train.complete$SalePrice <- train$SalePrice
test.complete <- full.complete[1460:2919,]

 # aggr(full.complete[,var_with_missing], 
     # combined = TRUE, bars = TRUE, sortVars = TRUE, cex.axis = 0.69)

############################################
### Section 3: Exploratory Data Analysis ###
############################################

# Checking data structure and the dependent variable
 # str(train)

 # Visualize the dependent variable SalePrice
density_plot_depvar <- ggplot(train.complete, aes(x= SalePrice)) +
  geom_density(color = "black", fill = "darkgreen") +
  geom_vline(aes(xintercept = mean(SalePrice)),
             color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Density plot of Housing Sale Price", x = "Sale Price", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())
density_plot_depvar

# Visualization of numeric variable correlations to SalePrice

numeric_var <- which(sapply(train.complete, is.numeric))
numeric_var_names <- names(numeric_var)

# Make correlation chart with all numeric variable
cor <- cor(train.complete[,numeric_var], use = "pairwise.complete.obs") 

# Sort chart column in descending order
cor_sales_vector <- as.matrix(sort(cor[,"SalePrice"], decreasing = TRUE))

# Find name index of columns, row with high correlation
high_cor_var <- names(which(apply(cor_sales_vector,1, function(x) abs(x)> 0.5))) 

# Plot correlation chart
cor.data <- as.data.frame(cor)[high_cor_var, high_cor_var] # Subset correlation chart
cor.data <- cor.data[order(-cor.data$SalePrice),] # Sort row in descending order
corrplot.mixed(as.matrix(cor.data), lower = "number", lower.col = "black", tl.pos = "lt") 

# Visualization of relationships between predictors highly correlated to SalePrice

Visualize_numeric <- function(data, n_predictor, xname) {
  ggplot(data, aes(x = data[,n_predictor], y = SalePrice)) +
    geom_point(na.rm = TRUE, col = "darkgreen", size = 1) +
    geom_smooth(na.rm = TRUE, method = "lm", se=FALSE, color="black", aes(group=1)) +
    scale_y_continuous(breaks = seq( 0, max(data$SalePrice), by = 100000), labels = scales::comma) +
    ylab("Sale Price in USD") +
    xlab(xname)
}

p1 <- Visualize_numeric(train.complete, "GrLivArea", "Above Ground Living Area")
p2 <- Visualize_numeric(train.complete, "GarageArea", "Garage Area")
p3 <- Visualize_numeric(train.complete, "TotalBsmtSF", "Total Basement Area in Sq Feet")
p4 <- Visualize_numeric(train.complete, "X1stFlrSF", "First Floor Area in Sq Feet")
grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Trend chart Visualization of continous highly correlated variables")

# We can also spot the outliers

which(train$GrLivArea > 4500)
which(train$GarageArea > 1250)
which(train$TotalBsmtSF > 4000)
which(train$X1stFlrSF > 4000)

# Visualization of discrete numeric variables
Visualize_category <- function(data, c_predictor, xname) {
  ggplot(data, aes(x= factor(data[,c_predictor]), y = SalePrice)) +
    geom_boxplot(na.rm = TRUE, col='darkgreen', outlier.color = "red") + 
    scale_y_continuous(breaks= seq( 0, max(data$SalePrice), by = 100000), labels = scales::comma) +
    ylab("Sale Price in USD") +
    xlab(xname)
}

p5 <- Visualize_category(train.complete, "OverallQual", "Overall Rating of House")
p6 <- Visualize_category(train.complete, "GarageCars", "Car capacity of garage")
p7 <- Visualize_category(train.complete, "FullBath", "Number of full bathrooms above ground")
p8 <- Visualize_category(train.complete, "TotRmsAbvGrd", "Total number of room above ground")
p9 <- Visualize_category(train.complete, "YearBuilt", "The house's construction year")
p10 <- Visualize_category(train.complete, "YearRemodAdd", "The house's remodel year")
grid.arrange(p5, p6, p7, p8, p9, p10, ncol = 2, top = "Box plot Visualization of discrete highly correlated variables")

# Using Random forest to analyze important variables

 # Specify Factor, ordered factor and numeric values
factor_var <- c("MSSubClass", "MSZoning", "Street", "Alley", "LandContour", "LotConfig",
                "Neighborhood", "Condition1", "Condition2", "BldgType", "RoofStyle", "RoofMatl",
                "Exterior1st", "Exterior2nd", "MasVnrType", "Foundation", "Heating", "GarageType", 
                "PavedDrive", "MiscFeature", "SaleType", "SaleCondition", "CentralAir", "MSZoning", 
                "HouseStyle", "Fence", "MoSold")
ordered_var <- c("LotShape", "Utilities", "LandSlope", "OverallQual",
                 "OverallCond", "ExterQual", "ExterCond", "BsmtQual",
                 "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "HeatingQC", "Electrical",
                 "KitchenQual", "Functional", "FireplaceQu", "GarageFinish", "GarageCars",
                 "GarageQual", "GarageCond", "PoolQC", "GarageYrBlt",
                 "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces")
numeric_var <- c("LotFrontage", "YearBuilt", "YearRemodAdd", "YrSold", "LotArea", "MasVnrArea", 
                 "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF","X1stFlrSF", "X2ndFlrSF", 
                 "LowQualFinSF", "GrLivArea", "GarageArea", "WoodDeckSF", "OpenPorchSF",
                 "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal",  "FullBath", 
                 "HalfBath", "BsmtFullBath", "BsmtHalfBath")
 # Check: length(factor_var) + length(ordered_var) + length(numeric_var) # We have 80 variables

# Duplicate the dataset to be used in random forest
full.rf <- full.complete %>% 
  mutate_at(factor_var, funs(factor(.))) %>% 
  mutate_at(ordered_var, funs(ordered(.)))

 # str(full.rf)

# Train random forest and pick out 20% of important variables
train.rf <- full.rf[1:1460,]
train.rf$SalePrice <- train$SalePrice

set.seed(123)
Rf <- randomForest(x = train.rf[,-80], y=train.rf$SalePrice, ntree=100, importance=TRUE)
var.importance <- importance(Rf)
var.importance <- data.frame(Variables = row.names(var.importance), MSE = var.importance[,1])
var.importance <- var.importance[order(var.importance$MSE, decreasing = TRUE),]
var.importance$Variables <- as.factor(var.importance$Variables)

# Plot important predictor variables
ggplot(var.importance[1:(nrow(var.importance)/5),], aes(x = reorder(Variables, desc(abs(MSE))), y = MSE)) +
  geom_bar(stat = "identity", color = "red", fill = "darkgreen") +
  labs(title = "Top important variables", x = "Top Important Variables", y = "Marginal Effect on MSE (% change)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 16),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold")) 

##################################################
### Section 4: Feature Engineering/ Extraction ###
##################################################

  # Build a function to help visualize available variables to extract new sensible ones
full.final <- full.complete
Visualize_suspect_var <- function(data, s_predictor, xname) {
                          ggplot(data, aes(x = as.factor(data[,s_predictor]), y = data[,"SalePrice"])) +
                              geom_bar(stat = "summary", fun.y = "mean", fill = "green") +
                              geom_bar(stat = "summary", fun.y = "median", fill = "blue", alpha = 0.7) +
                              scale_y_continuous(labels = scales::comma) +
                              geom_hline(yintercept= median(data$SalePrice), linetype="dashed", color = "red", size = 1.5) +
                              geom_hline(yintercept= mean(data$SalePrice), linetype="dashed", color = "orange", size = 1.5) +
                              geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
                              ylab("Sale Price in USD") +
                              xlab(xname)
                          }

  # Feature engineering using binary method

# Is the location densely populated?
Visualize_suspect_var(train.complete, "MSZoning", "Zone of property") 
full.final$IsDensePop <- ifelse(full.final$MSZoning %in% c("FV", "RL"),"Yes", "No")
full.final$IsDensePop <- as.factor(full.final$IsDensePop)

# Is the property newer than most?
Visualize_suspect_var(train.complete, "MSSubClass", "Class of property") 
full.final$IsNewer <- ifelse(full.final$MSSubClass %in% c("20","60", "120","160"), "Yes", "No")
full.final$IsNewer <- as.factor(full.final$IsNewer)

# Was the property bought during peak season?
Visualize_suspect_var(train.complete, "MoSold", "Month Sold of property") 
full.final$IsPeak <- ifelse(full.final$MoSold %in% c("3","4", "5","6", "7", "8"), "Yes", "No")
full.final$IsPeak <- as.factor(full.final$IsPeak)

# # The land slope gentle?
# Visualize_suspect_var(train.complete, "LandSlope", "Land Slope of property") 
# full.final$IsGentle <- ifelse(full.final$LandSlope %in% c("Gtl"), "Yes", "No")
# full.final$IsGentle <- as.factor(full.final$IsGentle)

# Is lot shape regular, any deformities?
Visualize_suspect_var(train.complete, "LotShape", "Lot Shape of property") 
full.final$IsRegular <- ifelse(full.final$LotShape %in% c("Reg"), "Yes", "No")
full.final$IsRegular <- as.factor(full.final$IsRegular)

# Visualize_suspect_var(train.complete, "Electrical", "Electricity of property") # Is electricity standard?
# full.final$IsStandard <- ifelse(full.final$Electrical %in% c("SBrkr"), "Yes", "No")
# full.final$IsStandard <- as.factor(full.final$IsStandard)

# Is Garage Detached?
Visualize_suspect_var(train.complete, "GarageType", "Garage Type of property") 
full.final$IsDetached <- ifelse(full.final$GarageType %in% c("Detchd", "Not Available", "CarPort"), 
                                "Yes", "No")
full.final$IsDetached <- as.factor(full.final$IsDetached)

# Is the Drive Paved?
Visualize_suspect_var(train.complete, "PavedDrive", "Drive of property") 
full.final$IsPaved <- ifelse(full.final$PavedDrive %in% c("Y"), "Yes", "No")
full.final$IsPaved <- as.factor(full.final$IsPaved)

# Has the house been changed/remodeled?
Visualize_suspect_var(train.complete, "YearRemodAdd", "Year Remodeled of property") 
full.final$IsRemod <- ifelse(full.final$YearBuilt != full.final$YearRemodAdd, "Yes", "No")
full.final$IsRemod <- as.factor(full.final$IsRemod)

# Was the property bought under loan/mortgage?
Visualize_suspect_var(train.complete, "SaleCondition", "Sales Condition of property") 
full.final$Invested <- ifelse(full.final$SaleCondition %in% c("Partial"), "Yes", "No")
full.final$Invested <- as.factor(full.final$Invested)

# Does the property have a fireplace?
full.final$HasFireplace <- ifelse(full.final$FireplaceQu %in% c("Not Available"), "No", "Yes")
full.final$HasFireplace <- as.factor(full.final$HasFireplace)

# Does the property have a basement?
full.final$HasBsmt <- ifelse(full.final$BsmtQual %in% c("Not Available"), "No", "Yes")
full.final$HasBsmt <- as.factor(full.final$HasBsmt)

# Is the property new?
full.final$IsNew <- ifelse(full.final$YearBuilt == full.final$YrSold, "Yes", "No")
full.final$IsNew <- as.factor(full.final$IsNew)

# Does the property have a second floor?
full.final$Has2ndFlr <- ifelse(full.final$X2ndFlrSF != 0, "Yes", "No")
full.final$Has2ndFlr <- as.factor(full.final$Has2ndFlr)

# Does the property have a Garage?
full.final$HasGarage <- ifelse(full.final$GarageArea != 0, "Yes", "No")  
full.final$HasGarage <- as.factor(full.final$HasGarage)

# Does the property have a WoodDeck?
full.final$HasWD <- ifelse(full.final$WoodDeckSF != 0, "Yes", "No")  # Has Wood Deck?
full.final$HasWD <- as.factor(full.final$HasWD)

# Does the property have a Open Porch?
full.final$HasOporch <- ifelse(full.final$OpenPorchSF != 0, "Yes", "No")  # Has Open Porch?
full.final$HasOporch <- as.factor(full.final$HasOporch)

# Does the property have a Close Porch?
full.final$HasCporch <- ifelse(full.final$EnclosedPorch != 0, "Yes", "No")  # Has Closed Porch?
full.final$HasCporch <- as.factor(full.final$HasCporch)

# Does the property have a Screen Porch?
full.final$HasSporch <- ifelse(full.final$ScreenPorch != 0, "Yes", "No")  # Has Screen Porch?
full.final$HasSporch <- as.factor(full.final$HasSporch)

# Does the property feels like its protecting the owners?
Visualize_suspect_var(train.complete, "Fence", "Fence of property") # Feels Protected?
full.final$Protected <- ifelse(full.final$Fence %in% c("GdPrv", "Not Available"), "Yes", "No")
full.final$Protected <- as.factor(full.final$Protected)

  # Feature engineering using binning method

# Visualize_suspect_var(train.complete, "Neighborhood", "Neighborhood of property")
ggplot(train.complete, aes(x = reorder(Neighborhood, SalePrice, FUN = mean), y = SalePrice)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "green") +
  geom_bar(stat = "summary", fun.y = "median", fill = "blue", alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept= median(train.complete$SalePrice), linetype="dashed", color = "red", size = 1.5) +
  geom_hline(yintercept= mean(train.complete$SalePrice), linetype="dashed", color = "orange", size = 1.5) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
  ylab("Sale Price in USD") +
  xlab("Neighborhood")


full.final$NeighborhoodQual <- ifelse(full.final$Neighborhood %in% c("MeadowV"), "Bad",
                                         ifelse(full.final$Neighborhood %in% c("OldTown", "Edwards", "BrkSide", "Sawyer", "Blueste", "SWISU", "NAmes", "NPkVill"),"Fair",
                                                ifelse(full.final$Neighborhood %in% c("Mitchel", "SawyerW", "Gilbert", "NWAmes", 
                                                                                         "Blmngtn", "CollgCr"), "Good", "Excellent")))
full.final$NeighborhoodQual <- as.ordered(full.final$NeighborhoodQual) # Categorize neighborhood by cluster visually

  # Feature engineering using interaction

# Aggregating some features to better reflect human decision making mentality
full.final$TotBath <- full.final$FullBath + (full.final$HalfBath*0.5) + 
                        full.final$BsmtFullBath + (full.final$BsmtHalfBath*0.5) # Total number of bathrooms 
full.final$TotSF <- full.final$GrLivArea + full.final$TotalBsmtSF # Total living space
full.final$HouseAge <- full.final$YrSold - full.final$YearBuilt # Total house Age


################################################################
### Section 5: Encoding data to be used for machine learning ###
################################################################


# Drop variables with lack of information/ duplicate information

Visualize_suspect_var(train.complete, "Utilities", "Utilities of property") # Very few observation to matter

cor.high <- cor
cor.high[lower.tri(cor.high ,diag = TRUE)] <- NA
cor.high <- subset(na.omit(data.frame(expand.grid(dimnames(cor.high)), value = c(cor.high))), value > .7) 
                            # Subset to find pairs with high correlation
                            # cor.high

var.drop <- c("YearRemodAdd", "Utilities", "GarageYrBlt", "GarageArea", "GarageCond", "TotalBsmtSF",
              "TotalRmsAbvGrd", "BsmtFinSF1")
full.final <- full.final[,!(names(full.final) %in% var.drop)]  # Drop these variables

# Convert the remaining into suitable variable type and add correct levels to ordered

ordered_var <- ordered_var[!ordered_var %in% c("Utilities", "GarageCond", "GarageYrBlt")]
full.final <- full.final %>% 
  mutate_at(factor_var, funs(factor(.))) %>% 
  mutate_at(ordered_var, funs(ordered(.)))

 str(full.final[,ordered_var])

full.final$LotShape <- ordered(full.final$LotShape, levels = c("IR3", "IR2", "IR1", "Reg"))
full.final$LandSlope <- ordered(full.final$LandSlope, levels = c("Sev", "Mod", "Gtl"))
full.final$ExterQual <- ordered(full.final$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
full.final$ExterCond <- ordered(full.final$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
full.final$BsmtQual <- ordered(full.final$BsmtQual, levels = c("Not Available", "Fa", "TA", "Gd", "Ex"))
full.final$BsmtCond <- ordered(full.final$BsmtCond, levels = c("Not Available","Po", "Fa", "TA", "Gd"))
full.final$BsmtExposure <- ordered(full.final$BsmtExposure, levels = c("Not Available","No", "Mn", "Av", "Gd"))
full.final$BsmtFinType1 <- ordered(full.final$BsmtFinType1, levels = c("Not Available","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
full.final$BsmtFinType2 <- ordered(full.final$BsmtFinType2, levels = c("Not Available","Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
full.final$HeatingQC <- ordered(full.final$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
full.final$Electrical <- ordered(full.final$Electrical, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"))
full.final$KitchenQual <- ordered(full.final$KitchenQual, levels = c("Fa", "TA", "Gd", "Ex"))
full.final$Functional <- ordered(full.final$Functional, levels = c("Sal", "Sev", "Maj1", "Maj2", "Mod", "Min2", "Min1", "Typ"))
full.final$FireplaceQu <- ordered(full.final$FireplaceQu, levels = c("Not Available", "Po","Fa", "TA", "Gd", "Ex"))
full.final$GarageFinish <- ordered(full.final$GarageFinish, levels = c("Not Available", "Unf","RFn", "Fin"))
full.final$GarageQual <- ordered(full.final$GarageQual, levels = c("Not Available", "Po","Fa", "TA", "Gd", "Ex"))
full.final$PoolQC <- ordered(full.final$PoolQC, levels = c("Not Available","Fa", "Gd", "Ex"))
full.final$TotRmsAbvGrd <- ordered(full.final$TotRmsAbvGrd, levels = c("2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15"))
full.final$Fireplaces <- ordered(full.final$Fireplaces, levels = c("0","1", "2", "3", "4"))

                                                         
# Removing outliers
full.final$SalePrice <- full$SalePrice
full.final <- full.final[-c(524,1299),]

## Prepocess numeric variables

nums <- unlist(lapply(full.final, is.numeric))  
full.numeric <- full.final[,nums]
full.numeric <- full.numeric[,!(names(full.numeric) %in% "SalePrice")]
full.factor <- full.final[,!nums]
cat('There are', length(full.numeric), 'numeric variables, and', length(full.factor), 'factor variables')

# Log transformation of numeric variables
describe(full.numeric)

for(i in 1:ncol(full.numeric)){
  if (abs(skew(full.numeric[,i]))>0.8){
    full.numeric[,i] <- log(full.numeric[,i] +1)
  }
}

# Normalizing the numeric variables
num.process <- preProcess(full.numeric, method=c("center", "scale"))
print(num.process)
full.numeric.scaled <- predict(num.process, full.numeric)

# Create dummy variables

full.dummy <- as.data.frame(model.matrix(~.-1, full.factor))
dim(full.dummy)

# Taking out a few categorical/dummy variables with very little information
var.lowinfo <- which(colSums(full.dummy[1:nrow(full.final[!is.na(full.final$SalePrice),]),])<10)
colnames(full.dummy[var.lowinfo])

full.dummy <- full.dummy[,-var.lowinfo] #removing variables
dim(full.dummy)

# Combining the predictor variables
full.encode <- cbind(full.numeric.scaled, full.dummy)


# Dealing with skewness in response variable
skew(full.final$SalePrice)
full.encode$SalePrice <-  log(full.final$SalePrice)
qqnorm(full.encode$SalePrice)
qqline(full.encode$SalePrice)    # Log transform and visualize SalePrice

# Divide encoded data into train and test set
train.encode <-  full.encode[1:1458,]
test.encode <- full.encode[1459:2917,]

#####################################################
### Section 6: Model selection and Model Training ###
#####################################################

# Use elastic net via cross validation to tune hyperparameter
set.seed(123)
mod.control <-trainControl(method="cv", number=5)
grid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.001))

regressor <- train(x=train.encode[,!(names(train.encode) %in% "SalePrice")], y=train.encode$SalePrice, 
                   method='glmnet', trControl= mod.control, tuneGrid= grid) 
regressor$bestTune

min(regressor$results$RMSE)

# Check the number of variables being selected for the best model.
contrib <- varImp(regressor,scale=F)$importance
var.selected <- length(which(contrib$Overall!=0))
var.nselected <- length(which(contrib$Overall==0))

cat('L-Regression uses', var.selected, 'variables in its model, and did not select', var.nselected, 'variables.')

ypred <- predict(regressor,test.encode)
ypred <- exp(ypred)
