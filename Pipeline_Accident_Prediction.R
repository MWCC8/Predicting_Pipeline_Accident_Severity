# Oil Spills from Pipeline Accidents in the United States since 2010 - Michael Church Carson

spills = read.csv('spills.csv')
attach(spills)

### Pre-processing - cleaning the dataset.

# Replace spaces in column names with '.' and '/' with '.'
names(spills) <- sub(" ", ".", names(spills))
names(spills) <- sub("/", ".", names(spills))
# Rename columns to change '.." to '.' and remove '.' from the end of four of the column names
names(spills)[names(spills) == 'Unintentional.Release..Barrels.'] = 'Unintentional.Release.Barrels'
names(spills)[names(spills) == 'Intentional.Release..Barrels.'] = 'Intentional.Release.Barrels'
names(spills)[names(spills) == 'Liquid.Recovery..Barrels.'] = 'Liquid.Recovery.Barrels'
names(spills)[names(spills) == 'Net.Loss..Barrels.'] = 'Net.Loss.Barrels'
# There are a lot of missing values (NAs) in the initial data set from Kaggle. We cannot simply omit all NAs, because if we do there are only 5 observations left.
# Remove the columns (variables) that have over 15% missing values, except for Intentional.Release.Barrels (57% missing values) because 
# its missing values should be 0's. I know this because there is another column that is a function of the Intentional.Release.Barrels column and
# so I can verify that the missing values are equivelent to 0s. This will be addressed in the next step.
spills = subset(spills, select = -c(Liquid.Subtype,Liquid.Name,Shutdown.Date.Time,
                                    Restart.Date.Time,Public.Evacuations,Operator.Employee.Injuries,Operator.Contractor.Injuries,
                                    Emergency.Responder.Injuries,Other.Injuries,Public.Injuries,All.Injuries,Operator.Employee.Fatalities,
                                    Operator.Contractor.Fatalities,Emergency.Responder.Fatalities,Other.Fatalities,
                                    Public.Fatalities,All.Fatalities))
# Replace missing numerical values with 0 in the Intentional.Release.Barrels column.
spills$Intentional.Release.Barrels[is.na(spills$Intentional.Release.Barrels)] = 0
# Remove remaining rows (observations) with missing values (NAs)
spills = na.omit(spills)
attach(spills)
# Remove the indentifier variables 'Report.Number' and	'Supplemental.Number' because they are not useful for 
# supervised or unsupervised learning tasks.
spills = subset(spills, select = -c(Report.Number,Supplemental.Number))
# Remove predictors that are not useful for supervised learning tasks because of their format or logical meaning. 
spills = subset(spills, select = -c(Accident.Date.Time,Operator.ID,Pipeline.Facility.Name,Accident.City,Accident.County,Accident.Latitude,
                                    Accident.Longitude))
#Change binary (YES/NO) categorical variables Liquid.Ignition, Liquid.Explosion, and Pipeline.Shutdown to 1 if YES and 0 if NO
spills$Liquid.Ignition.Yes = ifelse(spills$Liquid.Ignition == 'YES', 1, 0)
spills = subset(spills, select = -c(Liquid.Ignition))

spills$Liquid.Explosion.Yes = ifelse(spills$Liquid.Explosion == 'YES', 1, 0)
spills = subset(spills, select = -c(Liquid.Explosion))

spills$Pipeline.Shutdown.Yes = ifelse(spills$Pipeline.Shutdown == 'YES', 1, 0)
spills = subset(spills, select = -c(Pipeline.Shutdown))

#Change binary (ONSHORE/OFFSHORE) categorical variable Pipeline.Location to 1 if ONSHORE and 0 if OFFSHORE
spills$Pipeline.Location.Onshore = ifelse(spills$Pipeline.Location == 'ONSHORE', 1, 0)
spills = subset(spills, select = -c(Pipeline.Location))
# One hot endoding for the pipeline type categroical variable so that it is 1 if aboveground (most common) and 0 it is another 
# type (underground, tank, or transisiton area)
spills$Pipeline.Type.Above = ifelse(spills$Pipeline.Type == 'ABOVEGROUND', 1, 0)
spills = subset(spills, select = -c(Pipeline.Type))
# One hot endoding for the liquid type categroical variable so that it is 1 if crude oil (most common) and 0 it is another
# type (BIOFUEL / ALTERNATIVE FUEL(INCLUDING ETHANOL BLENDS,CO2, HVL OR OTHER FLAMMABLE OR TOXIC FLUID, GAS, or 
# REFINED AND/OR PETROLEUM PRODUCT (NON-HVL), LIQUID )
spills$Liquid.Type.Crude = ifelse(spills$Liquid.Type == 'CRUDE OIL', 1, 0)
spills = subset(spills, select = -c(Liquid.Type))
# Remove Net.Lose.Barrels because it is the sum of the other three variables loss variables. Thus, it is going 
# to be perfectly collinear with these other three variables if we included it as a dependent variable in a regression.
spills = subset(spills, select = -c(Net.Loss.Barrels))

# The target variable for classification moodelling is environmental costs. Remove other cost variables.
spills = subset(spills, select = -c(Property.Damage.Costs,Lost.Commodity.Costs,Public.Private.Property.Damage.Costs,
                                    Emergency.Response.Costs, Other.Costs, All.Costs))
# After pre-processing, there are 2777 observations of 15 variables.
# For the supervised modelling tasks there is 1 dependent (or target) variable Environmental.Remediation.Costs (the environmental cost of a pipeline accident) 
# and 14 possible independent (or predictor) variables. 

# Make all remaining categorical variables into factors.
spills$Operator.Name=as.factor(spills$Operator.Name)
spills$Accident.State=as.factor(spills$Accident.State)
spills$Cause.Category=as.factor(spills$Cause.Category)
spills$Cause.Subcategory=as.factor(spills$Cause.Subcategory)

attach(spills)

### DATA DESCRIPTION ### - Investigating the distributions of the different variables

#####Final Project Plots - VISUALIZATIONS OF DEPENDENT VARIABLE, AND NUMERIC/BINARY/CATEGORICAL INDEPENDENT VARIABLES####
attach(spills)
library(reshape2)
library(ggplot2)

##### PLOTS FOR DEPENDENT VARIABLES
# Log of environmental remediation costs per accident (in USD)
summary(Environmental.Remediation.Costs)
boxplot(Environmental.Remediation.Costs, col = 'forestgreen')
hist(x = log(Environmental.Remediation.Costs), breaks = 40, col ='forestgreen',
     main = 'Appendix 6.3: Environmental Costs',
     xlab = 'Log of Environmental Costs',
     ylab = 'Number of Accidents')

library(moments) 
skewness(Environmental.Remediation.Costs,na.rm = TRUE)

##### PLOTS FOR NUMERIC INDEPENDENT VARIABLES
# Year of each accident
hist(x = Accident.Year, breaks = 40, col ='forestgreen',
     main = 'Figure 1: Accidents Per Year (2010-2017)',
     xlab = 'Accident Year',
     ylab = 'Number of Accidents',
     ylim = c(0,500))
# Amount of oil unintentionally released per accident (in barrels)
hist(x = Unintentional.Release.Barrels, breaks = 40, col ='forestgreen',
     main = 'Oil Unintentionally Released',
     xlab = 'Number of Barrels Released Unitentionally',
     ylab = 'Number of Accidents')
# Amount of oil intentionally released per accident (in barrels)
hist(x = Intentional.Release.Barrels, breaks = 40, col ='forestgreen',
     main = 'Oil Intentionally Released',
     xlab = 'Number of Barrels Released Intentionally',
     ylab = 'Number of Accidents')
# Amount of oil recovered per accident (in barrels)
hist(x = Liquid.Recovery.Barrels, breaks = 40, col ='forestgreen',
     main = 'Oil Recovered',
     xlab = 'Number of Barrels Recovered',
     ylab = 'Number of Accidents')

# Log of amount of oil spilt per accident (in barrels)
hist(x = (log(Net.Loss.Barrels)), breaks = 40, col ='forestgreen',
     main = 'Total Barrels of Oil Lost Per Accident (Log of Barrels)',
     xlab = 'Log of Total Barrels Lost',
     ylab = 'Number of Accidents')

##### PLOTS FOR CATEGORICAL INDEPENDENT VARIABLES
# Accident State
# All states
x = table(Accident.State)
barplot(x[order(x, decreasing = TRUE)], col = 'forestgreen')

TopStates = sort(table(Accident.State), decreasing = TRUE)[1:15]
# Top states
barplot(TopStates, col = 'forestgreen',
        main = 'Appendix 6.2 Top 15 States for Pipeline Accidents',
        xlab = 'State',
        ylab = 'Number of Accidents',
        grid(nx = NA, ny = NULL, col = "black"),
        ylim = c(0,1200))

# Top causes
TopCauses = sort(table(Cause.Category), decreasing = TRUE)[1:3]
barplot(TopCauses, col = 'forestgreen',
        main = 'Top 3 Primary Causes of Pipeline Accidents',
        xlab = 'Cause',
        ylab = 'Number of Accidents')
# Top secondard causes
TopSubCauses = sort(table(Cause.Subcategory), decreasing = TRUE)[1:3]
barplot(TopSubCauses, col = 'forestgreen',
        main = 'Top 3 Secondary Causes of Pipeline Accidents',
        xlab = 'Cause',
        ylab = 'Number of Accidents')

# Top operators
TopOperators = sort(table(Operator.Name), decreasing = TRUE)[1:3]
barplot(TopOperators, col = 'forestgreen',
        main = 'Top 3 Operators for Pipeline Accidents',
        xlab = 'Operator',
        ylab = 'Number of Accidents',
        ylim = c(0,200))
# Calculate percentage of accident for top 3 operators
((195+179+156)/2777)*100

## Accident Location Map
# Import initial dataset again that includes every accident. 
spills = read.csv('spills.csv')
attach(spills)

spills_location = spills[,c(17,16)]
attach(spills_location)
# Remove acciddents that are so far offshore of continental U.S. that they make the map too small to read if they are included
spills_location = spills_location[!(spills_location$Accident.Longitude>=-50),]
spills_location = spills_location[!(spills_location$Accident.Latitude<=20),]

#install.packages('usmap')
#install.packages("rgdal")
library(rgdal)
library(usmap)
library(ggplot2)
# Transform latitude and longitude points for plotting
spills_location_transformed <- usmap_transform(spills_location)
# Create plot on U.S. map
map = plot_usmap("states", labels = TRUE) +
    geom_point(data = spills_location_transformed, aes(x = Accident.Longitude.1, y = Accident.Latitude.1),
               color = "red", alpha = 0.1) +
    labs(title = "Figure 2: US Pipeline Accidents",
         subtitle = "Source: Dept. of Transport, 2010-2017",
         size = "Magnitude") +
    theme(legend.position = "left") + 
    theme(plot.title = element_text(size=20))
map
##### PLOTS FOR BINARY INDEPENDENT VARIABLES
library(lessR)

# Location
location = data.frame(var = Pipeline.Location.Onshore)
PieChart(var, hole = 0, values = "%", data = location,
         fill = c("grey", "red"), main = "Location of Pipelines (onshore or offshore)")
legend("topleft", legend = c("Offshore", "Onshore"),
       fill =  c("grey", "red"))

# Was the pipeline shutdown?
shutdown = data.frame(var = Pipeline.Shutdown.Yes)
PieChart(var, hole = 0, values = "%", data = shutdown,
         fill = c("grey", "red"), main = "Was the Pipeline shutdown?")
legend("topleft", legend = c("No", "Yes"),
       fill =  c("grey", "red"))

# Was Pipeline above or below ground?
ground = data.frame(var = Pipeline.Type.Above)
PieChart(var, hole = 0, values = "%", data = ground,
         fill = c("grey", "red"), main = "Location of Pipelines (above or below ground)")
legend("topleft", legend = c("Below", "Above"),
       fill =  c("grey", "red"))

# Was the pipeline carrying crude oil?
crude = data.frame(var = Liquid.Type.Crude)
PieChart(var, hole = 0, values = "%", data = crude,
         fill = c("grey", "red"), main = "Was the Pipeline Carrying Crude Oil?")
legend("topleft", legend = c("No", "Yes"),
       fill =  c("grey", "red"))

# Did the oil ignite?
ignite = data.frame(var = Liquid.Ignition.Yes)
PieChart(var, hole = 0, values = "%", data = ignite,
         fill = c("grey", "red"), main = "Did the Spilled Oil Ignite?")
legend("topleft", legend = c("No", "Yes"),
       fill =  c("grey", "red"))

# Did the oil explode?
explode = data.frame(var = Liquid.Explosion)
PieChart(var, hole = 0, values = "%", data = explode,
         fill = c("grey", "red"), main = "Did the Spilled Oil Explode?")
legend("topleft", legend = c("No", "Yes"),
       fill =  c("grey", "red"))


# The above visualizations revealed a problem: Environmental.Remediation.Costs has a VERY high positive skewness value, thus my dependent varibable is very positively (right) skewed. This means the majority 
# of accidents have a small cost but there are some accidents that can be extremely expensive.

# Solution: Classify accidents as minor or severe depending on if their environmental costs are below or above $364,075 (mean evironmental cost)
# Create column with value of 1 if environmental costs are severe and 0 if minor.

#First, replace any missing numerical values with 0 in the Environmental.Remediation.Costs column.
spills$Environmental.Remediation.Costs[is.na(spills$Environmental.Remediation.Costs)] = 0

#Second: Create new column Severe.Environmental.Costs with value of 1 if environmental costs are severe and 0 if minor.
spills$Severe.Environmental.Costs = ifelse(spills$Environmental.Remediation.Costs >= 364075, 1, 0)
spills = subset(spills, select = -c(Environmental.Remediation.Costs))

attach(spills)

## Investigating relationships between variables

library(reshape2)
library(ggplot2)
require(lmtest)
require(plm)
library(car)
# Examine the correlation amongst the variables

# Create a dataframe with only the quantitative (numeric and binary) independent variables
spills1 = subset(spills, select = c(Accident.Year,
                                    Unintentional.Release.Barrels, 
                                    Intentional.Release.Barrels, 
                                    Liquid.Recovery.Barrels, 
                                    Liquid.Ignition.Yes,
                                    Liquid.Explosion.Yes,
                                    Pipeline.Shutdown.Yes,
                                    Pipeline.Location.Onshore,
                                    Pipeline.Type.Above,
                                    Liquid.Type.Crude))

# Create Pearson correlation matrix
corr_matrix_pearson = cor(spills1, method = c("pearson"), use = "complete.obs")	

# Create correlation a heat map for correlation matrix
melted_corr_matrix_pearson <- melt(corr_matrix_pearson)
ggplot(data = melted_corr_matrix_pearson, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +  ggtitle('Appendix 6.7: Pearson Correlation Heat Map') + xlab("Numeric and Binary Independent Variables") + ylab("Numeric and Binary Independent Variables")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# no two independent variables are highly correlated. All correlation under 0.5.

# Check for collinearity among variables - run VIF test on a logistic regression with the response variable Severe.Environmental.Costs,
# and the numberic and binary variables in the spills dataframe as predictors.
logit_noout=glm(Severe.Environmental.Costs~
                   Accident.Year+
                   Unintentional.Release.Barrels+
                   Intentional.Release.Barrels+
                   Liquid.Recovery.Barrels+
                   Liquid.Ignition.Yes+
                   Liquid.Explosion.Yes+
                   Pipeline.Shutdown.Yes+
                   Pipeline.Location.Onshore+
                   Pipeline.Type.Above+
                   Liquid.Type.Crude,data=spills)
summary(logit_noout)

vif(logit_noout)
# Do not seem to have collinearity present :)

# Check for outliers - run bonferroni test on logistic regression with the response variable Severe.Environmental.Costs,
# and the numberic and binary variables in the spills dataframe as predictors.
#run bonferroni test
outlierTest(logit)

# Remove the outliers
spills_final_NoOutliers=spills_final[-c(185,246,21,533,2458,1035,1847,1313,721,457),]
attach(spills_final_NoOutliers)

# The results of the random forest model and the boosted forest model below were not significantly different when run useing
# the spills_final_NoOutliers dataset, instead of the spills dataset. There are also not that may observtions in the spills 
# dataset to begin with. Therefore, the decision was made to run the remainder of the models in this script with the complete spills dataset, which
# will be renamed as spills_final in the next step.

# Create a final dataset to be used for modelling:
spills_final = spills

##### MODELLING #####

#PCA to analyse the relative importance of predictors:
library(ggplot2)
library(GGally)

attach(spills_final)

# create df with only numeric variables
spills_vars=spills_final[,-c(2:5)]

#Conduct PCA
pca=prcomp(na.omit(spills_vars), scale=TRUE)

pca

# plotting the first and second PCAs
library(ggfortify)

autoplot(pca, data = spills_vars, loadings = TRUE, loadings.label = TRUE, col = 'lightgreen', main = "Figure 4: Principal Component Analysis")

## PCA Findings: 
# - Accident.Year and Pipeline.Location.Onshore and Pipeline.Type.Above look negatively correlated with environmental costs
# - Liquid.Recovery.Barrels and Pipeline.Shutdown.Yes look correlated with environmental costs
# - Liquid explosion and liquid ignition are fairly orthogonal with environmental costs, may not be very useful predictors

## To determine optimal number of components, we creaate a percentage-of-variance-explained plot

pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1), main = "PVE Per Component",
     xlab = 'Principal Component',
     ylab = 'Prop. Varience Explained')
plot(cumsum(pve), ylim=c(0,1), main = "Cumulative PVE",
     xlab = 'Principal Component',
     ylab = 'Cumulative Prop. Varience Explained')

#print pve
pve
#calculate variance explained by first two components
0.16904127+0.13121841

# Using a Random Forest to indentify predictors
attach(spills_final)
library(randomForest)

myforest=randomForest(Severe.Environmental.Costs~
                          Accident.Year+
                          #Operator.Name+
                          Accident.State+
                          Cause.Category+
                          Cause.Subcategory+
                          Unintentional.Release.Barrels+
                          Intentional.Release.Barrels+
                          Liquid.Recovery.Barrels+
                          Liquid.Ignition.Yes+
                          Liquid.Explosion.Yes+
                          Pipeline.Shutdown.Yes+
                          Pipeline.Location.Onshore+
                          Pipeline.Type.Above+
                          Liquid.Type.Crude, ntree=500, data=spills_final, importance=TRUE, do.trace =50)

# Operator name has too many categories (more than 53), therefore we cannot include it in the Random Forest. See footnote 5
# in the report of more information on this.

myforest

# Note that the do.trace parameter indicates that the decrease in MSE from 350 trees to 500 trees is negligable. Therefore, if 
# computational power or time were a concern this random forest model could instead be run with 350 trees.

importance(myforest)
varImpPlot(myforest, main = 'Appendix 6.9: Variable Importance')

# FINAL MODEL => Run boosted forest to predict whether the accident will be severe or minor

library(gbm)
# Set random seed so that results are reproducible
set.seed (1)

boosted=gbm(Severe.Environmental.Costs~
                Operator.Name+
                Accident.State+
                Cause.Subcategory+
                Unintentional.Release.Barrels+
                Liquid.Recovery.Barrels+
                Pipeline.Shutdown.Yes+
                Pipeline.Type.Above+
                Liquid.Type.Crude,
            data=spills_final,distribution="bernoulli",n.trees=10000, interaction.depth=6)

summary(boosted)

# Test the model 100 times with a random sample of 15% of the accidents each time, then take the averge of the 100 errors calculated
# in the for loop.
library(dplyr)

for (i in 1:100) {
    set.seed(2)
    test = spills_final %>% sample_frac(.15)
    predicted_score=ifelse(predict(boosted, newdata=test, n.trees=10000, type="response") >=0.5, 1, 0)
    error[i]=mean(predicted_score != Severe.Environmental.Costs)
}
Avg_error = mean(error)
Avg_error

##### UNSUPERVISED MODELLING
##### k-means Clustering environmental costs and size of the spill in gallons
# Re-import the dataset so the Net Loss is included again and fix the formatting of Net Loss column name
spills = read.csv('spills.csv')
attach(spills)
names(spills)[names(spills) == 'Net.Loss..Barrels.'] = 'Net.Loss.Barrels'

# Make datafream with net loss of oil and environmental costs
spills_vars_c4=spills[,c(23,46)]

# Convert barrells to gallons of oil (42 gallons in a barrel) to improve interpretabilty of plot
spills_vars_c4$Net.Loss.Barrels=42*(spills_vars_c4$Net.Loss.Barrels)
# Remove any missing values
spills_vars_c4 = na.omit(spills_vars_c4)
attach(spills_vars_c4)

# Remove 0 values
spills_vars_c4<-spills_vars_c4[!(spills_vars_c4$Environmental.Remediation.Costs==0),]
spills_vars_c4<-spills_vars_c4[!(spills_vars_c4$Net.Loss.Barrels==0),]

# Take the log of both variables, which are positively (right) skewed
spills_vars_c4$logEnvironmental.Remediation.Costs=log(spills_vars_c4$Environmental.Remediation.Costs)
spills_vars_c4$logNet.Loss.Barrels=log(spills_vars_c4$Net.Loss.Barrels)

# Plot the data
library(ggplot2)
plot=ggplot(spills_vars_c4,aes(x=logNet.Loss.Barrels, y=logEnvironmental.Remediation.Costs))
plot+geom_point()

# Make cluster data by applying the following command:
km.2=kmeans(spills_vars_c4, 2) #2 clusters
km.3=kmeans(spills_vars_c4, 3) #3 clusters
km.4=kmeans(spills_vars_c4, 4) #4 clusters
km.5=kmeans(spills_vars_c4, 5) #5 clusters

# Check within cluster variation
km.2$tot.withinss
km.3$tot.withinss
km.4$tot.withinss
km.5$tot.withinss

# Find the optimal number of clusters
#install.packages('factoextra')
library(factoextra)

# Create sillhouette score plot. 
Figure3.1 = fviz_nbclust(spills_vars_c4, kmeans, method = "silhouette")+
    labs(subtitle = "Silhouette method")

# Plot results for 2 clusters
spills_vars_c4$cluster=as.factor(km.2$cluster)
attach(spills_vars_c4)
Figure3.2 = plot+geom_point(aes(colour=cluster))+xlab("Log of Oil Spill Per Accident (gallons)") + ylab('Log of Evironmental Costs ($)')

library(ggpubr)
# plot Figure 3.1 and 3.2 in the same window
ggarrange(Figure3.1,Figure3.2, labels = c("3.1", "3.2"), ncol = 2, nrow = 1)

# Create dataframe that specifies which accidents ended up in which cluster
clusters4=data.frame(rownames(spills_vars_c4),km.5$cluster)

# Count number of accidents in each cluster
table(clusters4$km.5.cluster)

### END OF CODE FILE ###


