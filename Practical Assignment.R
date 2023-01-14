# Assignment 2
#Libraries needed:
library(ggplot2)
library(dplyr)
library(corrplot)
library(lmtest)
library(car)
library(tsoutliers)
library(scales)
library(NbClust)
library(purrr)
library(Hmisc)

# Part 1
#Clear all if needed
rm(list=ls())



#Read the data
data = read.csv('dataArrests.csv',header=TRUE,sep=';')

#We find out that the data contains nan values, lets omit those rows
any(is.na(data)) 

#New data matrix with the nan values omitted
data2 = na.omit(data)

#No nan values found
any(is.na(data2))

#Let's check min, max, standard deviation and mean values for the data, and write a table file of them for the document
datatemp = data2[c(1,2,3,5,10)]
expldata = rbind(apply(datatemp,2,min), apply(datatemp,2,max), apply(datatemp,2,sd), apply(datatemp,2,mean))
expldata = round(expldata, digits = 2)
rownames(expldata) = c('Min','Max','STD','Mean')
write.table(expldata,'regression_expl.txt',sep="\t") #This creates a text file that we can use to import tables in Excel for example
#We see that CarAccidents has negative minimum, so let us check if the data has any other negative values since
# they might be faulty
sum(data2 < 0)
sum(data2$CarAccidents < 0)
#There seemed to be 2 negative values on the whole data, and both of them come from the CarAccidents data

#Plots for the explanatory variables Assault,UrbanPop,Traffic,CarAccidents
#2

par(mfrow=c(2,2))
plot(data2$Assault,data2$Murder,xlab='Assault',ylab = 'Murder')
plot(data2$UrbanPop,data2$Murder,xlab='Urban Population',ylab = 'Murder')
plot(data2$Traffic,data2$Murder,xlab='Traffic',ylab = 'Murder')
plot(data2$CarAccidents,data2$Murder,xlab='Car Accidents',ylab = 'Murder')
par(mfrow=c(1,1))

#The plot when you explain Murder variable with the Assault variable might have
#some correlation, but the rest of them seems to have large deviation between the values

#Box plots for the explanatory variables and Murder
par(mfrow=c(2,3))
boxplot(data2$Murder,main='Murder Boxplot')
boxplot(data2$Assault,main='Assault Boxplot')
boxplot(data2$UrbanPop,main='UrbanPop Boxplot')
boxplot(data2$Traffic,main='Traffic Boxplot')
boxplot(data2$CarAccidents,main='CarAccidents Boxplot')


#3
#Correlation matrix
cormat = abs(cor(data2))
cormatround = round(cormat,digits=3)
#When we check all of the values with correlation (pos or neg) higher than 0.5 and not 1 we get the following
abs(cormat)>0.5 & cormat != 1
#And we see that Murder only has correlation with Assault which is higher than 10, Assault has Murder and Drug, Traffic has CarAccidents and vice versa

abs(cormat[1,])
#When we check what absolute correlations murder has with the other variables, we see that it has 3 highest correlations with
# Assault (0.637), Drug (0.392) and UrbanPop (0.118)

#4

corrplot(cor(data2),method="number",addCoef.col = 'black',cl.pos='n',col='black')
# We see that this results are following the same pattern that we noticed on the 
# correlation matrix (which it is supposed to as well): Traffic and CarAccidents have high correlation, Drugs have correlation with murdre,assault and urbanpop, and assault has correlation with Murder, UrbanPop and Drugs

#Let's see which variables has over 0.8 correlation between themselves
abs(cormat)>0.8 & cormat != 1
#We see from the previous results that the explanatory variables Traffic and CarAccidents have over 0.8 correlation
# among themselves, which would cause multicollinearity for the model so 
# lets remove the one which has highest average correlation with all of the explanatory variables.
# They also do not have high correlation with the Murder variable. One of the 
# requirements for linear regression is that the model must not have multicollinearity because
# it will cause distortions for the model
mean(cormat[2:10,5])
mean(cormat[2:10,10])
cor(data2$CarAccidents,data2)

data3 = data2 %>% select(-c('CarAccidents'))

#6
linRegModel = lm(formula = Murder~Assault+UrbanPop+Drug+Cyber+Alcohol+Kidnapping+Domestic+Traffic,data=data3)
summary(linRegModel)

#7
# Our R-squared value does not seem to be very good (R2 = 0.4085), so we can consider removing some of the variables
# Kidnapping seems to have the largest p-value, so lets remove that first from the Model
linRegModel = lm(formula = Murder~Assault+UrbanPop+Drug+Cyber+Alcohol+Domestic+Traffic,data=data3)
summary(linRegModel)
#Now the Alcohol variable has the largest p-value so let's remove that
linRegModel = lm(formula = Murder~Assault+UrbanPop+Drug+Cyber+Domestic+Traffic,data=data3)
summary(linRegModel)
#Now the Domestic variable has the largest p-value so let's remove that
linRegModel = lm(formula = Murder~Assault+UrbanPop+Drug+Cyber+Traffic,data=data3)
summary(linRegModel)
#Now the Drug variable has the largest p-value so let's remove that
linRegModel = lm(formula = Murder~Assault+UrbanPop+Cyber+Traffic,data=data3)
summary(linRegModel)
#Now the Traffic variable has the largest p-value so let's remove that
linRegModel = lm(formula = Murder~Assault+UrbanPop+Cyber,data=data3)
summary(linRegModel)
#Now the Cyber variable has the largest p-value so let's remove that
linRegModel = lm(formula = Murder~Assault+UrbanPop,data=data3)
summary(linRegModel)
#Still the UrbanPop variable seems to not be significant even on the 0.1 significance level, so lets remove that
linRegModel = lm(formula = Murder~Assault,data=data3)
summary(linRegModel)

#8
# Our final model to explain variable Murder is y(X) = 1.569684+0.044784*x, where y is Murder and x is Assault
fit = fitted.values(linRegModel)
#Plot of our data and our  fit
plot(data3$Assault,data3$Murder,main = 'Linear regression of Murder with Assault as explanatory variable',xlab = 'Assault',ylab = 'Murder')
lines(data3$Assault,fit,'col' = 'red')
legend(50,30,legend=c('Data','Fitted line'),col=c("black", "red"), lty=1, cex=0.8)

#9
#1. OLS requires that the residuals of the fit has zero mean
resmean = mean(residuals(linRegModel))
resmean
#Which in this case is very close to zero -> we can assume that the mean of the residuals is zero

#2. OLS requires that the variance of the residuals is constant and finite -> Homoskedastic
#We can use Breusch-Pagan test for heteroskedasticity in this case
bptest(linRegModel)
#p-value is significantly lower than 0.05, so we accept the null hypothesis meaning that we have sufficient
# evidence to say that heteroscedasticity is present in the regression model.
#This can also be verified on the next plot where you can see that on higher values the residuals are more spread out
plot(fit,residuals(linRegModel))

#3. OLS requires that the residuals are not autocorrelated, meaning that they are linearly independent of one another
# we can use the Durbin-Watson test for this
durbinWatsonTest(linRegModel)
#Since our p-value is significantly higher than 0.05 (or even 0.1) and our test-statistic is very close to 2, we can conclude that there is enough evidence to accept the null hypothesis
# and that residuals in this regression model are not autocorrelated.

#4. There also can be no relationship between the residuals and each of the explanatory variables
cor(residuals(linRegModel),data3$Assault)
# The correlation is very close to 0 -> No correlation

#5. The last requirement is that the residuals must be normally distributed
# We can use the Jarque-Bera test to test this
JarqueBera.test(residuals(linRegModel))
# Our Null hypothesis here is that skewness = zero, and excess kurtosis = 0.
#However weseem to get high values for the test statistics which are evidence against the null hypothesis.
# Our p-values are also a lot higher than 0.05, which means that we have enough evidence to reject our null hypothesis
# and that the residuals are not normally distributed


#############################################################################

## Part 2
#Empty the data
rm(list=ls())

#1
data = read.csv('wholesale.csv',header=TRUE,sep=',')
str(data)
#The variables Channel and Region seems to be categorical data, meaning that it has only few types of values (categories)
#Let's see what those values are
unique(data$Channel) # categories 2 and 1
unique(data$Region) # categories 1, 2 and 3
#Rest of the variables seems to be annual spending variables with integer values, with differing values
#Let's check for any missing values
any(is.na(data))
#No missing values

#Let's check whats the max,min, mean and standard deviations values for all of the variables
expldata = rbind(apply(data,2,min), apply(data,2,max), apply(data,2,sd), apply(data,2,mean))
expldata = round(expldata, digits = 2)
rownames(expldata) = c('Min','Max','STD','Mean')
write.table(expldata,'part2_expl.txt',sep="\t") #This creates a text file that we can use to import tables in Excel for example
#The min and max values seem to differ alot from each other on the non-categorical variables
#The Fresh variable seems to have highest standard deviation

#Let's do boxplots to check out for the non-categorical variables symmetricity and for any outliers
par(mfrow=c(1,1))
boxplot(data$Fresh,data$Milk,data$Grocery,data$Frozen,data$Detergents_Paper,data$Delicassen,main='Boxplots for non-categorical variables',names=c('Fresh','Milk','Grocery','Frozen','Detergents Paper','Delicassen'))
boxplot(data$Frozen[data$Region==1])

par(mfrow=c(1,2))
barplot(table(data$Channel),main='Channel')
barplot(table(data$Region),main='Region')


#Let's plot our data
plot(data,main = 'All of the data')


#Let's see how the variables correlate between themselves
cormat = cor(data)
#All of the variables with higher than 0.5 correlation
abs(cormat)>0.5 & abs(cormat) != 1
#Channel variable seems to have correlation with Grocery and Detergents_Paper,
# Milk seems to be correlated with Grocery and Detergents_Paper and vice versa for both
#Visualization of the correlation matrix
corrplot(cormat,'number')
#Since Channel and Region are categorical variables, we must are they useful for us when we do clustering,
#meaning that we can consider leaving them completely out.
#Most notable mentions here is Milk's correlation with Grocery (0.73)  and Frozen's correlation with Detergents_Paper (0.92)

#Let's normalize our data using min max normalization
norm_data = apply(data,2,rescale, to=c(0,1))
# Elbow method
tot_within_ss = map_dbl(1:10, function(k) {
  model = kmeans(norm_data,centers=k, nstart = 25)
  model$tot.withinss
})

#From the plot we can say that the amount of cluster is either 3 or 4. (Let's see the results of other methods)


# Other Methods
silClust = NbClust(norm_data, distance='euclidean', min.nc = 2, max.nc = 10, method = 'kmeans', index= 'silhouette')

gapClust = NbClust(norm_data, distance='euclidean', min.nc = 2, max.nc = 10, method = 'kmeans', index= 'gap')

CHClust = NbClust(norm_data, distance='euclidean', min.nc = 2, max.nc = 10, method = 'kmeans', index= 'ch')


# Let's plot these methods
par(mfrow = c(2,2))
plot(1:10, tot_within_ss, type='o', xlab='Number of clusters', ylab='Total WSS', main='Elbow Method', panel.first = grid())

plot(2:10, silClust$All.index, type='o',xlab = ' Number of Clusters',
     ylab = 'Silhouette Score', panel.first = grid(), main='Silhouette Score')
plot(2:10, gapClust$All.index, type='o',xlab = ' Number of Clusters',
     ylab = 'Gap Statistic', panel.first = grid(),main='Gap Statistic')
plot(2:10, CHClust$All.index, type='o',xlab = ' Number of Clusters',
     ylab = 'Calinksi Harabasz Index', panel.first = grid(), main='Calinksi Harabasz Index')
par(mfrow = c(1,1))
#Here we need to find the highest Gap Statistic, highest Silhouhette Score and highest Calanski Harabasz score
#We can see that the Highest Silhouette Score is at 3, highest Gap Statistic is at 2 but 3 is very close behing and the highest 
# Calinski Harabasz score is at 9, but at lower amount of cluster 3 cluster has the highest score (under 6 clusters).
#Overall from all of the different methods it seems that 3 clusters is the most probable one to consider using.


#Let's run the k-means algorithm to our un-normalized data since the data sizes
# on the non-categorical variables seems to be around the same size, and we do not
# want to normalize our categorical varibales. Also it seems to give better results visually than
# if you run the model on the normalized data.
kmeansmdl = kmeans(data,centers = 3, nstart = 25)
par(mfrow = c(1,1))

#Lets plot how all of the different combinations of the data would look like
plot(data,col=kmeansmdl$cluster,pch=16, main = 'The whole Data in 3 clusters')

##Let's add cluster classes(memberships) to the data
datanew = data %>% mutate(member = factor(kmeansmdl$cluster))

#Let us investigate how the means and standard deviations differ between the members
datagroupmean = datanew %>% 
  group_by(member) %>%
  summarise_all(list(avg = mean))
datagroupstd = datanew %>% 
  group_by(member) %>%
  summarise_all(list(Std = sd))
#We see that usually 2 of the groups have similar sizes of the STD and mean, but there is always one
#one member which has a lot higher value.

# Let us plot the clustered data with Channel and Region as the variables and Fresh and Grocery as variables
# For closer inspection
ggplot(datanew, aes(x=Channel, y = Region, col=member))+
  geom_point() + 
  ggtitle('Clusters in the data set')

# Plot of the Fresh and Grocery variables with colored clusters
ggplot(datanew, aes(x=Fresh, y = Grocery, col=member))+
  geom_point() + 
  ggtitle('Clusters in the data set')

# Boxplot of fresh variable divided in its clusters
ggplot(datanew, aes(x = member, y = Fresh, fill=member))+
  geom_boxplot()+ggtitle('Distribution of Fresh by Cluster')+
  xlab('Cluster')+
  ylab('Fresh values')

    