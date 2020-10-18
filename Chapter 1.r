# install.packages("dplyr")
library('dplyr')
library('ISLR')
library('MASS')


# plot scatter of 2 rnorm
x = rnorm(50,0,1)
y = rnorm(50,0,1)
plot(x,y)

# show the distribution of rnorm
x = rnorm(50000,0,1)
hist(x)

# contour
x = seq(1,10)
y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour (x,y,f,nlevels =45, add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)
plot(Auto$cylinders, Auto$mpg)
hist(Auto$mpg,col = 2)
summary(College)

# Pairwise scatterplots
pairs(x = College[,1:10])
boxplot(College$Outstate ~ College$Private)
View(College)
College1 = dplyr::mutate(College,
                         Elite = as.factor(dplyr::case_when(College$Top10perc > 50 ~ "Yes",
                                                            TRUE ~ "No")))
View(College1)
summary(College1)
par(mfrow = c(1,2))
boxplot(College$Outstate ~ College$Private)
boxplot(College1$Outstate ~ College1$Elite)
View(Auto)
range(Auto$mpg)
min(Auto$mpg)
max(Auto$mpg)
summary(Auto)
str(Auto)
apply(Auto, 2, FUN = sd)

##########################################################################################################################################################

# 8 - using the College Dataset: do th following
# 8a) import the Dataset (csv)
# Dont have the CSV
# 8b) fix the CSV
# no need
# 8ci) use the summary() function to produce a numerical summary of the variables in the dataset
summary(College)
# 8cii) use the pairs() function to produce a scatterplot matrix of
# the first ten columns or variables of the data. Recall that
# you can reference the first ten columns of a matrix A using
# A[,1:10]. 
pairs(College[,1:10])
# 8ciii) Use the plot() function to produce side-by-side boxplots of
# Outstate versus Private.
boxplot(College$Outstate ~ College$Private)
# 8civ) Create a new qualitative variable, called Elite, by binning
# the Top10perc variable. We are going to divide universities
# into two groups based on whether or not the proportion
# of students coming from the top 10 % of their high school
# classes exceeds 50 %
College1 = dplyr::mutate(.data = College,
                         Elite = as.factor(dplyr::case_when(College$Top10perc > 50 ~ "Y",
                                                            TRUE ~ "N")),
                         CollegeName = row.names(College))

# Use the summary() function to see how many elite universities there are. Now use the plot() function to produce
# side-by-side boxplots of Outstate versus Elite.
summary(College1)
# 8cv) Use the hist() function to produce some histograms with
# differing numbers of bins for a few of the quantitative variables. You may find the command par(mfrow=c(2,2)) useful:
# it will divide the print window into four regions so that four
# plots can be made simultaneously. Modifying the arguments
# to this function will divide the screen in other ways.
names(College1)
str(College1)

islrhist <- function(nbin) {
  hist(x = College1$Apps, 
       n = nbin,
       main = paste0("number of bins = ",nbin))
}

par(mfrow = c(2,2))
islrhist(10)
islrhist(20)
islrhist(30)
islrhist(40)

# 8cvi) Continue exploring the data, and provide a brief summary
# of what you discover.
View(College1)
str(College1)
pairs(College1)
par(mfrow = c(1,1))
boxplot(College1$Grad.Rate ~ College1$Elite)


# 9 This exercise involves the Auto dataset studied in the lab. make sure that the missing values have been removed from the data
# 9a) which of the variables are quantitative and which are qualitative?
View(Auto)
str(Auto)
# 9b) what is the range of each quantitative factor?
numeric <- sapply(Auto, class) == "numeric"
Auto1 = Auto[,numeric]
AllRange = apply(Auto1, 2, range)