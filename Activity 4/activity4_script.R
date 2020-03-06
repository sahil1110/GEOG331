#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)
library(gridExtra)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

x<- c("Sepal.Length", "Petal.Length", "Sepal.Length")
y<- c("Sepal.Width", "Petal.Width", "Petal.Length")
lm.out<-list()

for(i in 1:3){
  lm.out[[i]]<- lm(versicolor[, paste(y[i])]~ versicolor[, paste(x[i])])
}

# Alternative way:
#lm(Sepal.Width~Sepal.Length, data= versicolor)

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

# iris left
# height right

iris2<- left_join(iris, height, by= "Species")

#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot

p1<- ggplot(data= iris, aes(x=Sepal.Length, y=Sepal.Width,
                            color=Species))+
  geom_point(size= 4)+
  theme_classic()

p2<- ggplot(data= iris, aes(x=Sepal.Length, y=Sepal.Width))+
  geom_jitter()+
  theme_classic()

grid.arrange(p1, p2, ncol=2)

#3b. make a scatter plot with ggplot and get rid of  busy grid lines


#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################	