# 7.1 Mini project
library(dplyr)
library(ggplot2)

states.data <- readRDS("states.rds")

glimpse(states.data)

#get labels

#I am not very familiar with the following command or what exactly it did and why
#where did the var.labels come from? It has descriptions for all of the variables
#and I don't know where they came from
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

# I like glimpse personally 
glimpse(states.info)

states.info

#Finding out how many NA values there are. There are 14
sum(is.na(states.data))

#Looking for any obvious outliers, Metro has a Max of 100 and 1 NA which seems odd,
#Does New Jersey count as 100% metropolitan?  I don't know but for now I am 
#going to continue coding as if that was meant to be there.
#Some of the numbers is House and Senate seem odd.  0% voting for a couple states

summary(states.data)

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

plot(sts.ex.sat)


summary(lm(csat ~ expense + percent, data = states.data))

sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

#Exercise answers

#contructing a very basic plot, seems to be little to no correlation on initial viewing
ggplot(states.data, aes(x = metro, y = energy)) + geom_point()

#creating a linear model to test earlier hypothesis.  
#Adjusted R-squared is .097, hypothesis confirmed
model1 <- lm(metro~ energy, data = states.data)
summary(model1)
#Placed the model on a graph 
plot(model1)

typeof(states.data)

#I was able to do this in the wine data set but I cannot here. I am guessing
#it has to do with the NA values in there. I dont know any quick ways to check for 
#correlations besides just typing in the variables one by one into a linear model 

#cor(as.numeric(states.data2))
model2 <- lm(metro~ energy +  waste + miles + toxic + green+
             density, data = states.data)
summary(model2)
# Only 3 variables considered significant, waste, miles, density
#but adjusted r-squared is now .5013, so thats better 

model3 <- lm(metro~   waste + miles + 
               density, data = states.data)
summary(model3)
#The adjusted r squared is lower for this one at .4982. I have no idea 
#why this happened. 

#So type and class are different things and I am not sure why and I do not
#know the subtleties of how it makes a difference in coding. I am sure it does
#but I don't know what the effect will be
typeof(states.data$region)
class(states.data$region)

states.data$region <- factor(states.data$region)


typeof(states.data$region)
class(states.data$region)

sat.region <- lm(csat ~ region,
                 data=states.data) 

coef(summary(sat.region)) 
anova(sat.region)
summary(sat.region)

#So region seems to be significant at least judging from the previous linear
#model. 

#I made the adjusted r squared increase slightly to .5099 but I dont know
#why the different regions are listed on different rows.  I am assuming
#it is due to the C function and contrasts but I don't know what those are
#or what they do. I looked at some of the documentation but I am having 
#difficulty making heads or tail of it. 
model4 <- lm(metro~   waste + miles + 
               density + region, data = states.data)
summary(model4)