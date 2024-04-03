#Anylysis project


#setting the path to the directory with the project
lokWD <- c("C:/Users/sebas/OneDrive/Pulpit/ryaner_datasets")
setwd(lokWD)
getwd()


#loading and reviewing data into the project
ryanair <- read.csv("ryanair_reviews.csv", sep = ",")
head(ryanair)


#summary of data
summary(ryanair)


#in the first step we will do cleaning data and prepare data for analysis

#to begin with, we will exclude from the collection the columns comment title and comment 
#which will not help us in the analysis and X which is just an index
ryanair <- ryanair[,-c(1,6,7)]
head(ryanair)
tail(ryanair)
str(ryanair)


#change of data types


#date published - leave the year of publication alone and replace with factor
ryanair$Date.Published <- substr(ryanair$Date.Published, 1, 4)
unique_date_publ <- unique(ryanair$Date.Published)
unique_date_publ
ryanair$Date.Published <- factor(ryanair$Date.Published, levels = unique_date_publ)


#overal rating replacement of blank values by median
ryanair$Overall.Rating[is.na(ryanair$Overall.Rating)] <- median(ryanair$Overall.Rating, na.rm = TRUE)


#pasenger country, we search for unique values and replace with factor
unique_pas_country <- unique(ryanair$Passenger.Country)
unique_pas_country

#remove obserwvation "Steven Bouchere16th September 2013"
ryanair <- subset(ryanair, Passenger.Country != "Steven Bouchere16th September 2013")
unique_pas_country <- unique(ryanair$Passenger.Country)
unique_pas_country

#change for factor
ryanair$Passenger.Country <- factor(ryanair$Passenger.Country, levels = unique_pas_country)


#trip veryfied, get unique values and change to factor
unique_trip_veryfied <- unique(ryanair$Trip_verified)
unique_trip_veryfied

#first, the need to convert to equal names, then categorize into three groups, verified, unverified, other
ryanair$Trip_verified <- ifelse(
  ryanair$Trip_verified %in% c("Trip Verified", "Verified Review"),
  "Trip Verified",
  ifelse(
    ryanair$Trip_verified %in% c("Not Verified", "NotVerified", "Unverified"),
    "Not Verified",
    "Others"
  )
)

unique_trip_veryfied <- unique(ryanair$Trip_verified)
unique_trip_veryfied

#now we can change to factor
ryanair$Trip_verified <- factor(ryanair$Trip_verified, levels = unique_trip_veryfied)


#aircraft, analogous proceedings like teip veryfied
unique_aircraft <- unique(ryanair$Aircraft)
unique_aircraft

#lots of unique values, in this case we will apply generalization and 
#leave only sorting into Boeing and Airbus and others
ryanair$Aircraft <- ifelse(
  grepl("Boeing|737|733|738|Boing", ryanair$Aircraft, ignore.case = TRUE),
  "Boeing",
  ifelse(
    grepl("A319|A-340|A340|A320", ryanair$Aircraft, ignore.case = TRUE),
    "Airbus",
    "Others"
  )
)

unique_aircraft <- unique(ryanair$Aircraft)
unique_aircraft

#now converting to factor
ryanair$Aircraft <- factor(ryanair$Aircraft, levels = unique_aircraft)
summary(ryanair)

#looking at the stylists a large value of "others" may mean that the 
#respondents did not know what plane they were flying on

#Type of traveller
unique_traveller <- unique(ryanair$Type.Of.Traveller)
unique_traveller

#in place of the empty value we will add the value of other
ryanair$Type.Of.Traveller <- ifelse(ryanair$Type.Of.Traveller == "", "Other", ryanair$Type.Of.Traveller)
unique_traveller <- unique(ryanair$Type.Of.Traveller)
unique_traveller

#now we can convert to factor
ryanair$Type.Of.Traveller <- factor(ryanair$Type.Of.Traveller, levels = unique_traveller)
summary(ryanair)

#seat type
unique_seattype <- unique(ryanair$Seat.Type)
unique_seattype

#change to factor
ryanair$Seat.Type <- factor(ryanair$Seat.Type, levels = unique_seattype)
summary(ryanair)


#origin
unique_origin <- unique(ryanair$Origin)
unique_origin

#change to factor
ryanair$Origin <- factor(ryanair$Origin, levels = unique_origin)
summary(ryanair)


#destination
unique_destination <- unique(ryanair$Destination)
unique_destination

#fill in the blanks and missing
ryanair$Destination <- ifelse(is.na(ryanair$Destination), "Others", ryanair$Destination)
ryanair$Destination <- ifelse(ryanair$Destination == "", "Others", ryanair$Destination)
unique_destination <- unique(ryanair$Destination)
unique_destination

#change to factor
ryanair$Destination <- factor(ryanair$Destination, levels = unique_destination)
summary(ryanair)


#date flown
unique_date_flown <- unique(ryanair$Date.Flown)
unique_date_flown

#fill in the blanks as others
ryanair$Date.Flown <- ifelse(ryanair$Date.Flown == "", "Others", ryanair$Date.Flown)
ryanair$Date.Flown <- ifelse(is.na(ryanair$Date.Flown), "Others", ryanair$Date.Flown)
unique_date_flown <- unique(ryanair$Date.Flown)
unique_date_flown

#change to factor
ryanair$Date.Flown <- factor(ryanair$Date.Flown, levels = unique_date_flown)
summary(ryanair)


#seat comfort, cabin staff service, food beverages, ground service, value for money - replacing Na's with median
ryanair$Seat.Comfort[is.na(ryanair$Seat.Comfort)] <- median(ryanair$Seat.Comfort, na.rm = TRUE)
ryanair$Cabin.Staff.Service[is.na(ryanair$Cabin.Staff.Service)] <- median(ryanair$Cabin.Staff.Service, na.rm = TRUE)
ryanair$Food...Beverages[is.na(ryanair$Food...Beverages)] <- median(ryanair$Food...Beverages, na.rm = TRUE)
ryanair$Ground.Service[is.na(ryanair$Ground.Service)] <- median(ryanair$Ground.Service, na.rm = TRUE)
ryanair$Value.For.Money[is.na(ryanair$Value.For.Money)] <- median(ryanair$Value.For.Money, na.rm = TRUE)

summary(ryanair)


#variable recomended first look at unique values
unique_recom <- unique(ryanair$Recommended)
unique_recom

#convert to factor
ryanair$Recommended <- factor(ryanair$Recommended, levels = unique_recom)
summary(ryanair)


#variables inflight entartainment and wifi connectivity are excluded from analysis due to large number of blanks
ncol <- ncol(ryanair)
ryanair <- ryanair[,-c(ncol - 1, ncol)]
summary(ryanair)


###at this stage the data is prepared for analysis

#first, let's analyze the overall airline rating for the entire collection to illustrate its descriptive statistics
#we will use the boxplot chart along with adding additional information for it
library(ggplot2)
ggplot(data = ryanair, aes(y = Overall.Rating))+
  geom_boxplot(fill = "lightgreen", color = "black", outlier.color = "red") + # Add fill color and border color
  scale_y_continuous(limits = c(1, 10), n.breaks = 10) +  #setting the y-axis range and axis spacing
  geom_point(aes(x = 0, y = round(mean(Overall.Rating),2), color = "darkred")) + #average value point
  geom_text(aes(x = 0, y = round(mean(Overall.Rating),2) + 0.25, label = paste("mean = ", round(mean(Overall.Rating),2)), color = "darkred"))+ #description of the average value
  labs(x = NULL, y = "Overall Rating", title = "Boxplot of Overall Rating") + #descriptive
  scale_color_manual(values = c("darkred" = "darkred"), guide = FALSE)+ #Setting the color of the dots
  theme_minimal() + theme(axis.text.x = element_blank()) #style plot

ggplot(data = ryanair, aes(x = Overall.Rating))+
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black")+ #histogram geometry and fill
  scale_x_continuous(limits = c(0, 11), n.breaks = 11)+ #change scale for x axis
  scale_y_continuous(limits = c(0, 900), n.breaks = 9)+ #change scale for y axis
  labs(x = "Rating", y = "Frequency of Rating", title = "Histogram of overal ratings")+ #descript...
  theme_minimal() #style of plot

#range takes values from 1 to 10 i.e. values occur throughout the set
#The median reaches 3 and is lower than the average which is 4.3
#there is right-sided asymmetry

#In the next step, we want to examine how overall rating depends on the
#variables(seat comfort, cabin staff service, food beverages ground service, value for money)

#first we will calculate the spearman correlation matrix for our data
cormatrix <- cor(ryanair[,c('Overall.Rating', 'Seat.Comfort', 'Cabin.Staff.Service', 'Food...Beverages', 'Ground.Service', 'Value.For.Money')], method = "spearman")
cormatrix
colnames(cormatrix) <- c("overall rating", "seat comfort", "cabin staff service", "food, beverages", "ground service", "value for money")
rownames(cormatrix) <- c("overall rating", "seat comfort", "cabin staff service", "food, beverages", "ground service", "value for money")

#now we can move on to creating the chart
library(corrplot)
library(wesanderson)
corrplot.mixed(cormatrix, lower = "number", upper = "circle", tl.pos="lt", diag = "l",
               tl.cex = 0.9, tl.col = "black", tl.offset = 1.3, lower.col = wes_palette("Zissou1", 10, type = "continuous"),
               upper.col = wes_palette("Zissou1", 10, type = "continuous"),
               tl.srt = 45, mar = c(1,1,5,1), title = "The correlation plot \n between overall rating and the level of flight service",
               cex.main=1.1, cl.align.text = "l")

#of the above chart can be drawn significant conclusions about the factors affecting the overall flight rating
#find the greatest impact on the overall evaluation of the price to quality ratio, correlation is very strong and is 0.83
#co indicates the dependence of the hand close to linear, consecutive two features that strongly influence on the evaluation are
#seat comfort and cabin staff service, what we have, that passengers pay significant attention to the comfort of the seat and.
#flight during the flight, traits that influence significantly less is the degree of service (although still level close to 0.5).
#and food and beverages - cheap airline rather not pay attention to this issue

summary(ryanair)


#Let's see how the ratings break down in the 3 countries most 
#frequently using the line and for comparison in Poland

summary(ryanair)

#first selector countries to chart (data profession)
top3andPL <- subset(ryanair, Passenger.Country == "United Kingdom" | Passenger.Country == "United States" |
                    Passenger.Country == "Ireland" | Passenger.Country == "Poland")

#calculation of the average value for each group
meangroup <- aggregate(Overall.Rating ~ Passenger.Country, data = top3andPL, FUN = mean)
meangroup$Overall.Rating <- round(meangroup$Overall.Rating, 2)
colnames(meangroup)[2] <- "mean_or"
meangroup


#create a graph with boxplot geometry using the ggplot2 library
boxplot_pas_country <- ggplot(data = top3andPL, aes(x = Passenger.Country, y = Overall.Rating, fill = Passenger.Country))+
  geom_boxplot(fill = "skyblue", color = "black", show.legend = TRUE, outlier.color = "red") + # Dodajemy kolor wypełnienia i kolor obramowania
  scale_y_continuous(limits = c(1, 10), n.breaks = 10) +  #ustawienie zakresu osi y oraz odstępu na osi
  geom_point(data = meangroup, aes(x = Passenger.Country, y = mean_or, color = "darkred")) + #punkt wartości średniej
  geom_text(data = meangroup, aes(x = Passenger.Country, y = mean_or + 0.25, label = paste("mean = ", mean_or), color = "darkred"))+ #opis wartości srednich
  labs(x = "Passenger Country", y = "Overall Rating", title = "Boxplot of Overall Rating by Passenger Country", subtitle = "Among the three most flown countries and Poland") +
  scale_color_manual(values = c("darkred" = "darkred"), guide = FALSE)+ # Ustawienie koloru punktów
  theme_bw() + labs(fill = "Country of Passengers") #ustawienie wyglądu wykresu

 
#calling up the chart
boxplot_pas_country


#Based on the chart, you can see a wide variation from country to country, the most dissatisfied passengers are 
#U.S. passengers, with an average rating of only 2.22 on a 10-point scale and a median or middle value of 1, which means,
#that as many as half of those taking part marked a value of 1. The most satisfied passengers were Irish, where the average was
#5.46 and the median as high as 7, an interesting fact is that among this group the median was the only one to exceed the average value
# as for the UK, the median is not very high at 3, the average value hits 5, which is a relatively not bad result in the group
#Polish people are relatively dissatisfied, which is also very noticeable the average value for Poland exceeds the median by the most (almost twice)
#all countries have a convergence of the first quartile with the minimum value and it is 1, while the 3rd quartile is already divergent, by far the highest
#for Ireland, quite high also for the UK, slightly lower in Poland, while in the US it is only 3. There are also visible outliers in the US
#marked in red on the chart


summary(ryanair)

#based on the boxplot from the previous section we have two pairs of data, using statistical tests we want to check
#whether the differences in flight ratings for the Poland-USA and Ireland-UK pairs are significantly different
#tests involve two independent samples, we take 5% as the significance level

#first we will build four simple histograms to see if the distribution is normal
#we need to select groups for the test
POL <- subset(ryanair, Passenger.Country == "Poland")$Overall.Rating
POL
USA <- subset(ryanair, Passenger.Country == "United States")$Overall.Rating
USA
UK <- subset(ryanair, Passenger.Country == "United Kingdom")$Overall.Rating
UK
IRL <- subset(ryanair, Passenger.Country == "Ireland")$Overall.Rating
IRL

#now we can visualize them
hist(POL) #distribution is not normal
hist(USA) #distribution is not normal
hist(UK) #distribution is not normal
hist(IRL) #distribution is not normal

#because the distribution is not close to normal and we are testing two 
#independent samples the Mann Whitney U test (nonparametric test) is appropriate. 
#(in R language the test is called Wilcox test for independent samples).

#as the first under the magnifying glass we take the pair Poland and USA
#H0: satisfaction in both groups is at similar levels
#H1: satisfaction in Poland is higher than in the U.S. with the airline's services 
### one-sided test which suggests a higher average in POL.

test_PL_USA <- wilcox.test(POL, USA, alternative = "greater", paired = F, exact = F)
test_PL_USA
#p-value = 0.009659, we reject the null hypothesis in favor of the alternative hypothesis Poland group was more satisfied with the airline company's services

#now we run a test for another pair of UK and Ireland
#H0: satisfaction in both groups is at similar levels
#H1: Satisfaction in IRL is higher than in UK ###one-sided test

test_UK_IRL <- wilcox.test(IRL, UK, alternative = "greater", paired = F, exact = F)
test_UK_IRL
#p-value = 0.01252, We reject the null hypothesis in favor of the alternative hypothesis Ireland group was more satisfied with the airline company's services


#in the next step, we want to answer the question of whether the rating increased in successive years of conducting the study
#in this regard, we will make a bar chart showing the average Overall Rating in successive years of publication (date published)
library(dplyr)
DPxOR <- aggregate(Overall.Rating ~ Date.Published, data = ryanair, FUN = mean) #data de-identification
DPxOR$Overall.Rating <- round(DPxOR$Overall.Rating, 2) #round data
DPxOR$Date.Published <- as.factor(DPxOR$Date.Published) #change date to factor
str(DPxOR)
library(forcats)
DPxOR$Date.Published <- fct_rev(DPxOR$Date.Published) #reversing the order to get ascending order on the chart

#creation of a chart
rate_over_years  <- ggplot(DPxOR, aes(x = Date.Published, y = Overall.Rating)) +
  geom_col(fill = "lightpink", color = "black") + #get geom col and fill
  labs(x = "Date Published", y = "Overall Rating", title = "MEAN overall ratings by year") + #desc labs of the plot
  theme_minimal() + #do style plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_smooth(method = lm, formula = y ~ x, color = "blue", se = FALSE, aes(group = 1))+ #get regression line
  geom_text(aes(x = Date.Published[6], y = 4.2, label = paste0(round(100 * coef(lm(Overall.Rating ~ as.numeric(Date.Published)), data = DPxOR)[2], 2), "%")), 
            vjust = -0.5, hjust = 0.5, color = "blue", size = 3) #desc to regression line

#calling up the chart
rate_over_years

#From the chart above, we can see that in the first years of the survey between 2012 and 2016, the average rating gradually increases, with a slight
#decrease in 2015, then from 2017 to 2023 we have a bottom, a noticeable drop of about 2 points downward settling through 2017-2023
#where the worst ratings come in 2019 and 2021 and are below 3 points, in 2024 we see a sudden increase in the average overall rating
#which reaches the highest score of all the surveyed years and exceeds 6 points


summary(ryanair)


#in the next step, we want to see what type of passengers 
#fly airlines and their percentage of the total surveyed

#first we will create a working df categorizing the data for the chart
piedf <- table(ryanair$Type.Of.Traveller)
piedf1 <- data.frame(Type.Of.Traveller = names(piedf), Count = as.integer(piedf))
piedf1

#then we will calculate the percentages
piedf1perc <- piedf1 %>%
  mutate(Percentage = Count / sum(Count) * 100)

#Display a new data frame with percentages
piedf1perc

#The pie chart will be used for this purpose
ggplot(piedf1perc, aes(x = "", y = Percentage, fill = Type.Of.Traveller)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Type of travellers", fill = "Legend") +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust = 0.5))


#on the basis of the chart you can see that as many as 27.3% of respondents chose the type other, in addition to the largest group are couples more than 26%. 
#and singles 24.4% a slightly smaller value are traveling families, at 16.6%, while the smallest group is
#business customers accounting for only 5.6% of respondents


summary(ryanair)

#Finally, we want to see how many people in each type of passenger recommend using the airline

barplot(table(ryanair$Recommended, ryanair$Type.Of.Traveller),
        beside = TRUE,
        main = "Recommended by Type of Travellers",
        xlab = "Type of Traveller",
        ylab = "Count",
        col = c("green", "red"),
        legend.text = c("Yes", "No"),
        ylim = c(0, 500), cex.names = 0.75)

#from the chart we can see that in general in each group airlines were not recommended
#the difference is quite pronounced in each group while in family leisure and copule leisure
#practically 2 times as many people do not recommend than recommend, surprisingly perhaps in the section
#categorized as other the result of the survey is positive where more people recommended flights than did not recommend
