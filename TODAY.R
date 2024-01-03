# Setting the working directory and loading the dataset
setwd("/Users/vaishnavipittala/Desktop/stats")
studentdata <- read.table("outstationstudentstress.csv", header=T, sep=",")

# Checking the class and structure of the dataset
class(studentdata)
str(studentdata)

# Creating a working dataframe without the first and last columns (if not needed)
stud.df <- studentdata[,-c(1,ncol(studentdata))]
stud.df

# Renaming columns for better readability and understanding
names(stud.df) <- c("Agreed_to_participate",
                    "Name",
                    "Age",
                    "Gender",
                    "Pursuing_Undergrad_or_Postgrad",
                    "Year_of_Study",
                    "Travel_Time_to_University_Minutes",
                    "Hometown_to_University_Hours",
                    "Outstation_Student",
                    "Resilience_Score",
                    "Stress_Management_Score",
                    "Perceived_Stress_Scale")
str(stud.df)

# Data cleaning and standardization process begins here
# Displaying the first 5 columns to check for data inconsistencies
stud.df[,1:5]

# Cleaning categorical columns: Agreed_to_participate, Gender, Pursuing_Undergrad_or_Postgrad, etc.
# Cleaning 'Agreed_to_participate' column.
stud.df$Agreed_to_participate[stud.df$Agreed_to_participate == "Y"] <- "Yes"
stud.df$Agreed_to_participate[stud.df$Agreed_to_participate == "N"] <- "No"
stud.df$Agreed_to_participate

# Cleaning 'Gender' column
stud.df$Gender[stud.df$Gender == "F"] <- "Female"
stud.df$Gender[stud.df$Gender == "M"] <- "Male"
stud.df$Gender[stud.df$Gender == "female"] <- "Female"
stud.df$Gender[stud.df$Gender == "male"] <- "Male"
stud.df$Gender[stud.df$Gender == "Female "] <- "Female"
stud.df$Gender[stud.df$Gender == "Male "] <- "Male"
stud.df$Gender

# Cleaning 'Pursuing_Undergrad_or_Postgrad' column
stud.df$Pursuing_Undergrad_or_Postgrad[stud.df$Pursuing_Undergrad_or_Postgrad == "ug"] <- "Under graduation"
stud.df$Pursuing_Undergrad_or_Postgrad[stud.df$Pursuing_Undergrad_or_Postgrad == "pg"] <- "Post graduation"
stud.df$Pursuing_Undergrad_or_Postgrad[stud.df$Pursuing_Undergrad_or_Postgrad == "bsc"] <- "Under graduation"
stud.df$Pursuing_Undergrad_or_Postgrad

# Convert 'Pursuing_Undergrad_or_Postgrad' column to a factor for categorical representation
as.factor(stud.df$Pursuing_Undergrad_or_Postgrad)

# Selecting columns 6 to 12 of the dataframe
stud.df[,6:12]

# Standardizing entries in columns 'Travel_Time_to_University_Minutes' and 'Pursuing_Undergrad_or_Postgrad'
stud.df$Travel_Time_to_University_Minutes[stud.df$Travel_Time_to_University_Minutes  == "2ND"] <- "2"
stud.df$Travel_Time_to_University_Minutes[stud.df$Travel_Time_to_University_Minutes  == "2nd"] <- "2"
stud.df$Pursuing_Undergrad_or_Postgrad[stud.df$Pursuing_Undergrad_or_Postgrad == "pg"] <- "Post graduation"
stud.df$Pursuing_Undergrad_or_Postgrad[stud.df$Pursuing_Undergrad_or_Postgrad == "bsc"] <- "Under graduation"
stud.df$Pursuing_Undergrad_or_Postgrad

# Handling non-numeric entries and missing values in 'Year_of_Study' and 'Travel_Time_to_University_Minutes' columns
as.numeric(stud.df$Year_of_Study)
is.na(as.numeric(stud.df$Year_of_Study))
stud.df$Year_of_Study[stud.df$Year_of_Study == "2ND"] <- "2"
stud.df$Year_of_Study[stud.df$Year_of_Study == "2nd"] <- "2"
stud.df$Year_of_Study

# Converting 'Travel_Time_to_University_Minutes' column to numeric type
as.numeric(stud.df$Travel_Time_to_University_Minutes)

# Checking for missing values in the numeric conversion of 'Travel_Time_to_University_Minutes'
is.na(as.numeric(stud.df$Travel_Time_to_University_Minutes))

# Finding the index of missing values in the converted column
idx <- which(is.na(as.numeric(stud.df$Travel_Time_to_University_Minutes)))

# showing rows with missing values in 'Travel_Time_to_University_Minutes'
stud.df$Travel_Time_to_University_Minutes[idx]
cbind(idx, stud.df$Travel_Time_to_University_Minutes[idx])

# Assigning specific values to missing entries at index 3 and 19 in 'Travel_Time_to_University_Minutes'
stud.df$Travel_Time_to_University_Minutes[3] <- 60
stud.df$Travel_Time_to_University_Minutes[19] <- 0

# Converting 'Hometown_to_University_Hours' column to numeric type
as.numeric(stud.df$Hometown_to_University_Hours)

# Checking for missing values in the numeric conversion of 'Hometown_to_University_Hours'
is.na(as.numeric(stud.df$Hometown_to_University_Hours))

# Finding the indices of missing values in the converted column
idx <- which(is.na(as.numeric(stud.df$Hometown_to_University_Hours)))

# Displaying rows with missing values in 'Hometown_to_University_Hours'
stud.df$Hometown_to_University_Hours[idx]
cbind(idx, stud.df$Hometown_to_University_Hours[idx])

stud.df$Hometown_to_University_Hours[1] <- 9
stud.df$Hometown_to_University_Hours[2] <- 3
stud.df$Hometown_to_University_Hours[4] <- 4
stud.df$Hometown_to_University_Hours[5] <- 5

#To see the columns from 6 to 12.
stud.df[,6:12]

# Converting 'Outstation_Student' column to boolean values
stud.df$Outstation_Student <- ifelse(stud.df$Outstation_Student == "Yes", TRUE, FALSE)
stud.df$Outstation_Student

# Creating a new column 'local' based on 'Outstation_Student' values
ifelse((stud.df$Outstation_Student == "Yes" | stud.df$Outstation_Student == "No"), "Local", "Non Local")
stud.df$Outstation_Student
cbind(stud.df$Outstation_Student, stud.df$local)

# Removing entries where students are not outstation students
stud.df <- stud.df[stud.df$Outstation_Student != "FALSE", ]
stud.df

# Writing the cleaned dataframe to a new CSV file
write.csv(stud.df, "outstationstudentsstress.csv", row.names=TRUE)
summary(stud.df)

#Q1 to Q16: Various statistical questions and summaries on the cleaned data, 
# including counts, means, and tables related to different columns and criteria.

#Q1: How many students upto age of 25?
table(stud.df$Age) 
round(prop.table(table(stud.df$Age)),25)*100

#Q2:How many students are male and how many are female?
table(stud.df$Gender)

#Q3:How many students are pursuing undergraduation/Postgraduation?
table(stud.df$Pursuing_Undergrad_or_Postgrad)

#Q4:Which year of masters/bachelors are students in?
table(stud.df$Year_of_Study)

#Q5: How many students coming from travelling less than an hour everyday? 
 
round(prop.table(table(stud.df$Travel_Time_to_University_Minutes)),60)*100
table(stud.df$Travel_Time_to_University_Minutes)

#Q6: How many hours is your hometown from the university?
table(stud.df$Hometown_to_University_Hours)

#Q7:How many out of station students are there?
table(stud.df$Outstation_Student)

#Q8:How many students are in undergraduate, postgraduate and what year are they studying in?
table(stud.df$Pursuing_Undergrad_or_Postgrad, stud.df$Year_of_Study)

#Q9:Mean resilience score of Post graduate students is?
mean(stud.df$Resilience_Score[stud.df$Pursuing_Undergrad_or_Postgrad == "Post graduation"], na.rm = TRUE)

#Q10:Mean resilience score of Under graduate students is?
mean(stud.df$Resilience_Score[stud.df$Pursuing_Undergrad_or_Postgrad == "Under graduation"], na.rm = TRUE)
aggregate(x = stud.df$Resilience_Score, by = list(Way = stud.df$Pursuing_Undergrad_or_Postgrad), FUN = mean, na.rm = TRUE)

#Q11:Mean stress management levels of students postgraduate students  is?
mean(stud.df$Stress_Management_Score[stud.df$Pursuing_Undergrad_or_Postgrad == "Post graduation"], na.rm = TRUE)

#Q12:Mean stress management levels of students postgraduate students  is?
mean(stud.df$Stress_Management_Score[stud.df$Pursuing_Undergrad_or_Postgrad == "Under graduation"], na.rm = TRUE)
aggregate(x = stud.df$Stress_Management_Score, by = list(Way = stud.df$Pursuing_Undergrad_or_Postgrad), FUN = mean, na.rm = TRUE)

#Q13:Mean percieved stress level scores of postgraduate students is?
mean(stud.df$Perceived_Stress_Scale[stud.df$Pursuing_Undergrad_or_Postgrad == "Post graduation"], na.rm = TRUE)

#Q14:Mean percieved stress level scores of undergraduate students is?
mean(stud.df$Perceived_Stress_Scale[stud.df$Pursuing_Undergrad_or_Postgrad == "Under graduation"], na.rm = TRUE)
aggregate(x = stud.df$Perceived_Stress_Scale, by = list(Way = stud.df$Pursuing_Undergrad_or_Postgrad), FUN = mean, na.rm = TRUE)


#Q15:What is the resilience, stress management score,  perceived stress scale of students who are below 28.
subset(stud.df , age < 28, select = c(Resilience_Score, Stress_Management_Score, Perceived_Stress_Scale))

#Q16: what is the mean resilience, stress management and percieved stress of the undergraduate and postgraduate out of station students?
aggregate(cbind(Resilience_Score, Stress_Management_Score, Perceived_Stress_Scale) ~ Pursuing_Undergrad_or_Postgrad, data = stud.df, FUN = mean, na.rm = TRUE)

str(stud.df)
table(stud.df$Agreed_to_participate)
table(stud.df$Name)
table(stud.df$Age)
table(stud.df$Gender)
table(stud.df$Pursuing_Undergrad_or_Postgrad)
table(stud.df$Year_of_Study)
table(stud.df$Travel_Time_to_University_Minutes)
table(stud.df$Hometown_to_University_Hours)
table(stud.df$Outstation_Student)
table(stud.df$Resilience_Score)
table(stud.df$Stress_Management_Score)
table(stud.df$Perceived_Stress_Scale)

# #Another homework  - not done in class
# ##ADD a VARIABLE
# #Suppose we want to create another variable starting from country
# #which is UE non UE
# ifelse((stud.df$country=="Italy" | stud.df$country=="Germany" |
#           stud.df$country=="Croatia" | stud.df$country=="Spain"), T, F)
# stud.df$eu <- ifelse((stud.df$country=="Italy" | stud.df$country=="Germany" |
#                         stud.df$country=="Croatia" | stud.df$country=="Spain"), TRUE, FALSE)
#
# cbind(stud.df$country, stud.df$eu)
#
# as.factor(stud.df$eu)


### Lecture 7 - module 2
df <- read.table("outstationstudentsstress.csv", header=T, sep=';', stringsAsFactors = T)
str(df)


##Frequency distribution
table(df$Agreed_to_participate)
table(df$Name)
table(df$Age)
table(df$Gender)
table(df$Pursuing_Undergrad_or_Postgrad)
table(df$Year_of_Study)
table(df$Travel_Time_to_University_Minutes)
table(df$Hometown_to_University_Hours)
table(df$Outstation_Student)
table(df$Resilience_Score)
table(df$Stress_Management_Score)
table(df$Perceived_Stress_Scale)

#Grouped frequency distribution
#we take the variable km_travel and consider

# Convert column to numeric if it's not already numeric
Travel_Time_to_University_Minutes <- cut(
  stud.df$Travel_Time_to_University_Minutes,
  breaks = c(0.99, 10, 20, 30, 40, 60),
  labels = c("very close", "close", "medium", "far", "very far")
)



#Bivariate frequency distribution

table(stud.df$Resilience_Score)
table(stud.df$Stress_Management_Score)
table(stud.df$Perceived_Stress_Scale)

table(stud.df$Resilience_Score, stud.df$Stress_Management_Score)



#Bar chart:
barplot(table(stud.df$Resilience_Score), col="green")
barplot(table(stud.df$Stress_Management_Score), col ="blue")
barplot(table(stud.df$Perceived_Stress_Scale), col="red")



# Specific color for each bar? Use a well known palette
library(RColorBrewer)
coul <- brewer.pal(5, "Set2")
coul <- brewer.pal(5, "Set1")

barplot(table(stud.df$Resilience_Score), col=c(1,2,3,4,5) )

# Change border color
barplot(table(stud.df$Stress_Management_Score), border="#69b3a2", col="blue" )

# axis label
barplot(table(stud.df$Perceived_Stress_Scale), xlab="categories",
        ylab="values",  main="Percieved stress", ylim=c(0,40))

#horizontal barplot
barplot(table(stud.df$Resilience_Score), col="#69b3a2",  horiz=T)

barplot(table(stud.df$Stress_Management_Score), col="#69b3a2",  horiz=T, las=1)

# The las argument allows to change the orientation of the axis labels:
# 0: always parallel to the axis
# 1: always horizontal
# 2: always perpendicular to the axis
# 3: always vertical.
# This is specially helpful for horizontal bar chart.

# Changing names
barplot(table(stud.df$Resilience_Score), names.arg=c("ResilienceScoresOfStudents"), col="orange")

# Customize the labels:
# font.axis: font: 1: normal, 2: bold, 3: italic, 4: bold italic
# col.axis: color
# cex.axis: size
# Customize axis title:
#
# font.lab
# col.lab
# cex.lab


# Customize labels (left)
barplot(table(stud.df$Resilience_Score),
        names.arg=c("ResilienceScoresOfStudents"),
        font.axis=2,
        col.axis="orange",
        cex.axis=1.5
)

# # If your group names are long, you need to: rotate them to avoid overlapping.
# # This is done with las
# # increase bottom margin size using the mar parameter of the par() function.
# # Four values are provided: bottom, left, top, right respectively.
# # Note: prefer a horizontal barplot in this case.


#####
########


#pie chart (not so much used)
pie(table(stud.df$Gender), main="Pie chart of Male and Female")
pie(table(stud.df$Pursuing_Undergrad_or_Postgrad), main="Pie chart of Undergrad and Postgrad")
pie(table(stud.df$Year_of_Study), main="Pie chart of Year of study")

##Stacked barchart
f_tab1 <- table(stud.df$Gender, stud.df$Pursuing_Undergrad_or_Postgrad)
barplot(f_tab1, beside = TRUE, legend = TRUE,
        main = "Stacked Bar Chart of Gender and Pursuing_Undergrad_or_Postgrad",
        xlab = "Gender", ylab = "Count",
        col = c("skyblue", "salmon"),
        names.arg = c("Male UG/PG", "Female UG/PG"))

# Stacked bar chart for Gender and Year_of_Study
f_tab2 <- table(stud.df$Gender, stud.df$Year_of_Study)
barplot(f_tab2, beside = TRUE, legend = TRUE,
        main = "Stacked Bar Chart of Gender and Year_of_Study",
        xlab = "Gender", ylab = "Count",
        col = c("skyblue", "lightgreen", "salmon"),
        names.arg = unique(stud.df$Year_of_Study))

# Stacked bar chart for Pursuing_Undergrad_or_Postgrad and Year_of_Study
f_tab3 <- table(stud.df$Pursuing_Undergrad_or_Postgrad, stud.df$Year_of_Study)
barplot(f_tab3, beside = TRUE, legend = TRUE,
        main = "Stacked Bar Chart of Pursuing_Undergrad_or_Postgrad and Year_of_Study",
        xlab = "Pursuing_Undergrad_or_Postgrad", ylab = "Count",
        col = c("skyblue", "lightgreen", "salmon"),
        args.legend = list(title = "Year_of_Study"),
        args.legend.text = list(c("1st Year", "2nd Year", "3rd Year")))

barplot(f_tab1, main = "Mutiple bar chart", xlab = "use R", names.arg =c('Yes', 'No'),
        ylab = "Frequency", col = c("red", "blue"), ylim=c(0,0.6),xlim=c(1,10),border=NA,  beside = TRUE,
        legend.text = TRUE, width=rep(1,4), args.legend = list(x = 9.3, y=0.3,
                                                               bty="n", border=F))
abline(h=0)


f_tab2<-table(df$km_cat, df$travel)/length(df$travel)

barplot(f_tab2, main = "Mutiple bar chart", xlab = "use R",
        ylab = "Frequency", col =brewer.pal(5, "Set2") , border=NA,  beside = TRUE,
        legend.text = TRUE, width=rep(1,7), args.legend = list(x = 9.3, y=0.36,
                                                               inset = c( 0, 0), bty="n", border=F))
abline(h=0)







#histogram
hist(df$age)
hist(df$height)

# With the breaks argument we can specify the number of cells we want in the histogram.
# However, this number is taken by R just a suggestion as it calculates the best number of cells,
# keeping this suggestion in mind.
hist(df$height, breaks = 15)




s <- c(19, 23, 26, 29, 33)
hist(df$age, breaks=s, xlim=c(19,33), right=F, col='lavenderblush',
     xlab="age", main='title', freq=F)

hist(df$age, breaks=s, xlim=c(19,33), right=F, xaxt='n', col='lavenderblush',
     yaxt='n', xlab="age", ylab = '', main='title', freq=F)
axis(1, tick=T, at = c(19, 23, 26, 29, 33))
axis(2, tick=T, at = c(0.00, 0.02, 0.04, 0.06, 0.07, 0.1, 0.15, 0.17))


#right = F (default) the right value of breaks is not included
#if right=T is is included
a <- hist(df$age, breaks=s, xlim=c(19,33), right=T, col=c(2,3,4, 5),
          xlab="age", main='title', freq=F)


abline(h=0.05, col="gray", lty=2, lwd=1.5)

plot('', xlim=c(18,34), ylim=c(0,.15))
abline(h=0.05, col="gray", lty=2, lwd=1.5)
abline(h=0.10, col="gray", lty=2, lwd=1.5)
abline(h=0.15, col="gray", lty=2, lwd=1.5)
plot(a, add=T)
text(a$mids,a$density,labels=round(a$density,2), adj=c(0.5, -0.5))

text(a$mids,a$density,labels=round(a$density,2), adj=c(0.5, -0.5))

str(a)

###Lists



#mean variance
mean(df$age)
mean(df$age^2)-mean(df$age)^2

median(df$age)
quantile(df$age)

quantile(df$age, p=0.4)


##boxplots
boxplot(df$age)

agelastyear <- round(rnorm(25, 23, 3))
a <- boxplot(df$age, agelastyear, main='boxplot of age', ann=T, names=c('age this year','age last year'))




mean(df$age*df$height)-mean(df$age)*mean(df$height)
cov(df$age,df$height)
cor(df$age,df$height)

cor(df$hours_stud, df$height, use="complete.obs")

(mean(df$age*df$height)-mean(df$age)*mean(df$height))/sqrt((mean(df$age^2)-mean(df$age)^2)*(mean(df$height^2)-mean(df$height)^2))

plot(df$age, df$height)

####
####
###Scatterplot
data(trees)
tdf <- trees

cor(tdf$Height, tdf$Volume)
cor(tdf$Height, tdf$Girth)


plot(tdf$Height, tdf$Volume)
plot(tdf$Height, tdf$Volume, xlab = "height", ylab="volume")
plot(tdf$Height, tdf$Volume, xlab = "height", ylab="volume", main="Tree", col="red")


plot(tdf$Height, tdf$Volume, xlab = "height", ylab="volume", main="Tree", col="red", xaxt='n', yaxt='n')
axis(1, tick=T, at = c(60,70,80))
axis(2, tick=T, at = c(20,40, 60, 80), font=2)



