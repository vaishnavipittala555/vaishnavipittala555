setwd("/Users/vaishnavipittala/Desktop/stats")
studentdata <- read.table("outstationstudentsstress.csv", header=T, sep=",")
class(studentdata)
str(studentdata)

stud.df <- studentdata[,-c(1,ncol(studentdata))]
stud.df
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
#cleaning the data
stud.df[,1:5]
stud.df$Agreed_to_participate [stud.df$Agreed_to_participate == "Y"] <- "Yes"
stud.df$Agreed_to_participate [stud.df$Agreed_to_participate == "N"] <- "No"
stud.df$Agreed_to_participate

stud.df$Gender [stud.df$Gender == "F"] <- "Female"
stud.df$Gender [stud.df$Gender == "M"] <- "Male"
stud.df$Gender

stud.df$Pursuing_Undergrad_or_Postgrad [stud.df$Pursuing_Undergrad_or_Postgrad == "ug"] <- "Under graduation"
stud.df$Pursuing_Undergrad_or_Postgrad [stud.df$Pursuing_Undergrad_or_Postgrad == "pg"] <- "Post graduation"
stud.df$Pursuing_Undergrad_or_Postgrad [stud.df$Pursuing_Undergrad_or_Postgrad == "bsc"] <- "Under graduation"
stud.df$Pursuing_Undergrad_or_Postgrad

as.factor(stud.df$Pursuing_Undergrad_or_Postgrad)

stud.df[,6:12]

stud.df$Travel_Time_to_University_Minutes [stud.df$Travel_Time_to_University_Minutes  == "2ND"] <- "2"
stud.df$Travel_Time_to_University_Minutes [stud.df$Travel_Time_to_University_Minutes  == "2nd"] <- "2"
stud.df$Pursuing_Undergrad_or_Postgrad [stud.df$Pursuing_Undergrad_or_Postgrad == "pg"] <- "Post graduation"
stud.df$Pursuing_Undergrad_or_Postgrad [stud.df$Pursuing_Undergrad_or_Postgrad == "bsc"] <- "Under graduation"
stud.df$Pursuing_Undergrad_or_Postgrad
#checking non numeric
as.numeric(stud.df$Year_of_Study)
is.na(as.numeric(stud.df$Year_of_Study))
stud.df$Year_of_Study  [stud.df$Year_of_Study  == "2ND"] <- "2"
stud.df$Year_of_Study  [stud.df$Year_of_Study  == "2nd"] <- "2"
stud.df$Year_of_Study

as.numeric(stud.df$Travel_Time_to_University_Minutes)
is.na(as.numeric(stud.df$Travel_Time_to_University_Minutes))
idx <- which(is.na(as.numeric(stud.df$Travel_Time_to_University_Minutes)))
stud.df$Travel_Time_to_University_Minutes[idx]
cbind(idx, stud.df$Travel_Time_to_University_Minutes[idx])

stud.df$Travel_Time_to_University_Minutes[3] <- 60
stud.df$Travel_Time_to_University_Minutes[19] <- 0

as.numeric(stud.df$Hometown_to_University_Hours)
is.na(as.numeric(stud.df$Hometown_to_University_Hours))
idx <- which(is.na(as.numeric(stud.df$Hometown_to_University_Hours)))
stud.df$Hometown_to_University_Hours[idx]
cbind(idx, stud.df$Hometown_to_University_Hours[idx])


stud.df$Hometown_to_University_Hours[1] <- 9
stud.df$Hometown_to_University_Hours[2] <- 3
stud.df$Hometown_to_University_Hours[4] <- 4
stud.df$Hometown_to_University_Hours[5] <- 5

stud.df[,6:12]

stud.df$Outstation_Student <- ifelse(stud.df$Outstation_Student== "Yes", TRUE, FALSE)
stud.df$Outstation_Student 


ifelse((stud.df$Outstation_Student =="Yes" | stud.df$Outstation_Student =="No"), "Local", "Non Local")
stud.df$Outstation_Student
cbind(stud.df$Outstation_Student, stud.df$local)
#lets put it close to the country
stud.df

#To eliminate those who are not outstation students
stud.df<- stud.df[stud.df$Outstation_Student != "FALSE", ]
stud.df

write.csv(stud.df, "outstationstudentsstress.csv", row.names=TRUE)
summary(stud.df)

Q1: How many students in between age 18- 25?
  table(stud.df$Age)
round(prop.table(table(stud.df$Age)),25)*100

Q2: How many students coming from travelling more than an hour everyday?
  table(stud.df$Travel_Time_to_University_Minutes)
  
