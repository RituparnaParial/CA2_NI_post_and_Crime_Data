########## Start of header ################
# Title: CA 2 NI post and crime data
#  
# Description: 
#
# <This second R script is a part of
#  section 2 CA 2 and we use NI CRIME DATA>
#
# Author: <Rituparna Parial>  
# Date: <13-04-2019>
########## End of header ###################

# a)

# Amalgamating all of the crime data from each csv file into one dataset.
# This dataset is saved into a csv file called AllNICrimeData. 
# They are counted and numbers of rows are
# shown in the AllNICrimeData dataset.


zipfile <- "NI Crime Data.zip"

# Unzipping the NI Crime Data

unzip(zipfile)

# Saving all the csv's under NI Crime Data to filenames using list.files

filenames <- list.files(path ="NI Crime Data/", recursive = TRUE)
filenames

# Creating a function to combine the csv's inside folder
# into one dataframe called AllNICrimeData 

combine_function <- function(file_list) {
  data_frame <- NULL
  for (file in file_list) {
    data <- read.csv(header = TRUE, paste("NI Crime Data/", file, sep = ""), 
                     stringsAsFactors = FALSE, na.strings = c("", "NA"))
    data_frame <- rbind(data_frame, data)
  }
  return(data_frame)
}
AllNICrimeData <- combine_function(filenames)

# Showing data in the dataframe

head(AllNICrimeData, 10)
str(AllNICrimeData)

#  Store AllNICrimeData dataframe in an external csv

write.csv(AllNICrimeData, "ALLNICrimeData.csv")

sprintf("Number of Rows in Dataframe are: %s", format(nrow(AllNICrimeData),big.mark = ","))

# b)

# Modifying the structure of the newly created AllNICrimeData csv file
# showing the structure of modified output

AllNICrimeData <- AllNICrimeData[, c(2, 5, 6, 7, 10)]
str(AllNICrimeData)

# c)

# Factorise the Crime type attribute

factorized_crime <- factor(AllNICrimeData$Crime.type, order = FALSE)
AllNICrimeData$Crime.type <- factorized_crime
str(AllNICrimeData)


# d)

# Modifying the location attribute

AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA
str(AllNICrimeData)
colSums(is.na(AllNICrimeData))
# sum(is.na(AllNICrimeData))

# omit rows where 'location' has a NA value

AllNICrimeData <- na.omit(AllNICrimeData, cols="Location")

# Changing to uppercase to match other dataset

AllNICrimeData$Location <- toupper(AllNICrimeData$Location)

sum(is.na(AllNICrimeData))

# e)

# Selecting random sample 

random_crime_sample<- AllNICrimeData[sample(1:nrow(AllNICrimeData), 1000, replace =
                                       FALSE),]

# To check null values in random crime sample

sum(is.na(random_crime_sample))

# Creating a function find_a_postcode that takes as an input each 
# location attribute from random_crime_sample
# and finds a suitable postcode value from the postcode dataset. 

install.packages("dplyr")
library(dplyr)

CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv", stringsAsFactors = FALSE)

# Primary Thorfare and postcode are important attributes
# for finding the Post code with reference to the previous dataset
# therefore these are filtered into a new dataframe

PostcodeThorfare_PostcodeThorfare_df<- tbl_df(CleanNIPostcodeData[, c(7, 14)])
str(df)
sum(is.na(df$Primary.Thorfare))
sum(is.na(df$Postcode))
PostcodeThorfare_df<- na.omit(df)
PostcodeThorfare_df<- tbl_df(df)

# Creating a function which takes input from each location 
# and finds suitable postcode value from postcode dataset

find_a_postcode <- lapply(random_crime_sample$Location, function(location) {
  
  matched_location <- filter(df, Primary.Thorfare == location)
  
  Postcode <- names(which.max(table(matched_location$Postcode)))
  
  
  return(Postcode)
})

# Converting postcodes to character vector so it can be properly 
# added to random_crime_sample dataframe

Postcode <- as.character(find_a_postcode)
random_crime_sample$Postcodes <- Postcode
str(random_crime_sample)
sprintf("Number of Rows in Dataframe are: %s", format(nrow(random_crime_sample),big.mark = ","))

# f)

# Save the modified random crime sample data frame
write.csv(random_crime_sample, "random_crime_sample.csv")

# g)

# Extract this data into a new data frame called updated_random_sample

updated_random_sample <- subset(random_crime_sample, select = 
                                c(Month:Postcodes))


# Sorting the chart_data dataframe by postcode where the postcode
# contains “BT1” and then by crime type

chart_data <- subset(updated_random_sample, select = 
                       c(Month:Postcodes))

chart_data <- chart_data[order(chart_data$Postcodes, chart_data$Crime.type),]
chart_data <- filter(chart_data, grepl("BT1", Postcode))
summary(chart_data)
str(chart_data)

# (h) 

# Creating a bar plot of the crime type from the chart_data data frame
# using the ggplot2 library

install.packages("ggplot2")

library(ggplot2)

ggplot(chart_data, aes(x=as.factor(Crime.type), 
                       fill=as.factor(Crime.type) )) +
                       geom_bar(width = 0.5 ) + 
                      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
                      ggtitle("Number of Crimes committed by Type") +
                      labs(x = "Type of Crime", y = "Number of Crimes committed")
