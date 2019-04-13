########## Start of header ################
# Title: CA 2 NI post and crime data
#  
# Description: 
#
# <This first R script is a part of
#  section 1 CA 2 and it deals with
#  cleaning of the NI_Postcode dataset>
#
# Author: <Rituparna Parial>  
# Date: <12-04-2019>
########## End of header ###################


# Starting the project by calling our first
# desired file into the console
# here we are specifying using header 
# = False that there
# are no column names in first row
# stringsAsFactors for deciding whether 
# characters should be converted to factor
# strip.white to strip out white spaces from strings

NI_PostCode <- read.csv("NIPostcodes.csv", header = FALSE,
                        stringsAsFactors = FALSE,strip.white = TRUE, 
                        na.strings = c("","NA"))
summary(NI_PostCode)

# Show the total number of rows

sprintf("Number of Rows in Dataframe: %s", format(nrow(NI_PostCode),big.mark = ","))

# Display the structure of the NI Crime data frame

str(NI_PostCode)

# First 10 rows of the NI Crime data frame

head(NI_PostCode, n=10)

# Adding a suitable title for each attribute of the data

colnames(NI_PostCode) <- c("Organisation Name", "Sub-Building Name", "Building Name",
                           "Number", "Primary Thorfare", "Alt Thorfare",
                           "Secondary Thorfare", "Locality", 
                           "Townland", "Town", "County", "Postcode",
                           "x-coordinates", "y-coordinates", 
                           "Primary Key (identifier)")

# Showing total number of missing and mean values for each column using sapply

sapply(NI_PostCode, function(x) sum(is.na(x)))
sapply(NI_PostCode, function(x) mean(is.na(x)))
sprintf("Overall mean for missing valuesin Dataframe: %s"
        , format(mean(is.na(NI_PostCode))))
sprintf("Total number of missing valuesin Dataframe: %s"
        , format(sum(is.na(NI_PostCode))))

# Dealing with missing values

# In this case we cant take median/mean/mode for the 
# values as they wouldn't make sense
# we shall remove the Null postcode values
# because here postcode is a valueble attribute
# without it, it'll be difficult to find the address
# Using complete.cases will keep only the complete rows

NI_PostCode <- NI_PostCode[complete.cases(NI_PostCode$Postcode), ]

sum(is.na(NI_PostCode$Postcode))
str(NI_PostCode)

# Modifying the County attribute to be a categorising factor

NI_PostCode$County <- factor(NI_PostCode$County)
str(NI_PostCode)

# Moving the primary key identifier to the start of the dataset

NI_PostCode <- NI_PostCode[ ,c(15, 1:14)]
str(NI_PostCode)

# Create a new dataset with only required value

Limavady_data <- subset(NI_PostCode,  Town == "LIMAVADY", select =`Locality`:`Town`)
str(Limavady_data)

#  Store Limavady dataframe in an external csv

write.csv(Limavady_data, "Limavady.csv")

# Storing the cleaned and modified postcode data into CleanNIPostcodeData

write.csv(NI_PostCode, "CleanNIPostcodeData.csv")