
user_data <- read.csv(file="CleanUserData_2024.csv", header = TRUE)


# Loading & cleaning user data --------------------------------------------

raw_user_data <- read.csv(file="reports_by_uuid.csv", header = TRUE)

#####IMPORTANT######
# AGUSTI MADE A MISTAKE AND SWITCHED COLUMN NAMES THIS TIME HE SENT THE REPORT
# THE CODE BELOW IS ONLU FOR THIS TIME NOT TO BE USED FOR OTHER VERSIONS OF THE RAW DATA, for older versions use code from meesage-nudging-2023

raw_user_data <- raw_user_data %>%
  rename(temp_name = registration_time)

raw_user_data <- raw_user_data %>%
  rename(registration_time = n, 
         n = temp_name)




#since the beginning of the project, there have been 354,712 registration/downloads
raw_user_data <- raw_user_data %>% 
  rename( User_ID = user_UUID,
          Registered_Participation_Date = registration_time,
          Registered_Total_Reports = n) %>% 
  mutate(Registered_Participation_Date = as.POSIXct(Registered_Participation_Date, format = "%Y-%m-%d %H:%M:%S"),
         Registered_Total_Reports = as.integer(Registered_Total_Reports)) %>%
  replace_na(list(Registered_Total_Reports = 0)) 




# Count of unique users that have submitted at least 1 report
nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1))

#  85550  have  filled a report until end of 2024 up from 70,799 of the registered users at the end of 2023



nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1 & 
                Registered_Participation_Date >= as.POSIXct("2020-10-02") & Registered_Participation_Date <= as.POSIXct("2023-12-31")))

# this data set shows 55213 registered users since cutoff until the end of 2023, with the previous data set it was  54,844 registered users in the same timeframe



nrow(raw_user_data %>%
       filter(!is.na(Registered_Total_Reports) & Registered_Total_Reports >= 1 & 
                Registered_Participation_Date >= as.POSIXct("2020-10-02") & Registered_Participation_Date <= as.POSIXct("2024-12-31")))

#Again there is a discrepencez beteween reprots data set and the unique users
# for 2023 there was 54,844 vs 54,853,
# now its 70,525 vs 70594 the difference grew a bit

#the number from reports csv is going to be used (again)



#checking the number of users registered after the update.
nrow(raw_user_data %>%
       filter(Registered_Participation_Date >= as.POSIXct("2020-10-02")))
# 380500users in total, this number is going to be used as the pool to attach to the main data set. P.S it was 310924  at end of 2023
  



user_data <- raw_user_data %>%
  filter(User_ID %in% c(survey_data_2024$User_ID, survey_data_2023$User_ID)) %>%
  mutate(Registered_Participation_Date = as.Date(Registered_Participation_Date))




#it is unnecesary here, but can be modified 
user_data <- user_data %>%
  mutate(Accuracy = (((hits_adult+maybes_adult)/total_adult)*100))




summary(user_data)


write.csv(user_data, "CleanUserData_2024.csv", row.names = FALSE)
rm(raw_user_data)

#rm(user_data)