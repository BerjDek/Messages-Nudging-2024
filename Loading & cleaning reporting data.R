
reports_data <- read.csv("CleanReportsData.csv")
reports_data_all <- read.csv("CleanReportsDataALL.csv")


# Loading User Reporting Data ---------------------------------------------



raw_reports_data <- read.csv(file="full_reports_table.csv", header = TRUE) %>% 
  mutate(creation_time= as.POSIXct(creation_time, format = "%Y-%m-%d"))

min(raw_reports_data$creation_time, na.rm = TRUE)

#Since the initiation of the Citizen Science project until 21/11/2023 179995 Reports have been filled


#To create a reports data that can be assessed and merged with a main data set 
#The raw data is cleaned to limit it to the final Update on 2nd of October 2020, after which all User Id's have been reset


reports_data <- raw_reports_data %>%
  dplyr::select(user_id, location_choice, creation_time, type) %>% 
  rename(User_ID = user_id,
         Rprt_Date = creation_time,
         Rprt_Type = type) %>% 
  filter(User_ID != "" & Rprt_Date >= as.POSIXct("2020-10-02") & Rprt_Date <= as.POSIXct("2024-12-31"))%>% 
  mutate(Rprt_Type = as.factor(Rprt_Type))


n_distinct(reports_data$User_ID) #  70594 unique users have filled unitl end of 2024reports up from 54853 when reports are checked until end of 2023

write.csv(reports_data, "CleanReportsData.csv", row.names = FALSE)
#Since 12/10/2020 till the date of the analysis 234371 reports have been filled up from  147,123 reports last time checked


rm(raw_reports_data)


## For Comparison of seasons --------------------------------------------------------------------

reports_data_all <- reports_data %>%
  filter(!User_ID %in% c(survey_data_2024$User_ID, survey_data_2023$User_ID)) %>% 
  group_by(User_ID) %>%  
  mutate(Total_Rprts_Filled = n(),
         Rprts_Filled_2024 = sum(Rprt_Date >= "2024-01-01" & Rprt_Date <= "2024-12-31"),  
         Rprts_Filled_2023 = sum(Rprt_Date >= "2023-01-01" & Rprt_Date <= "2023-12-31"),
         Rprts_Filled_2022 = sum(Rprt_Date >= "2022-01-01" & Rprt_Date <= "2022-12-31"),
         Rprts_Filled_2021 = sum(Rprt_Date >= "2021-01-01" & Rprt_Date <= "2021-12-31"),
         Season_Rprts_Filled_2024 = sum(Rprt_Date >= "2024-05-01" & Rprt_Date <= "2024-10-30"),
         Season_Rprts_Filled_2023 = sum(Rprt_Date >= "2023-05-01" & Rprt_Date <= "2023-10-30"),
         Season_Rprts_Filled_2022 = sum(Rprt_Date >= "2022-05-01" & Rprt_Date <= "2022-10-30"),
         Season_Rprts_Filled_2021 = sum(Rprt_Date >= "2021-05-01" & Rprt_Date <= "2021-10-30"),
         Active_Duration = as.integer(difftime(max(Rprt_Date, na.rm = TRUE), min(Rprt_Date, na.rm = TRUE), units = "days"))) %>% 
  slice(1L) %>% 
  ungroup() %>%
  select(-Rprt_Date, -Rprt_Type, -location_choice)  


reports_data_all <- reports_data_all %>%
  left_join(raw_user_data %>% select(User_ID, Registered_Participation_Date), by = "User_ID") %>% 
  mutate(Registered_Participation_Date = as.Date(Registered_Participation_Date))
  


summary(reports_data_all)
summary(user_data)

write.csv(reports_data_all, "CleanReportsDataALL.csv", row.names = FALSE)
