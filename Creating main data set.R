# Unifying Data Sets ------------------------------------------------

data_2023 <- read.csv(file="MainData_2023.csv", header = TRUE)
data_2024 <- read.csv(file="MainData_2024.csv", header = TRUE)
data <- read.csv(file="MainData.csv", header = TRUE)


## Creating Joint 2023 data ------------------------------------------------  


Unified_2023 <- full_join(survey_data_2023, message_data_2023, by = "User_ID")

Unified_2023  <- full_join(Unified_2023 , reports_data, by = "User_ID")


data_2023 <- Unified_2023 %>%
  filter(User_ID %in% survey_data_2023$User_ID) %>%
  group_by(User_ID) %>%
  mutate(Total_Rprts_Filled = n(),
         Season_Rprts_Filled_2024 = sum(Rprt_Date >= "2024-05-01" & Rprt_Date <= "2024-10-30"),
         Season_Rprts_Filled_2023 = sum(Rprt_Date >= "2023-05-01" & Rprt_Date <= "2023-10-30"),
         Season_Rprts_Filled_2022 = sum(Rprt_Date >= "2022-05-01" & Rprt_Date <= "2022-10-30"),
         Season_Rprts_Filled_2021 = sum(Rprt_Date >= "2021-05-01" & Rprt_Date <= "2021-10-30"),
         Total_Bite_Rprts_Filled = sum(Rprt_Type == "bite"),
         Total_Adult_Rprts_Filled = sum(Rprt_Type == "adult"),
         Total_Site_Rprts_Filled = sum(Rprt_Type == "site"),
         Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE),
         Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - days(Msg_Duration_Days)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
         Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(Msg_Duration_Days)), na.rm = TRUE)) %>% 
  slice(1L) %>%
  ungroup() %>%
  select(-Rprt_Date, -Rprt_Type, -location_choice) %>%
  left_join(user_data %>% select(User_ID, Registered_Participation_Date), by = "User_ID") %>% #adding registered participation date to the data frame
  mutate(Year_Survey_Taken = 2023) 



## Creating Joint 2024 data ------------------------------------------------


Unified_2024 <- full_join(survey_data_2024, message_data_2024, by = "User_ID")

Unified_2024  <- full_join(Unified_2024 , reports_data, by = "User_ID")


data_2024 <- Unified_2024 %>%
  filter(User_ID %in% survey_data_2024$User_ID) %>%
  group_by(User_ID) %>%
  mutate(Total_Rprts_Filled = n(),
         Season_Rprts_Filled_2024 = sum(Rprt_Date >= "2024-05-01" & Rprt_Date <= "2024-10-30"),
         Season_Rprts_Filled_2023 = sum(Rprt_Date >= "2023-05-01" & Rprt_Date <= "2023-10-30"),
         Season_Rprts_Filled_2022 = sum(Rprt_Date >= "2022-05-01" & Rprt_Date <= "2022-10-30"),
         Season_Rprts_Filled_2021 = sum(Rprt_Date >= "2021-05-01" & Rprt_Date <= "2021-10-30"),
         Total_Bite_Rprts_Filled = sum(Rprt_Type == "bite"),
         Total_Adult_Rprts_Filled = sum(Rprt_Type == "adult"),
         Total_Site_Rprts_Filled = sum(Rprt_Type == "site"),
         Rprts_During_Msging = sum(Rprt_Date >= First_Msg_Date & Rprt_Date <= Last_Msg_Date, na.rm = TRUE),
         Rprts_Before_Msging = sum(Rprt_Date >= (First_Msg_Date - days(Msg_Duration_Days)) & Rprt_Date < First_Msg_Date, na.rm = TRUE),
         Rprts_After_Msging = sum(Rprt_Date > Last_Msg_Date & Rprt_Date <= (Last_Msg_Date + days(Msg_Duration_Days)), na.rm = TRUE)) %>% 
  slice(1L) %>%
  ungroup() %>%
  select(-Rprt_Date, -Rprt_Type, -location_choice) %>%
  left_join(user_data %>% select(User_ID, Registered_Participation_Date), by = "User_ID") %>% 
  mutate(Year_Survey_Taken = 2024)  


summary(data_2024)


## Combined Data -----------------------------------------------------------

data <- rbind(data_2023, data_2024)


write.csv(data_2023, "G:/My Drive/Article about Messages/Messages-Nudging-2024/MainData_2023.csv", row.names = FALSE)

write.csv(data_2024, "G:/My Drive/Article about Messages/Messages-Nudging-2024/MainData_2024.csv", row.names = FALSE)

write.csv(data, "G:/My Drive/Article about Messages/Messages-Nudging-2024/MainData.csv", row.names = FALSE)

rm(Unified_2023,Unified_2024)
