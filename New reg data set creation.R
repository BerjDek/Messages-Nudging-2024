

# New Simpler Way of Getting Regression Data ------------------------------


# NOTE: Unlike when done earlier,  using this method and data set, all those who
# this one includes those who did not fill anz reports in 2023 are exluded,

# c("5280c75f-af25-4f30-a417-8341f353adae", "85b119ce-b2e3-4085-8360-6ab7625d1f39", 
#   +               "5d5c6982-2fdd-4a87-9483-aeac44652efb", "df897a53-bad6-457c-8bd9-0f2eb997d894", 
#   +               "0fb36846-417e-49f8-8c09-92a35edbd033", "6d7eef2e-328c-4e60-bff6-d1087c799d65", 
#   +               "311b06f1-6df8-4154-a0e0-b1ab3692dd42", "5856c91b-96bd-4f96-a177-9cfdf351892a", 
#   +               "9a00ace5-2683-4657-a5af-f00fca5c8cf0", "c44acb81-4dc6-47b7-bac4-730dbae562ea", 
#   +               "969623f8-25f7-41b8-899d-5103cdf127d1", "3f9477af-313b-4499-964d-4737caf75dab", 
#   +               "872d490b-5923-4ab0-a4e0-453410f8c061", "920c8e2d-12d2-4969-88a4-ab3f5e4fdfa4", 
#   +               "fccc8ffc-7b41-415f-86e4-8ddc837f7387", "6eb952d7-feb3-4f7d-98d7-67f89d78c75f", 
#   +               "4cff1cf3-a005-479e-95c4-de638c7c353f", "46a17093-29c6-4ca3-a001-651285fd87a0", 
#   +               "e0d2a6b9-d37a-4666-a6d9-15bb26fce1b5", "e7d57de8-af8e-46d4-92f7-68399d576014", 
#   +               "2b82b6f6-e05b-4162-bd12-e1366b017c66", "77a006fd-88e8-44dd-a71d-5ebf53964f86", 
#   +               "36a6cb71-6d56-415b-9dd8-2bf25f744251")



## Creating date sequence for 2023 -----------------------------------------


dates_2023  <- seq.Date(as.Date("2023-05-01"), as.Date("2023-10-31"), by = "day")



## Preparing data on messaging/nudging -------------------------------------


### Filtering Message data to keep only those that were part of the survey --------
### NOTE" those that joined and did


filtered_2023_message_data <- raw_message_data_2023 %>%
  filter(User_ID %in% survey_data_2023$User_ID) %>%
  mutate(Date = as.Date(Msg_Date)) %>%  
  filter(format(Date, "%Y") == "2023") %>% 
  select(-Msg_Date)

summary(filtered_2023_message_data)

### Creates Data frame from survey data set, containing every combination of UUID and Date --------
reg_msg_data_2023 <- expand.grid(
  User_ID = unique(survey_data_2023$User_ID),
  Date = dates_2023
)


reg_msg_data_2023 <- reg_msg_data_2023 %>%
  left_join(filtered_2023_message_data, by = c("User_ID", "Date"))



reg_msg_data_2023 <- reg_msg_data_2023 %>%
  mutate(
    Msg_Received = ifelse(!is.na(Msg_Type), 1, 0),  # 1 if message exists, otherwise 0
    Msg_Type = ifelse(!is.na(Msg_Type), Msg_Type, "None"),  # Message type or "None"
    Msg_Seen = ifelse(read_notification == "t", 1, 0)  # 1 if read, 0 otherwise
  ) %>%
  select(User_ID, Date, Msg_Received, Msg_Type, Msg_Seen)



##  Processing Report Data -------------------------------------

filtered_2023_reports <- reports_data %>%    
  filter(User_ID %in% survey_data_2023$User_ID)

# Aggregating Reports by Date, User, and Report Type
aggregated_2023_reports <- filtered_2023_reports %>%
  filter(Rprt_Date >= as.Date("2023-05-01") & Rprt_Date <= as.Date("2023-10-31")) %>%
  group_by(User_ID, Rprt_Date, Rprt_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Rprt_Type, values_from = count, values_fill = list(count = 0)) %>%
  rename(Date = Rprt_Date)

# Creating a Regular Grid for Reports and Merging
reg_rprt_data_2023 <- expand.grid(
  User_ID = unique(survey_data_2023$User_ID),
  Date = dates_2023
)

reg_rprt_data_2023 <- reg_rprt_data_2023 %>%
  left_join(aggregated_2023_reports, by = c("User_ID", "Date")) %>%
  mutate(
    adult = coalesce(adult, 0),  # Fill missing values with 0
    bite = coalesce(bite, 0),
    site = coalesce(site, 0),
    total_reports = adult + bite + site,  # Sum of all report types
    Report = ifelse(total_reports > 0, 1, 0)  # Binary flag for report sent
  ) %>%
  select(User_ID, Date, adult, bite, site, total_reports, Report)  # Keep required columns only




## Merging Message and Report Data -----------------------------------------


reg_data_2023 <- full_join(reg_msg_data_2023, reg_rprt_data_2023, by = c("User_ID", "Date"))


## Adding Survey Information and Filtering -----------------------------------------

reg_data_2023 <- reg_data_2023 %>% 
  left_join(survey_data_2023 %>% dplyr::select(User_ID, Gender, Reg_Orientation_Cat, Reg_Orientation, Promotion, Prevention), by = "User_ID") %>% 
  filter(!is.na(Msg_Type))


# Creating the Orientation Nudge Agreement Variable and Final Transformations
reg_data_2023 <- reg_data_2023 %>%
  mutate(Orientation_Nudge_Agreement = case_when(
    Msg_Type == "Neutral" & Reg_Orientation_Cat == "Neutral" ~ 1,
    Msg_Type == "Eager" & Reg_Orientation_Cat == "Promotion" ~ 1,
    Msg_Type == "Vigilant" & Reg_Orientation_Cat == "Prevention" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(Msg_Type = as.factor(Msg_Type),
         Orientation_Nudge_Agreement = as.factor(Orientation_Nudge_Agreement),
         Msg_Received = as.factor(Msg_Received),
         User_ID  = as.factor(User_ID),
         Date = as.Date(Date)) %>%
  filter(Date >= as.Date("2023-05-01")) 
  




summary(reg_data_2023)

write.csv(reg_data_2023, "reg_data_2023.csv", row.names = FALSE)


rm(dates_2023, filtered_2023_message_data, reg_msg_data_2023,filtered_2023_reports,aggregated_2023_reports, reg_rprt_data_2023 )


## Creating date sequence for 2024 -----------------------------------------

dates_2024  <- seq.Date(as.Date("2024-05-01"), as.Date("2024-10-31"), by = "day")

## Preparing data on messaging/nudging -------------------------------------

### Filtering Message data to keep only those that were part of the survey --------
filtered_2024_message_data <- raw_message_data_2024 %>%
  filter(User_ID %in% survey_data_2024$User_ID) %>%
  mutate(Date = as.Date(Msg_Date)) %>%  
  filter(format(Date, "%Y") == "2024") %>% 
  select(-Msg_Date) %>% 
  rename(read_notification = read)

summary(filtered_2024_message_data)

### Creates Data frame from survey dataset, containing every combination of UUID and Date --------
reg_msg_data_2024 <- expand.grid(
  User_ID = unique(survey_data_2024$User_ID),
  Date = dates_2024
)

reg_msg_data_2024 <- reg_msg_data_2024 %>%
  left_join(filtered_2024_message_data, by = c("User_ID", "Date"))

reg_msg_data_2024 <- reg_msg_data_2024 %>%
  mutate(
    Msg_Received = ifelse(!is.na(type), 1, 0),  # 1 if message exists, otherwise 0
    Msg_Type = ifelse(!is.na(type), type, "None"),  # Message type or "None"
    Msg_Seen = ifelse(read_notification == "t", 1, 0)  # 1 if read, 0 otherwise
  ) %>%
  select(User_ID, Date, Msg_Received, Msg_Type, Msg_Seen)

## Processing Report Data -------------------------------------

filtered_2024_reports <- reports_data %>%
  filter(User_ID %in% survey_data_2024$User_ID)

# Aggregating Reports by Date, User, and Report Type
aggregated_2024_reports <- filtered_2024_reports %>%
  filter(Rprt_Date >= as.Date("2024-05-01") & Rprt_Date <= as.Date("2024-10-31")) %>%
  group_by(User_ID, Rprt_Date, Rprt_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Rprt_Type, values_from = count, values_fill = list(count = 0)) %>%
  rename(Date = Rprt_Date)

# Creating a Regular Grid for Reports and Merging
reg_rprt_data_2024 <- expand.grid(
  User_ID = unique(survey_data_2024$User_ID),
  Date = dates_2024
)

reg_rprt_data_2024 <- reg_rprt_data_2024 %>%
  left_join(aggregated_2024_reports, by = c("User_ID", "Date")) %>%
  mutate(
    adult = coalesce(adult, 0),  # Fill missing values with 0
    bite = coalesce(bite, 0),
    site = coalesce(site, 0),
    total_reports = adult + bite + site,  # Sum of all report types
    Report = ifelse(total_reports > 0, 1, 0)  # Binary flag for report sent
  ) %>%
  select(User_ID, Date, adult, bite, site, total_reports, Report)  # Keep required columns only

## Merging Message and Report Data -----------------------------------------

reg_data_2024 <- full_join(reg_msg_data_2024, reg_rprt_data_2024, by = c("User_ID", "Date"))

## Adding Survey Information and Filtering -----------------------------------------

reg_data_2024 <- reg_data_2024 %>% 
  left_join(survey_data_2024 %>% dplyr::select(User_ID, Gender, Reg_Orientation_Cat, Reg_Orientation, Promotion, Prevention), by = "User_ID") %>% 
  filter(!is.na(Msg_Type))

# Creating the Orientation Nudge Agreement Variable and Final Transformations  
reg_data_2024 <- reg_data_2024 %>%
  mutate(Orientation_Nudge_Agreement = case_when(
    Msg_Type == "Neutral" & Reg_Orientation_Cat == "Neutral" ~ 1,
    Msg_Type == "Eager" & Reg_Orientation_Cat == "Promotion" ~ 1,
    Msg_Type == "Vigilant" & Reg_Orientation_Cat == "Prevention" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(Msg_Type = as.factor(Msg_Type),
         Orientation_Nudge_Agreement = as.factor(Orientation_Nudge_Agreement),
         Msg_Received = as.factor(Msg_Received),
         User_ID  = as.factor(User_ID),
         Date = as.Date(Date)) %>%
  filter(Date >= as.Date("2024-05-01")) 

summary(reg_data_2024)

write.csv(reg_data_2024, "reg_data_2024.csv", row.names = FALSE)


rm(dates_2024, filtered_2024_message_data, reg_msg_data_2024,filtered_2024_reports,aggregated_2024_reports, reg_rprt_data_2024 )
