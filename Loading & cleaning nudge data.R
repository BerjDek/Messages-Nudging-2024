
# Loading and cleaning nudging data ---------------------------------------
message_data_2023 <- read.csv("CleanMessageData_2023.csv")
message_data_2024 <- read.csv("CleanMessageData_2024.csv")

## Messages from 2023 ------------------------------------------------------


raw_message_data_2023 <- read.csv(file="all_messages_2023.csv", header = TRUE)
#separate notification column to type language and msg_nmbr
raw_message_data_2023 <- raw_message_data_2023 %>% separate(notification_label, c('type','language','msg_nmbr'))
#change the message date to simple date
raw_message_data_2023 <- raw_message_data_2023 %>% 
  rename(User_ID = user_uuid, Msg_Date = date_sent, Msg_Lang = language, Msg_Type = type) %>% 
  mutate(Msg_Nmbr = as.integer(msg_nmbr),
         Msg_Date = as.POSIXct(Msg_Date, format = "%Y-%m-%d %H:%M:%S"),
         Year = format(Msg_Date, "%Y"))%>%
  group_by(User_ID) %>%
  mutate(Repeat_User = n_distinct(Year) > 1)%>%
  mutate(Msg_Type = tools::toTitleCase(Msg_Type)) %>% 
  mutate(Msg_Type = recode(Msg_Type, "Prevention" = "Vigilant", "Promotion" = "Eager"))%>% 
  ungroup() 





message_data_2023 <- raw_message_data_2023 %>%
  filter(year(Msg_Date) == 2023) %>% 
  group_by(User_ID) %>%
  summarize(
    Msg_Type = first(Msg_Type),
    Msg_Lang = first(Msg_Lang),
    First_Msg_Date = as.POSIXct(format(min(Msg_Date), "%Y-%m-%d")),
    Last_Msg_Date = as.POSIXct(format(max(Msg_Date), "%Y-%m-%d")),
    Nmbr_Msgs_Sent = n(),
    Nmbr_Msgs_Seen = sum(read_notification == "t"),
    Msg_Duration_Days = as.integer(Last_Msg_Date - First_Msg_Date, units = "days")
  ) %>%
  ungroup() %>%
  mutate(Msg_Type = as.factor(Msg_Type))




#creating Messaging group based on date of first message received.
message_data_2023 <- message_data_2023 %>%
  mutate(
    Message_Group = case_when(
      month(First_Msg_Date) == 6 & day(First_Msg_Date) <= 14 ~ "A-June1",
      month(First_Msg_Date) == 6 & day(First_Msg_Date) > 14 ~ "B-June2",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) <= 14 ~ "C-July1",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) > 14 ~ "D-July2",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) <= 14 ~ "E-Aug1",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) > 14 ~ "F-Aug2",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) <= 14 ~ "G-Sept1",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) > 14 ~ "H-Sept2",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) <= 14 ~ "I-Oct1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "J-Oct2",
      month(First_Msg_Date) == 11 & day(First_Msg_Date) <= 14 ~ "K-Nov1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "L-Nov2",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(Message_Group = as.factor(Message_Group)) 



#The final count of users are 238 users that received messages, with 79 participants in each group and 80 for prevention orientation.
#The number corresponds with the Unique users that have initiated the survey and gave their consent


write.csv(message_data_2023, "G:/My Drive/Article about Messages/Messages-Nudging-2024/CleanMessageData_2023.csv", row.names = FALSE)



## Messages from2024 --------------------------------------------------------------------

#load message Data
raw_message_data_2024 <- read.csv(file="all_messages_2024.csv", header = TRUE)

raw_message_data_2024 <- raw_message_data_2024 %>% separate(notification_label, c('type','language','msg_nmbr'))

raw_message_data_2024 <- raw_message_data_2024 %>% 
  rename(User_ID = user_uuid, Msg_Date = date_sent, Msg_Lang = language) %>% 
  mutate(Msg_Nmbr = as.integer(msg_nmbr),
         Msg_Date = as.POSIXct(Msg_Date, format = "%Y-%m-%d %H:%M:%S"),
         Year = format(Msg_Date, "%Y")) %>%
  group_by(User_ID) %>%
  mutate(Repeat_User = n_distinct(Year) > 1)%>%
  mutate(type = tools::toTitleCase(type)) %>% 
  ungroup() 




message_data_2024 <- raw_message_data_2024 %>%
  filter(!is.na(Msg_Nmbr)) %>%                       #this column changed from 2023 code, since instead of haveing messages from multile years, we have a poll for those in St.Luis that need to be exluded
  group_by(User_ID) %>%
  summarize(
    Msg_Type = first(type),
    Msg_Lang = first(Msg_Lang),
    First_Msg_Date = as.POSIXct(format(min(Msg_Date), "%Y-%m-%d")),
    Last_Msg_Date = as.POSIXct(format(max(Msg_Date), "%Y-%m-%d")),
    Nmbr_Msgs_Sent = n(),
    Nmbr_Msgs_Seen = sum(read == "t"),                                             # changed from 2023 column read_notifications to read
    Msg_Duration_Days = as.integer(Last_Msg_Date - First_Msg_Date, units = "days")
  ) %>%
  ungroup() %>%
  mutate(Msg_Type = as.factor(Msg_Type))




#creating Messaging group based on date of first message received.
message_data_2024 <- message_data_2024 %>%
  mutate(
    Message_Group = case_when(
      month(First_Msg_Date) == 5 ~ "A-June1",                                #added since there is one message at end of May
      month(First_Msg_Date) == 6 & day(First_Msg_Date) <= 14 ~ "A-June1",
      month(First_Msg_Date) == 6 & day(First_Msg_Date) > 14 ~ "B-June2",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) <= 14 ~ "C-July1",
      month(First_Msg_Date) == 7 & day(First_Msg_Date) > 14 ~ "D-July2",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) <= 14 ~ "E-Aug1",
      month(First_Msg_Date) == 8 & day(First_Msg_Date) > 14 ~ "F-Aug2",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) <= 14 ~ "G-Sept1",
      month(First_Msg_Date) == 9 & day(First_Msg_Date) > 14 ~ "H-Sept2",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) <= 14 ~ "I-Oct1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "J-Oct2",
      month(First_Msg_Date) == 11 & day(First_Msg_Date) <= 14 ~ "K-Nov1",
      month(First_Msg_Date) == 10 & day(First_Msg_Date) > 14 ~ "L-Nov2",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(Message_Group = as.factor(Message_Group))

  
summary(message_data_2024)

#The final count of users are 362 users that received messages, with 120 participants in  Neutral group and 121 for prevention and Promotion orientation.
#The number corresponds with the Unique users that have initiated the survey and gave their consent



#since the number was different I checked why th3re were 2 who did not messages, "6c7e2143-1c46-42df-9d65-dd78b6fce431" and 
# 4272eff3-4fef-4235-a34a-2254c7e0143d both flled the survey in November
setdiff(
  raw_survey_data_2024 %>%
    filter(Consent == "Yes" & nzchar(User_ID)) %>%
    pull(User_ID),
  
  message_data_2024 %>%
    pull(User_ID)
)


write.csv(message_data_2024, "CleanMessageData_2024.csv", row.names = FALSE)


rm(raw_message_data_2024)

