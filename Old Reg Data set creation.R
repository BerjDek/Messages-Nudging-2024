
# Creating data set for regression analysis -------------------------------



dates_2023  <- seq.Date(as.Date("2023-05-01"), as.Date("2023-10-31"), by = "day") #create dates for every day in 2023 season


# extract reports filled by 2023 participants in season 2023
par_2023_reports <- reports_data %>% 
  filter(User_ID %in% survey_data_2023$User_ID) %>% 
  filter(year(as.Date(Rprt_Date)) == 2023) %>%
  mutate(Rprt_Type = as.character(Rprt_Type)) %>% 
  rename(Date = Rprt_Date) %>% 
  filter(Date >= "2023-05-01"& Date <= "2023-10-31")


# extract messages received by 2023 participants in season 2023
par_2023_nudges <- raw_message_data_2023 %>%
  filter(year(Msg_Date) == 2023) %>% 
  dplyr::select(-Msg_Lang,-msg_nmbr,-Repeat_User,-id) %>% 
  mutate(read_notification= as.integer(read_notification == "t"), Msg_Date = as.Date(Msg_Date)) %>% 
  dplyr::select(User_ID, Msg_Date, type, read_notification, Msg_Nmbr) %>% 
  rename(Msg_Type = type, Msg_Seen = read_notification, Date = Msg_Date) %>% 
  filter(Date >= "2023-05-01"& Date <= "2023-10-31")



#expand to every day in the season
par_2023_reports_expanded <- expand.grid(User_ID = unique(par_2023_reports$User_ID), Date = dates_2023)
par_2023_nudges_expanded <- expand.grid(User_ID = unique(par_2023_nudges$User_ID), Date = dates_2023)

 
season_2023_daily_reports <- merge(par_2023_reports_expanded, par_2023_reports, by = c("User_ID", "Date"), all = TRUE)
season_2023_daily_nudges <- merge(par_2023_nudges_expanded, par_2023_nudges, by = c("User_ID", "Date"), all = TRUE)


season_2023_daily_reports$Rprt_Type[is.na(season_2023_daily_reports$Rprt_Type)] <- "None"
season_2023_daily_nudges$Msg_Seen[is.na(season_2023_daily_nudges$Msg_Seen)] <- 0
season_2023_daily_nudges$Msg_Type[is.na(season_2023_daily_nudges$Msg_Type)] <- "None"
season_2023_daily_nudges$Msg_Nmbr[is.na(season_2023_daily_nudges$Msg_Nmbr)] <- 0        #should be moved down




season_2023_daily_reports_nudges<- left_join(season_2023_daily_reports, season_2023_daily_nudges, by = c("User_ID", "Date"))

season_2023_daily_reports_nudges <- season_2023_daily_reports_nudges %>% 
  mutate(Rprt_Filled = ifelse(Rprt_Type != "None", 1, 0), Msg_Received = ifelse(Msg_Type != "None", 1, 0))


season_2023_daily_reports_nudges$Rprt_Type <- as.factor(season_2023_daily_reports_nudges$Rprt_Type)
season_2023_daily_reports_nudges$Msg_Type <- as.factor(season_2023_daily_reports_nudges$Msg_Type)


# season_2023_daily_reports_nudges shows for each user, if on any day of the season, they have filled a report (along with its type), or recieved
# a nudge (along with its type)




report_msg_wide <- season_2023_daily_reports_nudges %>%
  mutate(report_indicator = as.integer(1)) %>%
  group_by(User_ID, Date, Rprt_Type) %>%
  summarize(report_indicator = sum(report_indicator, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Rprt_Type, 
    values_from = report_indicator, 
    values_fill = list(report_indicator = 0)  
  ) %>% 
  mutate(
    total_reports = adult + bite + site,
    Report = as.numeric(!None)) %>% 
  dplyr::select(-None)



msg_subset <- season_2023_daily_reports_nudges %>%
  group_by(User_ID, Date) %>%
  slice(1) %>%
  dplyr::select(User_ID, Date, Msg_Received, Msg_Type, Msg_Seen) %>%
  ungroup()



report_msg_wide <- report_msg_wide %>%
  left_join(msg_subset, by = c("User_ID", "Date"))


report_msg_wide <- report_msg_wide %>% 
  left_join(survey_data_2023 %>% dplyr::select(User_ID, Gender, Reg_Orientation_Cat, Reg_Orientation, Promotion, Prevention), by = "User_ID") %>% 
  filter(!is.na(Msg_Type))


#adding a column for nudge and orientation agreement
report_msg_wide <- report_msg_wide %>%
  mutate(Orientation_Nudge_Agreement = case_when(
    Msg_Type == "Neutral" & Reg_Orientation_Cat == "Neutral" ~ 1,
    Msg_Type == "Eager" & Reg_Orientation_Cat == "Promotion" ~ 1,
    Msg_Type == "Vigilant" & Reg_Orientation_Cat == "Prevention" ~ 1,
    TRUE ~ 0
  ))


#testing

rm(dates_2023, par_2023_reports, par_2023_nudges, 
   par_2023_reports_expanded, par_2023_nudges_expanded, 
   season_2023_daily_reports, season_2023_daily_nudges, 
   season_2023_daily_reports_nudges, report_msg_wide, 
   msg_subset, model)

