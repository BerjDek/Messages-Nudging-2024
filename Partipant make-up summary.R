
# Participant Make-up -----------------------------------------------------


# 2024 --------------------------------------------------------------------

summary(data_2024)



data_2024 <- data_2024 %>%
  mutate(
    Gender = as.factor(Gender),
    Country = as.factor(Country),
    Participation_Date = as.factor(Participation_Date),
    Reg_Orientation_Cat = as.factor(Reg_Orientation_Cat),
    Msg_Type = as.factor(Msg_Type),
    Msg_Lang = as.factor(Msg_Lang),
    Message_Group = as.factor(Message_Group),
    First_Msg_Date = as.Date(First_Msg_Date),
    Last_Msg_Date = as.Date(Last_Msg_Date),
    Registered_Participation_Date = as.Date(Registered_Participation_Date)
  )


summary(data_2024)


# 382 participants; 194 Male to 150 Female; Spain 205 Italy 73 Neatherlands 18; for year of initial participation
# 2023 with  151 was the highest (instead of 2024)  2022  with 72  2024 with 50 2021 with 26; equal treatment sizes 121 for eager and vigilant
# 120 for neutral


# Calculating percentage of male participation in each Msg_Type group

data_2024 %>%
  group_by(Msg_Type, Gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  spread(key = Gender, value = count, fill = 0) %>%
  print() %>%  
  mutate(
    total = `Female` + `Male` + `I prefer not to say` + `Non-binary` + `Other`,  
    male_percentage = (`Male` / total) * 100
  ) 



chisq.test(table(data_2024$Msg_Type, data_2024$Gender))

# age
data_2024 %>%
  group_by(Msg_Type) %>%
  summarise(average_age = mean(Age, na.rm = TRUE))

summary(aov(Age ~ Msg_Type, data = data_2024))



# The number of male participants was similar across the three treatment conditions (65 neu, 64 pre, 65 pro) Chi squared value of 0.1455, 
# so was teh average age 48.2 Neu 49.4 Pre 50 Pro P= 0.606






