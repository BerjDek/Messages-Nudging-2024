
#  Regression -------------------------------------------------------------




# Loading data sets -------------------------------------------------------
reg_data_2023 <- read.csv("reg_data_2023.csv")%>%
  mutate(
    User_ID = as.factor(User_ID),
    Msg_Type = as.factor(Msg_Type),
    Gender = as.factor(Gender),
    Date = as.Date(Date))

reg_data_2023$Day_of_Year <- (yday(reg_data_2023$Date)- 211)/30

reg_data_2023$DayIndex <- as.integer(
  difftime(reg_data_2023$Date, "2023-04-30", units = "days")
) + 1   
 
    
reg_data_2024 <- read.csv("reg_data_2024.csv")

reg_data <- bind_rows(reg_data_2023, reg_data_2024)


summary(reg_data_2023)
summary(reg_data_2024)

# Note you can remove the users that have not fillled a single report in 2023 as it doesnt really chage the results
# for now i kept it with the full data

# reg_data_2023_2 <- reg_data_2023 %>%
#   filter(!User_ID %in% c("5280c75f-af25-4f30-a417-8341f353adae", "85b119ce-b2e3-4085-8360-6ab7625d1f39", 
#                          "5d5c6982-2fdd-4a87-9483-aeac44652efb", "df897a53-bad6-457c-8bd9-0f2eb997d894", 
#                          "0fb36846-417e-49f8-8c09-92a35edbd033", "6d7eef2e-328c-4e60-bff6-d1087c799d65", 
#                          "311b06f1-6df8-4154-a0e0-b1ab3692dd42", "5856c91b-96bd-4f96-a177-9cfdf351892a", 
#                          "9a00ace5-2683-4657-a5af-f00fca5c8cf0", "c44acb81-4dc6-47b7-bac4-730dbae562ea", 
#                          "969623f8-25f7-41b8-899d-5103cdf127d1", "3f9477af-313b-4499-964d-4737caf75dab", 
#                          "872d490b-5923-4a0e-453410f8c061", "920c8e2d-12d2-4969-88a4-ab3f5e4fdfa4", 
#                          "fccc8ffc-7b41-415f-86e4-8ddc837f7387", "6eb952d7-feb3-4f7d-98d7-67f89d78c75f", 
#                          "4cff1cf3-a005-479e-95c4-de638c7c353f", "46a17093-29c6-4ca3-a001-651285fd87a0", 
#                          "e0d2a6b9-d37a-4666-a6d9-15bb26fce1b5", "e7d57de8-af8e-46d4-92f7-68399d576014", 
#                          "2b82b6f6-e05b-4162-bd12-e1366b017c66", "77a006fd-88e8-44dd-a71d-5ebf53964f86", 
#                          "36a6cb71-6d56-415b-9dd8-2bf25f744251"))




## 2023 --------------------------------------------------------------------


### Does Nudging (general) increase the chance of reporting? ------------------------------
summary(glmer(Report ~ Msg_Received + (1 + Day_of_Year | User_ID),
               data = reg_data_2023,
               family = binomial))

summary(glmer(
  Report ~ Msg_Received + Day_of_Year + (1 + Day_of_Year | User_ID),
  family = binomial, data = reg_data_2023
))  ## for teh explanation of this model go to google docs, messages-nudging-2024, titled model explanation


# Receiving a nudge significantly increases the probability of reporting (β = 0.524, p < 0.001, exact p = 7.66e-13)




### Does nudging increase the total number (intensity) of reporting ------------------------------


summary(glmer(total_reports ~ Msg_Received + (1 + Day_of_Year|User_ID), 
               data = reg_data_2023, 
               family = poisson)) 



summary(glmer(total_reports ~ Msg_Received + Day_of_Year + (1 + Day_of_Year | User_ID), 
              data = reg_data_2023, 
              family = poisson)) 


# receiving a nudge significantly increases the total number of reports (β = 0.338, p < 0.001, exact p = 1.05e-13).


### Does the framing of  the nudge matter ----------------------

levels(reg_data_2023$Msg_Type)
reg_data_2023$Msg_Type <- relevel(reg_data_2023$Msg_Type, ref = "None") #set the level to none




summary(glmer(Report ~ Msg_Type + (1 | User_ID),
              data = reg_data_2023,
              family = binomial(link = "logit")))



summary(glmer(Report ~ Msg_Type + (1 + Day_of_Year | User_ID),
               data = reg_data_2023,
               family = binomial(link = "logit")))


summary(glmer(Report ~ Msg_Type + Day_of_Year + (1 + Day_of_Year | User_ID),
              data = reg_data_2023,
              family = binomial(link = "logit")))

# all framed nudges significantly increase the probability of reporting compared to no nudge, with the Vigilant frame having 
# the strongest effect (β = 0.640, p < 0.001, exact p = 2.35e-08), followed by Eager (β = 0.563, p < 0.001, exact p = 1.61e-05) 
# and Neutral (β = 0.326, p = 0.0182). 



### Does agreement between framing and regulatory orientation impact reporting ----------------------

#### Vigilant/Prevention ----------------------
summary(glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
               data = reg_data_2023[reg_data_2023$Reg_Orientation_Cat == "Prevention", ], 
               family = binomial(link = "logit")))



summary(glmer( Report ~ Orientation_Nudge_Agreement + Day_of_Year + (1 + Day_of_Year | User_ID), 
              data = reg_data_2023[reg_data_2023$Reg_Orientation_Cat == "Prevention", ], 
              family = binomial(link = "logit")))

#new result near significance for vigilant 0.0822  and increases
# new results insignificant p = 0.203 
# estimated log-odds increase of 0.32 (SE = 0.159, p = 0.044)


#### Eager/Promotion ----------------------
summary(glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
               data = reg_data_2023[reg_data_2023$Reg_Orientation_Cat == "Promotion", ], 
               family = binomial(link = "logit")))

summary(glmer(Report ~ Orientation_Nudge_Agreement + Day_of_Year + (1 + Day_of_Year | User_ID),
              data = reg_data_2023[reg_data_2023$Reg_Orientation_Cat == "Promotion", ], 
              family = binomial(link = "logit")))

# insiginificant 0.92
# new results STILL insignificant p=  0.265
# estimated log-odds of -0.53 (SE = 0.476, p = 0.269)

#### Neutral/Neutral (not sure if important) ----------------------
summary(glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
               data = reg_data_2023[reg_data_2023$Reg_Orientation_Cat == "Neutral", ], 
               family = binomial(link = "logit")))


summary(glmer(Report ~ Orientation_Nudge_Agreement + Day_of_Year + (1 + Day_of_Year | User_ID),
              data = reg_data_2023[reg_data_2023$Reg_Orientation_Cat == "Neutral", ], 
              family = binomial(link = "logit")))

#insiginificant with outliers estimated log-odds of -0.12 (SE = 0.294, p = 0.692)




### try this to see if agreement matters for all

data_with_message <- subset(
  reg_data_2023, 
  Msg_Received == 1  # or however you mark "a message was received"
)

summary(glmer(
  Report ~ Orientation_Nudge_Agreement + (1 | User_ID),
  data = data_with_message,
  family = binomial
))

summary(glmer(
  Report ~ Reg_Orientation_Cat * Orientation_Nudge_Agreement + (1 | User_ID),
  data = data_with_message,
  family = binomial
))



summary(glmer(
  Report ~ Orientation_Nudge_Agreement + (1 | User_ID),
  data = subset(data_with_message, Reg_Orientation_Cat == "Prevention"),
  family = binomial
))


summary(glmer(
  Report ~ Orientation_Nudge_Agreement + (1 | User_ID),
  data = subset(data_with_message, Reg_Orientation_Cat == "Neutral"),
  family = binomial
))

summary(glmer(
  Report ~ Orientation_Nudge_Agreement + (1 | User_ID),
  data = subset(data_with_message, Reg_Orientation_Cat == "Promotion"),
  family = binomial
))


## 2024 --------------------------------------------------------------------


### Does Nudging (general) increase the chance of reporting? ------------------------------
summary(glmer(Report ~ Msg_Received + (1 + Date | User_ID),
              data = reg_data_2024,
              family = binomial))


# Receiving a nudge significantly increases the probability of reporting (β = 0.524, p < 0.001, exact p = 7.66e-13)




### Does nudging increase the total number (intensity) of reporting ------------------------------


summary(glmer(total_reports ~ Msg_Received + (1 + Date|User_ID), 
              data = reg_data_2024, 
              family = poisson)) 





# receiving a nudge significantly increases the total number of reports (β = 0.338, p < 0.001, exact p = 1.05e-13).


### Does the framing of  the nudge matter ----------------------

levels(reg_data_2024$Msg_Type)
reg_data_2024$Msg_Type <- relevel(reg_data_2024$Msg_Type, ref = "None") #set the level to none


summary(glmer(Report ~ Msg_Type + (1 + Date | User_ID),
              data = reg_data_2024,
              family = binomial(link = "logit")))



# all framed nudges significantly increase the probability of reporting compared to no nudge, with the Vigilant frame having 
# the strongest effect (β = 0.640, p < 0.001, exact p = 2.35e-08), followed by Eager (β = 0.563, p < 0.001, exact p = 1.61e-05)   
# and Neutral (β = 0.326, p = 0.0182). 



### Does agreement between framing and regulatory orientation impact reporting ----------------------

#### Vigilant/Prevention ----------------------
summary(glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
              data = reg_data_2024[reg_data_2024$Reg_Orientation_Cat == "Prevention", ], 
              family = binomial(link = "logit")))

#new result near significance for vigilant 0.0822  and increases
# new results insignificant p = 0.203 
# estimated log-odds increase of 0.32 (SE = 0.159, p = 0.044)


#### Eager/Promotion ----------------------
summary(glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
              data = reg_data_2024[reg_data_2024$Reg_Orientation_Cat == "Promotion", ], 
              family = binomial(link = "logit")))

# insiginificant 0.92
# new results STILL insignificant p=  0.265
# estimated log-odds of -0.53 (SE = 0.476, p = 0.269)

#### Neutral/Neutral (not sure if important) ----------------------
summary(glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
              data = reg_data_2024[reg_data_2024$Reg_Orientation_Cat == "Neutral", ], 
              family = binomial(link = "logit")))

#insiginificant with outliers estimated log-odds of -0.12 (SE = 0.294, p = 0.692)




## All  --------------------------------------------------------------------


### Does Nudging (general) increase the chance of reporting? ------------------------------
summary(glmer(Report ~ Msg_Received + (1 + Date | User_ID),
              data = reg_data,
              family = binomial))


# Receiving a nudge significantly increases the probability of reporting (β = 0.524, p < 0.001, exact p = 7.66e-13)




### Does nudging increase the total number (intensity) of reporting ------------------------------


summary(glmer(total_reports ~ Msg_Received + (1 + Date|User_ID), 
              data = reg_data, 
              family = poisson)) 





# receiving a nudge significantly increases the total number of reports (β = 0.338, p < 0.001, exact p = 1.05e-13).


### Does the framing of  the nudge matter ----------------------

levels(reg_data$Msg_Type)
reg_data$Msg_Type <- relevel(reg_data$Msg_Type, ref = "None") #set the level to none


summary(glmer(Report ~ Msg_Type + (1 + Date | User_ID),
              data = reg_data,
              family = binomial(link = "logit")))



# all framed nudges significantly increase the probability of reporting compared to no nudge, with the Vigilant frame having 
# the strongest effect (β = 0.640, p < 0.001, exact p = 2.35e-08), followed by Eager (β = 0.563, p < 0.001, exact p = 1.61e-05)   
# and Neutral (β = 0.326, p = 0.0182). 



### Does agreement between framing and regulatory orientation impact reporting ----------------------

#### Vigilant/Prevention ----------------------
summary(glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
              data = reg_data[reg_data$Reg_Orientation_Cat == "Prevention", ], 
              family = binomial(link = "logit")))

#new result near significance for vigilant 0.0822  and increases
# new results insignificant p = 0.203 
# estimated log-odds increase of 0.32 (SE = 0.159, p = 0.044)


#### Eager/Promotion ----------------------
summary(glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
              data = reg_data[reg_data$Reg_Orientation_Cat == "Promotion", ], 
              family = binomial(link = "logit")))

# insiginificant 0.92
# new results STILL insignificant p=  0.265
# estimated log-odds of -0.53 (SE = 0.476, p = 0.269)

#### Neutral/Neutral (not sure if important) ----------------------
summary(glmer(Report ~ Orientation_Nudge_Agreement + (1 + Date|User_ID), 
              data = reg_data[reg_data$Reg_Orientation_Cat == "Neutral", ], 
              family = binomial(link = "logit")))

#insiginificant with outliers estimated log-odds of -0.12 (SE = 0.294, p = 0.692)
