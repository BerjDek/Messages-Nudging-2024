Regression All Days/5

### Does nudging increase the total number (intensity) of reporting ------------------------------



summary(glmer(
  Report ~ Msg_Received + Day_of_Year + (1 + Day_of_Year | User_ID),
  family = binomial, data = reg_data_2023
)) 

# (Intercept)   -3.561743   0.109038 -32.665  < 2e-16 ***
#   Msg_Received1  0.541093   0.071711   7.546 4.51e-14 ***
#   Day_of_Year   -0.037702   0.005083  -7.418 1.19e-13 ***



### Does nudging increase the total number (intensity) of reporting ------------------------------




summary(glmer(total_reports ~ Msg_Received + (1 + Day_of_Year|User_ID), 
              data = reg_data, 
              family = poisson)) 

# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -3.79142    0.07988  -47.47   <2e-16 ***
#   Msg_Received1  0.36439    0.03339   10.91   <2e-16 ***


summary(glmer(total_reports ~ Msg_Received + Day_of_Year + (1 + Day_of_Year|User_ID), 
              data = reg_data, 
              family = poisson)) 

# (Intercept)   -3.857446   0.077396  -49.84   <2e-16 ***
#   Msg_Received1  0.370321   0.033407   11.09   <2e-16 ***
#   Day_of_Year   -0.033741   0.003401   -9.92   <2e-16 ***



### Does the framing of  the nudge matter ----------------------

levels(reg_data$Msg_Type)
reg_data$Msg_Type <- relevel(reg_data$Msg_Type, ref = "None") #set the level to none


summary(glmer(Report ~ Msg_Type + (1 + Day_of_Year|User_ID),
              data = reg_data,
              family = binomial(link = "logit")))

summary(glmer(Report ~ Msg_Type + Day_of_Year + (1 + Day_of_Year|User_ID),
              data = reg_data,
              family = binomial(link = "logit")))



summary(glmer(Report ~ Msg_Type + (1 + Day_of_Year | User_ID),
              data = reg_data_2023,
              family = binomial(link = "logit")))
