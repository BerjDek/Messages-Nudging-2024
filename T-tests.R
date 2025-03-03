
# seasonal reporting behavior change  -------------------------------------


## Paired t-test comparison for survey takers 2023 --------------------------------------------



### Difference between  2022  &  2023 ----------
data_2023 %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))

# mean difference 12.14184 t = 3.0093, df = 140, p-value = 0.003106 Significant


    

### Difference between  2021 & 2022  -------

data_2023 %>%
  filter(Registered_Participation_Date < as.Date('2022-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2022, Season_Rprts_Filled_2021, paired = TRUE))


# mean difference -0.5352113; t = -0.13508, df = 70, p-value = 0.8929 Less impressive, reduction no significance







## Paired t-test comparison for survey takers 2024--------------------------------------------

### Difference between  2023  &  2024 ----------

data_2024 %>%
  filter(Registered_Participation_Date < as.Date('2024-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2024, Season_Rprts_Filled_2023, paired = TRUE))

# mean difference 0.785 ; t = 0.778, df = 297, p-value = 0.4372  Slight increase, which is NON SIGNIFICANT




### Difference between  2021 & 2022  -------

data_2024 %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))

# mean difference -2.04918 ; t = -2.8053, df = 60, p-value = 0.006765  heavier reduction than before that IS significant




## No Intervention ----------

###  Difference between  2022  &  2023 ----------

reports_data_all %>%
  filter(Registered_Participation_Date < as.Date('2023-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2023, Season_Rprts_Filled_2022, paired = TRUE))

# mean difference -0.7272123  ; t = -27.369, df = 30081, p-value < 2.2e-16  Significant Reduction



### Difference between  2023  &  2024 ----------
reports_data_all %>%
  filter(Registered_Participation_Date < as.Date('2024-01-01')) %>%
  with(t.test(Season_Rprts_Filled_2024, Season_Rprts_Filled_2023, paired = TRUE))

# mean difference -1.087651   ; t = -53.946, df = 54773, p-value < 2.2e-16  Significant Reduction
