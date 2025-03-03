
# Difference Before and During Nudging ------------------------------------



## ALL ---------------------------------------------------------------------
  

### Reporting Difference During and Before Intervention Period --------------

with(data, t.test(Rprts_During_Msging, Rprts_Before_Msging,  paired = TRUE))

# mean difference 1.6 ; t = 3.0282, df = 599, p-value = 0.002566 Significant increase during intervention than before 


### Reporting Difference  During and After Intervention Period --------------

with(data, t.test(Rprts_During_Msging, Rprts_After_Msging,  paired = TRUE))

#mean difference 0.8766667 ; t = 1.6264, df = 599, p-value = 0.1044 Insignificant decrease

# Putting all Participants in both  years together, during intervention period they experience a significant increase in amount of reporting
# this average goes down again after the intervention stops but the drop is insignificant  