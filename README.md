# RoadMap

## Exportfiles:
- [ ] Upgrade Mira export file creation to take into account the case where several conditions are present. (Pending for Q to act)
- [ ] Mira: clean the table to remove bad unit before working with it. Will make code more readable.
- [ ] Ace TCR/deg files should be robust with regard of first line (iteration=...)
- [ ] NBTI exportfile to script.
- [ ] Rewrite for loops in the functions in order to enhance readability.


## Model:
#### EM:
- [ ] Create a function that estimate the lifetime of the device. par: Scale, Ea, n, A, DeviceID, ConditionTable. Make it stand alone if user want to use it to check something. And add a call in BlackModelization (optional parameter). Could be used to estimate quickly time for a new experiment.


#### TDDB
- [ ] Test TDDB modelization when several area are provided.
- [ ] Compare datacruncher results with amsRel for TDDB.

#### Common:
- [ ] Robust modelisation: fit 1 followed by a study on outliners leading to a second fit (Cook's distance).
- [ ] Implement hierachical models (mixed effect models) to check if they can be useful for self heating interference.
- [ ] Force fit option: avoid fitting again if the parameters are available
- [ ] Implement Kaplan-Meier and Exponential Greenwood Confidence Intervals
- [ ] Look into J.G. Stotvig codes. fitdistr is failing when censored samples are provided. Take advantage of know probability.
- [ ] Student's t-test function in order to analyze populations (eg: impact of lenght, or tool or wafer)
- [ ] Ficher's F-test to add to previous function.
- [ ] Copy the structure file locally (dimension of EM structures) and make comparison with time stamps. Would allow working without network.
- [ ] Find a way to store model in a file and directly call them. Avoid hard coding the models in the functions. Avoid redundancy (error prone). Ex: Black and TDDB lifetime.
- [ ] plyr strategie (split apply combine) on data with multiple structures inside the table?
- [ ] avoid loop in function where a list of files is given. Try to use apply



## Chart:
- [ ] Enhance legend presentation to include:
  - [ ] MTTF, Number of samples + number of censored samples<-- Create a table for this like in Minitab
  - [ ] Model parameters <- addressed with annotate + clipping off (2nd table)
  - [x] Minimum 3 decades per charts
  - [x] Revert renaming CreateGraph function.
  - [ ] Create new function for axis setting definition: deg scale or proba scale / linear or log scale

## Documentation:
