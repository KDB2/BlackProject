# RoadMap

## Exportfiles:
- [ ] Upgrade Mira export file creation to take into account the case where several conditions are present. (Pending for Q to act)
- [ ] Mira: clean the table to remove bad unit before working with it. Will make code more readable.
- [ ] Ace TCR/deg files should be robust with regard of first line (iteration=...)
- [ ] NBTI exportfile to script.
- [ ] Rewrite for loops in the functions in order to enhance readability.


## Model:
#### EM:
- [x] Handle case where W, H and S are not available for the modelization (i.e. file not found).
- [x] Modify ReadDataAce to remove the scale parameter. Should be fixed to Lognormal for Electromigration.
- [x] Modify CreateDataFrame to store dimension of the device. Preparation for TDDB where Area is needed.
- [x] Rethink BlackAnalysis to use the DeviceID stored in the exportfile and not the one from the title. --> File is generated by script and title is supposed to be accurate.
- [ ] Create a function that estimate the lifetime of the device. par: Scale, Ea, n, A, DeviceID, ConditionTable. Make it stand alone if user want to use it to check something. And add a call in BlackModelization (optional parameter). Could be used to estimate quickly time for a new experiment.
- [ ] Divide Modelization in sub function. One for Modelizing and one for calculating lifetime.  

#### TDDB
- [x] Start development for TDDB
- [x] Condition values for TDDB experiments: Voltage/temp. If several area are mixed, add area at the end. (create dataframe, search levels dimension. If lenght(levels)>1, paste old name+area)
- [x] TDDB is Negative if breakdown too fast. Take care of this.
- [x] Apply fix for EM and non ordered factors to TDDB chart (sort the condition list).
- [ ] Test TDDB modelization when several area are provided.
- [ ] Compare datacruncher results with amsRel for TDDB.

#### Common:
- [ ] Robust modelisation: fit 1 followed by a study on outliners leading to a second fit (Cook's distance).
- [ ] Implement hierachical models (mixed effect models) to check if they can be useful for self heating interference.
- [ ] Force fit option: avoid fitting again if the parameters are available
- [x] Error function is not providing full error bands --> Weibull scale was not implemented correctly
- [ ] Implement Kaplan-Meier and Exponential Greenwood Confidence Intervals
- [ ] Fix situation where some unfinished units have a lower TTF than finished ones. (typical in EM exp) --> See J.G. Stotvig report.
- [x] When modelization is not possible, use fitdistr to return parameters of the distro and to generate a model.
- [ ] Improve previous solution with J.G. Stotvig codes. fitdistr is failing when censored samples are provided. Take advantage of know probability.
- [ ] Student's t-test function in order to analyze populations (eg: impact of lenght, or tool or wafer)
- [ ] Ficher's F-test to add to previous function.
- [ ] Copy the structure file locally and make comparison with time stamps. Would allow working without network.
- [ ] 


## Chart:
- [ ] Enhance legend presentation to include:
  - [ ] MTTF, Number of samples + number of censored samples<-- Create a table for this like in Minitab
  - [ ] Model parameters <- addressed with annotate + clipping off (2nd table)
  - [x] Minimum 3 decades per charts
  - [ ] Revert renaming CreateGraph function. Create new option for y axis setting: deg scale or proba scale.

## Documentation:
