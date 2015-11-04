# RoadMap

## Exportfiles:
- Upgrade Mira export file creation to take into account the case where several conditions are present.
- Mira: clean the table to remove bad unit before working with it. Will make code more readable.
- NBTI exportfile to script.

## Model:
- [fixed] Handle case where W, H and S are not available for the modelization (i.e. file not found).
- [fixed] When modelization is not possible, use fitdistr to return parameters of the distro and to generate a model.
- [fixed] Modify ReadDataAce to remove the scale parameter. Should be fixed to Lognormal for Electromigration.
- [fixed] Modify CreateDataFrame to store dimension of the device. Preparation for TDDB where Area is needed.
- Robust modelisation: fit 1 followed by a study on outliners leading to a second fit (Cook's distance).
- Let's check hierachical models to check if they can be useful for self heating interference.
- Create a function that estimate the lifetime of the device. par: Scale, Ea, n, A, DeviceID, ConditionTable. Make it stand alone if user want to use it to check something. And add a call in BlackModelization (optional parameter). Could be used to estimate quickly time for a new experiment.
- Force fit option: avoid fitting again if the parameters are available
- Start development for TDDB (exportfiles are in excel format?)
- Condtion values for TDDB experiments: Voltage/temp. If several area are mixed, add area at the end. (create dataframe, search levels dimension. If lenght(levels)>1, paste old name+area)

## Chart:
- Enhance legend presentation to include:
  - MTTF, Number of samples + number of censored samples<-- Create a table for this like in Minitab
  - Model parameters <- addressed with annotate + clipping off (2nd table)

## Documentation:
