# RoadMap

## Exportfiles:
- Upgrade Mira export file creation to take into account the case where several conditions are present.
- Mira: clean the table to remove bad unit before working with it. Will make code more readable.
- [fixed] Force exportfile creation option: avoid creating an exportfile if not needed
- [Fixed] Handle the case where a second experiment with the same condition is started.
  - Remove the ForceOverWrite option
  - Detect if condition has already an exportfile
  - Propose to user 3 options: Keep, Replace, Stack
- [Fixed] Handle case where you want to Merge data from Ace and from Mira --> Error message saying this is not possible and old file is kept.
- NBTI exportfile to script.

## Model:
- [fixed] Take into account that several conditions can be present in one exportfile.
- [fixed] Handle case where 2 experiments with same conditions have been started.
- [fixed] Change ReadDataAce to take care of conditions present in different files. First read all the data and store them in dataframe. Then list all conditions (levels) and for each conditions calculate probabilities. Store this in the ExpDataTable. ReadDataAce needs to work with a list of file to load.
- [fixed] Change StackData function to use rbind
- [fixed] Depreciated StackData function as this is done by rbind?
- Robust modelisation: fit 1 followed by a study on outliners leading to a second fit.
- Let's check hierachical models to check if they can be useful for self heating interference.
- [fixed] Store the model param and fit result in a file.
- [fixed] Store all the data in the same file?
- Force fit option: avoid fitting again if the parameters are available

## Chart:
- Enhance legend presentation to include:
  - MTTF and scale
  - Number of samples + number of censored samples
  - Model parameters

## Documentation:
- [fixed] Use of roxygen to generate the doc.
