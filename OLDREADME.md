# Old RoadMap

Keep record of achievements.

## Exportfiles:
- [fixed] Force exportfile creation option: avoid creating an exportfile if not needed
- [Fixed] Handle the case where a second experiment with the same condition is started.
  - Remove the ForceOverWrite option
  - Detect if condition has already an exportfile
  - Propose to user 3 options: Keep, Replace, Stack
- [Fixed] Handle case where you want to Merge data from Ace and from Mira --> Error message saying this is not possible and old file is kept.

## Model:
- [fixed] Take into account that several conditions can be present in one exportfile.
- [fixed] Handle case where 2 experiments with same conditions have been started.
- [fixed] Change ReadDataAce to take care of conditions present in different files. First read all the data and store them in dataframe. Then list all conditions (levels) and for each conditions calculate probabilities. Store this in the ExpDataTable. ReadDataAce needs to work with a list of file to load.
- [fixed] Change StackData function to use rbind
- [fixed] Depreciated StackData function as this is done by rbind?
- [fixed] Store the model param and fit result in a file.
- [fixed] Store all the data in the same file?
- [fixed] Create a function to display the data without modelizing (avoid error when only 1 or 2 conditions are available)
- [fixed] Replace above function by the use of "try" function.
- [fixed] Adapt BlackAnalysis to detect if several structures have been mixed and plot all the chart accordingly
- [canceled] adapt ViewData to detect if TDDB or EM data are given.

## Chart:

## Documentation:
- [fixed] Use of roxygen to generate the doc.
