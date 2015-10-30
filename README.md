# RoadMap

## Exportfiles:
- Upgrade Mira export file creation to take into account the case where several conditions are present.
- Mira: clean the table to remove bad unit before working with it. Will make code more readable.
- NBTI exportfile to script.

## Model:
- Robust modelisation: fit 1 followed by a study on outliners leading to a second fit.
- Let's check hierachical models to check if they can be useful for self heating interference.
- Create a function that estimate the lifetime of the device. par: Scale, Ea, n, A, DeviceID, ConditionTable. Make it stand alone if user want to use it to check something. And add a call in BlackModelization (optional parameter).
- Force fit option: avoid fitting again if the parameters are available
- Start development for TDDB

## Chart:
- Enhance legend presentation to include:
  - MTTF, Number of samples + number of censored samples<-- Create a table for this like in Minitab
  - Model parameters <- adressed with annotate + clipping off (2nd table)

## Documentation:
