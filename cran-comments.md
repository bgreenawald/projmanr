## Test Environments
* Local Windows 10, R 3.4.3
* Local (dual boot) Linux Ubuntu 16.04, R 3.4.3
* win-builder

## R CMD check results
There were no ERRORS, WARNINGS, or NOTES on Local Windows 10 or Linux
On win-builder, there was 1 NOTE.


Possibly mis-spelled words in DESCRIPTION:
  Gantt (11:10)
  
This spelling of Gantt is indeed correct so this note can be ignored.

Looking at the Check results for the last version, and there is 1 note on some systems,

checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘R6’
  All declared Imports should be used.
  
However I am unable to reproduce this note locally.

## Downstream Dependencies
Using devtools::revdep_check(), all installed dependencies passed.