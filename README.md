# projmanr
## Project Management Tools for R

R library containing a basic set of tools for project management, including the computation of the critical path of a project and the generation of a gantt chart. This project aims to provide a free alternative for some of the basic features of MS Office.



## Installation 

This project is available on the CRAN network and can be installed via:

```R
install.packages("projmanr")
```

For the most up to date development version, use:

```R
install.packages("devtools")
devtools::install_github("https://github.com/bgreenawald/projmanr")
```



## Planned Features

* Allow for uncertainty in the task durations. This would run a simulation using the uncertain task durations and output a range of possible project durations.
* Increased customizability of gantt chart and network diagram.
* Add dependency lines to the gantt chart.