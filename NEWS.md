projmanr v0.1.1 3/7/18
==============

Changes:

* Fixed bug so now the start_date parameter in critical_path actually works
* Added option to network_diagram to label nodes using the task name


projmanr v0.2.1 7/26/18
==============

Changes:

* Added functionality for uncertain tasks, where the end time for a given task
  is not fully defined, but follows some distribution.
* Allow for Monte Carlo simulation on total project end time given uncertain tasks
* Results of the Monte Carlo simulation include the critical index for each task
* Added additional customization options to Gannt chart
