# OpenSthlm
Example R functions for reading data from the Open Stockholm Portal (aka. Dataportalen) 
using JSON. The Dataportalen is found at: 

http://dataportalen.stockholm.se/dataportalen/

In order to download data you need to register for an API_KEY (for free)
at http://api.stockholm.se/

See http://open.stockholm.se/ for further details. 

The contents are as follows:
* `osdata2df.R` R code for illustrating how to pull XML data from Dataportalen using JSON and convert into a nice `data.frame` to work with. 
* `pop.pdf` Resulting graphic from `osdata2df.R`showing the population in Stockholm Kommune for the years 2007-2012.
