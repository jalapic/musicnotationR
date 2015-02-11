# musicnotationR

An R package for producing music notation social graphs

This package is in its very early stages.  Currently there is one main function, `musicnot` which 
can be used to plot temporally organized social interation data according to the musical notation
visualization method of Ivan Chase (2006) " Music notation: a new method for visualizing social
interaction in animals and humans", Frontiers Zoology 3: 18.

=============


### Installation

To install this directly into R.

```
library(devtools)
install_github('jalapic/musicnotationR', username = "jalapic")
library(musicnotationR)

```


### Contents

data:
- dom 
- flies

functions:
- musicnot() - generate music notation social interaction graph
- gg_colors() - generates ggplot2 color palette
- random_datetime() - generates random sequence of dates and times


### Demo

A brief tutorial/demo can be found here -  http://rpubs.com/jalapic/musicnotationR

A shinyapp based on the raw code can be found here -  https://jalapic.shinyapps.io/music/
