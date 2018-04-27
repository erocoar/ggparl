
[![Build Status](https://travis-ci.org/erocoar/ggparl.svg?branch=master)](https://travis-ci.org/erocoar/ggparl)

### Note
Development of this package is continued as [ggpol](https://github.com/erocoar/ggpol). 
https://github.com/erocoar/ggpol

### About
`ggparl` adds functions for drawing parliament diagrams to `ggplot2`.

### Installation
`ggparl` can be installed via GitHub:

```r
if (!require(devtools)) {
    install.packages('devtools')
}
devtools::install_github('erocoar/ggparl')
```
### Features
`ggplot_parliament` draws a parliament diagram, clustering points along an arc by parties with each point representing a single member of parliament. 

```r
bt <- data.frame(
        parties = c("CDU", "CSU", "SPD", "AfD", "FDP", "Linke", "Gruene", "Fraktionslos"),
        seats   = c(200, 46, 153, 92, 80, 69, 67, 2),
        colors  = c("black", "blue", "red", "lightblue", "yellow","purple", "green", "grey"),
        stringsAsFactors = FALSE)
ggplot_parliament(bt$parties, bt$seats, bt$colors)
```

![parliament1](https://i.imgur.com/aNCpUDb.png)

`ggplot_parliament2` draws an arc bar chart with optional spacing showing the parties' proportional representation in parliament. 

```r
ggplot_parliament2(bt$parties, bt$seats, bt$colors, sep = 0.05)
```

![parliament2](https://i.imgur.com/q8k2eOw.png)

### Roadmap

While the current features are limited to parliaments (and all other uses arc bar charts may have), ggparl aims to in the future include more functions for convenient visualization of data frequently encountered in politics.
