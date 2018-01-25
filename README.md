
### About
`ggparl` adds functions for drawing parliaments to `ggplot2`.

### Installation
`ggparl` can be installed via GitHub:

```r
librar(devtools)
devtools::install_github('erocoar/ggparl')
```
### Features
`ggplot_parliament` draws a parliament diagram clustered by parties with each point representing a single member of parliament. 

```r
bt <- data.frame(
        parties = c("CDU", "CSU", "SPD", "AfD", "FDP", "Linke", "Gruene", "Fraktionslos"),
        seats   = c(200, 46, 153, 92, 80, 69, 67, 2),
        colors  = c("black", "blue", "red", "lightblue", "yellow","purple", "green", "grey"),
        stringsAsFactors = FALSE)
ggplot_parliament(bt$parties, bt$seats, bt$colors)
```

![parliament1](https://www.dropbox.com/s/mfwq0t6yeg6ba4g/parl1.png?dl=0)

`ggplot_parliament2` draws an arc bar diagram with optional spacing representing the proportional parliamentary representation of parties. 

```r
ggplot_parliament2(bt$parties, bt$seats, bt$colors, sep = 0.05)
```
