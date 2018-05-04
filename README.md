# Storm tracking for Real Projections

R package to analyse and plot storm tracks and storm track density from reanalyses and climate model data.

The package uses `data.table` and `ggplot2` for fast computation and quality graphics.


## Installation

```r
devtools::install_github('sieste/realstorms')
```


## Usage example

```r
library(realstorms)

# read storm tracks file
stormsfile = system.file('extdata', 'storms.txt', package='realstorms')
trx = read_tracks(stormsfile)

# calculate track density
td = track_density(trx, bw=5)

# plot tracks
plot(trx)

# plot track density
plot(td)
```


