# Storm tracking for Real Projections

R package to analyse and plot storm tracks and storm track density from reanalyses and climate model data.

The package uses `data.table` and `ggplot2` for fast computation and high quality graphics.


## Installation

```r
devtools::install()
```


## Usage example

```r
library(realstorms)

# read storm tracks file
stormsfile = system.file('extdata', 'storms.txt', package='realstorms')
trx = read_tracks(stormsfile)

# calculate track density on a 5x5 degree grid
td = track_density(trx, bw=5)

# plot storm tracks
plot(trx)

# plot track density
plot(td)

# plot point density ("feature density")
plot(td, what='point_density')
```


