# Copyright 2017 Stefan Siegert
# Subject to GPLv3 https://www.gnu.org/licenses/gpl.txt


#' Read storm tracks 
#'
#' Create a list of storm tracks from a file.
#'
#' @param filename Input file name
#' @return An object of class 'stormtracks'
#'
#' @details A stormtracks-object is a list of storm tracks. Each storm track is represented by a matrix with columns (at least) dates, lat, lon, vorticity, and possibly further variables like sea level pressure or precipitation.
#' @export
read_tracks = function(filename) {

  stopifnot(file.exists(filename))
  
  lines = readLines(filename)
  
  # search lines that start with "TRACK_ID"
  i_start = grep(pattern='^TRACK_ID', x=lines)
  
  # number of tracks
  n_tracks = length(i_start)
  
  # get number of points for each track
  n_points_lines = strsplit(lines[i_start + 1], ' +')
  n_points = as.numeric(sapply(n_points_lines, `[`, 2))
  
  # loop over lines and extract track data
  tracks = list()
  for (ii in seq_len(n_tracks)) {

    # start index and length of block
    start_ = i_start[ii] + 2
    length_ = n_points[ii]

    # extract lines
    lines_ = lines[start_ : (start_ + length_ - 1)]

    # remove all '&'s
    lines1_ = gsub(pattern='&', replacement='', x=lines_)

    # convert to matrix with columns date, lon, lat, var01, var02, ...
    data_ = do.call(rbind, strsplit(lines1_, ' +'))
    n_var = ncol(data_) - 3
    colnames(data_) = c('date', 
                        'lon_vor850', 'lat_vor850', 'vor850', 
                        'lon_mslpmin', 'lat_mslpmin', 'mslpmin',
                        'lon_wind925max', 'lat_wind925max', 'wind925max',
                        'lon_prcpmax', 'lat_prcpmax', 'prcpmax',
                        'lon_omega925min', 'lat_omega925min', 'omega925min')

    # convert to numeric
    data1_ = apply(data_, 2, as.numeric)

    # set NA's
    data1_[ data1_ == 1e25 ] = NA

    # get storm track ID
    id_nr_ = strsplit(lines[i_start[ii]], ' +')[[1]][2]
    id_ = paste('ID', id_nr_, sep='_')

    # add to list  
    tracks[[id_]] = data1_
  }

  # return object of class 'stormtracks'
  class(tracks) = 'stormtracks'
  return(tracks)
}

#' Unwrap longitudes and latitudes
#'
#' Unwrap longitudes to plot lines properly on a 2d map
#' 
#' @param lonlat A 2 column matrix, each row is a lon/lat pair.
#' @return A list of 2-column matrices lon/lat.
#' @details This is a helper function used in plot.stormtracks below. When
#' longitude crosses the date line, the line has to be split into two
#' segments. Otherwise R will connect the line segments across the entire
#' globe. The built-in R function maptools::nowrapSpatialLines seems to
#' be broken, so I coded it by hand. This function splits a 2 column
#' matrix of lons and lats into a list of 2-column matrices matrices,
#' separated whenever lon crosses the dateline.
unwrap_lonlat = function(lonlat) {
  l = list(lonlat[1, , drop=FALSE])
  k = 1
  for (i in 2:nrow(lonlat)) {
    if ((lonlat[i-1,1] >  100 & lonlat[i,1] < -100) |
        (lonlat[i-1,1] < -100 & lonlat[i,1] >  100)) {
      k = k + 1
      l[[k]] = lonlat[i, , drop=FALSE]
    } else {
      l[[k]] = rbind(l[[k]], lonlat[i, ]) 
    }
  }
  return(l)
}


#' Plot storm tracks
#'
#' Plotting function for objects of class 'stormtrack'
#' @param obj Object of class 'stormtrack'
#' @param ... additional arguments passed to plot() function
#' @return NULL
#' @examples
#' trx = read_tracks()
#' plot(trx, col='#00000010', lwd=5)
#' @export
plot.stormtracks = function(obj, ...) {

  library(sp)
  library(maps)
  # transform the track data into a list of "Lines" objects (package sp)
  tracks = 
    lapply(seq_along(obj), function(ii) {
      data_ = obj[[ii]]
      id_ = names(obj)[ii]
      lats_ = data_[, 'lat_vor850']
      lons_ = data_[, 'lon_vor850']
      lons_[lons_ > 180] = lons_[lons_ > 180] - 360
      lonlat_ = cbind(lons_, lats_)
      lonlat_ = unwrap_lonlat(lonlat_)
      Lines(lapply(lonlat_, Line), ID=id_)
    })
  spl_tracks = SpatialLines(tracks, proj4string=CRS('+proj=longlat +ellps=WGS84'))
  
  # world map
  map()

  # add tracks to the map, using arguments passed to the
  # plot.stormtracks function
  lines(spl_tracks, ...)

}


#' Filter by year
#' 
#' Filter storms that occurred within a range of years.
#'
#' @param obj Object of class 'stormtracks'
#' @param yr_lim Range of years for which to return storms
#' @return Object of class 'stormtracks' containing only storms in the selected range of years.
#' @details The year range is inclusive, i.e. setting `yr_lim=c(1990,1991)` returns all storms that occurred in 1990 and 1991.
#' @examples
#' trx = read_tracks()
#' trx_1990 = filter_by_year(trx, yr_lim=c(1990, 1990))
#' @export
filter_by_year = function(obj, yr_lim=c(-Inf, Inf)) {

  stopifnot(class(obj) == 'stormtracks')

  # get start year of each storm track
  start_yrs = sapply(obj, function(x) {
    d_ = paste(x[1, 'date']) 
    d_ = as.Date(d_, format='%Y%m%d%H')
    d_ = format(d_, '%Y')
    return(as.numeric(d_))
  })

  # filter stormtracks by yr_lim (inclusive)
  inds = which(start_yrs >= min(yr_lim) & start_yrs <= max(yr_lim))
  out = obj[inds]
  class(out) = 'stormtracks'

  return(out)
}



#' Filter by duration
#' 
#' Filter storms by how long they lasted
#'
#' @param obj Object of class 'stormtracks'
#' @param dur_lim Range of storm durations for which to return storms.
#' @param unit Unit in which duration is measured. Either `hours` or `days`. Can be abbreviated. Default is `hours`.
#' @return Object of class 'stormtracks' containing only storms in the selected range of duration.
#' @examples
#' trx = read_tracks()
#' trx_1day = filter_by_duration(trx, dur_lim=c(1,1), unit='d')
#' @export
filter_by_duration = function(obj, dur_lim=c(0, Inf), unit=c('hours', 'days')) {

  stopifnot(class(obj) == 'stormtracks')
  unit = match.arg(unit)

  # calculate storm duration in seconds
  duration_sec = sapply(obj, function(x) {
    d0_ = paste(x[1, 'date']) 
    d0_ = as.POSIXct(d0_, format='%Y%m%d%H')
    d1_ = paste(x[nrow(x), 'date']) 
    d1_ = as.POSIXct(d1_, format='%Y%m%d%H')
    dur_ = as.numeric(d1_) - as.numeric(d0_)
    return(dur_)
  })

  # convert to hours (and days if necessary)
  duration = duration_sec / (60 * 60)
  if (unit == 'days') {
    duration = duration / 24
  } 

  # filter stormtracks by duration (inclusive)
  inds = which(duration >= min(dur_lim) & duration <= max(dur_lim))
  out = obj[inds]
  class(out) = 'stormtracks'

  return(out)
}


#' Storm track density plot
#'
#' Plot storm track density on a map.
#'
#' @param obj Object of class 'stormtracks'
#' @param type Which type of density estimator to use. Options are `bin2d` (the default) and `kde2d`. See the help files of `ggplot2::stat_density_2d` and `ggplot2::stat_bin_2d` for details.
#' @return A ggplot object.
#'
#' @details This function takes all points in the stromtracks archive and constructs a 2d density plot using the ggplot2 function `stat_density2d`.
#'
#' @export

stormtracks_density = function(obj, type=c('kde2d', 'bin2d'), ...) {

  stopifnot(class(obj) == 'stormtracks')

  type = match.arg(type)

  library(sp)
  library(ggplot2)

  # transform the track data into a lon/lat data frame
  pts = 
    lapply(seq_along(obj), function(ii) {
      data_ = obj[[ii]]
      id_ = names(obj)[ii]
      lats_ = data_[, 'lat_vor850']
      lons_ = data_[, 'lon_vor850']
      lons_[lons_ > 180] = lons_[lons_ > 180] - 360
      lonlat_ = cbind(lons_, lats_)
      return(lonlat_)
    })
  pts = do.call(rbind, pts)
  colnames(pts) = c('lon', 'lat')
  pts = as.data.frame(pts)


  load('data/coastline.Rdata')

  plt = ggplot() + 
        geom_polygon(data=coastline, aes(x=long, y=lat, group=group),
                     color='black', fill=NA) +
        coord_fixed(xlim=c(-181, 181), ylim=c(-90,90), ratio=1)
  if (type == 'bin2d') {
     plt = plt + stat_bin2d(data=pts, aes(x=lon, y=lat, fill=..count.., alpha=..count.. * 0.5), 
                            binwidth=c(1,1), bins=25)   +
                 scale_alpha_continuous(range=c(0.7, 1))
  } else if (type == 'kde2d') {
     plt = plt + stat_density2d(data=pts, aes(x=lon, y=lat, fill=..level.., alpha=..level..), 
                                geom='polygon') +
                 scale_alpha_continuous(range=c(0.3, 0.7))
  }
  plt = plt +      
        theme_bw() +
        theme(legend.title=element_blank()) + theme(legend.position='none')

  return(plt)

}


