# Copyright 2017-2018 Stefan Siegert, Adrian Champion
# Subject to GPLv3 https://www.gnu.org/licenses/gpl.txt


#' Read storm tracks into a data.table
#'
#' Create a data.table of storm tracks from a file.
#'
#' @param filename Input file name
#' @param col_names Vector of column names for the data.table. Will be truncated/padded to correct length if necessary.
#' @return data.table with columns date, lat, lon, vorticity, and possibly further variables measured along the track (sea level pressure, precipitation, ...)
#'
#' @export
read_tracks = 
function(filename, 
         col_names=c('ID', 'date', 'lon_vor850', 'lat_vor850', 'vor850')) 
{

  stopifnot(file.exists(filename))
  
  lines = readLines(filename)
  
  # find all lines that start with "TRACK_ID"
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

    # transform to data.table
    data_ = as.data.table(data_)

    # convert everything from character to numeric
    data_ = dplyr::mutate_all(data_, as.numeric)

    # set NA's
    data_[ data_ == 1e25 ] = NA


    # get storm track ID
    id_nr_ = strsplit(lines[i_start[ii]], ' +')[[1]][2]
    id_ = paste('ID', id_nr_, sep='_')

    # add to list  
    tracks[[id_]] = data_
  }

  # combine list of data.tables to one long data.table
  tracks = rbindlist(tracks, idcol='ID')

  # transform storm ID from e.g. 'ID_123' to 123
  tracks[, ID := as.numeric(gsub('ID_', '', ID))]

  # set column names:
  # truncate/NA-pad col_names argument to correct length
  col_names = col_names[1:ncol(tracks)] 
  # replace any NA col_names by the auto-generated names (V5, V6, etc)
  col_names[is.na(col_names)] = colnames(tracks)[is.na(col_names)] 
  colnames(tracks) = col_names

  # return data table
  return(tracks)
}



#' Tidy storm tracks
#'
#' Tidy up storm tracks into a data frame for use with ggplot2.
#' @param obj Object of class 'stormtracks'
#' @return A tidy data frame.
#'
#' @importFrom broom tidy
#' @export
tidy.stormtracks = function(obj, ...) {
  
  library(plyr)
  obj_ = ldply(obj, .id='ID')

  # transform all longitudes from 0:360 to -180:180
  lon_cols = grep('^lon_', names(obj_))
  for (i in lon_cols) {
    inds = obj_[, i] > 180
    inds[is.na(inds)] = FALSE
    obj_[inds, i] = obj_[inds,i] - 360
  }

  # create new ID that changes when track wraps around date line
  obj_ = ddply(obj_, .(ID), transform, ID2=
           (function(lon) {
              n = length(lon)
              if (n == 1) return(1)
              wrap = c(FALSE, abs(lon[-1]-lon[-n])>180)
              id_new = cumsum(wrap) + 1
              return(id_new)
           })(lon_vor850)
         )
  obj_$ID2 = paste(obj_$ID, obj_$ID2, sep='.')

  return(obj_)

}


#' Plot storm tracks
#'
#' Plotting function for objects of class 'stormtracks'
#' @param obj Object of class 'stormtracks'
#' @param ... additional arguments passed to plot() function
#' @return A ggplot object.
#' @export
plot.stormtracks = function(obj, ...) {

  library(ggplot2)
  library(broom)

  obj_ = tidy(obj)
  data('coastsCoarse', package='rworldmap')
  coastline = tidy(coastsCoarse)

  plt = ggplot() + 
        geom_path(data=coastline, aes(x=long, y=lat, group=group), color='black') +
        geom_path(data=obj_, aes(x=lon_vor850, y=lat_vor850, group=ID2, ...)) +
        theme_bw() +
        theme(legend.title=element_blank()) + theme(legend.position='none')

  return(plt)

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
#' @param type Which type of density estimator to use. Options are `density_2d` (the default) and `bin_2d`. See the help files of `ggplot2::stat_bin_2d` and `ggplot2::stat_density_2d` for details.
#' @return A ggplot object.
#'
#' @details This function takes all points in the stromtracks archive and constructs a 2d density plot using the ggplot2 function `stat_density2d`.
#'
#' @export

stormtracks_density = function(obj, type=c('density_2d', 'bin_2d'), ...) {

  stopifnot(class(obj) == 'stormtracks')

  type = match.arg(type)

  library(sp)
  library(ggplot2)
  library(broom)

  # transform the track data into a lon/lat data frame
  obj_ = tidy(obj)


  data('coastsCoarse', package='rworldmap')
  coastline = tidy(coastsCoarse)

  plt = ggplot() + 
        geom_path(data=coastline, aes(x=long, y=lat, group=group), color='black') +
        coord_fixed(xlim=c(-180, 180), ylim=c(-90,90), ratio=1)
  if (type == 'bin_2d') {
     plt = plt + 
           stat_bin2d(data=obj_, 
                      aes(x=lon_vor850, y=lat_vor850, 
                          fill=..count.., alpha=..count.. * 0.5), 
                      binwidth=c(1,1), bins=25)   +
           scale_alpha_continuous(range=c(0.7, 1))
  } else if (type == 'density_2d') {
     plt = plt + 
           stat_density2d(data=obj_, 
                          aes(x=lon_vor850, y=lat_vor850, 
                              fill=..level.., alpha=..level..), 
                          geom='polygon') +
           scale_alpha_continuous(range=c(0.3, 0.7))
  }
  plt = plt +      
        theme_bw() +
        theme(legend.title=element_blank()) + 
        theme(legend.position='none')

  return(plt)
}


