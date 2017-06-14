# Copyright 2017 Stefan Siegert
# Subject to GPLv3 https://www.gnu.org/licenses/gpl.txt

read_tracks = function(filename=NULL) {

  if (is.null(filename)) {
    filename = '../data/tr_trs_VOR850_19792009_pos.addmslp_addspeed_addprecipNEW_addomega.new_1000km2dayfiltered_RealProjregionfiltered_trackinregion_INTERIM'
  }
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

# This is a helper function used in plot.stormtracks below. When
# longitude crosses the date line, the line has to be split into two
# segments. Otherwise R will connect the line segments across the entire
# globe. The built-in R function maptools::nowrapSpatialLines seems to
# be broken, so I coded it by hand. This function splits a 2 column
# matrix of lons and lats into a list of 2-column matrices matrices,
# separated whenever lon crosses the dateline
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


# plotting function for objects of class 'stormtrack'
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

#obj = read_tracks()
#plot(obj, col='#00000010')

