#' Read storm tracks into a data.table
#'
#' Create a data.table of storm tracks from a file.
#'
#' @param filename Input file name
#' @param var_names Vector of variable names to construct column names of the data table. 
#'
#' @return A data.table with columns id, date, lat, lon, var, and possibly further variables measured along the track (mslp, precip, ...)
#'
#' @examples 
#' storms_file = system.file('extdata', 'storms.txt', package='realstorms')
#' ## or during testing: storms_file = '../inst/extdata/storms.txt'
#' trx = read_tracks(storms_file, var_names=c('vor850', 'mslp', 'speed', 'precip', 'omega'))
#'
#' @import data.table
#' @export
#'
read_tracks = 
function(filename=NULL, var_names=NULL) {

  if (is.null(filename)) {
    filename = system.file('extdata', 'interim79.txt', package='realstorms')
  }

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
  
    # convert to numeric data.table
    data_ = do.call(rbind, strsplit(lines1_, ' +'))
    data_ = apply(data_, 2, as.numeric)
    data_ = as.data.table(data_)
  
    # set NA's
    data_[ data_ == 1e25 ] = NA
  
  
    # storm track ID (start time + track number from first line)
    l1_splt = strsplit(lines[i_start[ii]], ' +')[[1]]
    id_ = paste(l1_splt[4], l1_splt[2], sep='_')
  
    # add to list  
    tracks[[id_]] = data_
  }

  # combine list of data.tables to one long data.table
  tracks = rbindlist(tracks, idcol='ID')

  # create column names:

  # the first 5 columns are always the same:
  col_names = c('ID', 'date', 'lon_V1', 'lat_V1', 'var_V1')

  # extract the lat lon descriptor string (regex pattern '&[01]+') from the
  # appropriate header line 
  descr_line = grep('^TRACK_NUM', lines[1:10], value=TRUE)
  descr = sub('^.*(&[01]*).*$', '\\1', descr_line)
#  descr = sub('^.*(&[01]+).*$', '\\1', descr_line)

  # a 1 in the descriptor string denotes that there are lons and lats, a 0
  # denotes that only the variable was stored, so we append to col_names either
  # append (lon_VX, lat_VX, var_VX), or only (var_VX)
  # remove leading '&' and split into individual characters
  descr_list = as.list(strsplit(sub('&', '', descr), '')[[1]])
  for (jj in seq_along(descr_list)) {
    vname = paste('V', jj+1, sep='')
    if (descr_list[[jj]] == '1') {
      col_names = c(col_names, paste(c('lon', 'lat'), vname, sep='_'))
    }
    col_names = c(col_names, paste('var', vname, sep='_'))
  }

  # set user-supplied variable names, i.e. in col_names, replace every V1 by
  # var_names[1], every V2 by var_names[2] etc.
  for (jj in seq_along(var_names)) {
    col_names = gsub(paste('V',jj,'$', sep=''), var_names[jj], col_names)
  }

  # set the new colnames
  setnames(tracks, col_names)

  # map all longitudes to -180:180
  lon_cols = grep('^lon_', names(tracks), value=TRUE)
  tracks[, (lon_cols) := lapply(.SD, function(l) ifelse(l <= 180, l, l - 360)),
         .SDcols=lon_cols]

  # return data table
  class(tracks) = c('stormtracks', class(tracks))
  return(tracks)

} 


#' Add second ID for plotting.
#'
#' Adds a second ID to a stormtracks object that changes whenever a track crosses the dateline. This is useful to avoid plotting artifacts.
#'
#' @param obj Object of class `stormtracks`
#' @param var The longitudes corresponding to which variables should be used. The default (NULL) uses the first column that starts on 'lon_'.
#' @return The original object with an additional id (ID2) that changes when the stormtrack crosses the dateline
#' @export
#'
add_wrap_id = function(obj, var=NULL) {

  # pick longitude column
  lon_col = grep(paste0('^lon_', var), names(obj), value=TRUE)[1]
  
  # create new ID that changes when a track wraps around date line
  obj[, wrap := (function(lon) {
      n = length(lon)
      if (n == 1) return(1)
      wrap = c(FALSE, abs(lon[-1]-lon[-n])>180)
      id_new = cumsum(wrap) + 1
      return(id_new)
    })(get(lon_col)), by=ID]

  obj[, ID2 := paste(ID, wrap, sep='.')]

  obj[, wrap:=NULL]

  return(obj)

}



#  # POTENTIALLY USEFUL SNIPPET:
#  # calculate storm duration in seconds
#  duration_sec = sapply(obj, function(x) {
#    d0_ = paste(x[1, 'date']) 
#    d0_ = as.POSIXct(d0_, format='%Y%m%d%H')
#    d1_ = paste(x[nrow(x), 'date']) 
#    d1_ = as.POSIXct(d1_, format='%Y%m%d%H')
#    dur_ = as.numeric(d1_) - as.numeric(d0_)
#    return(dur_)
#  })

