#' Calculate storm track density on a grid.
#'
#' Count total number and normalised number of storm tracks or features on a grid.
#'
#' @param tracks Object of class `stormtracks` (see `read_tracks`)
#' @param bw The bin width (in degrees) of the grid on which track density is calculate. Default=1.
#' @param var Which variable to use to define latitude and longitude. If `NULL`
#' (the default) it uses the first column names that start on `lon_` and `lat_`.
#' @return grid of counts
#' @examples
#' tracks = read_tracks('../inst/extdata/storms.txt')
#' counts = count_storms(tracks)
#' @seealso read_tracks plot.track_density
#' @import data.table
#'
#' @export
#'
track_density = 
function(tracks, 
         bw = 1,
         var=NULL) {


  # check if obj is a stormtracks object
  stopifnot('stormtracks' %in% class(tracks))

  lon_breaks = seq(-180, 180, by=bw)
  lat_breaks = seq(-90, 90, by=bw)

  # make a deep copy to avoid manipulating the original object
  tracks = copy(tracks)

  # identify longitude and latitude column, abort if not found
  lon_pattern = paste('^lon_', var, sep='')
  lat_pattern = paste('^lat_', var, sep='')
  lon_col = grep(lon_pattern, colnames(tracks), value=TRUE)[1]
  lat_col = grep(lat_pattern, colnames(tracks), value=TRUE)[1]
  if (is.na(lon_col)) {
    stop(paste('Could not find longitude column (pattern `', 
               lon_pattern, '`)', sep=''))
  }
  if (is.na(lat_col)) {
    stop(paste('Could not find latitude column (pattern `', 
               lat_pattern, '`)', sep=''))
  }

  # throw away everything except id, lon and lat
  drop_cols = setdiff(names(tracks), c('ID', lon_col, lat_col))
  tracks[, (drop_cols) := NULL]
  setnames(tracks, c('ID', 'lon', 'lat'))

  # bin longitudes and latidutes
  tracks[, lon_bin := cut(lon, breaks=lon_breaks)]
  tracks[, lat_bin := cut(lat, breaks=lat_breaks)]

  # combine bins
  tracks[, bin := as.factor(paste(lon_bin, lat_bin, sep=','))]

  # count features per bin
  feature_count = tracks[, .(feature_count=.N) , keyby=bin]

  # count tracks per bin
  track_count = tracks[ , .(bin = unique(bin)) , by=ID ][ 
                        , .(track_count=.N), keyby=bin ]

  # merge feature and track counts
  counts = merge(feature_count, track_count)

  # calculate feature density and track density
  counts[, feature_density := feature_count / sum(feature_count, na.rm=TRUE)]
  counts[, track_density := track_count / sum(track_count, na.rm=TRUE)]

  # add empty bins (this improves the contour plots)
  all_bins = as.data.table(
               expand.grid(lon_bin=levels(tracks[, lon_bin]), 
                           lat_bin=levels(tracks[, lat_bin])))
  all_bins[, bin := as.factor(paste(lon_bin, lat_bin, sep=','))]

  counts = counts[J(all_bins$bin)]

  for (jj in c('feature_count', 'feature_density', 'track_count', 'track_density')) {
    set(counts, which(is.na(counts[[jj]])), jj, 0)
  }


  # also, calculate lower and upper bin limits and bin centers
  # split by comma:
  counts[, bin_splt := strsplit(as.character(bin), ',')]
  # remove any brackets:
  counts[, bin_splt := lapply(bin_splt, function(s) gsub('\\(|\\)|\\[|\\]', '', s))]
  # define lon/lat min/max/center
  counts[, bin_splt := lapply(bin_splt, as.numeric)]
  counts[, lon_min := sapply(bin_splt, `[`, 1)]
  counts[, lon_max := sapply(bin_splt, `[`, 2)]
  counts[, lat_min := sapply(bin_splt, `[`, 3)]
  counts[, lat_max := sapply(bin_splt, `[`, 4)]
  counts[, lon := 0.5 * (lon_min + lon_max)]
  counts[, lat := 0.5 * (lat_min + lat_max)]
  counts[, bin_splt := NULL]

  class(counts) = c('track_density', class(counts))

  return(counts)

}

