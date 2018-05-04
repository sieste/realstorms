#' Plot storm tracks
#'
#' Plotting function for objects of class 'stormtracks'
#' @param obj Object of class 'stormtracks'
#' @param var Which variable to use for plotting. The default (NULL) uses the first columns in the data table that start on 'lon_', 'lat_' and 'var_'.
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom broom tidy
#' @export
#'
plot.stormtracks = function(obj, var=NULL) {

  # TODO: deal with longitude wrapping

  lon_col = grep(paste0('^lon_', var), names(obj), value=TRUE)[1]
  lat_col = grep(paste0('^lat_', var), names(obj), value=TRUE)[1]
  var_col = grep(paste0('^var_', var), names(obj), value=TRUE)[1]

  if (is.na(lon_col)) stop(paste0('Could not find column name starting on lon_', var))
  if (is.na(lat_col)) stop(paste0('Could not find column name starting on lat_', var))
  if (is.na(var_col)) stop(paste0('Could not find column name starting on var_', var))

  data('coastsCoarse', package='rworldmap')
  coastline = tidy(coastsCoarse)

  plt = ggplot() + 
        geom_path(data=coastline, aes(x=long, y=lat, group=group), color='black') +
        geom_path(data=obj, aes_string(x=lon_col, y=lat_col, group='ID', colour=var_col)) +
        theme_bw() 

  plt = plt + theme(legend.title=element_blank()) + theme(legend.position='none')

  return(plt)

}

