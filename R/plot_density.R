#' Plot track or feature density
#'
#' @param obj Data table returned by function `count_storms`
#' @param what One of 'track_density', 'feature_density', 'track_count', 'feature_count'
#' @return A ggplot2 object
#' @seealso track_density
#' @import ggplot2
#' @import viridis
#' @importFrom broom tidy
#' @export
#'
plot.track_density =
function(
  obj, 
  what = c('track_density', 'feature_density', 'track_count', 'feature_count')) 
{

  what = match.arg(what)

  data('coastsCoarse', package='rworldmap')
  coastline = tidy(coastsCoarse)

  plt = ggplot(obj, aes(x=lon, y=lat)) + 
    geom_raster(aes_string(fill=what, alpha=what), interpolate=TRUE) +
    geom_contour(aes_string(z=what, colour='..level..')) +
    geom_path(data=coastline, aes(x=long, y=lat, group=group), color='black') +
    scale_alpha_continuous(range=c(0, 1), guide='none') +
    scale_fill_viridis() +
    scale_colour_viridis(guide='none') +
    theme_bw() +
    coord_equal(xlim = subset(obj, get(what) > 0)[, c(min(lon_min), max(lon_max))],
                ylim = subset(obj, get(what) > 0)[, c(min(lat_min), max(lat_max))])

  return(plt)
 
}


#' Storm track density plot
#'
#' Deprecated. Use the plot() function for objects of type `track_density` instead.
#'
#' @param obj Object of class 'stormtracks'
#' @param type Which type of density estimator to use. Options are `density_2d` (the default) and `bin_2d`. See the help files of `ggplot2::stat_bin_2d` and `ggplot2::stat_density_2d` for details.
#' @return A ggplot object.
#'
#' @details This function takes all points in the stromtracks archive and constructs a 2d density plot using the ggplot2 function `stat_density2d`.
#'
#' @import sp
#' @import ggplot2
#' @import broom
#'
stormtracks_density = function(obj, type=c('density_2d', 'bin_2d'), ...) {

  stopifnot(class(obj) == 'stormtracks')

  type = match.arg(type)


  # transform the track data into a lon/lat data frame
  obj = tidy(obj)

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


