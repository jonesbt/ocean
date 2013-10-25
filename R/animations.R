#' Generate a seq of png files that can be animated with imagemagik.
#'
#' @note Maps plot.att over times and write the output to png files for each
#' timestep.
#' 
#' @param grid The grid on which to plot the attribute
#' @param att  The attribute to animate
#' @param times The times over which to plot the attribute
#' @param directory The directory to which to write the png files
#' @param col The color scheme to plot
#' @param x The x values to plot
#' @param y The y values to plot
#' @note \code{animate.att} supports up to 10,000 timesteps with proper
#' padding.
#' @section Todo: redo to support ... to plot.att

animate.att <- function(grid, att, times, directory,
                        col=heat.colors, x='x', y='y') {
   for(t in 1:length(times)) {
       png(file = sprintf("%s/att_%s%03d.png", directory, t))
       plot.att(grid, att[,t], col, x, y)
       dev.off()
   }
}
