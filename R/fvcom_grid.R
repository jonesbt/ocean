#' Finite Volume Community Ocean Model grid
#'
#' \code{fvcom.grid} provides a represenation of the unstructured triangular
#' sigma grid used by the Finite Volume Community Ocean Model (FVCOM). As a
#' disclaimer, please note that the author of this package is a user of FVCOM,
#' but is not affiliated with its development.
#' 
#' @section Slots:
#'   \describe{
#'     \item{nodes.n}{The number of nodes in the grid.}
#'     \item{nodes.x}{x-coordinates of the nodes (m).}
#'     \item{nodes.y}{y-coordinates of the nodes (m).}
#'     \item{nodes.h}{z-coordinates (depth) of the nodes (m).}
#'     \item{nodes.lat}{Latitude of the nodes.}
#'     \item{nodes.lon}{Longitude of the nodes.}
#'     \item{elems.n}{Number of elements in the grid}
#'     \item{elems.v1}{1st set of node indices}
#'     \item{elems.v2}{2nd set of node indices}
#'     \item{elems.v3}{3rd set of node indices}
#'     \item{proj}{A string that could be passed to proj4::project to convert
#'                 x and y values to latitude and longitude.}
#'   }
#' 
#' @references http://fvcom.smast.umassd.edu/FVCOM/
setClass("fvcom.grid",
         representation(
             nodes.n="integer",
             nodes.x="numeric",
             nodes.y="numeric",
             nodes.h="numeric",
             nodes.lat="numeric",
             nodes.lon="numeric",
             elems.n="integer",
             elems.v1="integer",
             elems.v2="integer",
             elems.v3="integer",
             proj="character"
             )
         )

#' Load an FVCOM grid from a NetCDF output file.
#'
#' Loads enough of an FVCOM grid to use the other methods associated with
#' the \code{fvcom.grid} class. The (x,y,h) locations of the nodes and their
#' connections to form an unstructured triangular mesh are loaded.
#' @param filename The name of an output NetCDF file from FVCOM 2.7.
#' @return An instance of the \code{fvcom.grid} class.
loadFVCOMGrid27 <- function(filename, proj) {
    ncid <- nc_open(filename)
    x <- as.vector(ncvar_get(ncid, 'x'))
    y <- as.vector(ncvar_get(ncid, 'y'))
    h <- as.vector(ncvar_get(ncid, 'h'))
    if(proj != '') {
        ll <- project(data.frame(x, y), proj, inverse=TRUE)
    } else {
        ll <- data.frame(x=NULL, y=NULL)
    }
    ev <- ncvar_get(ncid, 'nv') ## Element vertices
    nc_close(ncid)
    return(new("fvcom.grid",
               nodes.n=length(x), nodes.x=x, nodes.y=y, nodes.h=h,
               nodes.lat=ll$y, nodes.lon=ll$x, proj=proj,
               elems.n=nrow(ev),
               elems.v1=ev[,1], elems.v2=ev[,2], elems.v3=ev[,3]))
}


#' Get the depth at each node in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return A vector of length get.nnodes(grid) with the depth at each node.
getFVCOMDepth <- function(grid)
    return(grid@nodes.h)
setGeneric("get.depth", getFVCOMDepth)
setMethod("get.depth", "fvcom.grid", getFVCOMDepth)

#' Get the indices of the element vertices in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return A \code{data.frame} with \code{v1}, \code{v2}, and \code{v3}
#' elements that correspond to the indices of the nodes at vertices 1, 2, and
#' 3.
getFVCOMElems <- function(grid)
    return(data.frame(v1=grid@elems.v1, v2=grid@elems.v2, v3=grid@elems.v3))
setGeneric("get.elems", getFVCOMElems)
setMethod("get.elems", "fvcom.grid", getFVCOMElems)

#' Get the number of elements in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return The number of elements in \code{grid}.
getFVCOMNElems <- function(grid)
    return(grid@elems.n)
setGeneric("get.nelems", getFVCOMNElems)
setMethod("get.nelems", "fvcom.grid", getFVCOMNElems)

#' Get the number of nodes in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return The number of nodes in \code{grid}.
getFVCOMNNodes <- function(grid)
    return(grid@nodes.n)
setGeneric("get.nnodes", getFVCOMNNodes)
setMethod("get.nnodes", "fvcom.grid", getFVCOMNNodes)

#' Get the values of the nodes in the grid.
#'
#' @param grid A \code{fvcom.grid} instance.
#' @return A \code{data.frame} with \code{x}, \code{y}, and \code{h} elements
#' that correspond to the x, y, and depth of each node.
getFVCOMNodes <- function(grid)
    return(data.frame(x=grid@nodes.x, y=grid@nodes.y, h=grid@nodes.h))
setGeneric("get.nodes", getFVCOMNodes)
setMethod("get.nodes", "fvcom.grid", getFVCOMNodes)

#' Convert a single scalar or node based quantity to element based. 
#' 
#' The length of \code{x} determines how it will be treated. If \code{x} has
#' length 1, it is returned as a single color. If the length of \code{x} is
#' the number of nodes in the grid, its value for each element is calculated
#' as the average of the value at the adjoining nodes. If the length of
#' \code{x} is the number of elements in the grid, it is returned as is. Any
#' other values throw an error.
#' \param grid An fvcom.grid instance.
#' \param x A vector of length 1 or \code{length get.nnodes(grid)}
#' \return A vector of length \code{get.nelems(grid)}
interpFVCOMGrid <- function(grid, x) {
    if(length(x) == 1) {
        ## Repeat the scalar value nelems times.
        x.ret <- rep(x, get.nelems(grid))
    } else if(length(x) == get.nnodes(grid)) {
        ## Average the values at each node to get the value in each element.
        x.ret <- sapply(seq(get.nelems(grid)), function(i)
                        return(mean(c(x[grid@elems.v1[i]],
                                      x[grid@elems.v2[i]],
                                      x[grid@elems.v3[i]]))))
    } else if(length(x) == get.nelems(grid)) {
        x.ret <- x
    } else {
        stop(paste('length(x) must be equal to 1, the number of nodes, or ',
                   'the number of elements\n', sep=''))
    }
    return(x.ret)
}
setGeneric("interp", interpFVCOMGrid)
setMethod("interp", "fvcom.grid", interpFVCOMGrid)

#' Plot a \code{fvcom.grid} instance.
#'
#' @param x A \code{fvcom.grid} instance.
#' @param z A vector to plot as a heatmap.
#' @param units Either 'll' for latitude and longitude or 'm' for meters.
#' @param col A list of colors, such as that returned by bathy.colors.
#' @param add Should the plot be added to the current plot?
#' #' @param xlim x-limits for the plot.
#' @param ylim y-limits for the plot.
#' @param zlim z-limits for the plot. 
#' @param border.col Color of the element borders. If not provided the borders
#'                   will be colored to match the adjacent polygons.
#' @param cex Character expansion for \code{cex} and \code{cex.*}.
imageFVCOMGrid <- function(x, z=get.depth(grid), units='ll',
                          col=bathy.colors(100), add=FALSE,
                          xlim=NA, ylim=NA, zlim=NA, legend=FALSE,
                          border.col=NA) {
    ## Check the parameters for validity.
    ## Convert the passed in values to an element based quantity.
    z <- interp(grid, z)
    
    ## Calculate z limits
    if(!is.na(zlim)[1]) {
        ## If zlimits were provided, any values that lie outside of them are
        ## converted to NA.
        z[z < zlim[1]] <- NA
        z[z > zlim[2]] <- NA
    } else {
        ## If no z limits were provided, calculate them as the minimum and
        ## maximum of z ignoring missing values.
        zlim <- c(min(z, na.rm=TRUE), max(z, na.rm=TRUE))
    }
    
    ## Get the lat/lon or x/y locations of the nodes surrounding the elements.
    ## c(rbind()) interleaves the vectors
    if(units == "ll") { # Lat/lon
        x <- grid@nodes.lon[c(rbind(grid@elems.v1, grid@elems.v2,
                                    grid@elems.v3))]
        y <- grid@nodes.lat[c(rbind(grid@elems.v1, grid@elems.v2,
                                    grid@elems.v3))]
    } else if(units == "m") {
        x <- grid@nodes.x[c(rbind(grid@elems.v1, grid@elems.v2,
                                  grid@elems.v3))]
        y <- grid@nodes.y[c(rbind(grid@elems.v1, grid@elems.v2,
                                  grid@elems.v3))]
    } else {
        stop("Invalid units, options are 'll' or 'm'.")
    }

    ## Calculate the colors for the polygons and the borders
    n.cols <- length(col)
    cols.idx <- floor((z - zlim[1]) / (zlim[2] - zlim[1]) * n.cols)
    cols.idx[cols.idx <= 0] <- 1
    cols.idx[cols.idx > n.cols] <- n.cols
    col <- col[cols.idx]
    if(is.na(border.col)[1])
        border.col <- col
    
    ## cut.poly adds NAs after each polygon, ensuring the the polygons are
    ## not connected to one another by spurious lines.
    cut.poly <- function(x, n.sides=3) {
        n <- length(x) %/% n.sides
        polys <- rep(NA, n*n.sides)
        polys[rep(c(rep(T, n.sides), F), n)] <- x
        return(polys)
    }

    if(is.na(xlim[1]))
        xlim <- c(min(x), max(x))
    if(is.na(ylim[1]))
        ylim <- c(min(y), max(y))
    
    ## Do the actual plotting
    if(!add)
        plot(0, xlim=xlim, ylim=ylim,
             xlab = "Longitude",
             ylab = "Latitude",
             cex.lab=2)
    polygon(cut.poly(x), cut.poly(y), col=col, border=border.col)
    #if(legend) (TODO)
    #    legend(min(x), max(y),
    #           legend=c(round(zlim[2], 2), rep("", n.cols-2),
    #           round(zlim[1], 2)),
    #           bt="n",
    #           col=rev(cols),
    #           y.intersp=10/n.cols,
    #           pch=15,
    #           cex=2)
}
setMethod("image", "fvcom.grid", imageFVCOMGrid)

#' Check if a \code{fvcom.grid} instance is valid.
validFVCOMGrid <- function(object) {
    if((length(object@nodes.x) != object@nodes.n) ||
       (length(object@nodes.y) != object@nodes.n) ||
       (length(object@nodes.h) != object@nodes.n))
       return("Number of x, y, and z coordinates of nodes must be equal.")
    else if((length(object@elems.v1) != object@elems.n) ||
            (length(object@elems.v2) != object@elems.n) ||
            (length(object@elems.v3) != object@elems.n))
        return("All elements must have 3 vertices.")
    else if((sum((c(object@nodes.x, object@nodes.y, object@nodes.h) == Inf))
             != 0 ||
             sum(is.na(c(object@nodes.x, object@nodes.y, object@nodes.h)))
             != 0))
        return("Nodes may not have Inf or NA coordinates")
    else if((sum((c(object@nodes.x, object@nodes.y, object@nodes.h) == Inf))
             != 0 ||
             sum(is.na(c(object@nodes.x, object@nodes.y, object@nodes.h)))
             != 0))
        return("Elems may not have Inf or NA indices")
    else return(TRUE)
}
setValidity("fvcom.grid", validFVCOMGrid)

#' Create a new FVCOM grid instance from a FVCOM NetCDF output file.
#' 
#' @param filename The name of an output NetCDF file from FVCOM.
#' @param proj A string to passed to proj4::project to convert x,y locations
#' to latitude and longitude.
#' @return An instance of the \code{fvcom.grid} class.
fvcom.grid <- function(filename, proj)
    return(loadFVCOMGrid27(filename, proj))
