#' Finite Volume Community Ocean Model grid
#'
#' \code{fvcom.grid} provides a represenation of the unstructured triangular
#' sigma grid used by the Finite Volume Community Ocean Model.
#' 
#' @section Slots:
#'   \describe{
#'     \item{\code{nodes.n}}{The number of nodes in the grid.}
#'     \item{\code{nodes.x}}{x-coordinates of the nodes (m).}
#'     \item{\code{nodes.y}}{y-coordinates of the nodes (m).}
#'     \item{\code{nodes.h}}{z-coordinates (depth) of the nodes (m).}
#'     \item{elems.n}{Number of elements in the grid}
#'     \item{elems.v1}{1st set of node indices}
#'     \item{elems.v2}{2nd set of node indices}
#'     \item{elems.v3}{3rd set of node indices}
#'   }
#' 
#' @references http://fvcom.smast.umassd.edu/FVCOM/
setClass("fvcom.grid",
         representation(
             nodes.n="integer",
             nodes.x="numeric",
             nodes.y="numeric",
             nodes.h="numeric",
             siglay="numeric",
             siglev="numeric",
             elems.n="integer",
             elems.v1="integer",
             elems.v2="integer",
             elems.v3="integer"
             )
         )

validFVCOMGrid <- function(object) {
    if((length(object@nodes.x) != object@nodes.n) ||
       (length(object@nodes.y) != object@nodes.n) ||
       (length(object@nodes.h) != object@nodes.n))
       return("Number of x, y, and h coordinates of nodes must be equal.")
    else if((length(object@elems.v1) != object@elems.n) ||
            (length(object@elems.v2) != object@elems.n) ||
            (length(object@elems.v3) != object@elems.n))
        return("All elements must have 3 vertices.")
    else if((sum(is.inf(c(object@nodes.x, object@nodes.y, object@nodes.h)))
             != 0 ||
             sum(is.na(c(object@nodes.x, object@nodes.y, object@nodes.h)))
             != 0))
        return("Nodes may not have Inf or NA coordinates")
    else if((sum(is.inf(c(object@nodes.x, object@nodes.y, object@nodes.h)))
             != 0 ||
             sum(is.na(c(object@nodes.x, object@nodes.y, object@nodes.h)))
             != 0))
        return("Elems may not have Inf or NA indices")
    else return(TRUE)
}
setValidity("fvcom.grid", validFVCOMGrid)

setMethod(
    f="load",
    signature="FVCOMGrid",
    definition=function(filename) {
        ## Open the grid file and create an empty grid
        ncid <- nc_open(filename)
        grid <- new(FVCOMGrid)
        
        ## Load the bathymetry
        grid@nodes.x <- ncvar_get(ncid, "x")
        grid@nodes.y <- ncvar_get(ncid, "y")
        grid@nodes.h <- ncvar_get(ncid, "h")
        grid@nodes.n <- length(grid@nodes.x)
        
        ## Set up the unstructured grid
        grid@siglay <- ncvar_get(ncid, "siglay")
        grid@siglev <- ncvar_get(ncid, "siglev")
        grid@elems.v1 <- ncvar_get(ncid, "nv", start=c(1,1), count=c(1,-1))
        grid@elems.v2 <- ncvar_get(ncid, "nv", start=c(2,1), count=c(1,-1))
        grid@elems.v2 <- ncvar_get(ncid, "nv", start=c(3,1), count=c(1,-1))
        grid@elems.n <- length(grid@elems.v1)
        
        ## Close the grid file
        nc_close(ncid)
        return(grid)
    }
)

setMethod(
    f = "plot",
    signature="FVCOMGrid",
    definition=function(grid, att=NULL, col=heat.colors(10), border.col=NA,
                        xlim=NULL, ylim=NULL, zlim=NULL,
                        add=FALSE, legend=TRUE) {
        ## Check parameters and assign as needed
        if(att == NULL)
            att = grid@nodes.h
        if(length(att) == 1) {
            att <- rep(att, grid@elems.n)
        } else if(length(att) == grid@nodes.n) {
            ## If we are plotting a nodal quantity, average the corners of
            ## each element

            att.tmp <- rep(0, grid@elems$n)
            for(i in 1:grid@elems$n)
                att.tmp[i] <- mean(c(att[grid@elems.v1[i]],
                                     att[grid@elems.v2[i]],
                                     att[grid@elems.v3[i]]))
            att <- att.tmp
            rm(att.tmp)
        } else if(length(att) == grid@elems.n) {
            ## Nothing to do, all set
        } else {
            stop(sprintf("Invalid length of att (%d), must be 1, %d, or %d",
                         grid@nodes.n, grid@elems.n))
        }

        if(xlim == NULL)
            xlim = c(min(grid@nodes.x), max(grid@nodes.x))
        if(ylim == NULL)
            ylim = c(min(grid@nodes.y), max(grid@nodes.y))
        if(zlim == NULL)
            zlim = c(min(att, na.rm=T), max(att, na.rm=T))
        
        ## Check if latitude/longitude or x/y
        
    }
)
