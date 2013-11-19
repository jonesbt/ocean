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
             nodes.z="numeric",
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
       (length(object@nodes.z) != object@nodes.n))
       return("Number of x, y, and z coordinates of nodes must be equal.")
    else if((length(object@elems.v1) != object@elems.n) ||
            (length(object@elems.v2) != object@elems.n) ||
            (length(object@elems.v3) != object@elems.n))
        return("All elements must have 3 vertices.")
    else if((sum(is.inf(c(object@nodes.x, object@nodes.y, object@nodes.z)))
             != 0 ||
             sum(is.na(c(object@nodes.x, object@nodes.y, object@nodes.z)))
             != 0))
        return("Nodes may not have Inf or NA coordinates")
    else if((sum(is.inf(c(object@nodes.x, object@nodes.y, object@nodes.z)))
             != 0 ||
             sum(is.na(c(object@nodes.x, object@nodes.y, object@nodes.z)))
             != 0))
        return("Elems may not have Inf or NA indices")
    else return(TRUE)
}
setValidity("fvcom.grid", validFVCOMGrid)

