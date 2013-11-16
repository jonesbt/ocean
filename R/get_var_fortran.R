#' Reads a single variable from a Fortran binary file
#'
#' \code{get.var.fortran} reads a single variable from the stream fid and
#' returns the data without the header or footer.
#' \param fid File connection to a Fortran binary file
#' \param type Type of data in the file. See load.fortran for options.
#' \return A vector with the data from the file loaded and converted to the
#'         proper type or integer(0) if at the end of the stream.
get.var.fortran <- function(fid, type) {
    if(type == "int") {
        size <- 4
        type <- "int"
    } else if(type == "float") {
        size <- 4
        type <- "numeric"
    } else if(type == "double") {
        size <- 8
        type <- "double"
    }
    len <- readBin(fid, "int", n=1, size=4)
    if(length(len) > 0) { # !EOF
        dat <- readBin(fid, type, n=len/size, size=size)
        tail <- readBin(fid, "int", n=1, size=4)
        if(len != tail)
            print("ERROR: Mismatch between header and footer.")
        return(dat)
    } else { #EOF
        return(integer(0))
    }
}
