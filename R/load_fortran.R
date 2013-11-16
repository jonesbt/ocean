#' Loads a Fortran binary file
#'
#' \code{load.fortran(filename)} reads the Fortran binary file specified by
#' filename into R.
#' \param filename Name of the file to read
#' \param type Type of data in the file. May be one of 'int', 'float', or
#'             'double'
#' \return A list of vectors, where each vector is one of the sequences of
#'         data in filename.
load.fortran <- function(filename, type) {
    fid <- file(filename, "rb")
    dat <- vector(mode="list", length=0)
    while(length(next.var <- get.var.fortran(fid, type)) > 0)
        dat[[length(dat) + 1]] <- next.var
    
    close(fid)
    return(dat)
}
