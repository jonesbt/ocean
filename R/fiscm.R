write.fiscm = function(data, filename) {
    unlink(filename)
    sink(filename)
    cat(nrow(data), '\n')
    sink()
    write.table(data, filename, append=TRUE, row.names=FALSE, col.names=FALSE)
}
