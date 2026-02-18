#' @keywords internal
.resolveBam <- function(bam) {

    missing_bam <- bam[!file.exists(bam)]
    if (length(missing_bam) > 0L)
        stop(
            "The following BAM file(s) do not exist:\n",
            paste0("- ", missing_bam, collapse = "\n")
        )
    if (any(!nzchar(names(bam))) || anyNA(names(bam)))
        stop("`bam` names must not be NA or empty.")

    if (is.null(names(bam))) names(bam) <- sub("\\.bam$", "", basename(bam))

    if (anyDuplicated(names(bam))) stop("`bam` sample names must be unique.")

    bam

}
