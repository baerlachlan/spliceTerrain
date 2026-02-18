#' @keywords internal
.check <- function(bam, region, annotation, lsv, highlight) {

    if (missing(bam) || is.null(bam) || length(bam) < 1L || !is.character(bam))
        stop("`bam` must be a non-empty character vector of BAM file paths.")
    if (anyNA(bam) || any(!nzchar(bam)))
        stop("`bam` must not contain NA or empty strings.")
    missing_bam <- bam[!file.exists(bam)]
    if (length(missing_bam) > 0L)
        stop(
            "The following BAM file(s) do not exist:\n",
            paste0("- ", missing_bam, collapse = "\n")
        )
    if (any(!nzchar(names(bam))) || anyNA(names(bam)))
        stop("`bam` names must not be NA or empty.")
    if (anyDuplicated(names(bam))) stop("`bam` sample names must be unique.")



}
