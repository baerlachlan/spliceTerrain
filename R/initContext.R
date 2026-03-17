#' @keywords internal
.initContext <- function(args, defaults) {
    ## This code chunk is also documentation for the context (ctx) structure
    ctx <- list(
        args = args,
        data = list(gal = NULL, cov = NULL, juncs = NULL),
        plot = list(map = NULL, plist = NULL)
    )
    ctx$args$strandedness <- match.arg(
        ctx$args$strandedness, eval(defaults$strandedness)
    )
    ctx <- .checkBam(ctx)
    ctx <- .checkColours(ctx)
    ctx
}

#' @keywords internal
.checkBam <- function(ctx) {
    bam <- ctx$args$bam
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
    ctx$args$bam <- bam
    ctx
}

#' @keywords internal
.checkColours <- function(ctx) {
    if (!(length(ctx$args$colours) %in% c(1, length(ctx$args$bam))))
        stop("`colours` must be length 1 or the number of BAMs")
    if (length(ctx$args$colours) == 1) ctx$args$colours <- rep(
        ctx$args$colours, length(ctx$args$bam)
    )
    names(ctx$args$colours) <- names(ctx$args$bam) # For subsetting below
    ctx
}
