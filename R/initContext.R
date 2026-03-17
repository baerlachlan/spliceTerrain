#' @keywords internal
.initContext <- function(args) {
    ## This code chunk is also documentation for the context (ctx) structure
    ctx <- list(
        args = args,
        data = list(gal = NULL, cov = NULL, juncs = NULL),
        plot = list(map = NULL, plist = NULL)
    )
    ctx <- .checkBam(ctx)
    ctx <- .checkColours(ctx)
    ctx <- .checkStrandedness(ctx)
    ctx <- .checkMinCoverage(ctx)
    ctx <- .checkMinJunctionReads(ctx)
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
    names(ctx$args$colours) <- names(ctx$args$bam)
    ctx
}

#' @keywords internal
.checkStrandedness <- function(ctx) {
    if (!(length(ctx$args$strandedness) %in% c(1, length(ctx$args$bam))))
        stop("`strandedness` must be length 1 or the number of BAMs")
    if (length(ctx$args$strandedness) == 1) ctx$args$strandedness <- rep(
        ctx$args$strandedness, length(ctx$args$bam)
    )
    ctx$args$strandedness <- vapply(
        ctx$args$strandedness, match.arg, c("unstranded", "forward", "reverse"),
        FUN.VALUE = character(1)
    )
    names(ctx$args$strandedness) <- names(ctx$args$bam)
    ctx
}

#' @keywords internal
.checkMinCoverage <- function(ctx) {
    if (!(length(ctx$args$min_coverage) %in% c(1, length(ctx$args$bam))))
        stop("`min_coverage` must be length 1 or the number of BAMs")
    if (length(ctx$args$min_coverage) == 1) ctx$args$min_coverage <- rep(
        ctx$args$min_coverage, length(ctx$args$bam)
    )
    names(ctx$args$min_coverage) <- names(ctx$args$bam)
    ctx
}

#' @keywords internal
.checkMinJunctionReads <- function(ctx) {
    if (!(length(ctx$args$min_junction_reads) %in% c(1, length(ctx$args$bam))))
        stop("`min_junction_reads` must be length 1 or the number of BAMs")
    if (length(ctx$args$min_junction_reads) == 1)
        ctx$args$min_junction_reads <- rep(
            ctx$args$min_junction_reads, length(ctx$args$bam)
        )
    names(ctx$args$min_junction_reads) <- names(ctx$args$bam)
    ctx
}
