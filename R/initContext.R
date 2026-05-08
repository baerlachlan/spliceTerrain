#' @keywords internal
.initContext <- function(args) {
    ## This also serves as dev documentation for the context (ctx) structure
    ctx <- list(
        input = c(args, list(gal = NULL, cov = NULL, juncs = NULL)),
        plot = list(
            cov = NULL, juncs = NULL, map = NULL, annotation = NULL,
            region = NULL, psi = NULL, highlight = NULL, plist = NULL
        )
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
    bam <- ctx$input$bam
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
    ctx$input$bam <- bam
    ctx
}

#' @keywords internal
.checkColours <- function(ctx) {
    if (!(length(ctx$input$colours) %in% c(1, length(ctx$input$bam))))
        stop("`colours` must be length 1 or the number of BAMs")
    if (length(ctx$input$colours) == 1) ctx$input$colours <- rep(
        ctx$input$colours, length(ctx$input$bam)
    )
    names(ctx$input$colours) <- names(ctx$input$bam)
    ctx
}

#' @keywords internal
.checkStrandedness <- function(ctx) {
    if (!(length(ctx$input$strandedness) %in% c(1, length(ctx$input$bam))))
        stop("`strandedness` must be length 1 or the number of BAMs")
    if (length(ctx$input$strandedness) == 1) ctx$input$strandedness <- rep(
        ctx$input$strandedness, length(ctx$input$bam)
    )
    choices <- c("unstranded", "forward", "reverse")
    ctx$input$strandedness <- vapply(
        ctx$input$strandedness,
        function(x) {
            tryCatch(
                match.arg(x, choices),
                error = function(e) {
                    stop(
                        "`strandedness` must be one of: ",
                        paste(choices, collapse = ", "),
                        call. = FALSE
                    )
                }
            )
        },
        FUN.VALUE = character(1)
    )
    names(ctx$input$strandedness) <- names(ctx$input$bam)
    ctx
}

#' @keywords internal
.checkMinCoverage <- function(ctx) {
    if (!(length(ctx$input$min_coverage) %in% c(1, length(ctx$input$bam))))
        stop("`min_coverage` must be length 1 or the number of BAMs")
    if (length(ctx$input$min_coverage) == 1) ctx$input$min_coverage <- rep(
        ctx$input$min_coverage, length(ctx$input$bam)
    )
    names(ctx$input$min_coverage) <- names(ctx$input$bam)
    ctx
}

#' @keywords internal
.checkMinJunctionReads <- function(ctx) {
    len <- length(ctx$input$min_junction_reads)
    len_check <- length(ctx$input$bam)
    if (!(len %in% c(1, len_check)))
        stop("`min_junction_reads` must be length 1 or the number of BAMs")
    if (length(ctx$input$min_junction_reads) == 1)
        ctx$input$min_junction_reads <- rep(
            ctx$input$min_junction_reads, length(ctx$input$bam)
        )
    names(ctx$input$min_junction_reads) <- names(ctx$input$bam)
    ctx
}
