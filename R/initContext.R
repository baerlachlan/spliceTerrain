#' @keywords internal
.initContext <- function(args) {
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
    ctx <- .checkArcSide(ctx)
    ctx <- .checkMinCoverage(ctx)
    ctx <- .checkMinJunctionReads(ctx)
    ctx <- .checkNormalisation(ctx)
    ctx
}

#' @keywords internal
.checkBam <- function(ctx) {
    bam <- ctx$input$bam
    missing_bam <- bam[!file.exists(bam)]
    if (length(missing_bam) > 0)
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
.checkArcSide <- function(ctx) {
    choices <- c("both", "above", "below")
    ctx$input$arc_side <- tryCatch(
        match.arg(ctx$input$arc_side, choices),
        error = function(e) {
            stop(
                "`arc_side` must be one of: ",
                paste(choices, collapse = ", "),
                call. = FALSE
            )
        }
    )
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

#' @keywords internal
.checkNormalisation <- function(ctx) {
    n_bam <- length(ctx$input$bam)
    lib_size <- ctx$input$lib_size
    norm_factors <- ctx$input$norm_factors
    normalise_to <- ctx$input$normalise_to

    if (is.null(lib_size)) {
        if (!is.null(norm_factors))
            stop("`norm_factors` requires `lib_size`.")
        if (!is.null(normalise_to))
            stop("`normalise_to` requires `lib_size`.")
        ctx$input$effective_lib_size <- NULL
        return(ctx)
    }

    if (length(lib_size) != n_bam)
        stop("`lib_size` must be NULL or the number of BAMs.")
    if (!is.numeric(lib_size) || any(!is.finite(lib_size)) ||
            any(lib_size <= 0))
        stop("`lib_size` values must be positive finite numbers.")

    if (is.null(norm_factors)) {
        norm_factors <- rep(1, n_bam)
    } else {
        if (length(norm_factors) != n_bam)
            stop("`norm_factors` must be NULL or the number of BAMs.")
        if (!is.numeric(norm_factors) || any(!is.finite(norm_factors)) ||
                any(norm_factors <= 0))
            stop("`norm_factors` values must be positive finite numbers.")
    }

    effective_lib_size <- lib_size * norm_factors
    if (is.null(normalise_to)) {
        normalise_to <- stats::median(effective_lib_size)
    } else if (length(normalise_to) != 1 || !is.numeric(normalise_to) ||
            !is.finite(normalise_to) || normalise_to <= 0) {
        stop("`normalise_to` must be a positive finite number.")
    }

    names(lib_size) <- names(ctx$input$bam)
    names(norm_factors) <- names(ctx$input$bam)
    names(effective_lib_size) <- names(ctx$input$bam)
    ctx$input$norm_factors <- norm_factors
    ctx$input$effective_lib_size <- effective_lib_size
    ctx$input$normalise_to <- normalise_to
    ctx
}
