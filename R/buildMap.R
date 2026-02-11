#' @keywords internal
.buildMap <- function(..., gap) {

    objs <- list(...)
    all_ranges <- do.call(c, objs)
    blocks <- GenomicRanges::reduce(all_ranges, ignore.strand = TRUE)
    blocks <- BiocGenerics::sort(blocks)

    n <- length(blocks)
    if (n == 0L) return(blocks)

    ## Genomic space
    g_start <- S4Vectors::start(blocks)
    g_end <- S4Vectors::end(blocks)
    g_width <- S4Vectors::width(blocks)

    ## Plot widths don't get squished
    p_width <- as.numeric(g_width)

    ## Cumulative plot starts (block1 starts at 1)
    p_start <- c(1, cumsum(p_width[-length(p_width)] + gap + 1))
    p_end <- p_start + p_width - 1

    m <- blocks
    m$g_start <- g_start
    m$g_end <- g_end
    m$p_start <- p_start
    m$p_end <- p_end
    m$gap <- gap
    m

}
