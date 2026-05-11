#' @keywords internal
.applyMap <- function(ctx) {
    if (!ctx$input$compress_introns) return(ctx)
    ctx$plot$map <- .buildMap(
        ctx$input$cov,
        .rangesToAnchors(ctx$input$juncs),
        ctx$input$annotation,
        .rangesToAnchors(ctx$input$region),
        .rangesToAnchors(ctx$input$psi),
        .rangesToAnchors(ctx$input$highlight),
        gap = ctx$input$intron_width
    )
    ## Because .mapGenomeToPlot may need to return NULL, we always return
    ## a list, otherwise the element will be removed from ctx
    ## TODO: is this the best strategy?
    ctx$plot["cov"] <- .mapGenomeToPlot(ctx$input$cov, ctx$plot$map)
    ctx$plot["juncs"] <- .mapGenomeToPlot(ctx$input$juncs, ctx$plot$map)
    ctx$plot["annotation"] <- .mapGenomeToPlot(
        ctx$input$annotation, ctx$plot$map
    )
    ctx$plot["region"] <- .mapGenomeToPlot(ctx$input$region, ctx$plot$map)
    ctx$plot["psi"] <- .mapGenomeToPlot(ctx$input$psi, ctx$plot$map)
    ctx$plot["highlight"] <- .mapGenomeToPlot(ctx$input$highlight, ctx$plot$map)
    ctx
}

#' @keywords internal
.buildMap <- function(..., gap) {
    objs <- list(...)
    all_ranges <- do.call(c, objs)
    blocks <- GenomicRanges::reduce(all_ranges, ignore.strand = TRUE)
    blocks <- BiocGenerics::sort(blocks)
    n <- length(blocks)
    if (n == 0) return(blocks)
    ## Genomic space
    g_start <- S4Vectors::start(blocks)
    g_end <- S4Vectors::end(blocks)
    g_width <- S4Vectors::width(blocks)
    ## Plot widths don't get squished
    p_width <- as.numeric(g_width)
    ## Cumulative plot starts (block1 starts at 1)
    p_start <- c(1, cumsum(p_width[-length(p_width)] + gap) + 1)
    p_end <- p_start + p_width - 1
    m <- blocks
    m$g_start <- g_start
    m$g_end <- g_end
    m$p_start <- p_start
    m$p_end <- p_end
    m
}

#' @keywords internal
.mapGenomeToPlot <- function(gr, map) {
    ## Always returns a list as gr may be NULL (see comment in .applyMap())
    if (is.null(gr)) return(list(gr))
    anchors <- .rangesToAnchors(gr)
    hits <- IRanges::findOverlaps(anchors, map)
    qh <- S4Vectors::queryHits(hits)
    sh <- S4Vectors::subjectHits(hits)
    pos <- BiocGenerics::start(anchors)
    mapped <- map$p_start[sh] + (pos[qh] - map$g_start[sh])
    out <- anchors
    IRanges::ranges(out) <- IRanges::IRanges(start = mapped, end = mapped)
    list(.anchorsToRanges(out, gr))
}

#' @keywords internal
.anchorsToRanges <- function(anchors, gr) {
    anc <- S4Vectors::mcols(anchors)$anchor
    start <- anchors[anc == "start"]
    end <- anchors[anc == "end"]
    x <- GenomicRanges::GRanges(
        seqnames = Seqinfo::seqnames(start),
        ranges = IRanges::IRanges(
            BiocGenerics::start(start), BiocGenerics::start(end)
        ),
        strand = BiocGenerics::strand(start)
    )
    S4Vectors::mcols(x) <- S4Vectors::mcols(gr)
    x
}
