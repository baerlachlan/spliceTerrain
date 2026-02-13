#' @title Visualise reads aligned to a genome as a sashimi plot
#'
#' @description `spliceTerrain()`
#'
#' @details
#' Additional details...
#'
#' @param x The path to the BAM file to read, a \link[Rsamtools]{BamFile} object, or a \link[Rsamtools]{BamViews} object.
#' @param annotation Must be GRangesList. Annotations will be grouped on a separate track by each element of the list.
#' @param ... Passed to \link[GenomicAlignments]{readGAlignments}
#'
#' @return A \link[GenomicAlignments]{GAlignments} object.
#'
#' @examples
#' fl <- system.file("extdata", "COG7.bam", package="spliceTerrain")
#' data("COG7_gene")
#' data("COG7_exons")
#' spliceTerrain(bam = fl, region = COG7_gene, annotation = COG7_exons)
#' spliceTerrain(bam = fl, region = COG7_gene, annotation = COG7_exons, squish_introns = TRUE, strandedness = "reverse", lsv = "16:23389087-23389087", highlight = "16:23392380-23393318")
#'
#' @rdname spliceTerrain-methods
#' @aliases spliceTerrain
#' @export
spliceTerrain <- function(
        bam, # TODO: change bam to x to allow other input types
        region,
        strandedness = c("unstranded", "forward", "reverse"),
        min_coverage = 10L,
        min_junction_reads = 10L,
        squish_introns = TRUE,
        squish_to = 50L,
        annotation = NULL,
        lsv = NULL,
        highlight = NULL,
        min_mapq = 0L,
        min_arrow = squish_to + 1L,
        arc_height = 0.15,
        panel_heights = 1
) {

    strandedness <- match.arg(strandedness)
    if (is.null(names(bam))) names(bam) <- sub("\\.bam$", "", basename(bam))

    region <- .resolveRegion(region)
    gal <- .loadAlignments(bam, region, strandedness, min_mapq)
    coverage <- .resolveCoverage(gal, region, min_coverage)
    junctions <- .resolveJunctions(
        gal, region, min_junction_reads, strandedness
    )
    if (!is.null(annotation))
        annotation <- .resolveAnnotation(annotation, region)
    if (!is.null(lsv)) lsv <- .resolveRegion(lsv)
    if (!is.null(highlight)) highlight <- .resolveRegion(highlight)

    if (squish_introns) {
        ranges <- list(coverage)
        if (!is.null(annotation)) ranges <- c(ranges, list(annotation))
        ranges <- do.call(c, ranges)
        anchors <- list(.rangesToAnchors(junctions), .rangesToAnchors(region))
        if (!is.null(lsv)) anchors <- c(anchors, list(.rangesToAnchors(lsv)))
        if (!is.null(highlight))
            anchors <- c(anchors, list(.rangesToAnchors(highlight)))
        anchors <- do.call(c, anchors)
        map <- .buildMap(ranges, anchors, gap = squish_to)
        coverage <- .mapGenomeToPlot(coverage, map)
        if (!is.null(annotation))
            annotation <- .mapGenomeToPlot(annotation, map)
        junctions <- .mapGenomeToPlot(junctions, map)
        region <- .mapGenomeToPlot(region, map)
        if (!is.null(lsv)) lsv <- .mapGenomeToPlot(lsv, map)
        if (!is.null(highlight)) highlight <- .mapGenomeToPlot(highlight, map)
    } else {
        map <- NULL
    }

    plist <- lapply(bam, \(i){ggplot2::ggplot()})
    plist <- .plotSamples(
        plist, coverage, junctions, lsv, arc_height, highlight
    )
    if (!is.null(annotation))
        plist <- .plotAnnotation(plist, annotation, min_arrow, highlight)
    .plotTerrain(plist, region, map, panel_heights)

}
