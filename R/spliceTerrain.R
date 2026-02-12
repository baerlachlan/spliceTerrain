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
#' spliceTerrain(bam = fl, region = COG7_gene, annotation = COG7_exons, squish_introns = TRUE, strandedness = "reverse", lsv = "16:23389087-23389087")
#'
#' @rdname spliceTerrain-methods
#' @aliases spliceTerrain
#' @export
spliceTerrain <- function(
        bam, # TODO: change bam to x to allow other input types
        region,
        annotation,
        strandedness = c("unstranded", "forward", "reverse"),
        min_mapq = 0L,
        min_coverage = 10L,
        min_junction_reads = 10L,
        squish_introns = FALSE,
        squish_to = 50L,
        min_arrow = squish_to + 1L,
        arc_height = 0.15,
        lsv = NULL
) {

    strandedness <- match.arg(strandedness)
    if (is.null(names(bam))) names(bam) <- sub("\\.bam$", "", basename(bam))

    region <- .resolveRegion(region)
    if (!is.null(lsv)) lsv <- .resolveRegion(lsv)
    annotation <- .resolveAnnotation(annotation, region)
    gal <- .loadAlignments(bam, region, strandedness, min_mapq)
    coverage <- .resolveCoverage(gal, region, min_coverage)
    junctions <- .resolveJunctions(
        gal, region, min_junction_reads, strandedness
    )

    if (squish_introns) {
        anchors <- c(.rangesToAnchors(junctions), .rangesToAnchors(region))
        if (!is.null(lsv)) anchors <- c(anchors, .rangesToAnchors(lsv))
        map <- .buildMap(annotation, coverage, anchors, gap = squish_to)
        annotation <- .mapGenomeToPlot(annotation, map)
        coverage <- .mapGenomeToPlot(coverage, map)
        junctions <- .mapGenomeToPlot(junctions, map)
        region <- .mapGenomeToPlot(region, map)
        if (!is.null(lsv)) lsv <- .mapGenomeToPlot(lsv, map)
    } else {
        map <- NULL
    }

    plot_list <- lapply(bam, \(i){ggplot2::ggplot()})
    plot_list <- .plotCoverage(plot_list, coverage)
    plot_list <- .plotJunctions(plot_list, junctions, coverage, lsv, arc_height)
    plot_list <- .plotAnnotation(plot_list, annotation, min_arrow)
    .plotTerrain(plot_list, region, map)

}
