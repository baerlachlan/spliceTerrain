#' @title Visualise reads aligned to a genome as a sashimi plot
#'
#' @description `spliceTerrain()`
#'
#' @details
#' Additional details...
#'
#' @param x The path to the BAM file to read, a \link[Rsamtools]{BamFile} object, or a \link[Rsamtools]{BamViews} object.
#' @param ... Passed to \link[GenomicAlignments]{readGAlignments}
#'
#' @return A \link[GenomicAlignments]{GAlignments} object.
#'
#' @examples
#' fl <- system.file("extdata", "COG7.bam", package="spliceTerrain")
#' data("COG7_gene")
#' data("COG7_exons")
#' spliceTerrain(bam = fl, region = COG7_gene, annotation = COG7_exons)
#' spliceTerrain(bam = fl, region = COG7_gene, annotation = COG7_exons, squish_introns = TRUE, strandedness = "reverse")
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
        squish_width = 100L,
        min_arrow = 101L
) {

    strandedness <- match.arg(strandedness)
    if (is.null(names(bam))) names(bam) <- sub("\\.bam$", "", basename(bam))

    region <- .resolveRegion(region)
    annotation <- .resolveAnnotation(annotation, region)
    gal <- .loadAlignments(bam, region, strandedness, min_mapq)
    coverage <- .resolveCoverage(gal, region, min_coverage)
    junctions <- .resolveJunctions(gal, region, min_junction_reads)

    if (squish_introns) {
        map <- .buildMap(
            annotation, coverage,
            .splitAnchors(junctions), .splitAnchors(region)
        )
        annotation <- .mapGenomeToPlot(annotation, map)
        coverage <- .mapGenomeToPlot(coverage, map)
        junctions <- .mapGenomeToPlot(junctions, map)
        region <- .mapGenomeToPlot(region, map)
        # browser()
        .mapPlotToGenome(c(1684, 1686, 650, 687), map)
    } else {
        map <- NULL
    }

    plot_list <- lapply(bam, \(i){ggplot2::ggplot()})
    plot_list <- .plotCoverage(plot_list, coverage)
    plot_list <- .plotJunctions(plot_list, junctions)
    plot_list <- .plotAnnotation(plot_list, annotation, min_arrow)
    .plotTerrain(plot_list, region, map)

}
