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
#' spliceTerrain(bam = fl, region = COG7_gene, annotation = COG7_exons[[1]])
#'
#' @rdname spliceTerrain-methods
#' @aliases spliceTerrain
#' @export
spliceTerrain <- function(
        bam,
        region,
        annotation,
        strandedness = c("unstranded", "forward", "reverse"),
        min_mapq = 0L,
        min_coverage = 10L,
        min_junction_reads = 10L,
        squish_introns = FALSE,
        squish_width = 100L,
        groups = NULL
) {

    strandedness <- match.arg(strandedness)
# browser()
    region <- .resolveRegion(region)
    annotation <- .resolveAnnotation(annotation, region)
    gal <- .loadAlignments(bam, region, strandedness, min_mapq)
    coverage <- .resolveCoverage(gal, region, min_coverage)
    junctions <- .resolveJunctions(gal, region, min_junction_reads)
    # browser()
    if (squish_introns) {
        junction_anchors <- .splitAnchors(junctions)
        region_anchors <- .splitAnchors(region)
        squish <- squishIntrons(
            annotation, coverage, junction_anchors, region_anchors,
            squish_width = squish_width
        )
        annotation <- squish[[1]]
        coverage <- squish[[2]]
        junction_anchors <- squish[[3]]
        region_anchors <- squish[[4]]
        junctions <- .mergeAnchors(junction_anchors, junctions)
        region <- .mergeAnchors(region_anchors, region)
    }

    p_bam <- lapply(seq_along(bam), \(i){ggplot2::ggplot()})
    p_bam <- .plotCoverage(p_bam, coverage)
    p_bam <- .plotJunctions(p_bam, junctions)
    p_ann <- .plotAnnotation(annotation)
    .plotTerrain(region, p_bam, p_ann)

}
