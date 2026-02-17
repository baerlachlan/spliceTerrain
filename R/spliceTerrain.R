#' Visualise RNA-seq alignments as a sashimi-style coverage + junction plot
#'
#' @description
#' \code{spliceTerrain()} draws a sashimi-style plot for one or more BAM files
#' over a genomic region, combining per-base coverage with splice junction arcs.
#' Optionally, introns can be compressed ("squished") to improve readability and
#' annotation features can be drawn on an additional track.
#'
#' @param bam A character vector of BAM file paths, or a named character vector.
#' Names (if present) are used as sample labels. If unnamed, labels are derived
#' from the BAM basenames with the \code{.bam} suffix removed.
#'
#' @param region Genomic interval to plot. May be a
#' \link[GenomicRanges]{GRanges}, or a string of the form
#' \code{"chr:start-end"} (or \code{"chr:start-end:strand"}), which will be
#' parsed into a genomic interval.
#'
#' @param strandedness Library strandedness. One of \code{"unstranded"},
#' \code{"forward"}, or \code{"reverse"}. This affects how reads and junctions
#' are interpreted with respect to strand.
#'
#' @param min_coverage Integer scalar. Minimum coverage required for a position
#' (or segment) to be retained for plotting.
#'
#' @param min_junction_reads Integer scalar. Minimum number of split reads
#' supporting a junction for it to be drawn.
#'
#' @param squish_introns Logical. If \code{TRUE}, compress introns to improve
#' visual clarity while keeping exonic structure readable.
#'
#' @param squish_to Integer scalar. Target width used for intronic gaps when
#' \code{squish_introns = TRUE}.
#'
#' @param annotation Optional annotation track. Must be a
#' \link[GenomicRanges]{GRangesList}. Each element is drawn as a separate
#' grouped feature track. Annotation is restricted to the plotting region.
#'
#' @param lsv Optional LSV region used for annotating junction labels with
#' within-LSV junction usage. Accepts the same formats as \code{region} (e.g.
#' \code{"chr:start-end"} or a \code{GRanges}), but must resolve to exactly one
#' genomic range at present.
#'
#' @param highlight Optional interval(s) to highlight. Accepts the same formats
#' as \code{region} (e.g. \code{"chr:start-end"} or a \code{GRanges}). May be
#' length > 1.
#'
#' @param min_mapq Integer scalar. Minimum mapping quality (MAPQ) for reads to
#' be included when computing coverage and junctions.
#'
#' @param min_arrow Integer scalar. Minimum width required before drawing
#' directional arrows/indicators on annotation features. This is most relevant
#' when introns are squished.
#'
#' @param arc_height Numeric scalar controlling the relative height of junction
#' arcs. Larger values produce taller arcs for the same junction span and
#' increase separation.
#'
#' @param panel_heights Numeric vector controlling relative heights of the plot
#' panels (samples and optional annotation). Recycled as needed.
#'
#' @details
#' The plot is built in three conceptual steps:
#' \enumerate{
#'   \item Alignments overlapping \code{region} are loaded from each BAM and
#'   filtered by mapping quality (\code{min_mapq}) and library strandedness
#'   (\code{strandedness}).
#'   \item Coverage and junction counts are computed and filtered by
#'   \code{min_coverage} and \code{min_junction_reads}.
#'   \item If \code{squish_introns = TRUE}, a piecewise mapping is constructed
#'   that compresses intronic gaps to a fixed width (\code{squish_to}) while
#'   preserving relative exon geometry. Coverage, junctions, and optional
#'   features (\code{annotation}, \code{lsv}, \code{highlight}) are mapped into
#'   the plot coordinate system.
#' }
#'
#' \code{lsv} and \code{highlight} are optional overlays:
#' \itemize{
#'   \item \code{lsv} defines a local splicing variation (LSV) region. Junctions
#'   within this region are additionally labelled with the fraction of junction
#'   reads out of the total junction reads in the LSV, expressed as a percent.
#'   \item \code{highlight} marks one or more genomic intervals to draw
#'   attention to a subregion (e.g. an exon or event).
#' }
#'
#' When \code{squish_introns = TRUE}, x-axis tick labels are reported in genome
#' coordinates (tick positions are in plot coordinates).
#'
#' @return
#' A \pkg{patchwork} object containing one plot per BAM (plus an optional
#' annotation track). The x-axis is shared across panels. Tick labels are shown
#' in genome coordinates, including when introns are squished.
#'
#' @seealso
#' \link[GenomicAlignments]{readGAlignments} and
#' \link[GenomicRanges]{GRanges}.
#'
#' @examples
#' fl <- system.file("extdata", "COG7.bam", package = "spliceTerrain")
#' data("COG7_gene")
#' data("COG7_exons")
#'
#' spliceTerrain(bam = fl, region = COG7_gene, annotation = COG7_exons)
#'
#' spliceTerrain(
#'   bam = fl,
#'   region = COG7_gene,
#'   annotation = COG7_exons,
#'   strandedness = "reverse",
#'   lsv = "16:23389087-23389087",
#'   highlight = "16:23392380-23393318"
#' )
#'
#' @rdname spliceTerrain-methods
#' @aliases spliceTerrain
#' @export
spliceTerrain <- function(
        bam,
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
