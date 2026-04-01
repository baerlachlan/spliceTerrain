#' Visualise RNA-seq alignments as a sashimi-style coverage + junction plot
#'
#' @description
#' \code{spliceTerrain()} draws a sashimi-style plot for one or more BAM files
#' over a genomic region, combining per-base coverage with splice junction arcs.
#' Optionally, introns can be compressed to improve readability, highlighted
#' intervals can be overlaid, and transcript-style annotation can be drawn on an
#' additional track.
#'
#' @param bam A character vector of BAM file paths, or a named character vector.
#' Names (if present) are used as sample labels. If unnamed, labels are derived
#' from the BAM basenames with the \code{.bam} suffix removed. BAM files should
#' be indexed for region-restricted import.
#'
#' @param region Genomic interval to plot. May be a
#' \link[GenomicRanges]{GRanges}, a \link[GenomicRanges]{GRangesList}, or a
#' string of the form \code{"chr:start-end"} (optionally including strand as
#' \code{"chr:start-end:strand"}). If multiple ranges are supplied, their span
#' is used as the plotting region.
#'
#' @param annotation Optional annotation track. Must be a
#' \link[GenomicRanges]{GRangesList}. Each element is drawn as a separate
#' grouped feature track. Names are used as y-axis labels, and if absent,
#' default group labels are generated internally. Annotation is restricted to
#' the plotting region.
#'
#' @param psi Optional psi region used for annotating junction labels with
#' within-psi junction usage. Accepts the same formats as \code{region} (e.g.
#' \code{"chr:start-end"} or a \code{GRanges}), but must resolve to exactly one
#' genomic range. Only junctions within the supplied psi are labelled with psi
#' percentages.
#'
#' @param highlight Optional interval(s) to highlight. Accepts the same formats
#' as \code{region}. Multiple ranges may be supplied. Highlighted intervals are
#' drawn as shaded vertical bands spanning the full panel height.
#'
#' @param strandedness Library strandedness for each BAM. Must be one of
#' \code{"unstranded"}, \code{"forward"}, or \code{"reverse"}, and may be
#' supplied either as a single value applied to all BAMs or as one value per
#' BAM.
#'
#' @param min_mapq Integer scalar. Minimum mapping quality (MAPQ) for reads to
#' be included when computing coverage and junctions.
#'
#' @param min_coverage Minimum coverage required for a position (or segment) to
#' be retained for plotting. May be supplied either as a single integer applied
#' to all BAMs or as one value per BAM.
#'
#' @param min_junction_reads Minimum number of split reads supporting a junction
#' for it to be drawn. May be supplied either as a single integer applied to all
#' BAMs or as one value per BAM.
#'
#' @param compress_introns Logical. If \code{TRUE}, compress introns to improve
#' visual clarity while keeping exonic structure readable.
#'
#' @param intron_width Integer scalar. Target width used for intronic gaps when
#' \code{compress_introns = TRUE}.
#'
#' @param min_arrow Integer scalar. Minimum width required before drawing
#' directional arrows/indicators on annotation introns. This is most relevant
#' when introns are squished.
#'
#' @param common_y Logical. If \code{TRUE}, use a common y-axis range across
#' all sample panels. If \code{FALSE}, each sample panel is scaled
#' independently. This affects sample panels only, not the annotation track.
#'
#' @param arc_height Numeric scalar controlling the relative height of junction
#' arcs. Larger values produce taller arcs for the same junction span and
#' increase separation.
#'
#' @param scale_arcs Logical. If \code{TRUE}, scale junction arc line width
#' according to junction support. If \code{FALSE}, use a constant line width for
#' all junction arcs.
#'
#' @param colours Colour used for each sample's coverage bars and junction arcs.
#' Must be either length 1, in which case the same colour is used for all BAMs,
#' or the same length as \code{bam}, in which case one colour is used per
#' sample.
#'
#' @param highlight_colour Fill and border colour used for highlighted regions.
#' The default is a semi-transparent red produced with
#' \code{scales::alpha("red", 0.2)}.
#'
#' @param anno_text_col Optional character scalar naming a metadata column in
#' \code{annotation} to display as text within annotation features.
#' Ignored if \code{annotation = NULL}.
#'
#' @param anno_text_size Numeric scalar giving the text size used for annotation
#' labels drawn from \code{anno_text_col}.
#'
#' @param junc_text_size Numeric scalar giving the size of junction count
#' labels. Passed to \code{ggplot2::geom_label()} for junction annotations.
#'
#' @param panel_heights Numeric vector controlling relative heights of the plot
#' panels (samples and optional annotation). Recycled as needed.
#'
#' @param axis_title_size Numeric scalar giving the axis title text size used
#' in the final plot theme.
#'
#' @param axis_text_size Numeric scalar giving the axis tick-label text size
#' used in the final plot theme.
#'
#' @param return_ctx Logical. Intended to control whether processed plotting
#' data are returned instead of the plot.
#'
#' @details
#' The plot is built in three conceptual steps:
#' \enumerate{
#'   \item Alignments overlapping \code{region} are loaded from each BAM and
#'   filtered by mapping quality (\code{min_mapq}) and library strandedness
#'   (\code{strandedness}).
#'   \item Coverage and junction counts are computed and filtered by
#'   \code{min_coverage} and \code{min_junction_reads}.
#'   \item If \code{compress_introns = TRUE}, a piecewise mapping is constructed
#'   that compresses intronic gaps to a fixed width (\code{intron_width}) while
#'   preserving relative exon geometry. Coverage, junctions, and optional
#'   features (\code{annotation}, \code{psi}, \code{highlight}) are mapped into
#'   the plot coordinate system.
#' }
#'
#' \code{region}, \code{psi}, and \code{highlight} may be supplied as genomic
#' ranges or as strings coercible to \code{GRanges}. If multiple ranges are
#' supplied for \code{region}, their span is used as the plotting window.
#' Optional genomic overlays are restricted to the plotting region before
#' plotting.
#'
#' \code{psi} and \code{highlight} are optional overlays:
#' \itemize{
#'   \item \code{psi} defines a local splicing variation (psi) region. Junctions
#'   within this region are additionally labelled with the fraction of junction
#'   reads out of the total junction reads in the psi, expressed as a percent.
#'   \item \code{highlight} marks one or more genomic intervals to draw
#'   attention to a subregion (e.g. an exon or event).
#' }
#'
#' The arguments \code{strandedness}, \code{min_coverage},
#' \code{min_junction_reads}, and \code{colours} may each be supplied either as
#' length 1, in which case the value is recycled across all BAMs, or as one
#' value per BAM.
#'
#' When \code{compress_introns = TRUE}, x-axis tick labels are reported in
#' genome coordinates even though tick positions are in plot coordinates.
#'
#' Annotation is drawn on a separate panel beneath the sample panels.
#' Highlighted intervals are overlaid across both sample and annotation panels
#' when present.
#'
#' @return
#' If \code{return_ctx = FALSE}, a \pkg{patchwork} object containing one plot
#' per BAM plus an optional annotation track.
#'
#' If \code{return_ctx = TRUE}, a list containing the processed plotting data
#' and the assembled plot.
#'
#' The x-axis is shared across panels. Tick labels are shown in genome
#' coordinates, including when introns are compressed.
#'
#' @seealso
#' \link[GenomicAlignments]{readGAlignments} for reading alignments and
#' \link[GenomicRanges]{GRanges} for representing genomic intervals.
#'
#' @examples
#' library(RNAseqData.HNRNPC.bam.chr14)
#' spliceTerrain(
#'   bam = RNAseqData.HNRNPC.bam.chr14_BAMFILES[c(7,1)],
#'   region = "chr14:70222436-70237375",
#'   psi = "chr14:70234854-70234854"
#' )
#'
#' @rdname spliceTerrain-methods
#' @aliases spliceTerrain
#' @export
spliceTerrain <- function(
        bam,
        region,
        annotation = NULL,
        psi = NULL,
        highlight = NULL,
        strandedness = "unstranded",
        min_mapq = 0L,
        min_coverage = 10,
        min_junction_reads = 10,
        compress_introns = TRUE,
        intron_width = 50,
        min_arrow = intron_width + 1,
        common_y = FALSE,
        arc_height = 0.15,
        scale_arcs = FALSE,
        colours = "black",
        highlight_colour = scales::alpha("red", 0.2),
        anno_text_col = NULL,
        anno_text_size = 3,
        junc_text_size = 3,
        panel_heights = 1,
        axis_title_size = 12,
        axis_text_size = 9,
        return_ctx = FALSE,
        ctx = NULL
) {
    if (is.null(ctx)) {
        ctx <- .initContext(as.list(environment()))
        ctx <- .resolveRegion(ctx)
        ctx <- .resolveAnnotation(ctx)
        ctx <- .resolveGr(ctx, "psi")
        ctx <- .resolveGr(ctx, "highlight")
        ctx <- .loadAlignments(ctx)
        ctx <- .getCoverage(ctx)
        ctx <- .getJunctions(ctx)
        ctx <- .applyMap(ctx)
        if (ctx$input$return_ctx) return(ctx)
    }
    ctx <- .plotSamples(ctx)
    ctx <- .plotAnnotation(ctx)
    .plotTerrain(ctx)
}
