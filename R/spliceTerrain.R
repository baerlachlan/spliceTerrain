#' Visualise RNA-seq alignments as sashimi-style plots
#'
#' @description
#' \code{spliceTerrain()} draws sashimi-style plots for one or more BAM files
#' over a genomic region. Plots combine per-base coverage, splice junction arcs,
#' optional transcript-style annotation, and optional highlighted regions.
#' Intronic or otherwise uninformative gaps can be compacted so the plotting
#' area is focused on observed or annotated features.
#'
#' @param bam Character vector of BAM file paths. If unnamed, sample labels are
#' derived from BAM basenames with the \code{.bam} suffix removed. If names are
#' supplied, all names must be non-empty and unique. BAM files should be indexed
#' for region-restricted import. Single-end and paired-end BAMs are both
#' supported; paired-end status is detected from the BAM flags.
#'
#' @param region Genomic interval to plot. May be a
#' \link[GenomicRanges]{GRanges}, a \link[GenomicRanges]{GRangesList}, or a
#' character string coercible to \code{GRanges}, such as
#' \code{"chr:start-end"} or \code{"chr:start-end:strand"}. Commas, spaces, and
#' en/em dashes in character regions are normalised before coercion. If multiple
#' ranges are supplied, they must all be on the same seqname and are reduced to
#' a single span used as the BAM query and plotting window.
#'
#' @param annotation Optional annotation track. Must be a
#' \link[GenomicRanges]{GRangesList}, with each list element representing one
#' feature group such as a transcript model. Each group is drawn on its own
#' annotation row. List element names are used as y-axis labels; if absent,
#' default group labels are generated. Annotation is restricted to the plotting
#' region.
#'
#' @param psi Optional genomic interval used to annotate junction labels with
#' local junction usage. Accepts the same formats as \code{region}. Junctions
#' with a start or end anchor overlapping \code{psi} are labelled with their
#' fraction of total junction reads among the selected junctions.
#'
#' @param highlight Optional interval(s) to highlight. Accepts the same formats
#' as \code{region}. Multiple ranges may be supplied. Highlighted intervals are
#' drawn as shaded vertical bands spanning the full panel height.
#'
#' @param strandedness Library strandedness for each BAM. Must be one of
#' \code{"unstranded"}, \code{"forward"}, or \code{"reverse"}, and may be
#' supplied either as a single value applied to all BAMs or as one value per
#' BAM. Use \code{"unstranded"} when reads from both strands should be included.
#' For stranded paired-end BAMs, this value is passed to
#' \link[GenomicAlignments]{readGAlignmentPairs} as \code{strandMode}. If
#' \code{region} includes a strand and \code{strandedness} is not
#' \code{"unstranded"}, imported alignments are restricted to overlaps on the
#' resolved region strand.
#'
#' @param min_mapq Integer scalar. Minimum mapping quality (MAPQ) for alignments
#' to be imported from the BAM file.
#'
#' @param min_coverage Integer scalar or integer vector. Minimum per-base
#' coverage required for positions to be retained for plotting. May be supplied
#' either as a single value applied to all BAMs or as one value per BAM.
#'
#' @param min_junction_reads Integer scalar or integer vector. Minimum number of
#' split reads supporting a junction for it to be retained for plotting. May be
#' supplied either as a single value applied to all BAMs or as one value per BAM.
#'
#' @param compress_introns Logical scalar. If \code{TRUE}, compact gaps between
#' observed or annotated genomic blocks so plotting space is focused on regions
#' containing coverage, junctions, annotations, or supplied overlays. If
#' \code{FALSE}, plot genomic coordinates directly.
#'
#' @param intron_width Integer scalar. Approximate plot-space width used for
#' compacted gaps when \code{compress_introns = TRUE}.
#'
#' @param min_arrow Integer scalar. Minimum annotation intron width required
#' before drawing directional arrows. This is most relevant when introns are
#' compacted.
#'
#' @param common_y Logical scalar. If \code{TRUE}, use a common y-axis range
#' across all sample panels. If \code{FALSE}, each sample panel is scaled
#' independently. This affects sample panels only, not the annotation panel.
#'
#' @param arc_height Numeric scalar controlling junction arc height relative to
#' the coverage scale. Larger values produce taller arcs and greater separation
#' between stacked arcs.
#'
#' @param scale_arcs Logical scalar. If \code{TRUE}, scale junction arc line
#' width by junction read count after filtering. If \code{FALSE}, use a constant
#' line width for all junction arcs.
#'
#' @param colours Character vector of colours used for each sample's coverage
#' bars and junction arcs. Must be either length 1, in which case the same
#' colour is used for all BAMs, or the same length as \code{bam}, in which case
#' one colour is used per sample.
#'
#' @param highlight_colour Fill and border colour used for highlighted regions.
#' The default is a semi-transparent red produced with
#' \code{scales::alpha("red", 0.2)}.
#'
#' @param anno_text_col Optional character scalar naming a metadata column in
#' \code{annotation} to display as text within annotation features. Ignored if
#' \code{annotation = NULL} or if the named column is absent.
#'
#' @param anno_text_size Numeric scalar giving the text size used for annotation
#' labels drawn from \code{anno_text_col}.
#'
#' @param junc_text_size Numeric scalar giving the size of junction count
#' labels. Passed to \code{ggplot2::geom_label()} for junction annotations.
#'
#' @param panel_heights Numeric vector controlling relative heights of the plot
#' panels. Values are passed to \code{patchwork::wrap_plots()}. The intended
#' length is the number of BAM sample panels plus one additional value if an
#' annotation panel is shown; shorter vectors are recycled by patchwork.
#'
#' @param axis_title_size Numeric scalar giving the axis title text size used
#' in the final plot theme.
#'
#' @param axis_text_size Numeric scalar giving the axis tick-label text size
#' used in the final plot theme.
#'
#' @param return_ctx Logical scalar. If \code{FALSE}, return the assembled
#' sashimi plot. If \code{TRUE}, return the processed context list after BAM
#' import, coverage and junction summarisation, filtering, and coordinate
#' mapping, but before final plot assembly. This is mainly useful for inspecting
#' or modifying processed data before plotting.
#'
#' @param ctx Optional context list previously returned by
#' \code{spliceTerrain(..., return_ctx = TRUE)}. When supplied, BAM import,
#' summarisation, filtering, and coordinate mapping are skipped, and the context
#' is plotted directly. This supports advanced workflows where users inspect or
#' modify processed data before drawing the final plot.
#'
#' @details
#' The plot is built in three conceptual steps:
#' \enumerate{
#'   \item Alignments overlapping \code{region} are imported from each BAM file.
#'   Secondary and supplementary alignments are ignored, and \code{min_mapq} is
#'   applied during import.
#'   \item Coverage and splice junction counts are summarised from the imported
#'   alignments and filtered by \code{min_coverage} and
#'   \code{min_junction_reads}.
#'   \item If \code{compress_introns = TRUE}, a plot-space map is constructed to
#'   compact gaps between observed or annotated genomic blocks. Coverage,
#'   junctions, annotation, and optional overlays are then mapped into this
#'   compacted coordinate system.
#' }
#'
#' \code{region}, \code{psi}, and \code{highlight} may be supplied as genomic
#' ranges or as character strings coercible to \code{GRanges}. If multiple
#' ranges are supplied for \code{region}, they must all be on the same seqname;
#' their span is used as the BAM query and plotting window. Optional genomic
#' overlays are restricted to the plotting region before plotting.
#'
#' \code{psi} and \code{highlight} are optional overlays:
#' \itemize{
#'   \item \code{psi} identifies a local region used to add percentage labels to
#'   selected junctions. Junctions with an anchor overlapping \code{psi} are
#'   labelled with their fraction of total reads among those selected junctions.
#'   \item \code{highlight} marks one or more genomic intervals to draw
#'   attention to a subregion, such as an exon, splice site, or event.
#' }
#'
#' The arguments \code{strandedness}, \code{min_coverage},
#' \code{min_junction_reads}, and \code{colours} may each be supplied either as
#' length 1, in which case the value is recycled across all BAMs, or as one
#' value per BAM.
#'
#' Strand filtering is applied after BAM import. Unstranded libraries retain
#' alignments from both strands. For stranded libraries, reads are interpreted
#' according to \code{strandedness}; when the resolved \code{region} has a
#' specific strand, alignments are then restricted to that strand.
#'
#' When \code{compress_introns = TRUE}, the plot uses compacted coordinates
#' internally, but x-axis tick labels are reported as approximate genomic
#' coordinates.
#'
#' Annotation is drawn on a separate panel beneath the sample panels.
#' Highlighted intervals are overlaid across both sample and annotation panels
#' when present.
#'
#' For advanced workflows, \code{return_ctx = TRUE} can be used to inspect or
#' modify the processed context before plotting. Passing the modified context
#' back through \code{ctx} skips the data-processing steps and rebuilds the plot
#' from that context.
#'
#' @return
#' If \code{return_ctx = FALSE}, a \pkg{patchwork} object containing one sample
#' panel per BAM file plus an optional annotation panel.
#'
#' If \code{return_ctx = TRUE}, a context list with \code{input} and \code{plot}
#' components. The \code{input} component contains validated user inputs and
#' processed alignment, coverage, and junction data. The \code{plot} component
#' contains plotting ranges and mapped plot-space data, but not the final
#' assembled plot.
#'
#' In the returned plot, the x-axis is shared across panels. Tick labels are
#' shown in genome coordinates, including when introns are compressed.
#'
#' @seealso
#' \link[GenomicAlignments]{readGAlignments} and
#' \link[GenomicAlignments]{readGAlignmentPairs} for reading alignments, and
#' \link[GenomicRanges]{GRanges} for representing genomic intervals.
#'
#' @examples
#' if (requireNamespace("RNAseqData.HNRNPC.bam.chr14", quietly = TRUE)) {
#'   bams <- RNAseqData.HNRNPC.bam.chr14::RNAseqData.HNRNPC.bam.chr14_BAMFILES
#'   spliceTerrain(
#'     bam = bams[c(7, 1)],
#'     region = "chr14:70222436-70237375",
#'     psi = "chr14:70234854-70234854"
#'   )
#' }
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
        min_coverage = 0,
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
