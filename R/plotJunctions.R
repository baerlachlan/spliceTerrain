#' @keywords internal
.plotJunctions <- function(plot_list, junctions, coverage, lsv, arc_height) {

    junctions <- split(junctions, junctions$sample)
    coverage <- split(coverage, coverage$sample)

    out_plots <- lapply(names(plot_list), function(sample_id) {

        junc <- junctions[[sample_id]]
        cov <- coverage[[sample_id]]

        layout <- .junctionArcLayout(junc, cov, arc_height)
        arcs <- .junctionArcPoints(layout)
        labels <- .junctionArcLabels(layout, junc, lsv)

        .junctionGeoms(plot_list[[sample_id]], arcs, labels)

    })

    names(out_plots) <- names(plot_list)
    out_plots
}

#' @keywords internal
.coverageAtPos <- function(coverage, pos) {
    hits <- IRanges::findOverlaps(pos, coverage)
    out <- numeric(length(pos))
    if (length(hits)) {
        qh <- S4Vectors::queryHits(hits)
        sh <- S4Vectors::subjectHits(hits)
        out[qh] <- coverage$coverage[sh]
    }
    out
}
