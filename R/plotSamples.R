#' @importFrom rlang .data
#' @keywords internal
.plotSamples <- function(
        plist, coverage, junctions, lsv, arc_height, highlight
) {

    coverage <- split(coverage, coverage$sample)
    junctions <- split(junctions, junctions$sample)

    out <- lapply(names(plist), \(i){
        p <- plist[[i]]
        if (!is.null(highlight)) p <- .plotHighlight(p, highlight)
        p <- .plotCoverage(p, coverage[[i]])
        p <- .plotJunctions(
            p, junctions[[i]], coverage[[i]], lsv, arc_height
        )
        p <- p + ggplot2::scale_y_continuous(
            ## Don't show y < 0
            breaks = \(x) {
                b <- scales::breaks_extended()(x)
                b[b >= 0]
            },
            expand = c(0.1, 0.1) # helps lsv labels being clipped
        )
        p <- p + ggplot2::labs(
            x = "", y = i
        )
        p
    })

    names(out) <- names(plist)
    out

}
