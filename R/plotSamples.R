#' @importFrom rlang .data
#' @keywords internal
.plotSamples <- function(
        plist, region,  coverage, junctions, lsv, arc_height, highlight,
        colours, j_text_size, highlight_colour, common_y
) {

    if (!(length(colours) %in% c(1, length(plist))))
        stop("`colours` must be length 1 or the number of BAMs")
    if (length(colours) == 1) colours <- rep(colours, length(plist))
    names(colours) <- names(plist) # For subsetting below

    coverage <- split(coverage, coverage$sample)
    junctions <- split(junctions, junctions$sample)

    out <- lapply(names(plist), \(i){
        p <- plist[[i]]
        p <- .plotCoverage(p, coverage[[i]], colours[[i]])
        p <- .plotJunctions(
            p, junctions[[i]], coverage[[i]], lsv, arc_height,
            colours[[i]], j_text_size
        )
        if (!is.null(highlight)) p <- .plotHighlight(
            p, highlight, highlight_colour
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

    if (common_y) {
        st <- BiocGenerics::start(region)
        en <- BiocGenerics::end(region)
        ## TODO: check this is the correct way to access ggplot obj data
        ys <- unlist(lapply(out, \(x){
            pdat <- ggplot2::ggplot_build(x)@data
            unlist(lapply(pdat, `[[`, "y"))
        }))
        out <- lapply(out, \(p){
            p + ggplot2::coord_cartesian(
                xlim = c(st, en), ylim = c(min(ys), max(ys)), clip = "off"
            )
        })
    } else {
        out <- lapply(out, \(p){
            p + ggplot2::coord_cartesian(xlim = c(st, en), clip = "off")
        })
    }

    names(out) <- names(plist)
    out

}
