#' @keywords internal
.prepareTerrain <- function(ctx) {
    st <- BiocGenerics::start(ctx$plot$region)
    en <- BiocGenerics::end(ctx$plot$region)
    w <- BiocGenerics::width(ctx$plot$region)
    n_break <- ifelse(w < 5, w, 5)
    breaks <- seq(st, en, length.out = n_break)
    if (is.null(ctx$plot$map)) {
        labels <- scales::comma(round(breaks))
    } else {
        labels <- scales::comma(round(.mapPlotToGenome(breaks, ctx$plot$map)))
    }
    ctx$plot$plist <- lapply(ctx$plot$plist, \(p){
        p <- p + ggplot2::scale_x_continuous(breaks = breaks, labels = labels)
        p + .theme_spliceTerrain(
            axis_title_size = ctx$input$axis_title_size,
            axis_text_size = ctx$input$axis_text_size
        )
    })
    ctx
}
