#' @keywords internal
.plotTerrain <- function(ctx) {
    p <- patchwork::wrap_plots(
        ctx$plot$plist, ncol = 1, heights = ctx$input$panel_heights
    )
    p <- p + patchwork::plot_layout(axes = "collect_x")
    p
}
