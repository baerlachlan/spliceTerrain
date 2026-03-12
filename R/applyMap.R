#' @keywords internal
.applyMap <- function(ctx) {
    browser()
    ctx$plot$map <- .buildMap(
        ctx$data$cov, ctx$input$annotation, .rangesToAnchors(ctx$data$juncs),
        .rangesToAnchors(ctx$input$region), .rangesToAnchors(ctx$input$lsv),
        .rangesToAnchors(ctx$input$highlight), gap = ctx$args$squish_to
    )
    ctx$data$cov <- .mapGenomeToPlot(ctx$data$cov, ctx$plot$map)
    ctx$data$junctions <- .mapGenomeToPlot(ctx$data$junctions, ctx$plot$map)
    ctx$input$annotation <- .mapGenomeToPlot(ctx$input$annotation, ctx$plot$map)
    ctx$input$region <- .mapGenomeToPlot(ctx$input$region, ctx$plot$map)
    ctx$input$lsv <- .mapGenomeToPlot(ctx$input$lsv, ctx$plot$map)
    ctx$input$highlight <- .mapGenomeToPlot(ctx$input$highlight, ctx$plot$map)
    ctx
}
