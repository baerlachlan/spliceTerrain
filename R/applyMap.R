#' @keywords internal
.applyMap <- function(ctx) {
    if (!ctx$input$compress_introns) return(ctx)
    ctx$plot$map <- .buildMap(
        ctx$input$cov,
        .rangesToAnchors(ctx$input$juncs),
        ctx$input$annotation,
        .rangesToAnchors(ctx$input$region),
        .rangesToAnchors(ctx$input$lsv),
        .rangesToAnchors(ctx$input$highlight),
        gap = ctx$input$intron_width
    )
    ## Because .mapGenomeToPlot may need to return NULL, we always return
    ## a list, otherwise the element will be removed from ctx
    ## TODO: is this the best strategy?
    ctx$plot["cov"] <- .mapGenomeToPlot(ctx$input$cov, ctx$plot$map)
    ctx$plot["juncs"] <- .mapGenomeToPlot(ctx$input$juncs, ctx$plot$map)
    ctx$plot["annotation"] <- .mapGenomeToPlot(
        ctx$input$annotation, ctx$plot$map
    )
    ctx$plot["region"] <- .mapGenomeToPlot(ctx$input$region, ctx$plot$map)
    ctx$plot["lsv"] <- .mapGenomeToPlot(ctx$input$lsv, ctx$plot$map)
    ctx$plot["highlight"] <- .mapGenomeToPlot(ctx$input$highlight, ctx$plot$map)
    ctx
}
