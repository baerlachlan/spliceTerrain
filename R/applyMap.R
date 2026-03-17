#' @keywords internal
.applyMap <- function(ctx) {
    if (!ctx$args$squish_introns) return(ctx)
    ctx$plot$map <- .buildMap(
        ctx$data$cov,
        .rangesToAnchors(ctx$data$juncs),
        ctx$args$annotation,
        .rangesToAnchors(ctx$args$region),
        .rangesToAnchors(ctx$args$lsv),
        .rangesToAnchors(ctx$args$highlight),
        gap = ctx$args$squish_to
    )
    ## Because .mapGenomeToPlot may need to return NULL, we always return
    ## a list, otherwise the element will be removed
    ## TODO: alternative strategy
    ctx$data["cov"] <- .mapGenomeToPlot(ctx$data$cov, ctx$plot$map)
    ctx$data["juncs"] <- .mapGenomeToPlot(ctx$data$juncs, ctx$plot$map)
    ctx$args["annotation"] <- .mapGenomeToPlot(
        ctx$args$annotation, ctx$plot$map
    )
    ctx$args["region"] <- .mapGenomeToPlot(ctx$args$region, ctx$plot$map)
    ctx$args["lsv"] <- .mapGenomeToPlot(ctx$args$lsv, ctx$plot$map)
    ctx$args["highlight"] <- .mapGenomeToPlot(ctx$args$highlight, ctx$plot$map)
    ctx
}
