#' @keywords internal
.normaliseCounts <- function(x, sample, ctx) {
    if (is.null(ctx$input$lib_size)) return(x)
    x / ctx$input$effective_lib_size[sample] * ctx$input$normalise_to
}
