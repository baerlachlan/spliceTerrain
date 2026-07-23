#' @keywords internal
.normaliseCounts <- function(x, sample, ctx) {
    if (is.null(ctx$input$lib_size)) return(x)
    x / ctx$input$effective_lib_size[sample] * ctx$input$normalise_to
}

#' @keywords internal
.resolveMinThreshold <- function(threshold, values) {
    if (!is.character(threshold)) return(unname(threshold))
    threshold <- trimws(threshold)
    if (!grepl("%$", threshold)) return(as.numeric(threshold))
    percentage <- as.numeric(sub("%$", "", threshold)) / 100
    if (!length(values)) return(0)
    percentage * max(values)
}
