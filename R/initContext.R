#' @keywords internal
.initContext <- function(args, defaults) {
    ## This code chunk also acts as documentation to the structure of ctx
    inputs <- c("bam", "region", "annotation", "lsv", "highlight")
    ctx <- list(
        args = args[!(names(args) %in% inputs)],
        input = args[inputs],
        data = list(gal = NULL, cov = NULL, juncs = NULL),
        plot = list(map = NULL, plist = NULL)
    )
    ctx$args$strandedness <- match.arg(
        ctx$args$strandedness, eval(defaults$strandedness)
    )
    ctx
}
