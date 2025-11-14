#' @title Visualise reads aligned to a genome as a sashimi plot
#'
#' @description `tmpSashimi()`
#'
#' @details
#' Additional details...
#'
#' @param x The path to the BAM file to read, a \link[Rsamtools]{BamFile} object, or a \link[Rsamtools]{BamViews} object.
#' @param ... Passed to \link[GenomicAlignments]{readGAlignments}
#'
#' @return A \link[GenomicAlignments]{GAlignments} object.
#'
#' @examples
#' fl <- system.file("extdata", "COG7.bam", package="tmpSashimi")
#' tmpSashimi(x = fl)
#'
#' @importFrom GenomicAlignments readGAlignments
#' @importFrom methods setMethod
#' @rdname tmpSashimi-methods
#' @aliases tmpSashimi
#' @export
setMethod(
    "tmpSashimi",
    signature = signature(x = c("character")),
    function(x, ...){

        ga <- GenomicAlignments::readGAlignments(x, ...)
        ga

    }
)
#' @importFrom GenomicAlignments readGAlignments
#' @importFrom methods setMethod
#' @rdname tmpSashimi-methods
#' @aliases tmpSashimi
#' @export
setMethod(
    "tmpSashimi",
    signature = signature(x = c("BamFile")),
    function(x, ...){

        ga <- GenomicAlignments::readGAlignments(x, ...)
        ga

    }
)
#' @importFrom GenomicAlignments readGAlignments
#' @importFrom methods setMethod
#' @rdname tmpSashimi-methods
#' @aliases tmpSashimi
#' @export
setMethod(
    "tmpSashimi",
    signature = signature(x = c("BamViews")),
    function(x, ...){

        ga <- GenomicAlignments::readGAlignments(x, ...)
        ga

    }
)
