#' @export
squishIntrons <- function(..., squish_width = 100L) {

    objs <- list(...)
    all_ranges <- do.call(c, objs)
    blocks <- GenomicRanges::reduce(all_ranges, ignore.strand = TRUE)
    blocks <- BiocGenerics::sort(blocks)
    n <- length(blocks)
    if (n <= 1L) return(all_ranges) # Return original obj if no introns
    ## Add 1L to keep coords 1-based
    squish_start <- vapply(0:(n-1), \(i){
        sum(BiocGenerics::width(blocks[0:i])) + i * squish_width + 1L
    }, integer(1))
    block_start <- BiocGenerics::start(blocks)
    ## Squished loc = squished start + difference between loc and block start
    map_pos <- function(loc, i){squish_start[i] + (loc - block_start[i])}
    lapply(objs, \(exons){
        hits <- IRanges::findOverlaps(exons, blocks, select = "first")
        out <- GenomicRanges::GRanges(
            seqnames = "plot_space",
            ranges = IRanges::IRanges(
                start = map_pos(BiocGenerics::start(exons), hits),
                end = map_pos(BiocGenerics::end(exons), hits)
            ),
            strand = BiocGenerics::strand(exons),
            seqinfo = Seqinfo::Seqinfo(
                seqnames = "plot_space",
                seqlengths = max(map_pos(BiocGenerics::end(exons), hits))
            )
        )
        S4Vectors::mcols(out) <- S4Vectors::mcols(exons)
        out
    })

}
