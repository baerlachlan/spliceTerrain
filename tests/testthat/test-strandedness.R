.strand_ctx <- function(region, strandedness) {
    spliceTerrain(
        bam = .hnrnpc_bams()[7],
        region = region,
        strandedness = strandedness,
        min_coverage = 1,
        min_junction_reads = 1,
        return_ctx = TRUE
    )
}

.n_alignments <- function(ctx) {
    length(ctx$gal[[1]])
}

.junction_coverage <- function(ctx) {
    sum(ctx$juncs$coverage)
}

test_that("single-end reverse-stranded alignments invert read strand", {
    expect_identical(
        as.character(spliceTerrain:::.invertStrand(c("+", "-", "*"))),
        c("-", "+", "*")
    )
})

test_that("reinterpreted reverse single-end alignments filter by transcript strand", {
    aln <- GenomicAlignments::GAlignments(
        seqnames = S4Vectors::Rle(c("chr1", "chr1")),
        pos = c(1L, 10L),
        cigar = c("5M", "5M"),
        strand = c("-", "+")
    )
    region <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(1L, 20L),
        strand = "+"
    )

    raw_direct <- IRanges::subsetByOverlaps(aln, region)
    reverse <- aln
    BiocGenerics::strand(reverse) <- spliceTerrain:::.invertStrand(
        BiocGenerics::strand(reverse)
    )
    interpreted <- IRanges::subsetByOverlaps(reverse, region)

    expect_identical(BiocGenerics::start(raw_direct), 10L)
    expect_identical(BiocGenerics::start(interpreted), 1L)
})

test_that("unstranded regions retain both strands regardless of strandedness", {
    unstranded <- .strand_ctx(.hnrnpc_region(), "unstranded")
    forward <- .strand_ctx(.hnrnpc_region(), "forward")
    reverse <- .strand_ctx(.hnrnpc_region(), "reverse")

    strand <- as.character(BiocGenerics::strand(unstranded$region))
    expect_identical(strand, "*")
    expect_identical(.n_alignments(forward), .n_alignments(unstranded))
    expect_identical(.n_alignments(reverse), .n_alignments(unstranded))
    expect_identical(
        .junction_coverage(forward),
        .junction_coverage(unstranded)
    )
    expect_identical(
        .junction_coverage(reverse),
        .junction_coverage(unstranded)
    )
})

test_that("stranded regions use strandedness to select alignments", {
    plus_forward <- .strand_ctx(paste0(.hnrnpc_region(), ":+"), "forward")
    plus_reverse <- .strand_ctx(paste0(.hnrnpc_region(), ":+"), "reverse")
    minus_forward <- .strand_ctx(paste0(.hnrnpc_region(), ":-"), "forward")
    minus_reverse <- .strand_ctx(paste0(.hnrnpc_region(), ":-"), "reverse")

    expect_identical(.n_alignments(plus_forward), .n_alignments(minus_reverse))
    expect_identical(.n_alignments(plus_reverse), .n_alignments(minus_forward))
    expect_false(identical(
        .n_alignments(plus_forward),
        .n_alignments(plus_reverse)
    ))
    expect_identical(
        .junction_coverage(plus_forward),
        .junction_coverage(minus_reverse)
    )
    expect_identical(
        .junction_coverage(plus_reverse),
        .junction_coverage(minus_forward)
    )
})

test_that("unstranded libraries ignore supplied region strand", {
    no_strand <- .strand_ctx(.hnrnpc_region(), "unstranded")
    plus <- .strand_ctx(paste0(.hnrnpc_region(), ":+"), "unstranded")
    minus <- .strand_ctx(paste0(.hnrnpc_region(), ":-"), "unstranded")

    expect_identical(.n_alignments(plus), .n_alignments(no_strand))
    expect_identical(.n_alignments(minus), .n_alignments(no_strand))
    expect_identical(.junction_coverage(plus), .junction_coverage(no_strand))
    expect_identical(.junction_coverage(minus), .junction_coverage(no_strand))
})
