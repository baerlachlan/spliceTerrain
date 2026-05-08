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
    length(ctx$input$gal[[1]])
}

.junction_coverage <- function(ctx) {
    sum(ctx$input$juncs$coverage)
}

test_that("unstranded regions retain both strands regardless of strandedness", {
    unstranded <- .strand_ctx(.hnrnpc_region(), "unstranded")
    forward <- .strand_ctx(.hnrnpc_region(), "forward")
    reverse <- .strand_ctx(.hnrnpc_region(), "reverse")

    strand <- as.character(BiocGenerics::strand(unstranded$input$region))
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
