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

.with_single_end_bam <- function(code) {
    sam <- tempfile(fileext = ".sam")
    destination <- sub("\\.sam$", "", sam)
    bam <- paste0(destination, ".bam")
    on.exit(unlink(c(sam, bam, paste0(bam, ".bai"))))

    ## Synthetic reads: two forward reads share one junction; one reverse
    ## read shares their first block but uses a different second block.
    records <- data.frame(
        qname = c("forward1", "forward2", "reverse1"),
        flag = c(0L, 0L, 16L),
        rname = "synthetic", pos = 100L, mapq = 60L,
        cigar = c("10M90N10M", "10M90N10M", "10M190N10M"),
        rnext = "*", pnext = 0L, tlen = 0L,
        seq = strrep("A", 20), qual = strrep("I", 20)
    )
    writeLines(c(
        "@HD\tVN:1.6\tSO:coordinate",
        "@SQ\tSN:synthetic\tLN:1000",
        do.call(paste, c(records, sep = "\t"))
    ), sam)
    Rsamtools::asBam(sam, destination = destination, indexDestination = TRUE)
    code(bam)
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

test_that("single-end BAM strand selection preserves expected signal", {
    .with_single_end_bam(function(bam) {
        cases <- data.frame(
            protocol = rep(c("forward", "reverse", "unstranded"), each = 3),
            strand = rep(c("+", "-", "*"), 3),
            signal = c(
                "plus", "minus", "both", "minus", "plus", "both",
                "both", "both", "both"
            )
        )
        cov_starts <- list(
            plus = c(100L, 200L), minus = c(100L, 300L),
            both = c(100L, 200L, 300L)
        )
        cov_counts <- list(
            plus = c(2L, 2L), minus = c(1L, 1L), both = c(3L, 2L, 1L)
        )
        junc_ends <- list(plus = 199L, minus = 299L, both = c(199L, 299L))
        junc_counts <- list(plus = 2L, minus = 1L, both = c(2L, 1L))

        for (i in seq_len(nrow(cases))) {
            signal <- cases$signal[i]
            info <- paste(cases$protocol[i], cases$strand[i])
            ctx <- spliceTerrain(
                bam = c(sample = bam),
                region = paste0("synthetic:90-320:", cases$strand[i]),
                strandedness = cases$protocol[i],
                min_junction_reads = 1, return_ctx = TRUE
            )
            expected_cov <- data.frame(
                start = cov_starts[[signal]], end = cov_starts[[signal]] + 9L,
                coverage_raw = cov_counts[[signal]]
            )
            expected_junc <- data.frame(
                start = 110L, end = junc_ends[[signal]],
                coverage_raw = junc_counts[[signal]]
            )
            fields <- c("start", "end", "coverage_raw")
            expect_s4_class(ctx$gal[[1]], "GAlignments")
            expect_equal(length(ctx$gal[[1]]), sum(junc_counts[[signal]]),
                         info = info)
            expect_identical(as.data.frame(ctx$cov)[fields], expected_cov,
                             info = info)
            expect_identical(as.data.frame(ctx$juncs)[fields], expected_junc,
                             info = info)
            expect_identical(ctx$cov$coverage, cov_counts[[signal]],
                             info = info)
            expect_identical(ctx$juncs$coverage, junc_counts[[signal]],
                             info = info)
        }
    })
})

test_that("single-end BAMs respect per-sample strandedness", {
    .with_single_end_bam(function(bam) {
        ctx <- spliceTerrain(
            bam = c(forward_sample = bam, reverse_sample = bam),
            region = "synthetic:90-320:+",
            strandedness = c("forward", "reverse"),
            min_junction_reads = 1, return_ctx = TRUE
        )

        expect_identical(
            ctx$juncs$sample, c("forward_sample", "reverse_sample")
        )
        expect_identical(BiocGenerics::start(ctx$juncs), c(110L, 110L))
        expect_identical(BiocGenerics::end(ctx$juncs), c(199L, 299L))
        expect_identical(ctx$juncs$coverage_raw, c(2L, 1L))
    })
})

test_that("paired singleton strand uses read number and library protocol", {
    cases <- data.frame(
        read = c("first_plus", "second_minus", "first_minus", "second_plus"),
        forward = c("+", "+", "-", "-"),
        reverse = c("-", "-", "+", "+"),
        junction_end = c(199L, 199L, 299L, 299L)
    )
    for (i in seq_len(nrow(cases))) {
        .with_paired_bam(function(bam) {
            for (protocol in c("unstranded", "forward", "reverse")) {
                for (target in c("+", "-", "*")) {
                    keep <- protocol == "unstranded" || target == "*" ||
                        target == cases[[protocol]][i]
                    ctx <- spliceTerrain(
                        bam = bam, region = paste0("synthetic:90-550:", target),
                        strandedness = protocol, min_junction_reads = 1,
                        return_ctx = TRUE
                    )
                    info <- paste(cases$read[i], protocol, target)
                    expect_identical(
                        BiocGenerics::end(ctx$juncs),
                        cases$junction_end[i][keep], info = info
                    )
                    expect_equal(
                        ctx$juncs$coverage_raw, rep(1L, as.integer(keep)),
                        info = info
                    )
                    expect_equal(
                        sum(BiocGenerics::width(ctx$cov) *
                            ctx$cov$coverage_raw),
                        if (keep) 20 else 0, info = info
                    )
                }
            }
        }, keep = cases$read[i])
    }
})

test_that("fragment junction counts respect strand within mate groups", {
    reads <- GenomicAlignments::GAlignments(
        seqnames = rep("synthetic", 6), pos = rep(100L, 6),
        cigar = rep("10M90N10M", 6), strand = c("+", "+", "-", "-", "+", "-")
    )
    aln <- GenomicAlignments::GAlignmentsList(
        plus = reads[1:2], minus = reads[3:4], discordant = reads[5:6]
    )
    for (target in c("+", "-", "*")) {
        ctx <- list(
            input = list(
                gal = list(sample = aln),
                region = GenomicRanges::GRanges(
                    paste0("synthetic:90-250:", target)
                ),
                strandedness = c(sample = "forward"),
                min_junction_reads = c(sample = 1), lib_size = NULL,
                annotation = NULL
            ),
            plot = list()
        )
        juncs <- spliceTerrain:::.getJunctions(ctx)$input$juncs

        expect_identical(IRanges::ranges(juncs), IRanges::IRanges(110L, 199L))
        expect_identical(juncs$coverage_raw, if (target == "*") 3L else 2L)
    }
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
