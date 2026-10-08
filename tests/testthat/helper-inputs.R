.placeholder_bams <- function(n = 1L) {
    paths <- vapply(seq_len(n), function(i) {
        tempfile(pattern = paste0("sample", i, "_"), fileext = ".bam")
    }, character(1))
    stopifnot(all(file.create(paths)))
    paths
}

.prepare_plot_context <- function(ctx) {
    ctx <- spliceTerrain:::.restoreContext(ctx)
    spliceTerrain:::.preparePlotContext(ctx)
}

.with_paired_bam <- function(code, keep = NULL) {
    sam <- tempfile(fileext = ".sam")
    destination <- sub("\\.sam$", "", sam)
    bam <- paste0(destination, ".bam")
    on.exit(unlink(c(sam, bam, paste0(bam, ".bai"))))

    ## Synthetic complete pairs, overlapping mates, and singletons from
    ## either read/strand. Final records exercise MAPQ and flag filtering.
    records <- data.frame(
        qname = c(rep(c("pair_plus", "pair_minus", "overlap"), each = 2),
                  "first_plus", "second_minus", "first_minus", "second_plus",
                  "low_mapq", "secondary", "supplementary", "unmapped"),
        flag = c(99L, 147L, 83L, 163L, 99L, 147L, 73L, 153L, 89L, 137L,
                 73L, 329L, 2121L, 77L),
        rname = "synthetic",
        pos = c(100L, 300L, 500L, 100L, 700L, 700L, rep(100L, 8)),
        mapq = c(rep(60L, 10), 5L, 60L, 60L, 0L),
        cigar = c("10M90N10M", "20M", "20M", "10M190N10M",
                  rep("10M90N10M", 4), rep("10M190N10M", 2),
                  rep("10M90N10M", 3), "*"),
        rnext = c(rep("=", 6), rep("*", 8)),
        pnext = c(300L, 100L, 100L, 500L, 700L, 700L, rep(0L, 8)),
        tlen = c(220L, -220L, -420L, 420L, 110L, -110L, rep(0L, 8)),
        seq = strrep("A", 20), qual = strrep("I", 20)
    )
    if (!is.null(keep)) records <- records[records$qname %in% keep, ]
    writeLines(c(
        "@HD\tVN:1.6\tSO:coordinate", "@SQ\tSN:synthetic\tLN:1000",
        do.call(paste, c(records, sep = "\t"))
    ), sam)
    Rsamtools::asBam(sam, destination = destination, indexDestination = TRUE)
    code(bam)
}
