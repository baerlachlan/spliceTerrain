.placeholder_bams <- function(n = 1L) {
    paths <- vapply(seq_len(n), function(i) {
        tempfile(pattern = paste0("sample", i, "_"), fileext = ".bam")
    }, character(1))
    stopifnot(all(file.create(paths)))
    paths
}
