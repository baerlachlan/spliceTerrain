.hnrnpc_bams <- function() {
    testthat::skip_if_not_installed("RNAseqData.HNRNPC.bam.chr14")
    RNAseqData.HNRNPC.bam.chr14::RNAseqData.HNRNPC.bam.chr14_BAMFILES
}

.hnrnpc_region <- function() {
    "chr14:70222436-70237375"
}

.hnrnpc_psi <- function() {
    "chr14:70234854-70234854"
}

.hnrnpc_highlight <- function() {
    "chr14:70234056-70234097"
}

.hnrnpc_annotation <- function() {
    testthat::skip_if_not_installed("AnnotationFilter")
    testthat::skip_if_not_installed("EnsDb.Hsapiens.v75")
    testthat::skip_if_not_installed("ensembldb")

    ann <- ensembldb::exonsBy(
        EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75,
        by = "tx",
        filter = AnnotationFilter::GeneIdFilter("ENSG00000100650")
    )
    ann <- ann[1:2]
    ensembldb::seqlevelsStyle(ann) <- "ucsc"
    ann
}
