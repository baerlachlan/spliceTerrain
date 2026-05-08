.with_temp_pdf_device <- function(code) {
    plot_file <- tempfile(fileext = ".pdf")
    grDevices::pdf(plot_file)
    on.exit({
        grDevices::dev.off()
        unlink(plot_file)
    })

    force(code)
}

.expect_ggplot_renders <- function(p) {
    expect_s3_class(p, "ggplot")
    .with_temp_pdf_device(
        expect_no_error(suppressWarnings(ggplot2::ggplotGrob(p)))
    )
    invisible(p)
}

.expect_patchwork_renders <- function(p) {
    expect_s3_class(p, "patchwork")
    .with_temp_pdf_device(
        expect_no_error(suppressWarnings(patchwork::patchworkGrob(p)))
    )
    invisible(p)
}
