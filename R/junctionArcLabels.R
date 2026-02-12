.junctionArcLabels <- function(layout, junc, lsv) {

    labels <- junc$coverage
    if (!is.null(lsv)) {
        hits <- IRanges::findOverlaps(lsv, junc)
        sh <- S4Vectors::subjectHits(hits)
        labels[sh] <- paste0(
            labels[sh], "\n",
            "(", scales::percent(round(labels[sh] / sum(labels[sh]), 3)), ")"
        )
    }

    data.frame(
        x = layout$mid,
        y = layout$heights,
        label = labels
    )

}
