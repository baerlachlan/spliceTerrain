.junctionArcLabels <- function(layout, juncs, lsv) {
    labels <- juncs$coverage
    if (!is.null(lsv)) {
        anchors <- .rangesToAnchors(juncs)
        st <- anchors[anchors$anchor == "start"]
        hits_st <- IRanges::findOverlaps(lsv, st)
        sh_st <- S4Vectors::subjectHits(hits_st)
        en <- anchors[anchors$anchor == "end"]
        hits_en <- IRanges::findOverlaps(lsv, en)
        sh_en <- S4Vectors::subjectHits(hits_en)
        sh <- unique(c(sh_st, sh_en))
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
