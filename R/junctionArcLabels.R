.junctionArcLabels <- function(layout, juncs, psi) {
    labels <- juncs$coverage
    if (!is.null(psi)) {
        anchors <- .rangesToAnchors(juncs)
        st <- anchors[anchors$anchor == "start"]
        hits_st <- IRanges::findOverlaps(psi, st)
        sh_st <- S4Vectors::subjectHits(hits_st)
        en <- anchors[anchors$anchor == "end"]
        hits_en <- IRanges::findOverlaps(psi, en)
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
