#' @keywords internal
.junctionArcPoints <- function(layout) {
    mid <- layout$mid
    hw <- layout$hw
    above <- layout$above
    diff_l <- layout$diff_l
    diff_r <- layout$diff_r
    cov_l <- layout$cov_l
    cov_r <- layout$cov_r
    ## Build arc points
    t <- seq(0, 1, length.out = 50L)
    tt <- c(-rev(t), t) # Both halves combined
    ## The exponent controls how round or square the arcs become
    bump_l <- 1 - rev(t)^32 # left half
    bump_r <- 1 - t^32 # right half
    ## Matrix of dims: number of juncs x 100
    x_mat <- mid + hw * rep(tt, each = length(mid))
    dim(x_mat) <- c(length(mid), length(tt))
    ## Build left and right halves
    y_l <- diff_l * rep(bump_l, each = length(diff_l))
    dim(y_l) <- c(length(diff_l), length(bump_l))
    y_r <- diff_r * rep(bump_r, each = length(diff_r))
    dim(y_r) <- c(length(diff_r), length(bump_r))
    ## Add anchor baselines for "above" arcs
    if (any(above)) {
        y_l[above,] <- y_l[above,] + cov_l[above]
        y_r[above,] <- y_r[above,] + cov_r[above]
    }
    ## Bind halves
    y_mat <- cbind(y_l, y_r)
    id <- rep(seq_len(length(mid)), each = ncol(x_mat))
    data.frame(
        x = c(t(x_mat)),
        y = c(t(y_mat)),
        id = id,
        size_on = layout$cov_j[id],
        size_off = 1
    )
}
