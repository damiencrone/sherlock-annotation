plotSegmentSimilarity = function (segment_concept_mat, dat) {
  
  # Plot segment-wise similarity matrix
  dist_mat = dist(segment_concept_mat)
  dist_mat_ordinal = as.matrix(dist_mat)
  dist_mat_ordinal[] = rank(as.matrix(dist_mat))
  
  image(
    x = 1:nrow(segment_concept_mat),
    y = 1:nrow(segment_concept_mat),
    z = dist_mat_ordinal,
    xlab = "Segment",
    ylab = "Segment"
  )
  
  abline(
    h = which(!is.na(dat$`Scene Segments`)),
    lwd = 0.5,
    col = "blue"
  )
  
  abline(
    v = which(!is.na(dat$`Scene Segments`)),
    lwd = 0.5,
    col = "blue"
  )
  
}

plotConceptTimeseries = function (segment_concept_mat, dat) {
  
  x = 1:nrow(segment_concept_mat)
  y = segment_concept_mat[, dimname]
  
  smoothed_dat = loess.smooth(
    x = x,
    y = y, span = 1/75,
    evaluation = length(y)
  )
  x_smoothed = smoothed_dat$x
  y_smoothed = smoothed_dat$y
  
  # Plot semantic similarity over time
  plot(
    x = 0,
    type = "n",
    main = dimname,
    xlim = range(x),
    ylim = range(y_smoothed),
    xlab = "Segment number",
    ylab = "Cosine similarity"
  )
  
  abline(h = 0, lwd = 0.5)
  
  # Add vertical lines at scene transitions
  abline(
    v = which(!is.na(dat$`Scene Segments`)),
    lty = 2,
    col = "gray"
  )
  
  points(
    x = x,
    y = y,
    cex = 1/4
  )
  
  lines(
    x = x_smoothed,
    y = y_smoothed,
    col = "red"
  )
  
}