# Weighted quantile
weighted.quantile <- function(x, w, probs=0.25, na.rm=TRUE) {
  if(length(x) == 0 | length(w) == 0){
    return(NaN)
  }
  value_table <- data.frame(x,w)
  value_table <- value_table[order(value_table$x),]
  value_table$rw <- cumsum(value_table$w)
  pos <- sum(value_table$w) * probs

  if (length(which(value_table$rw == pos)) == 1){
    return(value_table[which(value_table$rw == pos),]$x)
  }
  lower_median_pos <- max(which(value_table$rw <= pos))
  upper_median_pos <- min(which(value_table$rw >= pos))

  lower_proportion <- (value_table[upper_median_pos,]$rw - pos) /
    (value_table[upper_median_pos,]$rw - value_table[lower_median_pos,]$rw)
  upper_proportion <- (pos - value_table[lower_median_pos,]$rw) /
    (value_table[upper_median_pos,]$rw - value_table[lower_median_pos,]$rw)

  return(value_table[lower_median_pos,]$x * lower_proportion +
           value_table[upper_median_pos,]$x * upper_proportion)
}

# Weighted median
weighted.median <- function(x, w, na.rm=TRUE) {
  unname(weighted.quantile(x, probs=0.5, w=w, na.rm=na.rm))
}
