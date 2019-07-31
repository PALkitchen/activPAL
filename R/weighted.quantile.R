# Weighted quantile
weighted.quantile <- function(values, weights, probs=0.5) {
  if(length(values) == 0 | length(weights) == 0){
    return(NaN)
  }
  if(length(values) == 1 | length(weights) == 1){
    return(values)
  }
  value_table <- data.frame(values,weights)
  value_table <- value_table[order(value_table$values),]
  value_table$rw <- cumsum(value_table$weights)
  pos <- sum(value_table$weights) * probs

  if (length(which(value_table$rw == pos)) == 1){
    return(value_table[which(value_table$rw == pos),]$values)
  }

  if(pos < min(value_table$rw)){
    return(value_table[1,]$values)
  }

  if(pos > max(value_table$rw)){
    return(value_table[nrow(value_table),]$values)
  }

  lower_median_pos <- max(which(value_table$rw <= pos))
  upper_median_pos <- min(which(value_table$rw >= pos))

  lower_proportion <- (value_table[upper_median_pos,]$rw - pos) /
    (value_table[upper_median_pos,]$rw - value_table[lower_median_pos,]$rw)
  upper_proportion <- (pos - value_table[lower_median_pos,]$rw) /
    (value_table[upper_median_pos,]$rw - value_table[lower_median_pos,]$rw)

  return(value_table[lower_median_pos,]$values * lower_proportion +
           value_table[upper_median_pos,]$values * upper_proportion)
}

# Weighted median
weighted.median <- function(values, weights) {
  unname(weighted.quantile(values, weights, probs=0.5))
}
