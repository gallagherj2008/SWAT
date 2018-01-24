rescale <- function(data, digits = 2, na.rm = T) {
  
  if (class(data) %in% 'data.frame') stop("Data Object must be a vector")

  #if(isTRUE(na.rm)) data <- data[!is.na(data)]



rng <- range(data)

scaled <- (data - rng[1])/(rng[2]-rng[1])
round(scaled, digits = digits)

}

