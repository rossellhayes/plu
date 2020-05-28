plu_nge <- function(x, ends = FALSE) {
  x <- gsub(" +", " ", x)

  if (ends) {
    x <- gsub("^ +", "", x)
    x <- gsub(" +$", "", x)
  }

  x
}
