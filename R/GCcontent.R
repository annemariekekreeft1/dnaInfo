#' Calculating GC content
#'
#' @param x A character vector, only consisting of AGC(T/U)
#'
#' @return
#' @export
#'
#' @examples
#' x <- "AGC TTA GCG CTA GC"
#' GCcontent(x)
GCcontent <- function(x) {
  # Remove spaces or any non-nucleotide characters
  x <- gsub("[^AGCT]", "", toupper(x))  # Keeps only A, G, C, T characters

  # Count occurrences of G and C
  gc_count <- stringr::str_count(x, "G") + stringr::str_count(x, "C")

  # Total length of the sequence
  total_length <- nchar(x)

  # Check if total length is zero to avoid division by zero
  if (total_length == 0) {
    return("Invalid sequence")
  }

  # Calculate GC content (percentage of G and C)
  gc_content <- (gc_count / total_length) * 100

  # Return the GC content with percentage symbol
  return(paste0(round(gc_content, 2), "%"))
}
