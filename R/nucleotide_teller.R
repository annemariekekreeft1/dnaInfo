nucleotide_teller <- function(x, nucleotide) {
  # Validate nucleotide input
  nucleotide <- tolower(as.character(nucleotide))  # Ensure the nucleotide is in lowercase
  if (!nucleotide %in% c("a", "g", "c", "t", "u")) {
    stop("Nucleotide must be one of 'a', 'g', 'c', 't', or 'u'.")
  }

  # Convert the input string to lowercase for consistency
  x <- tolower(x)

  # Count the occurrences of the nucleotide in the string
  count <- stringr::str_count(x, nucleotide)

  # Return the count
  return(count)
}
