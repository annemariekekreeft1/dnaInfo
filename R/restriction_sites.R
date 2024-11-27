#' Title
#'
#' @param x A character vector
#' @param restriction_sites A character vector
#'
#' @return A data frame
#' @export
#'
#' @examples
#' x <- "AGCTGGGGAAATTAGCTAGGCT"
#' y <- c("AGCT", "GAAA")
#' restriction_site(x,y)
restriction_site <- function(x, restriction_sites) {
  # Checking input restriction site
  invalid_chars <- stringr::str_count(restriction_sites, "[^ACGTacgt]")
  if (any(stringr::str_detect(restriction_sites, "[^ACGTacgt]"))) {
    stop("All restriction sites must only contain nucleotide characters (A, C, G, T).")
  }

  # Combine all DNA sequence lines into one string
  dna_sequence <- paste(x, collapse = "")

  # Search for restriction sites
  for (site in restriction_sites) {
    cat("Restriction site:", site, "\n")

    # Count occurrences
    site_count <- stringr::str_count(dna_sequence, stringr::regex(site, ignore_case = TRUE))
    cat("Count of", site, "in DNA sequence:", site_count, "\n")

    # Find positions
    site_positions <- stringr::str_locate_all(dna_sequence, stringr::regex(site, ignore_case = TRUE))[[1]]
    if (nrow(site_positions) > 0) {
      cat("Positions (start, end):\n")
      print(data.frame(Start = site_positions[, "start"], End = site_positions[, "end"]))
    } else {
      cat("No occurrences found for", site, "\n")
    }
    cat("\n")
  }
}
