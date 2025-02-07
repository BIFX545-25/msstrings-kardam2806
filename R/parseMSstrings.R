# parseMSstrings.R
# utilities for parsing MS strings



#' parse_mods
#' utilities for parsing MS strings
#'
#' This function takes strings from spectronaut output, pulls metadata on protein modifications, and displays the data in a nice format.
#'
#' @param seqs character vector, Peptide sequence sequence strings from Spectronaut output
#' @param foramat character value
#'
#' @return A `protein_mod` object, which is a data.frame with the amino acid sequences and modification.
#' @export
#' @importFrom Biostrings AAStringSet
#' @importFrom dplyr tibble
#' @importFrom stringr str_extract_all str_replace str_replace_all str_split

# library(Biostrings) # for `AAStringSet` - BiocManager::install('Biostrings')
# library(dplyr) # for `tibble`
# library(purrr) # for `map` and `map_chr`
# library(stringr) # for `str_extract_all`, `str_replace`, `str_replace_all`, and `str_split`

parse_mods <- function(seqs, format = 'Spectronaut')
{
  # remove '_'
  seqs <- seqs |>
    str_replace_all('_', '')


  # locate modification(s)
  mods <- str_extract_all(seqs, "\\[.*?\\]") |>
    map(~ .x |>
          str_replace(fixed("["), '') |>
          str_replace(fixed("]"), '') |>
          str_replace("\\(.*?\\)", '') |>
          trimws())


  # remove modifications from seqs
  seqs <- str_split(seqs, "\\[.*?\\]")


  # find where the mods are at
  mods_at <- map(seqs, ~
                   {
                     retval <- nchar(.x) |>
                       cumsum()

                     retval[-length(retval)] # this is the end of the peptide, not a modification
                   })


  # return the data.frame
  retval <- tibble(sequence = map_chr(seqs, ~ paste(.x, collapse = '')) |>
                     AAStringSet() |>
                     as.list(),
                   mods = mods,
                   mods_at = mods_at)

  class(retval) <- c('protein_mods', class(retval))

  return(retval)
}
