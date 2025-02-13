% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseMSstrings.R
\name{parse_mods}
\alias{parse_mods}
\title{parse_mods
utilities for parsing MS strings}
\usage{
parse_mods(seqs, format = "Spectronaut")
}
\arguments{
\item{seqs}{character vector, Peptide sequence sequence strings from Spectronaut output}

\item{format}{character value}
}
\value{
A \code{protein_mod} object, which is a data.frame with the amino acid sequences and modification.
}
\description{
This function takes strings from spectronaut output, pulls metadata on protein modifications, and displays the data in a nice format.
}
