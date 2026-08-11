#' Search Proteomics Peptides in UniProt (Deprecated)
#'
#' This function is Deprecated. Please use
#'   \code{\link{rba_uniprot_proteomics_non_ptm_search}} instead.\cr
#'   UniProt maps proteomics peptides from different sources to the proteins'
#'   sequences. Using this function, you can search for  proteomics
#'   peptides that has been map to UniProt proteins. You may also refine your
#'   search with modifiers such as data_source, peptide etc. See
#'   "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'   \cr see also: \href{https://www.uniprot.org/help/proteomics}{Mass
#'   spectrometry-based proteomics data in UniProtKB}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/nonPtm"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param data_source Character: (optional) Proteomics data source. You can
#'   supply up to two values. Use \code{rba_uniprot_proteomics_species()} to
#'   retrieve the sources currently available for each species and category.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param upid Character: (optional)
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Character: (optional) Peptide sequence(s). You can supply up
#'   to 20 sequences.
#' @param unique Logical: (optional) Filter by peptide uniqueness. If
#'   \code{TRUE}, return peptides mapping to one protein; if \code{FALSE},
#'   return non-unique peptides; if \code{NULL}, do not apply this filter.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list Where each element correspond to a UniProt protein and
#'   proteomics peptides are organized under the "features" sub-list.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \dontrun{
#' #Deprecated
#' rba_uniprot_proteomics_search(peptide = "MEDYTKIEK")
#' }
#' \dontrun{
#' #Deprecated
#' ### this will generate a very large response!
#'   rba_uniprot_proteomics_search(taxid = 9606,
#'   data_source = "PeptideAtlas",
#'   progress = TRUE, timeout = 999999, unique = TRUE)
#' }
#'
#' @family "Deprecated functions"
#' @export
rba_uniprot_proteomics_search <- function(accession = NULL,
                                          data_source = NULL,
                                          taxid = NULL,
                                          upid = NULL,
                                          peptide = NULL,
                                          unique = NULL,
                                          ...) {

  .Deprecated(new = "rba_uniprot_proteomics_non_ptm_search")

  return(rba_uniprot_proteomics_non_ptm_search(
    accession = accession,
    taxid = taxid,
    data_source = data_source,
    upid = upid,
    peptide = peptide,
    unique = unique,
    ...
  ))
}

#' Get Proteomics Peptides Mapped to UniProt Protein (Deprecated)
#'
#' This function is Deprecated. Please use
#'   \code{\link{rba_uniprot_proteomics_non_ptm}} instead.\cr
#' UniProt maps proteomics peptides from different sources to the proteins'
#'   sequences. Using this function, you can retrieve all the proteomics
#'   peptides features that has been map to a given UniProt protein's sequence.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/nonPtm/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the proteomics peptides features of your supplied
#'   UniProt protein's sequence.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \dontrun{
#' #Deprecated
#' rba_uniprot_proteomics(accession = "P25942")
#' }
#'
#' @family "Deprecated functions"
#' @export
rba_uniprot_proteomics <- function(accession,
                                   ...) {

  .Deprecated("rba_uniprot_proteomics_non_ptm")

  return(rba_uniprot_proteomics_non_ptm(accession = accession, ...))
}

#' Search Post-Translational Modification in UniProt (Deprecated)
#'
#' This function is Deprecated. Please use
#'   \code{\link{rba_uniprot_proteomics_ptm_search}} instead.\cr
#' UniProt maps proteomics peptides from different sources to the proteins'
#'   sequences. Using this function, you can search for  proteomics
#'   peptides that has been map to UniProt proteins. You may also refine your
#'   search with modifiers such as data_source, peptide etc. See
#'   "Arguments section" for more information.
#'
#'   Note that this is a search function. Thus, you are not required to fill
#'   every argument; You may use whatever combinations of arguments you see
#'   fit for your query.
#'   \cr see also:
#'   \href{https://www.uniprot.org/help/ptm_processing_section}{PTM /
#'   Processing section in UniProtKB}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/ptm"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param ptm Character: (optional) Post-translational modification name.
#' @param data_source Character: (optional) Proteomics data source. You can
#'   supply up to two values. Use \code{rba_uniprot_proteomics_species()} to
#'   retrieve the sources currently available for each species and category.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}.
#'   You can supply up to 20 taxon IDs.
#' @param upid Character: (optional)
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Character: (optional) Peptide sequence(s). You can supply up
#'   to 20 sequences.
#' @param unique Logical: (optional) Filter by peptide uniqueness. If
#'   \code{TRUE}, return peptides mapping to one protein; if \code{FALSE},
#'   return non-unique peptides; if \code{NULL}, do not apply this filter.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list Where each element correspond to a UniProt protein and
#'   post-translational modification are organized under the "features"
#'   sub-list.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \dontrun{
#' #Deprecated
#' rba_uniprot_ptm_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
#' }
#'
#' @family "Deprecated functions"
#' @export
rba_uniprot_ptm_search <- function(accession = NULL,
                                   ptm = NULL,
                                   data_source = NULL,
                                   taxid = NULL,
                                   upid = NULL,
                                   peptide = NULL,
                                   unique = NULL,
                                   ...) {

  .Deprecated("rba_uniprot_proteomics_ptm_search")

  return(rba_uniprot_proteomics_ptm_search(
    accession = accession,
    ptm = ptm,
    taxid = taxid,
    data_source = data_source,
    upid = upid,
    peptide = peptide,
    unique = unique,
    ...
  ))
}

#' Get Post-Translational Modification of UniProt Protein (Deprecated)
#'
#' This function is Deprecated. Please use
#'   \code{\link{rba_uniprot_proteomics_ptm}} instead.\cr
#' UniProt maps post-translational modification features from different sources
#'   to the proteins'  sequences. Using this function, you can retrieve all
#'   the post-translational modification features that has been map to a given
#'   UniProt protein's sequence.
#'
#'   see also:
#'   \href{https://www.uniprot.org/help/ptm_processing_section}{PTM /
#'   Processing section in UniProtKB}
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/ptm/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the post-translational modification features of
#' your supplied UniProt protein's sequence.
#'
#' @references \itemize{
#'   \item The UniProt Consortium. (2025). UniProt: the Universal Protein
#'   Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
#'   https://doi.org/10.1093/nar/gkae1010
#'   \item Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales,
#'   L., Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
#'   Proteins API: Accessing key integrated protein and genome information.
#'   Nucleic Acids Research, 45(W1), W539–W544.
#'   https://doi.org/10.1093/nar/gkx237
#'   \item \href{https://www.ebi.ac.uk/proteins/api/doc/}{Proteins API
#'   Documentation}
#'   \item \href{https://www.uniprot.org/help/publications}{Citations note
#'   on UniProt website}
#'   }
#'
#' @examples
#' \dontrun{
#' #Deprecated
#' rba_uniprot_ptm(accession = "P04234")
#' }
#'
#' @family "Deprecated functions"
#' @export
rba_uniprot_ptm <- function(accession,
                            ...) {

  .Deprecated("rba_uniprot_proteomics_ptm")

  return(rba_uniprot_proteomics_ptm(accession = accession, ...))
}
