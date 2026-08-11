#### Metadata endpoints #####

#' Get UniProt Proteomics Metadata
#'
#' Retrieve information on the available Species proteomics data sources in
#'   UniProt.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/species"
#'
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A data frame with the available species as rows, and the columns
#'   indicating the proteomics data sources, separated by three main data
#'   categories: PTM (Post-Translational Modification), non-PTM, and
#'   HPP (Human Proteome Project)
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
#' \donttest{
#'   rba_uniprot_proteomics_species()
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_species <- function(...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args()

  .msg(
    "Retrieving UniProt proteomics metadata."
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics/species"),
    accept = "application/json",
    parser = "json->df",
    save_to = .rba_file("uniprot_species.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#### Proteomics-non-PTM Endpoints ####

#' Search Proteomics data in UniProt
#'
#' Search for non-post-translational-modification proteomics features mapped to
#'   UniProt proteins. Refine the search by data source, peptide, or other
#'   supported criteria.
#'
#' At least one of \code{accession}, \code{taxid}, \code{data_source},
#'   \code{upid}, or \code{peptide} is required. \code{unique} only refines
#'   those criteria.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources: "GET
#'   https://www.ebi.ac.uk/proteins/api/proteomics/nonPtm"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You can supply up to
#'   20 taxon IDs.
#' @param data_source Character: (optional) Proteomics data source. You can
#'   supply up to two values. Use \code{rba_uniprot_proteomics_species()} to
#'   retrieve the sources currently available for each species and category.
#' @param upid Character: (optional)
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Character: (optional) Peptide sequence(s). You can supply up
#'   to 20 sequences.
#' @param unique Logical: (optional) Filter by peptide uniqueness. If
#'   \code{TRUE}, return peptides mapping to one protein; if \code{FALSE},
#'   return non-unique peptides; if \code{NULL}, do not apply this filter.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#' @return A list in which each element corresponds to a UniProt protein and
#'   proteomics data are stored under the \code{features} element.
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
#' \donttest{
#'   rba_uniprot_proteomics_non_ptm_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_non_ptm_search <- function(accession = NULL,
                                                  taxid = NULL,
                                                  data_source = NULL,
                                                  upid = NULL,
                                                  peptide = NULL,
                                                  unique = NULL,
                                                  ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(
        arg = "taxid", class = c("numeric", "integer"),
        max_len = 20, min_val = 1
      ),
      list(arg = "upid", class = "character", max_len = 100),
      list(arg = "data_source", class = "character", max_len = 2),
      list(arg = "peptide", class = "character", max_len = 20),
      list(arg = "unique", class = "logical", len = 1L)
    ),
    cond = list(
      list(
        quote(all(
          is.null(accession), is.null(taxid), is.null(upid),
          is.null(data_source), is.null(peptide)
        )),
        "Supply at least one search criterion: accession, taxid, upid, data_source, or peptide."
      ),
      list(
        quote(!is.null(taxid) && any(!is.finite(taxid) | taxid %% 1 != 0)),
        "`taxid` values should be finite, positive whole numbers."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving proteomics nonPTM features of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("upid", !is.null(upid), paste0(upid, collapse = ",")),
    list("datasource", !is.null(data_source), paste0(data_source, collapse = ",")),
    list("peptide", !is.null(peptide), paste0(peptide, collapse = ",")),
    list("unique", !is.null(unique), ifelse(unique, "true", "false"))
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics/nonPtm"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_proteomics_non_ptm_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Proteomics data in UniProt
#'
#' UniProt maps proteomics data from different sources to the proteins'
#'   sequences. Using this function, you can retrieve all the
#'   non-post-translational-modification proteomics features mapped to a given
#'   UniProt protein's sequence.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
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
#' @return A list containing the proteomics data features of
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
#' \donttest{
#'   rba_uniprot_proteomics_non_ptm(accession = "P04234")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_non_ptm <- function(accession,
                                           ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving proteomics non-PTM features mapped to protein %s.",
    accession
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics/nonPtm/", accession),
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_proteomics_non_ptm.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#### Proteomics-PTM Endpoints ####

#' Search Post-Translational Modification Proteomics in UniProt
#'
#' Search for post-translational-modification proteomics features mapped to
#'   UniProt proteins. Refine the search by data source, peptide, or other
#'   supported criteria.
#'
#' At least one search criterion is required; \code{unique} only refines
#'   another criterion.
#'
#' See also:
#'   \href{https://www.uniprot.org/help/post-translational_modification}{PTM /
#'   Processing section in UniProtKB}
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources: "GET
#'   https://www.ebi.ac.uk/proteins/api/proteomics/ptm"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param ptm Character: (optional) Post-translational modification name used
#'   by the Proteomics API, such as "Phosphorylation" or "SUMOylation".
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You can supply up to
#'   20 taxon IDs.
#' @param data_source Character: (optional) Proteomics data source. You can
#'   supply up to two values. Use \code{rba_uniprot_proteomics_species()} to
#'   retrieve the sources currently available for each species and category.
#' @param upid Character: (optional)
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Character: (optional) Peptide sequence(s). You can supply up
#'   to 20 sequences.
#' @param unique Logical: (optional) Filter by peptide uniqueness. If
#'   \code{TRUE}, return peptides mapping to one protein; if \code{FALSE},
#'   return non-unique peptides; if \code{NULL}, do not apply this filter.
#' @param confidence_score Character: (optional) One or more of "Bronze",
#'   "Silver", or "Gold"; you can supply all three values. UniProt classifies
#'   modified residues by false localization rate across multiple datasets.
#'   See \href{https://www.uniprot.org/help/mod_res_large_scale}{Large-scale
#'   modified residues} for more information.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#'
#' @return A list in which each element corresponds to a UniProt protein and
#'   post-translational modifications are stored under the \code{features}
#'   element.
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
#' \donttest{
#'   rba_uniprot_proteomics_ptm_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_ptm_search <- function(accession = NULL,
                                              ptm = NULL,
                                              taxid = NULL,
                                              data_source = NULL,
                                              upid = NULL,
                                              peptide = NULL,
                                              unique = NULL,
                                              confidence_score = NULL,
                                              ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(arg = "ptm", class = "character", len = 1L),
      list(
        arg = "taxid", class = c("numeric", "integer"),
        max_len = 20, min_val = 1
      ),
      list(arg = "upid", class = "character", max_len = 100),
      list(arg = "data_source", class = "character", max_len = 2),
      list(arg = "peptide", class = "character", max_len = 20),
      list(arg = "unique", class = "logical", len = 1L),
      list(
        arg = "confidence_score", class = "character", max_len = 3,
        val = c("Bronze", "Silver", "Gold")
      )
    ),
    cond = list(
      list(
        quote(all(
          is.null(accession), is.null(ptm), is.null(taxid), is.null(upid),
          is.null(data_source), is.null(peptide), is.null(confidence_score)
        )),
        "Supply at least one search criterion: accession, ptm, taxid, upid, data_source, peptide, or confidence_score."
      ),
      list(
        quote(!is.null(taxid) && any(!is.finite(taxid) | taxid %% 1 != 0)),
        "`taxid` values should be finite, positive whole numbers."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving proteomics Post-translational modification features of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("ptm", !is.null(ptm), ptm),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("upid", !is.null(upid), paste0(upid, collapse = ",")),
    list("datasource", !is.null(data_source), paste0(data_source, collapse = ",")),
    list("peptide", !is.null(peptide), paste0(peptide, collapse = ",")),
    list("unique", !is.null(unique), ifelse(unique, "true", "false")),
    list(
      "confidence_score", !is.null(confidence_score),
      paste0(confidence_score, collapse = ",")
    )
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics/ptm"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_proteomics_ptm_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get Post-Translational Modification of UniProt Protein
#'
#' UniProt maps post-translational modification proteomics data from different
#'   sources to the proteins' sequences. Using this function, you can retrieve
#'   all the post-translational-modification features mapped to a
#'   given UniProt protein's sequence.
#'
#' see also:
#'   \href{https://www.uniprot.org/help/post-translational_modification}{PTM /
#'   Processing section in UniProtKB}
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/ptm/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param confidence_score Character: (optional) One or more of "Bronze",
#'   "Silver", or "Gold"; you can supply all three values. UniProt classifies
#'   modified residues by false localization rate across multiple datasets.
#'   See \href{https://www.uniprot.org/help/mod_res_large_scale}{Large-scale
#'   modified residues} for more information.
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
#' \donttest{
#'   rba_uniprot_proteomics_ptm(accession = "P04234")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_ptm <- function(accession,
                                       confidence_score = NULL,
                                       ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L),
      list(
        arg = "confidence_score", class = "character", max_len = 3,
        val = c("Bronze", "Silver", "Gold")
      )
    )
  )

  .msg(
    "Retrieving proteomics post-translational modification features mapped to the sequence of protein %s.",
    accession
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list(),
    list(
      "confidence_score", !is.null(confidence_score),
      paste0(confidence_score, collapse = ",")
    )
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics/ptm/", accession),
    query = call_query,
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_proteomics_ptm.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}


#### Proteomics-HPP Endpoints ####

#' Search HPP Proteomics data in UniProt
#'
#' Search for Human Proteome Project (HPP) proteomics features mapped to UniProt
#'   proteins. Refine the search by data source, peptide, or other supported
#'   criteria.
#'
#' At least one of \code{accession}, \code{taxid}, \code{data_source},
#'   \code{upid}, or \code{peptide} is required. \code{unique} only refines
#'   those criteria.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources: "GET
#'   https://www.ebi.ac.uk/proteins/api/proteomics/hpp"
#'
#' @param accession Character: (optional)
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}(s). You can supply up to 100
#'   accession numbers.
#' @param taxid Numeric: (optional) NIH-NCBI
#'   \href{https://www.uniprot.org/taxonomy/}{Taxon ID}. You can supply up to
#'   20 taxon IDs.
#' @param data_source Character: (optional) Proteomics data source. You can
#'   supply up to two values. Use \code{rba_uniprot_proteomics_species()} to
#'   retrieve the sources currently available for each species and category.
#' @param upid Character: (optional)
#'   \href{https://www.uniprot.org/help/proteome_id}{UniProt Proteome
#'   identifier (UPID)}. You can supply up to 100 UPIDs.
#' @param peptide Character: (optional) Peptide sequence(s). You can supply up
#'   to 20 sequences.
#' @param unique Logical: (optional) Filter by peptide uniqueness. If
#'   \code{TRUE}, return peptides mapping to one protein; if \code{FALSE},
#'   return non-unique peptides; if \code{NULL}, do not apply this filter.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s arguments
#'   manual for more information on available options.
#' @return A list in which each element corresponds to a UniProt protein and
#'   proteomics data are stored under the \code{features} element.
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
#' \donttest{
#'   rba_uniprot_proteomics_hpp_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_hpp_search <- function(accession = NULL,
                                              taxid = NULL,
                                              data_source = NULL,
                                              upid = NULL,
                                              peptide = NULL,
                                              unique = NULL,
                                              ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", max_len = 100),
      list(
        arg = "taxid", class = c("numeric", "integer"),
        max_len = 20, min_val = 1
      ),
      list(arg = "upid", class = "character", max_len = 100),
      list(arg = "data_source", class = "character", max_len = 2),
      list(arg = "peptide", class = "character", max_len = 20),
      list(arg = "unique", class = "logical", len = 1L)
    ),
    cond = list(
      list(
        quote(all(
          is.null(accession), is.null(taxid), is.null(upid),
          is.null(data_source), is.null(peptide)
        )),
        "Supply at least one search criterion: accession, taxid, upid, data_source, or peptide."
      ),
      list(
        quote(!is.null(taxid) && any(!is.finite(taxid) | taxid %% 1 != 0)),
        "`taxid` values should be finite, positive whole numbers."
      )
    )
  )

  .msg(
    "Searching UniProt and retrieving proteomics HPP features of proteins that match your supplied inputs."
  )

  ## Build GET API Request's query
  call_query <- .rba_query(
    init = list("size" = "-1"),
    list("accession", !is.null(accession), paste0(accession, collapse = ",")),
    list("taxid", !is.null(taxid), paste0(taxid, collapse = ",")),
    list("upid", !is.null(upid), paste0(upid, collapse = ",")),
    list("datasource", !is.null(data_source), paste0(data_source, collapse = ",")),
    list("peptide", !is.null(peptide), paste0(peptide, collapse = ",")),
    list("unique", !is.null(unique), ifelse(unique, "true", "false"))
  )

  ## Build Function-Specific Call
  parser_input <- list("json->list", .rba_uniprot_search_namer)

  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics/hpp"),
    query = call_query,
    accept = "application/json",
    parser = parser_input,
    save_to = .rba_file("uniprot_hpp_search.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}

#' Get HPP Proteomics data in UniProt
#'
#' UniProt maps Human Proteome Project (HPP) proteomics data from different
#'   sources to protein sequences. Retrieve all HPP proteomics features mapped
#'   to a given UniProt protein's sequence.
#'
#' UniProt categorizes proteomics data sources into three main data categories:
#'   PTM (Post-Translational Modification), non-PTM, and HPP (Human Proteome
#'   Project); each with corresponding API endpoints, and thus, rbioapi
#'   functions.
#'
#' @section Corresponding API Resources:
#'  "GET https://www.ebi.ac.uk/proteins/api/proteomics/hpp/\{accession\}"
#'
#' @param accession Character:
#'   \href{https://www.uniprot.org/help/accession_numbers}{
#'   UniProtKB primary or secondary accession}.
#' @param ... rbioapi option(s). See \code{\link{rba_options}}'s
#'   arguments manual for more information on available options.
#'
#' @return A list containing the proteomics data features of
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
#' \donttest{
#'   rba_uniprot_proteomics_hpp(accession = "P04234")
#' }
#'
#' @family "UniProt - Proteomics"
#' @export
rba_uniprot_proteomics_hpp <- function(accession,
                                       ...) {
  ## Load Global Options
  .rba_ext_args(...)

  ## Check User-input Arguments
  .rba_args(
    cons = list(
      list(arg = "accession", class = "character", len = 1L)
    )
  )

  .msg(
    "Retrieving HPP proteomics features mapped to protein %s.",
    accession
  )

  ## Build Function-Specific Call
  input_call <- .rba_httr(
    httr = "get",
    url = .rba_stg("uniprot", "url"),
    path = paste0(.rba_stg("uniprot", "pth"), "proteomics/hpp/", accession),
    accept = "application/json",
    parser = "json->list",
    save_to = .rba_file("uniprot_hpp.json")
  )

  ## Call API
  final_output <- .rba_skeleton(input_call)
  return(final_output)
}
